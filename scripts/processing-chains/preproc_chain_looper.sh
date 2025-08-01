#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/processing-chains/preproc_chain_looper.sh
#=============================================================================
# DESCRIPTION:
#   Loops over multiple hurricane segments and submits preprocessing chains.
#
# USAGE:
#   ./preproc_chain_looper.sh start_segment end_segment -c|--config config_file
#
# ARGUMENTS:
#   start_segment  - First segment number to process
#   end_segment    - Last segment number to process
#
# OPTIONS:
#   -c, --config   - Path to TOML configuration file (required)
#                    Relative paths are resolved from script directory
#   -h, --help     - Show this help message
#
# EXAMPLES:
#   ./preproc_chain_looper.sh 1 8 -c ../../config/hurricane_config.toml
#   ./preproc_chain_looper.sh 3 7 -c ../../config/hurricane_config.toml
#
# AUTHOR:
#   GitHub Copilot
#
# DATE:
#   June 2025
#=============================================================================

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
echo "Script directory: ${SCRIPT_DIR}"

# Load shared configuration handler
source "${SCRIPT_DIR}/../../utilities/config_handler.sh"

# Parse config argument (but don't load config - just resolve path)
CONFIG_ARG=$(parse_config_argument "$@")

# Handle config path resolution without loading
if [[ -z "$CONFIG_ARG" ]]; then
    echo "Error: Config file is required"
    echo "Usage: $0 start_segment end_segment -c|--config config_file"
    exit 1
fi

CONFIG_FILE_ABS=$(readlink -f "$CONFIG_ARG")
echo "Config file: $CONFIG_FILE_ABS"

# Remove config arguments and parse remaining arguments
REMAINING_ARGS=($(remove_config_args "$@"))

# Parse arguments
start_segment=""
end_segment=""

for arg in "${REMAINING_ARGS[@]}"; do
    case $arg in
        -h|--help)
            echo "Usage: $0 start_segment end_segment -c|--config config_file"
            echo ""
            echo "Arguments:"
            echo "  start_segment     First segment number to process"
            echo "  end_segment       Last segment number to process"
            echo ""
            echo "Options:"
            show_config_help
            echo ""
            echo "Examples:"
            echo "  $0 1 8 -c ../../config/hurricane_config.toml"
            echo "  $0 3 7 -c ../../config/hurricane_config.toml"
            exit 0
            ;;
        -*)
            echo "Error: Unknown option $arg"
            echo "Use --help for usage information"
            exit 1
            ;;
        *)
            if [[ -z "$start_segment" ]]; then
                start_segment="$arg"
            elif [[ -z "$end_segment" ]]; then
                end_segment="$arg"
            else
                echo "Error: Too many positional arguments"
                echo "Usage: $0 start_segment end_segment -c|--config config_file"
                exit 1
            fi
            ;;
    esac
done

# Validate inputs
if [[ -z "$start_segment" ]] || [[ -z "$end_segment" ]]; then
    echo "Error: Both start_segment and end_segment are required"
    echo "Usage: $0 start_segment end_segment -c|--config config_file"
    exit 1
fi

if ! [[ "$start_segment" =~ ^[0-9]+$ ]]; then
    echo "Error: start_segment must be a non-negative integer"
    exit 1
fi

if ! [[ "$end_segment" =~ ^[0-9]+$ ]]; then
    echo "Error: end_segment must be a non-negative integer"
    exit 1
fi

if [ "$start_segment" -gt "$end_segment" ]; then
    echo "Error: start_segment ($start_segment) cannot be greater than end_segment ($end_segment)"
    exit 1
fi

echo "Processing segments: $start_segment to $end_segment"
echo "============================================================================="

# Arrays to store job IDs
declare -a grid_jobs
declare -a extpar_jobs
declare -a ic_jobs
declare -a bc_jobs
declare -a testrun_jobs

# Loop over segments
for ((iseg = start_segment; iseg <= end_segment; iseg++)); do
    echo "Submitting preprocessing chain for segment $iseg..."
    
    # Submit the preprocessing chain for this segment
    chain_output=$(bash ./run_hurricane_segments_preproc_chain.sh "$iseg" -c "$CONFIG_FILE_ABS" 2>&1)
    
    if [ $? -ne 0 ]; then
        echo "Error: Failed to submit preprocessing chain for segment $iseg"
        echo "$chain_output"
        exit 1
    fi
    
    # Extract and store job IDs
    grid_job=$(echo "$chain_output" | grep "Grid job submitted with ID:" | awk '{print $NF}')
    extpar_job=$(echo "$chain_output" | grep "Extpar job submitted with ID:" | awk '{print $NF}')
    ic_job=$(echo "$chain_output" | grep "IC job submitted with ID:" | awk '{print $NF}')
    bc_job=$(echo "$chain_output" | grep "BC job submitted with ID:" | awk '{print $NF}')
    testrun_job=$(echo "$chain_output" | grep "Testrun job submitted with ID:" | awk '{print $NF}')
    
    grid_jobs[$iseg]="$grid_job"
    extpar_jobs[$iseg]="$extpar_job"
    ic_jobs[$iseg]="$ic_job"
    bc_jobs[$iseg]="$bc_job"
    testrun_jobs[$iseg]="$testrun_job"
    
    echo "  Grid: $grid_job | Extpar: $extpar_job | IC: $ic_job | BC: $bc_job | Testrun: $testrun_job"
done

echo "============================================================================="
echo "All segments submitted successfully!"
echo "Use 'squeue -u $USER' to monitor job status"