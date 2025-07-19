#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/processing-chains/production_looper.sh
#=============================================================================
# DESCRIPTION:
#   Hurricane production chain looper script that runs multiple segments
#   sequentially with proper dependency chaining between segments.
#
# USAGE:
#   ./production_looper.sh start_segment end_segment [options] [slurm_options]
#
# ARGUMENTS:
#   start_segment   - First segment number to process
#   end_segment     - Last segment number to process
#
# OPTIONS:
#   -c, --config    - Path to TOML configuration file (required)
#                     Relative paths are resolved from script directory
#   -h, --help      - Show this help message
#
# SLURM OPTIONS:
#   --nodes=N         - Number of compute nodes (default: 64)
#   --time=HH:MM:SS   - Job time limit (default: 08:00:00)
#   --dependency=TYPE - Job dependency specification for first segment (default: none)
#
# EXAMPLES:
#   ./production_looper.sh 1 5 -c ../../config/hurricane_config.toml
#   ./production_looper.sh 1 3 -c ../../config/hurricane_config.toml --nodes=128 --time=12:00:00
#   ./production_looper.sh 2 4 -c ../../config/hurricane_config.toml --dependency=afterok:12345
#
#=============================================================================

set -e

#=============================================================================
# Configuration and Argument Parsing
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
    echo "Usage: $0 start_segment end_segment -c|--config config_file [options]"
    exit 1
fi

CONFIG_FILE_ABS=$(readlink -f "$CONFIG_ARG")
echo "Config file: $CONFIG_FILE_ABS"

# Remove config arguments and parse remaining arguments
REMAINING_ARGS=($(remove_config_args "$@"))

# Parse arguments
start_segment=""
end_segment=""
slurm_options=()

# Default SLURM parameters (similar to starter.sh)
nodes=64
ctime="08:00:00"
dependency=""

for arg in "${REMAINING_ARGS[@]}"; do
    case $arg in
        -h|--help)
            echo "Usage: $0 start_segment end_segment [options] [slurm_options]"
            echo ""
            echo "Arguments:"
            echo "  start_segment     First segment number to process"
            echo "  end_segment       Last segment number to process"
            echo ""
            echo "Options:"
            show_config_help
            echo ""
            echo "SLURM Options:"
            echo "  --nodes=N         Number of compute nodes (default: 64)"
            echo "  --time=HH:MM:SS   Job time limit (default: 08:00:00)"
            echo "  --dependency=TYPE Job dependency specification for first segment (default: none)"
            echo ""
            echo "Examples:"
            echo "  $0 1 5 -c ../../config/hurricane_config.toml"
            echo "  $0 1 3 -c ../../config/hurricane_config.toml --nodes=128 --time=12:00:00"
            echo "  $0 2 4 -c ../../config/hurricane_config.toml --dependency=afterok:12345"
            exit 0
            ;;
        --nodes=*|--time=*|--dependency=*)
            # These are SLURM options
            slurm_options+=("$arg")
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
                echo "Usage: $0 start_segment end_segment [options] [slurm_options]"
                exit 1
            fi
            ;;
    esac
done

#=============================================================================
# Validation
#=============================================================================

# Validate required arguments
if [[ -z "$start_segment" ]]; then
    echo "Error: start_segment is required"
    echo "Usage: $0 start_segment end_segment [options] [slurm_options]"
    exit 1
fi

if [[ -z "$end_segment" ]]; then
    echo "Error: end_segment is required"
    echo "Usage: $0 start_segment end_segment [options] [slurm_options]"
    exit 1
fi

# Validate segment numbers
if ! [[ "$start_segment" =~ ^[0-9]+$ ]]; then
    echo "Error: start_segment must be a number"
    exit 1
fi

if ! [[ "$end_segment" =~ ^[0-9]+$ ]]; then
    echo "Error: end_segment must be a number"
    exit 1
fi

if [[ $start_segment -gt $end_segment ]]; then
    echo "Error: start_segment ($start_segment) must be <= end_segment ($end_segment)"
    exit 1
fi

# Override defaults with any provided SLURM options
for option in "${slurm_options[@]}"; do
    case "$option" in
        --nodes=*)
            nodes="${option#*=}"
            echo "Using custom node count: $nodes"
            ;;
        --time=*)
            ctime="${option#*=}"
            echo "Using custom time limit: $ctime"
            ;;
        --dependency=*)
            dependency="${option#*=}"
            echo "Using custom initial dependency: $dependency"
            ;;
    esac
done

echo "Production Loop Configuration:"
echo "  Segments: $start_segment to $end_segment"
echo "  Config: $CONFIG_FILE_ABS"
echo "  Nodes: $nodes"
echo "  Time: $ctime"
if [[ -n "$dependency" ]]; then
    echo "  Initial Dependency: $dependency"
fi

#=============================================================================
# Production Chain Loop
#=============================================================================

echo ""
echo "Starting production chain loop..."
echo "========================================"

current_dependency="$dependency"

for iseg in $(seq $start_segment $end_segment); do
    echo ""
    echo "Processing segment $iseg..."
    echo "----------------------------------------"
    
    # Build production chain command
    chain_cmd="bash ${SCRIPT_DIR}/../processing-chains/run_hurricane_production_chain.sh $iseg -c $CONFIG_FILE_ABS --nodes=$nodes --time=$ctime"
    
    # Add dependency if we have one
    if [[ -n "$current_dependency" ]]; then
        chain_cmd="$chain_cmd --dependency=$current_dependency"
        echo "Using dependency: $current_dependency"
    else
        echo "No dependency for this segment"
    fi
    
    echo "Executing: $chain_cmd"
    echo ""
    
    # Execute the production chain and capture output
    chain_output=$(eval "$chain_cmd" 2>&1)
    chain_exit_code=$?
    
    echo "$chain_output"
    
    # Check if the command succeeded
    if [[ $chain_exit_code -ne 0 ]]; then
        echo "❌ ERROR: Production chain failed for segment $iseg (exit code: $chain_exit_code)"
        exit $chain_exit_code
    fi
    
    # Extract job ID from the output (look for "Submitted batch job XXXXXX")
    job_id=$(echo "$chain_output" | grep -o "Submitted batch job [0-9]*" | grep -o "[0-9]*" | tail -n 1)
    
    if [[ -z "$job_id" ]]; then
        echo "⚠️  WARNING: Could not extract job ID from output for segment $iseg"
        echo "Next segment will run without dependency"
        current_dependency=""
    else
        echo "✅ Successfully submitted segment $iseg with job ID: $job_id"
        # Set dependency for next iteration
        current_dependency="afterok:$job_id"
        echo "Next segment will depend on: $current_dependency"
    fi
    
    echo "----------------------------------------"
done

echo ""
echo "========================================"
echo "✅ Production chain loop completed successfully!"
echo "Processed segments: $start_segment to $end_segment"
echo "========================================"

exit 0
