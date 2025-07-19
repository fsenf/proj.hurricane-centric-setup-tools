#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/processing-chains/run_hurricane_production_chain.sh
#=============================================================================
# DESCRIPTION:
#   This script runs the hurricane production chain for a given segment.
#   It orchestrates the complete workflow by:
#   1. Running remap and merge operations on IC/BC files
#   2. Creating the production runscript
#   3. Submitting the production job with proper dependency chaining
#
# USAGE:
#   ./run_hurricane_production_chain.sh [segment_number] [-c|--config config_file] [--nodes N] [--time HH:MM:SS] [--dependency TYPE]
#
# ARGUMENTS:
#   segment_number  - The hurricane segment number to check
#
# OPTIONS:
#   -c, --config      - Path to TOML configuration file (required)
#   --nodes=N         - Number of compute nodes (default: 20)
#   --time=HH:MM:SS   - Job time limit (default: 01:00:00)
#   --dependency=TYPE - Job dependency specification for first sbatch (default: none)
#   -h, --help        - Show this help message
#
# EXIT CODES:
#   0 - Success, production chain completed successfully
#   1 - Invalid arguments or configuration
#   2 - Missing required files or configuration errors
#   3 - Job submission failures
#   4 - Runscript creation failed
#
# AUTHOR:
#   Fabian Senf (senf@tropos.de)
#
# DATE:
#   June 2025
#=============================================================================
#SBATCH --job-name=production_chain  # Specify job name
#SBATCH --partition=shared           # Specify partition name
#SBATCH --mem=2G                     # Specify amount of memory needed
#SBATCH --time=00:05:00              # Set a limit on the total run time
#SBATCH --account=bb1376             # Charge resources on this project account
#SBATCH --output=../LOG/slurm-%x-%j.out


#=============================================================================
# Configuration and Argument Parsing
#=============================================================================

# Get script directory - handle both SLURM and direct execution
ORIGINAL_SCRIPT_DIR="${SLURM_SUBMIT_DIR}"

if [[ -z "$ORIGINAL_SCRIPT_DIR" ]]; then
    ORIGINAL_SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
fi

SCRIPT_DIR=${ORIGINAL_SCRIPT_DIR}

echo "Script directory: ${ORIGINAL_SCRIPT_DIR}"

# Load shared configuration handler
source "${SCRIPT_DIR}/../../utilities/config_handler.sh"

# Parse config argument
CONFIG_ARG=$(parse_config_argument "$@")

# Handle configuration loading
if [[ -z "$CONFIG_ARG" ]]; then
    echo "Error: Config file is required"
    echo "Usage: $0 [segment_number] -c|--config config_file"
    exit 1
fi

handle_config "$SCRIPT_DIR" "" "$CONFIG_ARG"
echo "Using config file: $CONFIG_FILE"

# Load TOML reader
source "${SCRIPT_DIR}/../../utilities/toml_reader.sh"
read_toml_config "$CONFIG_FILE"

# Remove config arguments and parse remaining arguments
REMAINING_ARGS=($(remove_config_args "$@"))
iseg=""
slurm_options=()

# Default SLURM parameters
nodes=64
ctime="08:00:00"
dependency=""

for arg in "${REMAINING_ARGS[@]}"; do
    case $arg in
        -h|--help)
            echo "Usage: $0 [segment_number] -c|--config config_file [--nodes N] [--time HH:MM:SS] [--dependency TYPE]"
            echo ""
            echo "Arguments:"
            echo "  segment_number    Hurricane segment number to check"
            echo ""
            show_config_help
            echo ""
            echo "SLURM Options:"
            echo "  --nodes=N         Number of compute nodes (default: 20)"
            echo "  --time=HH:MM:SS   Job time limit (default: 01:00:00)"
            echo "  --dependency=TYPE Job dependency specification for first sbatch (default: none)"
            echo ""
            echo "Examples:"
            echo "  $0 5 -c ../../config/hurricane_config.toml"
            echo "  $0 5 -c ../../config/hurricane_config_width100km_reinit12h.toml --nodes=50 --time=02:00:00"
            echo "  $0 5 -c ../../config/hurricane_config.toml --dependency=afterok:12345"
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
            if [[ -z "$iseg" ]]; then
                iseg="$arg"
            else
                echo "Error: Too many positional arguments"
                exit 1
            fi
            ;;
    esac
done

#------------------------------------------------------------------------------
# Validation
#------------------------------------------------------------------------------
if [[ -z "$iseg" ]]; then
    echo "Error: segment_number is required"
    echo "Usage: $0 [segment_number] -c|--config config_file [--nodes N] [--time HH:MM:SS] [--dependency TYPE]"
    exit 1
fi

if ! [[ "$iseg" =~ ^[0-9]+$ ]]; then
    echo "Error: segment_number (iseg) must be a number."
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
            echo "Using custom dependency: $dependency"
            ;;
    esac
done

echo "SLURM configuration:"
echo "  Nodes: $nodes"
echo "  Time: $ctime"
if [[ -n "$dependency" ]]; then
    echo "  Dependency: $dependency"
fi

#=============================================================================
# Submit Remap and Merge Job
#=============================================================================
echo "Changing to ic-bc directory..."
cd "${SCRIPT_DIR}/../ic-bc" 

# Prepare sbatch command for merge job with optional dependency
merge_sbatch_cmd="sbatch --parsable"
if [[ -n "$dependency" ]]; then
    merge_sbatch_cmd="$merge_sbatch_cmd --dependency=$dependency"
fi
merge_sbatch_cmd="$merge_sbatch_cmd remap_and_merge_runner.sh $iseg -c $CONFIG_FILE"

echo "Submitting merge job with command:"
echo "$merge_sbatch_cmd"

merge_job=$(eval "$merge_sbatch_cmd")


#=============================================================================
# Create and Submit Production Runscript
#=============================================================================

echo "Changing to runscripts directory..."
cd "${SCRIPT_DIR}/../runscripts" 

echo -e "\nStarting creation of production script"
# Capture output from create_runscript.sh
creator_output=$(bash ./create_runscript.sh $iseg -c $CONFIG_FILE)
echo "$creator_output"

# Parse the output to extract runscript filename (assuming it contains "exp.")
production_runscript=$(echo "$creator_output" | grep -o "exp\.[^[:space:]]*" | head -n 1)

if [ -z "$production_runscript" ]; then
    echo -e "\n❌ ERROR: Could not detect runscript filename in create_runscript.sh output"
    exit 4
fi

echo -e "\nRunscript detected: $production_runscript"
echo "Submitting production run to queue..."

# Submit the production job using starter.sh with dependency on merge job
bash starter.sh --dependency=afterok:$merge_job --nodes=$nodes --time=$ctime $production_runscript

exit 0