#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/processing-chains/run_hurricane_testrun_chain.sh
#=============================================================================
# DESCRIPTION:
#   This script checks if all required files for a hurricane segment have been
#   properly created before allowing runscript creation. It verifies:
#   1. Grid files
#   2. External parameter files (extpar)
#   3. Initial condition files (IC)
#   4. Boundary condition files (BC)
#
# USAGE:
#   ./run_hurricane_testrun_chain.sh [segment_number] [-c|--config config_file] [--nodes N] [--account ACCOUNT]
#
# ARGUMENTS:
#   segment_number  - The hurricane segment number to check
#
# OPTIONS:
#   -c, --config    - Path to TOML configuration file (required)
#   --nodes=N       - Number of compute nodes (default: 20)
#   --account=ACCOUNT - SLURM account name (default: $SBATCH_ACCOUNT)
#   -h, --help      - Show this help message
#
# EXIT CODES:
#   0 - Success, all files exist and are valid
#   1 - Invalid arguments
#   2 - Missing or invalid files
#   3 - Insufficient file count
#
# AUTHOR:
#   Fabian Senf (senf@tropos.de)
#
# DATE:
#   June 2025
#=============================================================================

#=============================================================================
# Platform Detection and Module Loading
#=============================================================================

# Get script directory

ORIGINAL_SCRIPT_DIR="${SLURM_SUBMIT_DIR}"

if [[ -z "$ORIGINAL_SCRIPT_DIR" ]]; then
    ORIGINAL_SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
fi

SCRIPT_DIR=${ORIGINAL_SCRIPT_DIR}
echo "Script directory: ${ORIGINAL_SCRIPT_DIR}"

# Detect platform and load platform-specific modules
PLATFORM=$("${SCRIPT_DIR}/../../utilities/detect_platform.sh")
echo "Detected platform: ${PLATFORM}"
echo "Hostname: $(hostname)"

# Load platform-specific modules
module_loader_path="${SCRIPT_DIR}/../../config/${PLATFORM}/module_loader.sh"
if [[ -f "$module_loader_path" ]]; then
    echo "Loading modules for platform: ${PLATFORM}"
    source "$module_loader_path"
else
    echo "Warning: No module loader found for platform ${PLATFORM} at ${module_loader_path}"
fi

#=============================================================================
# Configuration and Argument Parsing
#=============================================================================




# Load SLURM environment variables
source "$SCRIPT_DIR/../../config/${PLATFORM}/sbatch_env_setter.sh" "testrun"

# Load shared configuration handler
source "${SCRIPT_DIR}/../../utilities/config_handler.sh"

# Parse config argument
CONFIG_ARG=$(parse_config_argument "$@")

# Handle configuration loading
if [[ -z "$CONFIG_ARG" ]]; then
    echo "Error: Config file is required"
    echo "Usage: $0 [segment_number] -c|--config config_file [--nodes N] [--account ACCOUNT]"
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
nodes="$SBATCH_NODES"
time="$SBATCH_TIME"
account="$SBATCH_ACCOUNT"

for arg in "${REMAINING_ARGS[@]}"; do
    case $arg in
        -h|--help)
            echo "Usage: $0 [segment_number] -c|--config config_file [--nodes N] [--account ACCOUNT]"
            echo ""
            echo "Arguments:"
            echo "  segment_number    Hurricane segment number to check"
            echo ""
            show_config_help
            echo ""
            echo "SLURM Options:"
            echo "  --nodes=N         Number of compute nodes (default: 20)"
            echo "  --account=ACCOUNT SLURM account name (default: \$SBATCH_ACCOUNT)"
            echo ""
            echo "Examples:"
            echo "  $0 5 -c ../../config/hurricane_config.toml"
            echo "  $0 5 -c ../../config/hurricane_config_width100km_reinit12h.toml --nodes=30"
            echo "  $0 5 -c ../../config/hurricane_config.toml --account=myproject"
            exit 0
            ;;
        --nodes=*)
            # SLURM option
            slurm_options+=("$arg")
            ;;
        --account=*)
            # SLURM option
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
    echo "Usage: $0 [segment_number] -c|--config config_file [--nodes N] [--account ACCOUNT]"
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
        --account=*)
            account="${option#*=}"
            echo "Using custom account: $account"
            ;;
    esac
done

echo "Checking segment: $iseg"

#=============================================================================
# Determine required file count based on configuration
#=============================================================================

# Extract segment_reinit_hours from config to determine if it's 12h or 24h
segment_reinit_hours=${DOMAINS_SEGMENT_REINIT_HOURS}
echo "Segment reinit hours: $segment_reinit_hours"

# Convert to integer (properly handling floating point values)
reinit_hours_int=$(printf "%.0f" "$segment_reinit_hours")
nnests=$(printf "%.0f" "$DOMAINS_NESTS")

# Calculate minimum required files: reinit_hours + 3 * nests
# This accounts for:
# - BC files (~reinit_hours files)
# - Grid files (nnests files)
# - Extpar files (nnests files)  
# - IC files (nnests files)
min_required_files=$((reinit_hours_int + 1 + 3 * nnests))
echo "Using configuration with ${reinit_hours_int}h reinit and ${nnests} nests, minimum required files: $min_required_files"

#=============================================================================
# Run check for all file types
#=============================================================================


echo -e "\nRunning file validation check..."
check_output=$(python "${SCRIPT_DIR}/../../utilities/check_preprocessing_files.py" "$CONFIG_FILE" "$iseg" all)
check_status=$?

# Print the output for the user to see
echo "$check_output"

# Check for errors
if [ $check_status -ne 0 ]; then
    echo -e "\n❌ ERROR: File validation failed. Please fix the issues before proceeding."
    exit 2
fi

# Check for invalid files
invalid_files=$(echo "$check_output" | grep "Invalid files:" | tail -n 1 | awk '{print $3}')

if [ -z "$invalid_files" ]; then
    echo -e "\n❌ ERROR: Could not determine the number of invalid files."
    exit 2
fi

if [ "$invalid_files" -gt 0 ]; then
    echo -e "\n❌ ERROR: Found $invalid_files invalid files."
    echo "Please fix all missing or invalid files before proceeding."
    exit 2
fi

# Check if we have enough valid files
valid_files=$(echo "$check_output" | grep "Valid files:" | tail -n 1 | awk '{print $3}')

if [ -z "$valid_files" ]; then
    echo -e "\n❌ ERROR: Could not determine the number of valid files."
    exit 2
fi

if [ "$valid_files" -lt "$min_required_files" ]; then
    echo -e "\n❌ ERROR: Insufficient files available."
    echo "Found $valid_files valid files, but $min_required_files are required for segment_reinit_hours=$segment_reinit_hours"
    exit 3
fi
echo -e "\n✅ All checks passed! Found $valid_files valid files."

#=============================================================================
# All checks passed, change to runscripts directory
#=============================================================================

echo "Changing to runscripts directory..."
cd "${SCRIPT_DIR}/../runscripts" 

echo -e "\nStarting creation of testrun script"
# Capture output from create_runscript.sh
creator_output=$(bash ./create_runscript.sh $iseg -c $CONFIG_FILE -t)
echo "$creator_output"

# Parse the output to extract runscript filename (assuming it contains "exp.")
test_runscript=$(echo "$creator_output" | grep -o "exp\.[^[:space:]]*" | head -n 1)

if [ -z "$test_runscript" ]; then
    echo -e "\n❌ ERROR: Could not detect runscript filename in create_runscript.sh output"
    exit 4
fi

echo -e "\nRunscript detected: $test_runscript"
echo "Submitting test run to queue..."

# Run the job using starter.sh with specified parameters
bash starter.sh --nodes=$nodes --time=$itme --account=$account $test_runscript

exit 0
