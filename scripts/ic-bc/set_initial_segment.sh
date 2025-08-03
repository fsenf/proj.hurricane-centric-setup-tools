#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/ic-bc/set_initial_segment.sh
#=============================================================================
# DESCRIPTION:
#   Set up initial segment by copying LAM input files from test run to IC/BC directory.
#   Validates that test run completed successfully before copying files.
#
# USAGE:
#   ./set_initial_segment.sh [segment_number] [-c|--config config_file]
#
# ARGUMENTS:
#   segment_number  - Hurricane segment number to process
#
# OPTIONS:
#   -c, --config    - Path to TOML configuration file (optional)
#                     Default: ../../config/hurricane_config.toml
#                     Relative paths are resolved from script directory
#   -h, --help      - Show this help message
#
# DEPENDENCIES:
#   - ../../utilities/config_handler.sh
#   - ../../utilities/toml_reader.sh
#   - ../../utilities/print_timings.py
#   - ../../config/hurricane_config.toml
#
#=============================================================================

set -e
ulimit -s unlimited
ulimit -c 0

#=============================================================================
# Configuration and Argument Parsing
#=============================================================================

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
echo "Script directory: ${SCRIPT_DIR}"

# Load shared configuration handler
source "${SCRIPT_DIR}/../../utilities/config_handler.sh"

# Parse config argument
CONFIG_ARG=$(parse_config_argument "$@")

# Handle configuration loading
handle_config "$SCRIPT_DIR" \
              "${SCRIPT_DIR}/../../config/hurricane_config.toml" \
              "$CONFIG_ARG"

# Remove config arguments and parse remaining arguments
REMAINING_ARGS=($(remove_config_args "$@"))

# Parse segment number
iseg=""
for arg in "${REMAINING_ARGS[@]}"; do
    case $arg in
        -h|--help)
            echo "Usage: $0 [segment_number] [options]"
            echo ""
            echo "Arguments:"
            echo "  segment_number    Hurricane segment number to process"
            echo ""
            echo "Description:"
            echo "  Checks if test run completed successfully and copies LAM input files"
            echo "  to the IC/BC directory for production runs."
            echo ""
            show_config_help
            exit 0
            ;;
        -*)
            echo "Error: Unknown option $arg"
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

# Validate segment number
if [[ -z "$iseg" ]]; then
    echo "Error: segment_number is required"
    exit 1
fi

if ! [[ "$iseg" =~ ^[0-9]+$ ]]; then
    echo "Error: segment_number must be a positive integer"
    exit 1
fi

echo "Processing segment: $iseg"

# Load python module for timestamp calculation
module load python3 2>/dev/null || true

# Get timestamp for this segment
TIMESTAMP=$(python3 "${SCRIPT_DIR}/../../utilities/print_timings.py" "$CONFIG_FILE" "$iseg" "INIT_DATE")
if [ $? -ne 0 ] || [ -z "$TIMESTAMP" ]; then
    echo "Error: Could not calculate timestamp for segment $iseg"
    exit 1
fi

# Convert YYYYMMDDTHHMMZ to YYYYMMDDTHHMMSSZ (add seconds)
TIMESTAMP_WITH_SECONDS="${TIMESTAMP/Z/00Z}"

echo "Segment timestamp: $TIMESTAMP"
echo "Timestamp with seconds: $TIMESTAMP_WITH_SECONDS"

#=============================================================================
# Define paths and variables
#=============================================================================

# Set up domain name and paths
DOMNAME="${PROJECT_NAME}/seg${iseg}_${PROJECT_WIDTH_CONFIG}"
ICBC_DIR="${OUTPUT_ICBC_BASEDIR}/${DOMNAME}"

# Check if IC/BC directory exists, create if needed
if [ ! -d "${ICBC_DIR}" ]; then
    echo "Creating IC/BC directory: ${ICBC_DIR}"
    mkdir -p "${ICBC_DIR}"
fi

#=============================================================================
# Find experiment directory
#=============================================================================

# Pattern matching the experiment directory like in remap_and_merge_runner.sh
TOOLS_ICON_BUILD_DIR="${TOOLS_ICON_BUILD_DIR}"
EXPERIMENT_BASE_DIR="${TOOLS_ICON_BUILD_DIR}/experiments"
EXPERIMENT_PATTERN="${EXPERIMENT_BASE_DIR}/${PROJECT_NAME}-${PROJECT_WIDTH_CONFIG}-segment${iseg}-????????T????Z-exp*"

echo "Looking for experiment directory with pattern: $EXPERIMENT_PATTERN"
EXPERIMENT_DIRS=($EXPERIMENT_PATTERN)

if [[ ${#EXPERIMENT_DIRS[@]} -eq 0 ]]; then
    echo "Error: No experiment directory found matching pattern: $EXPERIMENT_PATTERN"
    echo "This suggests no test run was performed for segment $iseg"
    exit 1
elif [[ ${#EXPERIMENT_DIRS[@]} -gt 1 ]]; then
    echo "Warning: Multiple experiment directories found:"
    printf "  %s\n" "${EXPERIMENT_DIRS[@]}"
    echo "Using the first one: ${EXPERIMENT_DIRS[0]}"
fi

EXPERIMENT_DIR="${EXPERIMENT_DIRS[0]}"
echo "Found experiment directory: $EXPERIMENT_DIR"

#=============================================================================
# Check for successful test run
#=============================================================================

echo ""
echo "=== Checking for successful test run ==="

# Look for LAM input files that should have been created by test run
# Pattern: lam_input_IC_DOM0X_ML_YYYYMMDDTHHMMSSZ.nc
LAM_FILES_FOUND=0
MISSING_DOMAINS=()
LAM_FILES=()

for idom in $(seq 1 ${DOMAINS_NESTS}); do
    LAM_FILE="${EXPERIMENT_DIR}/lam_input_IC_DOM0${idom}_ML_${TIMESTAMP_WITH_SECONDS}.nc"
    
    echo "Checking for domain $idom: $(basename $LAM_FILE)"
    
    if [ -f "${LAM_FILE}" ]; then
        echo "✓ Found LAM input file for domain $idom"
        LAM_FILES_FOUND=$((LAM_FILES_FOUND + 1))
        LAM_FILES+=("${LAM_FILE}")
    else
        echo "✗ Missing LAM input file for domain $idom: $LAM_FILE"
        MISSING_DOMAINS+=(${idom})
    fi
done

if [ ${LAM_FILES_FOUND} -eq 0 ]; then
    echo "Error: No LAM input files found for segment ${iseg}"
    echo "Expected pattern: lam_input_IC_DOM0X_ML_${TIMESTAMP_WITH_SECONDS}.nc"
    echo "Please ensure test run completed successfully before running this script."
    exit 1
elif [ ${#MISSING_DOMAINS[@]} -gt 0 ]; then
    echo "Warning: Missing LAM input files for domains: ${MISSING_DOMAINS[*]}"
    echo "Found ${LAM_FILES_FOUND} out of ${DOMAINS_NESTS} expected domains"
    read -p "Continue anyway? (y/N): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Aborted by user"
        exit 1
    fi
else
    echo "✓ All ${LAM_FILES_FOUND} LAM input files found for ${DOMAINS_NESTS} domains"
fi

#=============================================================================
# Copy LAM input files to IC/BC directory
#=============================================================================

echo ""
echo "=== Copying LAM input files to IC/BC directory ==="

COPIED_FILES=0

# Copy each found LAM file to the IC/BC directory with proper naming
for idom in $(seq 1 ${DOMAINS_NESTS}); do
    SOURCE_FILE="${EXPERIMENT_DIR}/lam_input_IC_DOM0${idom}_ML_${TIMESTAMP_WITH_SECONDS}.nc"
    
    if [ ! -f "$SOURCE_FILE" ]; then
        echo "Skipping domain $idom (file not found)"
        continue
    fi
    
    # Target filename follows the pattern: YYYYMMDDTHHMMZ_DOM0X_ini.nc
    TARGET_FILE="${ICBC_DIR}/${TIMESTAMP}_DOM0${idom}_ini.nc"
    
    echo "Copying domain $idom:"
    echo "  From: $(basename $SOURCE_FILE)"
    echo "  To:   $(basename $TARGET_FILE)"
    
    # Check if target file already exists
    if [ -f "$TARGET_FILE" ]; then
        echo "  Warning: Target file already exists, overwriting..."
    fi
    
    # Copy the file
    if cp "$SOURCE_FILE" "$TARGET_FILE"; then
        echo "  ✓ Successfully copied"
        COPIED_FILES=$((COPIED_FILES + 1))
    else
        echo "  ✗ Failed to copy"
        exit 1
    fi
    
    echo ""
done

#=============================================================================
# Summary
#=============================================================================

echo "=== Summary ==="
echo "Segment: ${iseg}"
echo "Timestamp: ${TIMESTAMP}"
echo "Target directory: ${ICBC_DIR}"
echo "Files copied: ${COPIED_FILES} out of ${DOMAINS_NESTS} domains"

if [ ${COPIED_FILES} -eq 0 ]; then
    echo "Error: No files were copied successfully"
    exit 1
elif [ ${COPIED_FILES} -lt ${DOMAINS_NESTS} ]; then
    echo "Warning: Only ${COPIED_FILES} out of ${DOMAINS_NESTS} expected files were copied"
    echo "Some domains may be missing"
else
    echo "✓ Successfully set up initial segment ${iseg}"
    echo "Initial condition files are ready for production runs"
fi

echo ""
echo "Created files:"
for idom in $(seq 1 ${DOMAINS_NESTS}); do
    TARGET_FILE="${ICBC_DIR}/${TIMESTAMP}_DOM0${idom}_warmini.nc"
    if [ -f "$TARGET_FILE" ]; then
        echo "  ✓ $(basename $TARGET_FILE)"
    fi
done

#-----------------------------------------------------------------------------
exit
#-----------------------------------------------------------------------------
