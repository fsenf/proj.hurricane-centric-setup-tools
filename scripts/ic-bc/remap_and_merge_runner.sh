#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/ic-bc/remap_and_merge_runner.sh
#=============================================================================
# DESCRIPTION:
#   Handler script for remapping and merging IC files between hurricane segments.
#   This script processes initial conditions from a source segment and merges
#   them with background conditions for a target segment, creating blended
#   initial condition files for hurricane model warm starts.
#
#   The script automatically processes all configured domains and handles
#   file pattern resolution, remapping between different grid configurations,
#   and spatial blending of meteorological fields.
#
# USAGE:
#   ./remap_and_merge_runner.sh [to_segment] -c|--config config_file
#
# ARGUMENTS:
#   to_segment       Target segment number (source segment = to_segment - 1)
#
# OPTIONS:
#   -c, --config     Path to configuration file (TOML format)
#   -h, --help       Show this help message
#
# EXAMPLES:
#   # Process segment 3 using default config
#   ./remap_and_merge_runner.sh 3 -c ../../config/hurricane_config.toml
#
#   # Show help
#   ./remap_and_merge_runner.sh --help
#
# REQUIREMENTS:
#   - CDO (Climate Data Operators) module
#   - Python3 module for grid blending
#   - Configuration file with experiment paths and domain settings
#   - Input IC files from source segment
#   - Background IC files for target segment
#   - Grid files for both source and target segments
#
# OUTPUT:
#   Creates blended initial condition files in the specified output directory:
#   ${OUTPUT_ICBC_BASEDIR}/${project_name}/seg${to_segment}_${project_width_config}/
#   
#   Filename format: ${YYYYMMDDTHHMMZ}_DOM0${domain}_warmini.nc
#=============================================================================
# Levante cpu batch job parameters
#
#SBATCH --account=bb1376
#SBATCH --job-name=remap-and-merge
#SBATCH --partition=shared
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --distribution=block:block
#SBATCH --mem=30G
#SBATCH --time=02:00:00
#SBATCH --output=../LOG/slurm-%x-%j.out
#=============================================================================


#=============================================================================
# Configuration and Argument Parsing
#=============================================================================

# Get script directory

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
handle_config "$SCRIPT_DIR" \
              "${SCRIPT_DIR}/../../config/hurricane_config.toml" \
              "$CONFIG_ARG"

echo "Using config file: $CONFIG_FILE"

# Load TOML reader
source "${SCRIPT_DIR}/../../utilities/toml_reader.sh"
read_toml_config "$CONFIG_FILE"

# Remove config arguments and parse remaining arguments
REMAINING_ARGS=($(remove_config_args "$@"))

# Parse remaining arguments
to_iseg=""

for arg in "${REMAINING_ARGS[@]}"; do
    case $arg in
        -h|--help)
            echo "Usage: $0 [to_segment] -c|--config config_file"
            echo ""
            echo "Arguments:"
            echo "  to_segment       Target segment number"
            echo ""
            show_config_help
            echo ""
            echo "Examples:"
            echo "  $0 3 -c ../../config/hurricane_config.toml"
            exit 0
            ;;
        -*)
            echo "Error: Unknown option $arg"
            echo "Use --help for usage information"
            exit 1
            ;;
        *)
            if [[ -z "$to_iseg" ]]; then
                to_iseg="$arg"
            else
                echo "Error: Too many positional arguments"
                exit 1
            fi
            ;;
    esac
done

# Validate arguments
if [[ -z "$to_iseg" ]]; then
    echo "Error: to_segment is required"
    echo "Usage: $0 [to_segment] -c|--config config_file"
    exit 1
fi

if ! [[ "$to_iseg" =~ ^[0-9]+$ ]]; then
    echo "Error: to_segment must be a number"
    exit 1
fi

from_iseg=$((to_iseg - 1))

# Calculate segment start time for naming
module load python3
segment_start_time=$(python3 "${SCRIPT_DIR}/../../utilities/print_timings.py" "$CONFIG_FILE" "$to_iseg" "INIT_DATE")

echo "Processing from segment $from_iseg to segment $to_iseg"
echo "Target segment start time: $segment_start_time"

#=============================================================================
# FUNCTIONS
#=============================================================================

# Function to resolve file pattern and ensure exactly one match
resolve_unique_file() {
    local pattern="$1"
    local files=($pattern)
    
    if [[ ${#files[@]} -eq 0 ]]; then
        echo "Error: No files found matching pattern: $pattern" >&2
        return 1
    elif [[ ${#files[@]} -gt 1 ]]; then
        echo "Error: Multiple files found matching pattern: $pattern" >&2
        echo "Found files:" >&2
        printf "  %s\n" "${files[@]}" >&2
        return 2
    fi
    
    echo "${files[0]}"
    return 0
}

#=============================================================================
# Configuration from TOML file
#=============================================================================

# Use variables from config file
tools_icon_build_dir="${TOOLS_ICON_BUILD_DIR}"
project_name="${PROJECT_NAME}"
project_width_config="${PROJECT_WIDTH_CONFIG}"
experiment_dir="${tools_icon_build_dir}/experiments"

echo "Configuration:"
echo "  Tools directory: $tools_icon_build_dir"
echo "  Project name: $project_name"
echo "  Width config: $project_width_config"
echo "  Experiment directory: $experiment_dir"
echo "  Processing ${DOMAINS_NESTS} domains"

#=============================================================================
# Domain Loop
#=============================================================================

# Loop through domains as specified in TOML config
for idom in $(seq 1 ${DOMAINS_NESTS}); do
    echo ""
    echo "========================================================"
    echo "Processing domain $idom of ${DOMAINS_NESTS}"
    echo "========================================================"
    
    #=============================================================================
    # File Pattern Resolution
    #=============================================================================

    # Define patterns with wildcards instead of hardcoded dates
    # ic_bg_pattern="${experiment_dir}/${project_name}-${project_width_config}-segment${to_iseg}-????????-exp108/IC_vertically_interpolated_DOM0${idom}.nc"
    
    ic_bg_dir="${experiment_dir}/${project_name}-${project_width_config}-segment${to_iseg}-????????T????Z-exp110"
    ic_bg_pattern="${ic_bg_dir}/lam_input_IC_DOM0${idom}_ML_????????T??????Z.nc"
    to_grid_pattern="${ic_bg_dir}/hurricane-paulette2020-segments-seg${to_iseg}_dom${idom}_DOM01.nc"
    
    ic_seg_dir="${experiment_dir}/${project_name}-${project_width_config}-segment${from_iseg}-????????T????Z-exp111"
    ic_seg_pattern="${ic_seg_dir}/lam_input_IC_DOM0${idom}_ML_????????T??????Z.nc"
    from_grid_pattern="${ic_seg_dir}/hurricane-paulette2020-segments-seg${from_iseg}_dom${idom}_DOM01.nc"

    # Resolve patterns to actual files
    echo "Resolving file patterns for domain $idom..."

    ic_bg_file=$(resolve_unique_file "$ic_bg_pattern")

    files_missing=".FALSE."

    if [[ ! -f "$ic_bg_file" ]]; then
        echo "Background IC file does not exist: $ic_bg_file"
        files_missing=".TRUE."
    fi

    ic_seg_file=$(resolve_unique_file "$ic_seg_pattern")
    if [[ ! -f "$ic_seg_file" ]]; then
        echo "Segment IC file does not exist: $ic_seg_file"
        files_missing=".TRUE."
    fi

    to_grid=$(resolve_unique_file "$to_grid_pattern")
    if [[ ! -f "$to_grid" ]]; then
        echo "Target grid file does not exist: $to_grid"
        files_missing=".TRUE."
    fi

    from_grid=$(resolve_unique_file "$from_grid_pattern")
    if [[ ! -f "$from_grid" ]]; then
        echo "Source grid file does not exist: $from_grid"
        files_missing=".TRUE."
    fi

    if [[ "files_missing" == ".TRUE." ]]; then
        echo "Skipping domain $idom"
        continue
    fi

    echo "Resolved files for domain $idom:"
    echo "  Background IC file: $ic_bg_file"
    echo "  Segment IC file: $ic_seg_file"
    echo "  Target grid file: $to_grid"
    echo "  Source grid file: $from_grid"

    # Use output directory from config
    output_dir="${OUTPUT_ICBC_BASEDIR}/${project_name}/seg${to_iseg}_${project_width_config}"
    output_file="${output_dir}/${segment_start_time}_DOM0${idom}_warmini.nc"

    echo "Output will be saved to: $output_file"

    # Make sure output directory exists
    mkdir -p "$output_dir"

    #=============================================================================
    # Execute remapping and merging
    #=============================================================================

    echo "Running remap and merge for domain $idom..."
    bash "${SCRIPT_DIR}/remap_and_merge_icfiles_for_warmstart.sh" \
        "${ic_seg_file}" \
        "${ic_bg_file}" \
        "${from_grid}" \
        "${to_grid}" \
        "${output_file}"

    exit_code=$?

    if [ $exit_code -eq 0 ]; then
        echo "✅ Domain $idom: Remap and merge completed successfully!"
        echo "Output file: $output_file"
    else
        echo "❌ Domain $idom: Remap and merge failed with exit code $exit_code"
        echo "Continuing with next domain..."
    fi
    
    echo "Completed domain $idom"
done

echo ""
echo "========================================================"
echo "All domains processed"
echo "========================================================"

