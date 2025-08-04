#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/runscripts/create_runscript.sh
#=============================================================================
# DESCRIPTION:
#   Creates a runscript for hurricane simulations by dynamically generating
#   the configurable parameters in lines 48-63, leaving the rest unchanged.
#
# USAGE:
#   ./create_runscript.sh [segment_number] [-t] [-c config_file] [-o output_file]
#
# ARGUMENTS:
#   segment_number  - Hurricane segment number to process
#
# OPTIONS:
#   -t, --test      - Enable test mode (single domain)
#   -c, --config    - Path to TOML configuration file
#   -o, --output    - Output filename
#   -h, --help      - Show this help message
#=============================================================================

set -e

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Parse arguments
iseg=""
test_mode=".FALSE."
test_flag=""
output_file=""
config_file="${SCRIPT_DIR}/../../config/hurricane_config.toml"

while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help)
            echo "Usage: $0 [segment_number] [-t] [-c config_file] [-o output_file]"
            exit 0
            ;;
        -t|--test)
            test_mode=".TRUE."
            test_flag="-t"
            shift
            ;;
        -c|--config)
            config_file="$2"
            shift 2
            ;;
        -o|--output)
            output_file="$2"
            shift 2
            ;;
        -*)
            echo "Error: Unknown option: $1" >&2
            exit 1
            ;;
        *)
            if [[ -z "$iseg" ]]; then
                iseg="$1"
            else
                echo "Error: Too many positional arguments" >&2
                exit 1
            fi
            shift
            ;;
    esac
done

# Validate arguments
if [[ -z "$iseg" ]]; then
    echo "Error: segment_number is required" >&2
    exit 1
fi

# Load TOML reader and read config
source "${SCRIPT_DIR}/../../utilities/toml_reader.sh"
read_toml_config "$config_file"

# Format segment number with leading zero for consistent naming
iseg_string=$(printf "%02d" $iseg)

module load python3

# Generate start and end dates using print_timings.py
start_date=$(python3 "${SCRIPT_DIR}/../../utilities/print_timings.py" "$config_file" "$iseg" "START")
end_date=$(python3 "${SCRIPT_DIR}/../../utilities/print_timings.py" "$config_file" "$iseg" "END" ${test_flag})

init_date=$(python3 "${SCRIPT_DIR}/../../utilities/print_timings.py" "$config_file" "$iseg" "INIT_DATE")

if [[ $test_mode == ".TRUE." ]]; then
    expname="${PROJECT_NAME}-${PROJECT_WIDTH_CONFIG}-segment${iseg_string}-${init_date}-exp110"
else
    expname="${PROJECT_NAME}-${PROJECT_WIDTH_CONFIG}-segment${iseg_string}-${init_date}-exp111"
fi

# Set default output filename if not specified
if [[ -z "$output_file" ]]; then
    output_file="exp.${expname}"
fi

# Path to template file
template_file="${SCRIPT_DIR}/exp.TEMPLATE_for_segment_runscript"

# initialize output file
touch "$output_file"

# Generate our dynamic parameters section
cat > "$output_file" << EOF
# 1. Segment number - read from command line argument iseg=${iseg}
iseg="${iseg}"

project_name="${PROJECT_NAME}"
project_width_config="${PROJECT_WIDTH_CONFIG}"

grids_base_folder="${OUTPUT_GRID_BASEDIR}"
icbc_base_folder="${OUTPUT_ICBC_BASEDIR}"
domains_nests="${DOMAINS_NESTS}"


# 2. Test mode flag - set from command line argument -t
test_mode="${test_mode}"

# 3. Set start/end dates based on segment
start_date="${start_date}"
end_date="${end_date}"

EOF


# Define grid information
grid_dir="${OUTPUT_GRID_BASEDIR}/${PROJECT_NAME}/seg${iseg_string}_${PROJECT_WIDTH_CONFIG}"
grid_file="${grid_dir}/${PROJECT_NAME}-seg${iseg_string}_dom1_DOM01.nc"

# Add regional bounds extracted from grid file
region_bounds=$(python3 "${SCRIPT_DIR}/../../utilities/extract_region_bounds.py" "$grid_file" 2>/dev/null)
echo "$region_bounds" >> "$output_file"

# Extract everything from line 30 onwards from template
tail -n +40 "$template_file" >> "$output_file"

# Make executable
chmod +x "$output_file"

echo "Created runscript: $output_file"
