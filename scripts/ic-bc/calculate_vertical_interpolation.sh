#!/bin/bash
#=============================================================================
# DESCRIPTION:
#   Extract height information from ICON model geopotential output.
#   This script handles file pattern resolution and data extraction.
#
# USAGE:
#   ./calculate_vertical_interpolation.sh [experiment_directory] [domain_number]
#
# ARGUMENTS:
#   experiment_directory - Full path to experiment directory
#   domain_number        - The domain number to process (1, 2, or 3)
#=============================================================================

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

# Function to process vertical level interpolation for a given variable
interpolate_in_vertical() {
    local zvar="$1"
    local src_file="$2"
    local target_ic="$3"
    
    echo "Processing variable: ${zvar} from ${src_file} -> ${target_ic}"
    
    # Create temporary files
    local source_z=`mktemp -p ${work_dir} src_z.XXXXXX`
    local target_z=`mktemp -p ${work_dir} target_z.XXXXXX`
    
    # Extract the variable from the source/target file
    cdo selname,${zvar} ${src_file} ${source_z}
    cdo selname,${zvar} ${target_file} ${target_z}
    
    local OPT="-P 32"
    
    local tmpfile=`mktemp -p ${work_dir} tmpfile.XXXXXX`
    
    # Remove variable from source file and interpolate to target levels
    cdo delname,${zvar} ${src_file} ${tmpfile}
    cdo ${OPT} intlevelx3d,${target_z} ${tmpfile} ${source_z} ${target_ic}
    
    # Cleanup temporary files
    rm -f ${source_z} ${target_z} ${tmpfile}
}

#=============================================================================
# PARSE ARGUMENTS
#=============================================================================

# Check if both arguments are provided
if [ $# -lt 2 ]; then
    echo "Error: Both experiment_directory and domain_number are required"
    echo "Usage: $0 [experiment_directory] [domain_number]"
    echo "  experiment_directory: Full path to experiment directory"
    echo "  domain_number: 1, 2, or 3"
    exit 1
fi

experiment_dir="$1"
idom="$2"

# Validate experiment directory
if [ ! -d "$experiment_dir" ]; then
    echo "Error: Experiment directory does not exist: $experiment_dir"
    exit 1
fi

# Validate domain number
if ! [[ "$idom" =~ ^[1-3]$ ]]; then
    echo "Error: Domain number must be 1, 2, or 3"
    exit 1
fi

# Get script directory
script_dir="/home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/ic-bc"

#=============================================================================
# CONFIGURATION
#=============================================================================

work_dir="${SCRATCH}/icontools"

# Create work directory if it doesn't exist
mkdir -p "${work_dir}"

# Load CDO module
module load cdo

echo "Processing experiment: ${experiment_dir}"
echo "Processing domain: ${idom}"

#=============================================================================
# FILE RESOLUTION
#=============================================================================

# Resolve source file pattern
src_pattern="${experiment_dir}/dwdFG_R*B*_DOM0${idom}.nc"
src_file=$(resolve_unique_file "$src_pattern") || exit 1
echo "Using source file: ${src_file}"

# Resolve target file pattern
target_pattern="${experiment_dir}/3d_full_geo_DOM0${idom}_ML_*T*Z.nc"
target_file=$(resolve_unique_file "$target_pattern") || exit 1
echo "Using target file: ${target_file}"
echo

# Resolve the grid file for this domain
grid_pattern="${experiment_dir}/hurricane-paulette2020-segments-seg*_dom${idom}_DOM01.nc"
grid_file=$(resolve_unique_file "$grid_pattern") || exit 1
echo "Using grid file: ${grid_file}"
echo

#=============================================================================
# DATA PROCESSING
#=============================================================================

target_ic_intermediate=`mktemp -p ${work_dir} target_ic_intermediate_DOM0${idom}.XXXXXX`
target_ic_final=`mktemp -p ${work_dir} target_ic_final_DOM0${idom}.XXXXXX`

# Process z_ifc variable
interpolate_in_vertical "z_ifc" "${src_file}" "${target_ic_intermediate}"
interpolate_in_vertical "z_mc" "${target_ic_intermediate}" "${target_ic_final}"

# Replace u,v components with vn normal component
echo "Replacing u,v with vn in ${target_ic_final}..."
bash ${script_dir}/replace_uv_with_vn_in_icfile.sh "${grid_file}" "${target_ic_final}"

# Copy final result to experiment directory
cp -b "${target_ic_final}.new" "${experiment_dir}/IC_vertically_interpolated_DOM0${idom}.nc"
echo "Created ${experiment_dir}/IC_vertically_interpolated_DOM0${idom}.nc"
echo ""

echo "Processing for domain ${idom} completed successfully."

