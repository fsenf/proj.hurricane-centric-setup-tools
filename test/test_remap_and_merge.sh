#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/test/test_remap_and_merge.sh

to_iseg=$1
idom=$2
from_iseg=$((to_iseg - 1))

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

experiment_dir=/work/bb1376/user/fabian/model/icon/icon-builds/icon-release-2024.07/experiments
project_name="hurricane-paulette2020-segments"
project_width_config="width200km_reinit12h"

i=${idom} # Domain number, can be 1, 2, or 3
# Define patterns with wildcards instead of hardcoded dates
ic_bg_pattern="${experiment_dir}/${project_name}-${project_width_config}-segment${to_iseg}-????????-exp108/IC_vertically_interpolated_DOM0${i}.nc"
ic_seg_pattern="${experiment_dir}/${project_name}-${project_width_config}-segment${from_iseg}-????????-exp109/lam_input_IC_DOM0${i}_ML_????????T??????Z.nc"
to_grid_pattern="${experiment_dir}/${project_name}-${project_width_config}-segment${to_iseg}-????????-exp108/hurricane-paulette2020-segments-seg${to_iseg}_dom${i}_DOM01.nc"
from_grid_pattern="${experiment_dir}/${project_name}-${project_width_config}-segment${from_iseg}-????????-exp109/hurricane-paulette2020-segments-seg${from_iseg}_dom${i}_DOM01.nc"

# Resolve patterns to actual files
echo "Resolving file patterns..."

ic_bg_file=$(resolve_unique_file "$ic_bg_pattern")
ic_seg_file=$(resolve_unique_file "$ic_seg_pattern")
to_grid=$(resolve_unique_file "$to_grid_pattern")
from_grid=$(resolve_unique_file "$from_grid_pattern")

echo "Background IC file: $ic_bg_file"
echo "Segment IC file: $ic_seg_file"
echo "Target grid file: $to_grid"
echo "Source grid file: $from_grid"

output_file="/work/bb1376/data/icon/bc-init/${project_name}/seg${to_iseg}_${project_width_config}/ifces2-atlanXL-ML_hurricane-paulette2020-segments_seg${to_iseg}_${project_width_config}_DOM0${i}_warmini.nc"
echo "Output will be saved to: $output_file"

echo "Changing to IC-BC directory..."
cd ../scripts/ic-bc || exit 1

echo "Running remap and merge..."
bash remap_and_merge_icfiles_for_warmstart.sh "${ic_seg_file}" "${ic_bg_file}" "${from_grid}" "${to_grid}" "${output_file}"

