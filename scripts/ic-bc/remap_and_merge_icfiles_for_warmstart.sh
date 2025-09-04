#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/ic-bc/replace_uv_with_vn_in_icfile.sh

# =============================================================================
# CONFIGURATION
# =============================================================================
ic_segment_file=$1
ic_background_file=$2

from_gridfile=$3
to_gridfile=$4

output_file=$5
work_dir=$6

script_dir=`readlink -f $(dirname $0)`

cd ${work_dir}

# Create temporary file for intermediate result
ic_segment_novn_file=$(mktemp -p ${work_dir} ic_segment_novn.XXXXXX)
interpolated_segment_file=$(mktemp -p ${work_dir} ic_segment_interpolated.XXXXXX)
intermediate_merged_file=$(mktemp -p ${work_dir} ic_merged.XXXXXX)

# Convert u and v edge components to vn normal component
echo "Remapping ic segment file from ${from_gridfile} to ${to_gridfile}..."

cdo delname,vn ${ic_segment_file} ${ic_segment_novn_file} || { echo "ERROR: CDO delname failed! Exiting."; exit 1; }
cdo -P 32 remapdis,$to_gridfile:1 -setgrid,$from_gridfile:1 ${ic_segment_novn_file} ${interpolated_segment_file} || { echo "ERROR: CDO remapping failed! Exiting."; exit 1; }

# Merging the interpolated segment file with the background file
echo "Merging interpolated segment file with background file...\n\n"

ics="$interpolated_segment_file $ic_background_file"
grids="$from_gridfile $to_gridfile"

echo "Running script to merge IC files..."
echo "IC files: $ics"
echo "Grids: $grids\n\n"

python ${script_dir}/../../utilities/58-Updated-Method-to-Combine-Center-Grids-from-Different-Segments.py $ics $grids $intermediate_merged_file
echo "Merging completed."
echo "Merged: ${intermediate_merged_file}\n\n"

# Replace u,v components with vn normal component
echo "Replacing u,v with vn in ${target_ic_final}..."
bash ${script_dir}/replace_uv_with_vn_in_icfile.sh "${to_gridfile}" "${intermediate_merged_file}" "${work_dir}"

# Copy final result to experiment directory
cp -b "${intermediate_merged_file}.new" "${output_file}"
echo "Created ${output_file}"
echo ""

echo "Processing for domain ${idom} completed successfully."


# Cleanup temporary files
rm -f "${ic_segment_novn_file}"
rm -f "${interpolated_segment_file}"
rm -f "${intermediate_merged_file}"
rm -f "${intermediate_merged_file}.new"
