#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/ic-bc/replace_uv_with_vn_in_icfile.sh

gridfile=$1
icfile=$2

# remove_uv_flag="True"
remove_uv_flag="False"


script_dir=`readlink -f $(dirname $0)`

work_dir=$SCRATCH/icontools
cd ${work_dir}

# Create temporary file for intermediate result
tmp_file=$(mktemp -p ${work_dir} icfile_without_uv.XXXXXX)
uvedge_file=$(mktemp -p ${work_dir} uvedge.XXXXXX)
uvcenter_file=${uvedge_file/'uvedge'/'uvcenter'}
vnormal_file=${uvedge_file/'uvedge'/'vnormal'}

# Load CDO module
module load cdo
module load python3

# Convert u and v edge components to vn normal component
echo "Extracting u,v components from ${icfile}..."
cdo selname,u,v ${icfile} ${uvcenter_file}
cdo -P 32 remapdis,$gridfile:2 -setgrid,$gridfile:1 ${uvcenter_file} ${uvedge_file}

# Generate vn from u,v components
echo "Converting u,v edge components to vn normal component..."
python ${script_dir}/../../utilities/uvedge_to_vnormal_converter.py ${gridfile} ${uvedge_file}
# The script creates ${vnormal_file}

# Remove u and v variables from icfile
if [ "${remove_uv_flag}" == "True" ]; then
    echo "Removing u and v variables from ${icfile}..."
    cdo delname,u,v ${icfile} ${tmp_file}
else
    echo "Keeping u and v variables in ${icfile}..."
    cp ${icfile} ${tmp_file}
fi

# Merge the file without u,v with the ${vnormal_file} file containing vn
echo "Merging vn into the IC file..."
cdo -P 32 merge ${tmp_file} ${vnormal_file} ${icfile}.new

# Replace original with new file
# mv ${icfile}.new ${icfile}

# Cleanup temporary files
#rm -f ${tmp_file} ${uvedge_file} ${vnormal_file}

echo "Completed: ${icfile} now contains vn instead of u,v"



