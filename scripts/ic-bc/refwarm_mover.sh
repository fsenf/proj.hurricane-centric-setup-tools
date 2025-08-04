#!/bin/bash



flist="dwdFG_R2B1?_DOM0?.nc"


for fname in ${flist}; do
    echo "Processing file: ${fname}"
    init_name=`readlink -f $fname`

    dom=$(echo "$fname" | grep -o 'DOM0[0-9]')
    echo "Extracted domain: $dom"

    source_name=IC_vertically_interpolated_$dom.nc
    target_name=${init_name/'ini.nc'/'warmini.nc'} # replace dwdFG with warmini

    echo "moving $source_name to $target_name"
    mv -i $source_name $target_name
done