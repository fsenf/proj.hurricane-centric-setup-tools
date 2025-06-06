#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/processing-chains/preproc_chain_looper.sh
#=============================================================================
# DESCRIPTION:
#   Loops over multiple hurricane segments and submits preprocessing chains.
#
# USAGE:
#   ./preproc_chain_looper.sh [nsegments] [start_segment]
#
# ARGUMENTS:
#   nsegments      - Number of segments to process (default: 8)
#   start_segment  - Starting segment number (default: 1)
#
# AUTHOR:
#   GitHub Copilot
#
# DATE:
#   June 2025
#=============================================================================

# Default values
nsegments=${1:-8}
start_segment=${2:-1}

# Validate inputs
if ! [[ "$nsegments" =~ ^[0-9]+$ ]] || [ "$nsegments" -le 0 ]; then
    echo "Error: nsegments must be a positive integer"
    exit 1
fi

if ! [[ "$start_segment" =~ ^[0-9]+$ ]]; then
    echo "Error: start_segment must be a non-negative integer"
    exit 1
fi

# Calculate end segment
end_segment=$((start_segment + nsegments - 1))

echo "Processing segments: $start_segment to $end_segment"
echo "============================================================================="

# Arrays to store job IDs
declare -a grid_jobs
declare -a extpar_jobs
declare -a ic_jobs

# Loop over segments
for ((iseg = start_segment; iseg <= end_segment; iseg++)); do
    echo "Submitting preprocessing chain for segment $iseg..."
    
    # Submit the preprocessing chain for this segment
    chain_output=$(bash ./run_hurricane_segments_preproc_chain.sh "$iseg" 2>&1)
    
    if [ $? -ne 0 ]; then
        echo "Error: Failed to submit preprocessing chain for segment $iseg"
        echo "$chain_output"
        exit 1
    fi
    
    # Extract and store job IDs
    grid_job=$(echo "$chain_output" | grep "Grid job submitted with ID:" | awk '{print $NF}')
    extpar_job=$(echo "$chain_output" | grep "Extpar job submitted with ID:" | awk '{print $NF}')
    ic_job=$(echo "$chain_output" | grep "IC job submitted with ID:" | awk '{print $NF}')
    
    grid_jobs[$iseg]="$grid_job"
    extpar_jobs[$iseg]="$extpar_job"
    ic_jobs[$iseg]="$ic_job"
    
    echo "  Grid: $grid_job | Extpar: $extpar_job | IC: $ic_job"
done

echo "============================================================================="
echo "All segments submitted successfully!"
echo "Use 'squeue -u $USER' to monitor job status"