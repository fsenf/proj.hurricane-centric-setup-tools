#!/bin/bash
#=============================================================================
# DESCRIPTION:
#   This script handles initial condition preparation for hurricane-centric 
#   warmstart simulations. It processes a specific hurricane segment by:
#   1. Creating initial conditions through icon2icon transformations
#   2. Generating a weighted combination of the processed IC data
#
# USAGE:
#   ./run_inits_for_warmstart_hurri-segments.sh [segment_number] [sbatch_options]
#
# ARGUMENTS:
#   segment_number  - The hurricane segment number to process
#   sbatch_options  - Options to pass to sbatch, including dependencies
#
# DEPENDENCIES:
#   This script calls the following bash/sh scripts:
#   - ../ic-bc/icon2icon_offline_lam_ini.bash
#   - ../ic-bc/weighted_combination_of_icdata.sh
#   - ../ic-bc/warmstart-config.sh (referenced but not directly executed)
#
# AUTHOR:
#   Fabian Senf (senf@tropos.de)
#
# DATE:
#   May 2025
#=============================================================================

set -e
set -x

#-----------------------------------------------------------------------------
# Parse script arguments
#-----------------------------------------------------------------------------
iseg=$1
opt=$2

pp_path=".."

#-----------------------------------------------------------------------------
# Parse opt and extract job IDs if --dependency=afterany is present
#-----------------------------------------------------------------------------
dependency_job_ids=""
if [[ $opt == *"--dependency=afterany:"* ]]; then
    dependency_job_ids=$(echo $opt | grep -oP '(?<=--dependency=afterany:)[^ ]+')
    printf "Extracted dependency job IDs: $dependency_job_ids\n"
fi


#-----------------------------------------------------------------------------
# PART IC for Hurricane Segments
#-----------------------------------------------------------------------------
cd ${pp_path}/ic-bc

ic_jobs=()  # Array to store job IDs

for i in {1..3}; do
    ic_job=$(sbatch --parsable $opt ./icon2icon_offline_lam_ini.bash warmstart-config.sh $iseg $i $i)
    ic_jobs+=($ic_job)  # Collect job ID
    printf "... IC job submitted with ID: $ic_job\n\n"
done

# Combine all job IDs into a single dependency string
all_dependencies="${dependency_job_ids:+$dependency_job_ids:}$(IFS=,; echo "${ic_jobs[*]}")"
dependency_opt="--dependency=afterany:$all_dependencies"


#-----------------------------------------------------------------------------
# PART II weighted combination of IC data
#-----------------------------------------------------------------------------
cd ${pp_path}/ic-bc

sbatch --parsable $opt $dependency_opt ./weighted_combination_of_icdata.sh $iseg