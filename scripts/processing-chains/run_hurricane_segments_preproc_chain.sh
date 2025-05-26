#!/bin/bash
#=============================================================================
# DESCRIPTION:
#   This script executes the complete preprocessing chain for hurricane-centric
#   simulations. It processes a specific hurricane segment by:
#   1. Creating the computational grid
#   2. Processing external parameters (extpar)
#   3. Generating initial conditions (IC)
#   4. Creating boundary conditions (BC)
#
# USAGE:
#   ./run_hurricane_segments_preproc_chain.sh [segment_number] [sbatch_options]
#
# ARGUMENTS:
#   segment_number  - The hurricane segment number to process
#   sbatch_options  - Additional options to pass to sbatch, typically dependencies
#
# DEPENDENCIES:
#   This script calls the following bash/sh scripts:
#   - ../grid-extpar/generate_grid_for_hurricane_segments.sh
#   - ../grid-extpar/run_extpar_levante.bash
#   - ../ic-bc/icon2icon_offline_lam_ini.bash
#   - ../ic-bc/icon2icon_offline_lam_lbc.bash
#
# AUTHOR:
#   Fabian Senf (senf@tropos.de)
#
# DATE:
#   May 2025
#=============================================================================


iseg=$1
ADDED_ARG=$2   # intended for additional slurm dependencies, e.g. --dependency=afterok: 

pp_path=".."

#-----------------------------------------------------------------------------
# PART I: Create Grid
#-----------------------------------------------------------------------------
cd ${pp_path}/grid-extpar
grid_job=$(sbatch --parsable $ADDED_ARG ./generate_grid_for_hurricane_segments.sh $iseg)
printf "... Grid job submitted with ID: $grid_job\n\n"


#-----------------------------------------------------------------------------
# PART II: Process Extpar
#-----------------------------------------------------------------------------
extpar_job=$(sbatch --parsable --dependency=afterany:$grid_job ./run_extpar_levante.bash $iseg)
printf "... Extpar job submitted with ID: $extpar_job\n\n"


#-----------------------------------------------------------------------------
# PART III: IC for Hurricane Segments
#-----------------------------------------------------------------------------
cd ${pp_path}/ic-bc
ic_job=$(sbatch  --parsable --dependency=afterany:$grid_job ./icon2icon_offline_lam_ini.bash  $iseg)
printf "... IC job submitted with ID: $ic_job\n\n"


#-----------------------------------------------------------------------------
# PART IV: BC for Hurricane Segments
#-----------------------------------------------------------------------------
bc_job=$(sbatch  --parsable --dependency=afterany:$ic_job ./icon2icon_offline_lam_lbc.bash  $iseg)
printf "... BC job submitted with ID: $bc_job\n\n"