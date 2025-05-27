#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/utilities/ic_bc_utils.sh
#=============================================================================
# DESCRIPTION:
#   Utility functions for IC/BC processing with TOML configuration support.
#
# USAGE:
#   source utilities/ic_bc_utils.sh
#   init_date=$(get_init_date_from_config "$CONFIG_FILE" "$iseg")
#   datafiles=$(get_datafile_list_from_config "$CONFIG_FILE" "$iseg")
#=============================================================================

# Function to get initialization date from segment number
get_init_date_from_config() {
    local config_file="$1"
    local iseg="$2"
    
    get_init_date_from_segment "$iseg"
}

# Function to get datafile list from segment number
get_datafile_list_from_config() {
    local config_file="$1"
    local iseg="$2"
    
    # Get the pattern for the given segment
    local pattern=$(get_datafile_pattern_from_segment "$iseg")
    local data_dir="${REFERENCE_INPUT_ICBC_DIR}/${REFERENCE_INPUT_ICBC_SUBDIR}"
    
    # Find files matching the pattern
    find "${data_dir}" -iname "${pattern}" | sort
}

# Function to set up IC/BC configuration variables from TOML
setup_ic_bc_config() {
    local iseg="$1"
    
    # Get initialization date
    init_date=$(get_init_date_from_config "$CONFIG_FILE" "$iseg")
    
    # Set up input grid
    export INGRID="$REFERENCE_INPUT_GRID"
    
    # Set up input file
    local data_dir="${REFERENCE_INPUT_ICBC_DIR}/${REFERENCE_INPUT_ICBC_SUBDIR}"
    export INFILE="${data_dir}/lam_input_IC_DOM02_ML_${init_date}T000000Z.nc"
    
    # Set up domain name and output grid
    export DOMNAME="${PROJECT_NAME}/seg${iseg}_${PROJECT_WIDTH_CONFIG}"
    export OUTGRID="${OUTPUT_GRID_BASEDIR}/${DOMNAME}/paulette-seg${iseg}_dom1_DOM01.nc"
    
    # Set up datafile list for boundary conditions
    export DATAFILELIST=$(get_datafile_list_from_config "$CONFIG_FILE" "$iseg")
    
    # Set up output name for initial conditions
    local scratch_dir="${SCRATCH:-/scratch/b/b380352}"
    export OUTNAME="${scratch_dir}/icontools/ifces2-atlanXL-${init_date}T000000Z_${DOMNAME//\//_}_DOM01_ini.nc"
}