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

# Function to get initialization date from TOML config
get_init_date_from_config() {
    local config_file="$1"
    local iseg="$2"
    
    # Read segment dates as array from TOML
    local segment_dates_string="$SIMULATION_SEGMENT_DATES"
    local -a segment_dates_array=($segment_dates_string)
    
    # Return the date for the given segment
    if [[ $iseg -ge 0 && $iseg -lt ${#segment_dates_array[@]} ]]; then
        echo "${segment_dates_array[$iseg]}"
    else
        echo "Invalid segment: $iseg" >&2
        return 1
    fi
}

# Function to get datafile list from TOML config
get_datafile_list_from_config() {
    local config_file="$1"
    local iseg="$2"
    
    # Read file patterns as array from TOML
    local file_patterns_string="$IC_BC_FILE_PATTERNS"
    local -a file_patterns_array=($file_patterns_string)
    
    # Get the pattern for the given segment
    if [[ $iseg -ge 0 && $iseg -lt ${#file_patterns_array[@]} ]]; then
        local pattern="${file_patterns_array[$iseg]}"
        local data_dir="${IC_BC_INPUT_EXPERIMENT_DIR}/${IC_BC_INPUT_DATA_SUBDIR}"
        
        # Find files matching the pattern
        find "${data_dir}" -iname "${pattern}" | sort
    else
        echo "Invalid segment: $iseg" >&2
        return 1
    fi
}

# Function to set up IC/BC configuration variables from TOML
setup_ic_bc_config() {
    local iseg="$1"
    
    # Get initialization date
    init_date=$(get_init_date_from_config "$CONFIG_FILE" "$iseg")
    
    # Set up input grid
    export INGRID="$IC_BC_INPUT_GRID"
    
    # Set up input file
    local data_dir="${IC_BC_INPUT_EXPERIMENT_DIR}/${IC_BC_INPUT_DATA_SUBDIR}"
    export INFILE="${data_dir}/lam_input_IC_DOM02_ML_${init_date}T000000Z.nc"
    
    # Set up domain name and output grid
    export DOMNAME="${PROJECT_NAME}/seg${iseg}_${PROJECT_WIDTH_CONFIG}"
    export OUTGRID="${PATHS_OUTPUT_BASE}/${DOMNAME}/paulette-seg${iseg}_dom1_DOM01.nc"
    
    # Set up datafile list for boundary conditions
    export DATAFILELIST=$(get_datafile_list_from_config "$CONFIG_FILE" "$iseg")
    
    # Set up output name for initial conditions
    local scratch_dir="${SCRATCH:-/scratch/b/b380352}"
    export OUTNAME="${scratch_dir}/icontools/ifces2-atlanXL-${init_date}T000000Z_${DOMNAME//\//_}_DOM01_ini.nc"
}