#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/utilities/toml_reader.sh
#=============================================================================
# DESCRIPTION:
#   Enhanced TOML reader for bash scripts with improved array support.
#=============================================================================

read_toml_config() {
    local config_file="$1"
    local current_section=""
    
    if [[ ! -f "$config_file" ]]; then
        echo "Error: Config file $config_file not found!"
        exit 1
    fi
    
    # Read TOML file and convert to bash variables
    while IFS= read -r line; do
        # Skip comments and empty lines
        [[ "$line" =~ ^[[:space:]]*# ]] && continue
        [[ -z "${line// }" ]] && continue
        
        # Handle section headers
        if [[ "$line" =~ ^\[([^\]]+)\]$ ]]; then
            current_section="${BASH_REMATCH[1]}"
            continue
        fi
        
        # Parse key = value pairs
        if [[ "$line" =~ ^[[:space:]]*([^=]+)[[:space:]]*=[[:space:]]*(.+)$ ]]; then
            key="${BASH_REMATCH[1]// /}"
            value="${BASH_REMATCH[2]}"
            
            # Create variable name with section prefix
            if [[ -n "$current_section" ]]; then
                section_prefix=$(echo "$current_section" | tr '.' '_' | tr '[:lower:]' '[:upper:]')
                var_name="${section_prefix}_${key}"
            else
                var_name="$key"
            fi
            
            # Convert to uppercase
            var_name=$(echo "$var_name" | tr '[:lower:]' '[:upper:]')
            
            # Remove quotes and handle arrays
            if [[ "$value" =~ ^\[.*\]$ ]]; then
                # Handle multi-line arrays
                local array_content="$value"
                
                # If the array spans multiple lines, read them
                while [[ ! "$array_content" =~ \]$ ]] && IFS= read -r next_line; do
                    [[ "$next_line" =~ ^[[:space:]]*# ]] && continue
                    array_content="$array_content $next_line"
                done
                
                # Clean up array content
                value=$(echo "$array_content" | sed 's/\[//g; s/\]//g; s/,/ /g; s/"//g' | tr -s ' ')
            else
                # Remove quotes from simple values
                value=$(echo "$value" | sed 's/^"//;s/"$//')
            fi
            
            # Handle booleans
            case "$value" in
                "true") value="TRUE" ;;
                "false") value="FALSE" ;;
            esac
            
            # Export variable
            export "$var_name"="$value"
        fi
    done < "$config_file"
}

# Function to get config file path relative to script location
get_config_path() {
    local script_dir="$1"
    echo "${script_dir}/../../config/hurricane_config.toml"
}

# Function to get initialization date based on segment number
get_init_date_from_segment() {
    local iseg="$1"
    case $iseg in
        0) echo '20200907' ;;
        1) echo '20200908' ;;
        2) echo '20200909' ;;
        3) echo '20200910' ;;
        4) echo '20200911' ;;
        5) echo '20200912' ;;
        6) echo '20200913' ;;
        7) echo '20200914' ;;
        *) echo 'Invalid segment' ;;
    esac
}

# Function to get datafile list pattern based on segment number
get_datafile_pattern_from_segment() {
    local iseg="$1"
    case $iseg in
        0) echo "lam_input_BC_DOM02_ML_2020090[7-8]T??0000Z.nc" ;;
        1) echo "lam_input_BC_DOM02_ML_2020090[8-9]T??0000Z.nc" ;;
        2) echo "lam_input_BC_DOM02_ML_202009[01][09]T??0000Z.nc" ;;
        3) echo "lam_input_BC_DOM02_ML_2020091[0-1]T??0000Z.nc" ;;
        4) echo "lam_input_BC_DOM02_ML_2020091[1-2]T??0000Z.nc" ;;
        5) echo "lam_input_BC_DOM02_ML_2020091[2-3]T??0000Z.nc" ;;
        6) echo "lam_input_BC_DOM02_ML_2020091[3-4]T??0000Z.nc" ;;
        7) echo "lam_input_BC_DOM02_ML_2020091[4-5]T??0000Z.nc" ;;
        *) echo 'Invalid segment' ;;
    esac
}