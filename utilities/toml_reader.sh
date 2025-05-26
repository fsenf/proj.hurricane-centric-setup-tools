#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/utilities/toml_reader.sh
#=============================================================================
# DESCRIPTION:
#   Enhanced TOML reader for bash scripts with section prefix support.
#
# USAGE:
#   source utilities/toml_reader.sh
#   read_toml_config "path/to/config.toml"
#   echo $PROJECT_NAME
#   echo $PATHS_OUTPUT_BASE
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
                # Replace dots with underscores for nested sections
                section_prefix=$(echo "$current_section" | tr '.' '_' | tr '[:lower:]' '[:upper:]')
                var_name="${section_prefix}_${key}"
            else
                var_name="$key"
            fi
            
            # Convert to uppercase
            var_name=$(echo "$var_name" | tr '[:lower:]' '[:upper:]')
            
            # Remove quotes
            value=$(echo "$value" | sed 's/^"//;s/"$//')
            
            # Handle arrays
            if [[ "$value" =~ ^\[.*\]$ ]]; then
                # Convert array to space-separated string
                value=$(echo "$value" | sed 's/\[//;s/\]//;s/,/ /g' | sed 's/"//g')
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