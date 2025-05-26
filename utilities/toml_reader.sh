#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/utilities/toml_reader.sh
#=============================================================================
# DESCRIPTION:
#   Simple TOML reader for bash scripts. Reads key-value pairs from TOML files.
#
# USAGE:
#   source utilities/toml_reader.sh
#   read_toml_config "path/to/config.toml"
#   echo $PROJECT_NAME
#=============================================================================

read_toml_config() {
    local config_file="$1"
    
    if [[ ! -f "$config_file" ]]; then
        echo "Error: Config file $config_file not found!"
        exit 1
    fi
    
    # Read TOML file and convert to bash variables
    while IFS= read -r line; do
        # Skip comments and empty lines
        [[ "$line" =~ ^[[:space:]]*# ]] && continue
        [[ -z "${line// }" ]] && continue
        
        # Skip section headers
        [[ "$line" =~ ^\[.*\]$ ]] && continue
        
        # Parse key = value pairs
        if [[ "$line" =~ ^[[:space:]]*([^=]+)[[:space:]]*=[[:space:]]*(.+)$ ]]; then
            key="${BASH_REMATCH[1]// /}"
            value="${BASH_REMATCH[2]}"
            
            # Remove quotes and convert to uppercase
            value=$(echo "$value" | sed 's/^"//;s/"$//')
            key=$(echo "$key" | tr '[:lower:]' '[:upper:]')
            
            # Handle arrays (simple approach)
            if [[ "$value" =~ ^\[.*\]$ ]]; then
                value=$(echo "$value" | sed 's/\[//;s/\]//;s/,/ /g')
            fi
            
            # Export variable
            export "$key"="$value"
        fi
    done < "$config_file"
}

# Function to get config file path relative to script location
get_config_path() {
    local script_dir="$1"
    echo "${script_dir}/../../config/hurricane_config.toml"
}