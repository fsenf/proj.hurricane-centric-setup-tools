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