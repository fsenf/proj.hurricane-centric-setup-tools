#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/utilities/config_handler.sh
#=============================================================================
# DESCRIPTION:
#   Shared utility for handling configuration files across all scripts.
#   Provides standardized config file parsing, path resolution, and loading.
#
# FUNCTIONS:
#   parse_config_argument() - Extract config argument from command line
#   handle_config()         - Process and load configuration file
#   show_config_help()      - Display help for config options
#
# USAGE:
#   source "${SCRIPT_DIR}/../../utilities/config_handler.sh"
#   CONFIG_ARG=$(parse_config_argument "$@")
#   handle_config "$SCRIPT_DIR" "$DEFAULT_CONFIG" "$CONFIG_ARG"
#
#=============================================================================

# Function to extract config argument from command line arguments
parse_config_argument() {
    local args=("$@")
    local config_file=""
    
    for ((i=0; i<${#args[@]}; i++)); do
        case "${args[i]}" in
            -c)
                if [[ $((i+1)) -lt ${#args[@]} ]]; then
                    config_file="${args[$((i+1))]}"
                else
                    echo "Error: -c requires a config file argument"
                    exit 1
                fi
                break
                ;;
            --config=*)
                config_file="${args[i]#*=}"
                break
                ;;
            --config)
                if [[ $((i+1)) -lt ${#args[@]} ]]; then
                    config_file="${args[$((i+1))]}"
                else
                    echo "Error: --config requires a config file argument"
                    exit 1
                fi
                break
                ;;
        esac
    done
    
    echo "$config_file"
}

# Function to handle config file argument parsing and loading
handle_config() {
    local script_dir="$1"
    local default_config="$2"
    local config_arg="${3:-}"
    
    local config_file=""
    
    if [[ -z "$config_arg" ]]; then
        config_file="$default_config"
        echo "Using default config file: $config_file"
    else
        # Check if config file path is relative
        if [[ "$config_arg" != /* ]]; then
            # Relative path - prepend script directory
            config_file="${script_dir}/${config_arg}"
            echo "Using relative config file: $config_file"
        else
            config_file="$config_arg"
            echo "Using absolute config file: $config_file"
        fi
    fi
    
    # Verify config file exists
    if [[ ! -f "$config_file" ]]; then
        echo "Error: Config file not found: $config_file"
        echo "  Checked path: $config_file"
        if [[ "$config_file" != "$default_config" ]]; then
            echo "  Default would be: $default_config"
        fi
        exit 1
    fi
    
    echo "Loading config file: $config_file"
    
    # Load TOML reader and configuration
    local toml_reader_path="${script_dir}/../../utilities/toml_reader.sh"
    if [[ ! -f "$toml_reader_path" ]]; then
        echo "Error: TOML reader not found at: $toml_reader_path"
        exit 1
    fi
    
    source "$toml_reader_path"
    read_toml_config "$config_file"
    
    # Export the config file path for other scripts
    export CONFIG_FILE="$config_file"
    
    echo "Configuration loaded successfully"
}

# Function to remove config arguments from argument array
remove_config_args() {
    local args=("$@")
    local filtered_args=()
    local skip_next=false
    
    for ((i=0; i<${#args[@]}; i++)); do
        if [[ "$skip_next" == true ]]; then
            skip_next=false
            continue
        fi
        
        case "${args[i]}" in
            -c|--config)
                skip_next=true
                ;;
            --config=*)
                # Skip this argument entirely
                ;;
            *)
                filtered_args+=("${args[i]}")
                ;;
        esac
    done
    
    echo "${filtered_args[@]}"
}

# Function to show standardized help for config options
show_config_help() {
    echo "Configuration Options:"
    echo "  -c, --config FILE     Path to TOML configuration file"
    echo "                        Relative paths resolved from script directory"
    echo "                        Default: ../../config/hurricane_config.toml"
    echo ""
    echo "Config Examples:"
    echo "  $0 5                           # Use default config"
    echo "  $0 5 -c my_config.toml        # Use relative config"
    echo "  $0 5 --config=/abs/path.toml  # Use absolute config"
}