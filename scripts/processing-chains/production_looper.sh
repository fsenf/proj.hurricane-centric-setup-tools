#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/processing-chains/production_looper.sh
#=============================================================================
# DESCRIPTION:
#   Hurricane production chain looper script that runs multiple segments
#   sequentially with proper dependency via ICON post proc capabilities.
#
# USAGE:
#   ./production_looper.sh start_segment end_segment [options] [slurm_options]
#
# ARGUMENTS:
#   start_segment   - First segment number to process
#   end_segment     - Last segment number to process
#
# OPTIONS:
#   -c, --config    - Path to TOML configuration file (required)
#                     Relative paths are resolved from script directory
#   -h, --help      - Show this help message
#
# SLURM OPTIONS:
#   --nodes=N         - Number of compute nodes (default: 64)
#   --time=HH:MM:SS   - Job time limit (default: 08:00:00)
#
# EXAMPLES:
#   ./production_looper.sh 1 5 -c ../../config/hurricane_config.toml
#   ./production_looper.sh 1 3 -c ../../config/hurricane_config.toml --nodes=128 --time=12:00:00
#
#=============================================================================

set -e

#=============================================================================
# Configuration and Argument Parsing
#=============================================================================

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
echo "Script directory: ${SCRIPT_DIR}"

# Load shared configuration handler
source "${SCRIPT_DIR}/../../utilities/config_handler.sh"

# Parse config argument (but don't load config - just resolve path)
CONFIG_ARG=$(parse_config_argument "$@")

# Handle config path resolution without loading
if [[ -z "$CONFIG_ARG" ]]; then
    echo "Error: Config file is required"
    echo "Usage: $0 start_segment end_segment -c|--config config_file [options]"
    exit 1
fi

CONFIG_FILE_ABS=$(readlink -f "$CONFIG_ARG")
echo "Config file: $CONFIG_FILE_ABS"

# Load TOML reader
source "${SCRIPT_DIR}/../../utilities/toml_reader.sh"
read_toml_config "$CONFIG_FILE_ABS"

# Remove config arguments and parse remaining arguments
REMAINING_ARGS=($(remove_config_args "$@"))

# Parse arguments
start_segment=""
end_segment=""
slurm_options=()

# Default SLURM parameters (similar to starter.sh)
nodes=64
ctime="08:00:00"

for arg in "${REMAINING_ARGS[@]}"; do
    case $arg in
        -h|--help)
            echo "Usage: $0 start_segment end_segment [options] [slurm_options]"
            echo ""
            echo "Arguments:"
            echo "  start_segment     First segment number to process"
            echo "  end_segment       Last segment number to process"
            echo ""
            echo "Options:"
            show_config_help
            echo ""
            echo "SLURM Options:"
            echo "  --nodes=N         Number of compute nodes (default: 64)"
            echo "  --time=HH:MM:SS   Job time limit (default: 08:00:00)"
            echo ""
            echo "Examples:"
            echo "  $0 1 5 -c ../../config/hurricane_config.toml"
            echo "  $0 1 3 -c ../../config/hurricane_config.toml --nodes=128 --time=12:00:00"
            exit 0
            ;;
        --nodes=*|--time=*)
            # These are SLURM options
            slurm_options+=("$arg")
            ;;
        -*)
            echo "Error: Unknown option $arg"
            echo "Use --help for usage information"
            exit 1
            ;;
        *)
            if [[ -z "$start_segment" ]]; then
                start_segment="$arg"
            elif [[ -z "$end_segment" ]]; then
                end_segment="$arg"
            else
                echo "Error: Too many positional arguments"
                echo "Usage: $0 start_segment end_segment [options] [slurm_options]"
                exit 1
            fi
            ;;
    esac
done

#=============================================================================
# Validation
#=============================================================================

# Validate required arguments
if [[ -z "$start_segment" ]]; then
    echo "Error: start_segment is required"
    echo "Usage: $0 start_segment end_segment [options] [slurm_options]"
    exit 1
fi

if [[ -z "$end_segment" ]]; then
    echo "Error: end_segment is required"
    echo "Usage: $0 start_segment end_segment [options] [slurm_options]"
    exit 1
fi

# Validate segment numbers
if ! [[ "$start_segment" =~ ^[0-9]+$ ]]; then
    echo "Error: start_segment must be a number"
    exit 1
fi

if ! [[ "$end_segment" =~ ^[0-9]+$ ]]; then
    echo "Error: end_segment must be a number"
    exit 1
fi

if [[ $start_segment -gt $end_segment ]]; then
    echo "Error: start_segment ($start_segment) must be <= end_segment ($end_segment)"
    exit 1
fi

# Override defaults with any provided SLURM options
for option in "${slurm_options[@]}"; do
    case "$option" in
        --nodes=*)
            nodes="${option#*=}"
            echo "Using custom node count: $nodes"
            ;;
        --time=*)
            ctime="${option#*=}"
            echo "Using custom time limit: $ctime"
            ;;
    esac
done

echo "Production Loop Configuration:"
echo "  Segments: $start_segment to $end_segment"
echo "  Config: $CONFIG_FILE_ABS"
echo "  Nodes: $nodes"
echo "  Time: $ctime"

#=============================================================================
# Production Chain Loop
#=============================================================================
module load python3

# Get ICON run directory from config
icon_run_dir=${TOOLS_ICON_BUILD_DIR}/run

if [[ ! -d "$icon_run_dir" ]]; then
    echo "❌ ERROR: ICON run directory does not exist: $icon_run_dir"
    exit 1
fi

echo "ICON run directory: $icon_run_dir"

echo ""
echo "Starting production chain loop..."
echo "========================================"

# Initialize previous post-processing script variable
previous_postproc_script=""

for iseg in $(seq $start_segment $end_segment); do
    echo ""
    echo "Processing segment $iseg..."
    echo "----------------------------------------"
    
    # Prepare Post-Processing script

    # Generate experiment name using segment and date
    init_date=$(python3 "${SCRIPT_DIR}/../../utilities/print_timings.py" "$CONFIG_FILE_ABS" "$iseg" "INIT_DATE")
    expname="${PROJECT_NAME}-${PROJECT_WIDTH_CONFIG}-segment${iseg}-${init_date}-exp111"
    echo "Experiment name: $expname"

    actual_postproc_script="${SCRIPT_DIR}/../runscripts/post.${expname}.run"
    template_file="${SCRIPT_DIR}/../runscripts/post.TEMPLATE_for_segment_runscript"

    # Check if template file exists
    if [[ ! -f "$template_file" ]]; then
        echo "❌ ERROR: Template file not found: $template_file"
        exit 1
    fi

    # Create post-processing script from template
    cat "$template_file" > "$actual_postproc_script"
    echo "cd ${SCRIPT_DIR}" >> "$actual_postproc_script"
    chmod +x "$actual_postproc_script"
    echo "Post-processing script created: $actual_postproc_script"


    # Set main production command
    chain_cmd="bash run_hurricane_production_chain.sh $iseg -c $CONFIG_FILE_ABS --nodes=$nodes --time=$ctime"

    if [[ $iseg -eq $start_segment ]]; then

        echo "Executing: $chain_cmd"
        echo ""
        
        # Execute the production chain and capture output
        chain_output=$(eval "$chain_cmd" 2>&1)
        chain_exit_code=$?
        
        echo "$chain_output"
        
        # Check if the command succeeded
        if [[ $chain_exit_code -ne 0 ]]; then
            echo "❌ ERROR: Production chain failed for segment $iseg (exit code: $chain_exit_code)"
            exit $chain_exit_code
        fi
        
        echo "✅ Successfully submitted segment $iseg"
        echo "----------------------------------------"

    else
        echo "Preparing Post-Processing script for segment $iseg"
        
        # Only process previous script if it exists
        if [[ -n "$previous_postproc_script" && -f "$previous_postproc_script" ]]; then
            echo "Post-processing script: $previous_postproc_script"
            
            # Prepare the post-processing script for the previous segment
            echo "$chain_cmd" >> "$previous_postproc_script"

            cp "$previous_postproc_script" "$icon_run_dir"
            echo "Copied post-processing script to ICON run directory: $icon_run_dir"
        else
            echo "⚠️  WARNING: No previous post-processing script found"
        fi
    fi


    if [[ $iseg -eq $end_segment ]]; then
        echo "Finalizing post-processing script for segment $iseg"
        echo "Post-processing script: $actual_postproc_script"

        # Add final commands to the post-processing script
        echo "echo 'Post-processing completed for segment $iseg'" >> "$actual_postproc_script"
        echo "echo 'All segments processed successfully!'" >> "$actual_postproc_script"

        cp $actual_postproc_script $icon_run_dir
        echo "Copied post-processing script to ICON run directory: $icon_run_dir"
    else
        echo "Continuing to next segment..."
    fi

    # Update previous script reference for next iteration
    previous_postproc_script="$actual_postproc_script"
done

echo ""
echo "========================================"
echo "✅ Production chain loop completed successfully!"
echo "Processed segments: $start_segment to $end_segment"
echo "========================================"

exit 0
