#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/runscripts/starter.sh
#=============================================================================
# DESCRIPTION:
#   ICON experiment starter script that handles configuration files and
#   submits ICON model runs via SLURM.
#
# USAGE:
#   ./starter.sh expname [options] [sbatch_options]
#
# ARGUMENTS:
#   expname         - Experiment name (with or without "exp." prefix)
#
# OPTIONS:
#   -c, --config    - Path to TOML configuration file (optional)
#                     Relative paths are resolved from script directory
#   -h, --help      - Show this help message
#
# EXAMPLES:
#   ./starter.sh ifces2-atlanLEM-segment2-20200909-exp107
#   ./starter.sh exp.ifces2-atlanLEM-segment2-20200909-exp107 --nodes=50
#   ./starter.sh ifces2-atlanLEM-segment2-20200909-exp107 -c my_config.toml
#
#=============================================================================

set -e
set -x

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
if [[ -n "$CONFIG_ARG" ]]; then
    CONFIG_FILE_ABS=`readlink -f "$CONFIG_ARG"`
    
    # Prepare config option for forwarding
    CONFIG_OPTION="-c $CONFIG_FILE_ABS"
    echo "Config will be forwarded as: $CONFIG_OPTION"
else
    CONFIG_OPTION=""
fi

# Remove config arguments and parse remaining arguments
REMAINING_ARGS=($(remove_config_args "$@"))

# Parse arguments
expname=""
sbatch_options=()
exp_arguments=()

for arg in "${REMAINING_ARGS[@]}"; do
    case $arg in
        -h|--help)
            echo "Usage: $0 expname [options] [sbatch_options] [exp_arguments]"
            echo ""
            echo "Arguments:"
            echo "  expname           Experiment name (with or without 'exp.' prefix)"
            echo ""
            echo "Options:"
            show_config_help
            echo ""
            echo "SLURM Options:"
            echo "  --nodes=N         Number of compute nodes"
            echo "  --time=HH:MM:SS   Job time limit"
            echo "  --account=ACCOUNT Account name"
            echo "  --dependency=TYPE Job dependency specification"
            echo ""
            echo "Experiment Arguments:"
            echo "  Any other arguments are passed to the experiment script"
            echo ""
            echo "Examples:"
            echo "  $0 ifces2-atlanLEM-segment2-20200909-exp107"
            echo "  $0 ifces2-atlanLEM-segment2-20200909-exp107 --nodes=50"
            echo "  $0 ifces2-atlanLEM-segment2-20200909-exp107 -c my_config.toml"
            echo "  $0 ifces2-atlanLEM-segment2-20200909-exp107 2 -t --nodes=25"
            exit 0
            ;;
        --nodes=*|--time=*|--account=*|--dependency=*)
            # These are SLURM options
            sbatch_options+=("$arg")
            ;;
        *)
            if [[ -z "$expname" ]]; then
                expname="$arg"
            else
                # All other arguments are experiment arguments
                exp_arguments+=("$arg")
            fi
            ;;
    esac
done

# Validate experiment name
if [[ -z "$expname" ]]; then
    echo "Error: expname is required"
    echo "Usage: $0 expname [options] [sbatch_options] [exp_arguments]"
    exit 1
fi

# Clean up experiment name - remove "exp." prefix if present
if [[ "$expname" == exp.* ]]; then
    expname="${expname#exp.}"
    echo "Removed 'exp.' prefix from experiment name: $expname"
fi

echo "Experiment name: $expname"
if [[ ${#sbatch_options[@]} -gt 0 ]]; then
    echo "SLURM options: ${sbatch_options[*]}"
fi
if [[ ${#exp_arguments[@]} -gt 0 ]]; then
    echo "Experiment arguments: ${exp_arguments[*]}"
fi

#=============================================================================
# ICON Run Configuration
#=============================================================================

# ICON run directory
icon_run_dir=${TOOLS_ICON_BUILD_DIR}/run

# Default SLURM parameters
nodes=160
ctime="08:00:00"
account="${PROJECT_ACCOUNT:-bb1376}"
dependency=""

# Override defaults with any provided SLURM options
for option in "${sbatch_options[@]}"; do
    case "$option" in
        --nodes=*)
            nodes="${option#*=}"
            echo "Using custom node count: $nodes"
            ;;
        --time=*)
            ctime="${option#*=}"
            echo "Using custom time limit: $ctime"
            ;;
        --account=*)
            account="${option#*=}"
            echo "Using custom account: $account"
            ;;
        --dependency=*)
            dependency="${option#*=}"
            echo "Using custom dependency: $dependency"
            ;;
    esac
done

echo "SLURM configuration:"
echo "  Nodes: $nodes"
echo "  Time: $ctime"
echo "  Account: $account"
if [[ -n "$dependency" ]]; then
    echo "  Dependency: $dependency"
fi

#=============================================================================
# Prepare Experiment Script
#=============================================================================

# Find experiment script
expscript_name="exp.${expname}"
expscript_path=""

# Look for experiment script in current directory first
if [[ -f "./${expscript_name}" ]]; then
    expscript_path="$(readlink -f ./${expscript_name})"
elif [[ -f "${SCRIPT_DIR}/${expscript_name}" ]]; then
    expscript_path="$(readlink -f ${SCRIPT_DIR}/${expscript_name})"
else
    echo "Error: Experiment script not found: ${expscript_name}"
    echo "Searched in:"
    echo "  Current directory: ./${expscript_name}"
    echo "  Scripts directory: ${SCRIPT_DIR}/${expscript_name}"
    exit 1
fi

echo "Found experiment script: $expscript_path"

# Change to ICON run directory
cd "${icon_run_dir}" || {
    echo "Error: Cannot change to ICON run directory: $icon_run_dir"
    exit 1
}

echo "Changed to ICON run directory: $(pwd)"

# Copy experiment script
cp "${expscript_path}" .
echo "Copied experiment script to run directory"

#=============================================================================
# Create Target Runscript
#=============================================================================

# Create platform-dependent runscript
make_target='./make_target_runscript'

if [[ ! -f "$make_target" ]]; then
    echo "Error: make_target_runscript not found: $make_target"
    exit 1
fi

echo "Creating target runscript..."
${make_target} \
    memory=0 \
    in_script="exp.${expname}" \
    in_script=exec.iconrun \
    EXPNAME="${expname}" \
    cpu_time="${ctime}" \
    no_of_nodes="${nodes}" \
    account_no="${account}"

# Clean up temporary experiment script
rm "exp.${expname}"
echo "Cleaned up temporary experiment script"

#=============================================================================
# Submit Job
#=============================================================================

# Prepare experiment arguments string
exp_args_string=""
if [[ -n "$CONFIG_OPTION" ]]; then
    exp_args_string="$CONFIG_OPTION"
fi

for arg in "${exp_arguments[@]}"; do
    exp_args_string="$exp_args_string $arg"
done

# Prepare sbatch command - pass resolved script directory as first argument
sbatch_cmd="sbatch  --mem=0"
if [[ -n "$dependency" ]]; then
    sbatch_cmd="$sbatch_cmd --dependency=$dependency"
fi
sbatch_cmd="$sbatch_cmd exp.${expname}.run"
if [[ -n "$exp_args_string" ]]; then
    sbatch_cmd="$sbatch_cmd $exp_args_string"
fi

echo "Submitting job with command:"
echo "$sbatch_cmd"

# Execute sbatch command
eval "$sbatch_cmd"

echo "Job submitted successfully"

