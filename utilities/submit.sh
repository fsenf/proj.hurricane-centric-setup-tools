#!/bin/bash
#=============================================================================
# DESCRIPTION:
#   Universal job submission utility for hurricane-centric setup tools
#   Automatically detects platform, script type, and submits with appropriate SBATCH settings
#
# USAGE:
#   submit.sh <script> [sbatch_options] [script_arguments]
#
# ARGUMENTS:
#   script              - Path to script to submit
#
# SBATCH OPTIONS:
#   --dependency=TYPE   - Job dependency specification
#   --parsable         - Return job ID only (set by default)
#   --no-parsable      - Disable parsable output
#   Other sbatch options are passed through
#
# EXAMPLES:
#   submit.sh generate_grid_for_hurricane_segments.sh -c config.toml 2
#   submit.sh icon2icon_offline_lam_lbc.bash --dependency=afterok:12345 -c config.toml 1
#   JOB_ID=$(submit.sh script.sh args)  # Capture job ID
#=============================================================================

# Parse arguments
if [[ $# -lt 1 ]]; then
    echo "Error: Script argument required" >&2
    echo "Usage: $0 <script> [sbatch_options] [script_arguments]" >&2
    exit 1
fi

SCRIPT="$1"
shift

# Check if script exists
if [[ ! -f "$SCRIPT" ]]; then
    echo "Error: Script '$SCRIPT' not found" >&2
    exit 1
fi

# Auto-detect script type from filename
detect_script_type() {
    local script_name=$(basename "$1")
    
    case "$script_name" in
        *grid*|generate_grid*)
            echo "grid_gen"
            ;;
        *extpar*|run_extpar*)
            echo "extpar"
            ;;
        *testrun*|test*)
            echo "testrun"
            ;;
        *production*|*starter*)
            echo "production"
            ;;
        *lbc*|*icon2icon*lbc*)
            echo "lbc"
            ;;
        *ini*|*icon2icon*ini*)
            echo "ini"
            ;;
        *)
            echo "default"
            ;;
    esac
}

TYPE=$(detect_script_type "$SCRIPT")

# Detect platform and load SBATCH configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PLATFORM=${PLATFORM:-$($SCRIPT_DIR/detect_platform.sh)}
source "$SCRIPT_DIR/../config/${PLATFORM}/sbatch_env_setter.sh" "$TYPE"

# Parse sbatch options and script arguments
sbatch_args=()
script_args=()
parsable="--parsable"  # Default to parsable for job ID capture
dependency=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --dependency=*)
            dependency="$1"
            sbatch_args+=("$1")
            shift
            ;;
        --parsable)
            parsable="--parsable"
            shift
            ;;
        --no-parsable)
            parsable=""
            shift
            ;;
        --nodes=*|--ntasks=*|--time=*|--partition=*|--account=*|--output=*|--job-name=*|--mem=*|--exclusive=*|--cpus-per-task=*)
            # Override SBATCH settings from command line
            sbatch_args+=("$1")
            shift
            ;;
        --*)
            # Other sbatch options
            sbatch_args+=("$1")
            shift
            ;;
        -*)
            # Script options (start with single dash)
            script_args+=("$1")
            shift
            ;;
        *)
            # Positional arguments go to script
            script_args+=("$1")
            shift
            ;;
    esac
done

# Build sbatch command with detected configuration
sbatch_cmd=(
    "sbatch"
    ${parsable:+"$parsable"}
    "--job-name=$SBATCH_JOB_NAME"
    "--partition=$SBATCH_PARTITION"
    "--cpus-per-task=$SBATCH_CPUS_PER_TASK"
    "--time=$SBATCH_TIME"
    "--account=$SBATCH_ACCOUNT"
    "--output=$SBATCH_OUTPUT"
    "--mem=$SBATCH_MEM"
)

# Add either --ntasks or --nodes depending on what's configured
if [[ -n "${SBATCH_NTASKS:-}" ]]; then
    sbatch_cmd+=("--ntasks=$SBATCH_NTASKS")
elif [[ -n "${SBATCH_NODES:-}" ]]; then
    sbatch_cmd+=("--nodes=$SBATCH_NODES")
fi

# Add any additional sbatch arguments (including dependency)
sbatch_cmd+=("${sbatch_args[@]}")

# Add the script and its arguments
sbatch_cmd+=("$SCRIPT" "${script_args[@]}")

# Debug output (if requested) - send to stderr to not interfere with job ID
if [[ "${HURRICANE_DEBUG_SUBMIT:-false}" == "true" ]]; then
    echo "Platform: $PLATFORM" >&2
    echo "Script type: $TYPE" >&2
    echo "SBATCH command: ${sbatch_cmd[*]}" >&2
fi

# Execute the sbatch command
exec "${sbatch_cmd[@]}"
