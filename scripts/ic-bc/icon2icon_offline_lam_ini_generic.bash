#!/bin/bash
#=============================================================================
# DESCRIPTION:
#   Initial condition processing for hurricane segments using ICON tools.
#
# USAGE:
#   ./icon2icon_offline_lam_ini.bash [segment_number] [-c|--config config_file]
#
#=============================================================================

set -eux
ulimit -s unlimited
ulimit -c 0

#=============================================================================
# Configuration and Argument Parsing
#=============================================================================

# Get script directory
ORIGINAL_SCRIPT_DIR="${SLURM_SUBMIT_DIR}"
echo "Script directory: ${ORIGINAL_SCRIPT_DIR}"

# Load shared configuration handler
source "${ORIGINAL_SCRIPT_DIR}/../../utilities/config_handler.sh"

# Parse config argument
CONFIG_ARG=$(parse_config_argument "$@")

# Handle configuration loading
handle_config "$ORIGINAL_SCRIPT_DIR" \
              "${ORIGINAL_SCRIPT_DIR}/../../config/hurricane_config.toml" \
              "$CONFIG_ARG"

# Remove config arguments and parse remaining arguments
REMAINING_ARGS=($(remove_config_args "$@"))

# Parse segment number
iseg=""
for arg in "${REMAINING_ARGS[@]}"; do
    case $arg in
        -h|--help)
            echo "Usage: $0 [segment_number] [options]"
            echo ""
            echo "Arguments:"
            echo "  segment_number    Hurricane segment number to process"
            echo ""
            show_config_help
            exit 0
            ;;
        -*)
            echo "Error: Unknown option $arg"
            exit 1
            ;;
        *)
            if [[ -z "$iseg" ]]; then
                iseg="$arg"
            else
                echo "Error: Too many positional arguments"
                exit 1
            fi
            ;;
    esac
done

# Validate segment number
if [[ -z "$iseg" ]]; then
    echo "Error: segment_number is required"
    exit 1
fi

if ! [[ "$iseg" =~ ^[0-9]+$ ]]; then
    echo "Error: segment_number must be a positive integer"
    exit 1
fi

echo "Processing segment: $iseg"

# Format segment number with leading zero for consistent naming
iseg_string=$(printf "%02d" $iseg)
echo "Formatted segment string: $iseg_string"

#=============================================================================
# Environment variables
#=============================================================================
ncpus=${SLURM_CPUS_PER_TASK}

cd $PROJECT_WORKING_DIR


# Find IC file using Python utility
INFILE=$(python "${ORIGINAL_SCRIPT_DIR}/../../utilities/find_icbc_file.py" "$CONFIG_FILE" "$iseg" "IC")
TIMESTAMP=$(python3 "${ORIGINAL_SCRIPT_DIR}/../../utilities/print_timings.py" "$CONFIG_FILE" "$iseg" "INIT_DATE")

if [ $? -ne 0 ] || [ -z "$INFILE" ]; then
    echo "Error: IC file not found for segment $iseg"
    exit 1
fi
echo "Found IC file: $INFILE"

#=============================================================================
# Define path to binaries and for input/output 
#=============================================================================

# Directory containing dwd_icon_tool binaries
ICONTOOLS_DIR="$TOOLS_ICONTOOLS_DIR"


# Set up domain name and output grid
DOMNAME="${PROJECT_NAME}/seg${iseg_string}_${PROJECT_WIDTH_CONFIG}"

# Create directory for weights
WORKDIR=`mktemp -d -p ${PROJECT_WORKING_DIR}`
[ ! -d ${WORKDIR} ] && mkdir -p ${WORKDIR}

# Set up input grid
INGRID="${WORKDIR}/ingrid.nc"

# select target variable from ingrid
cdo -selname,cell_area ${REFERENCE_INPUT_GRID} ${INGRID}


# Loop through domains as specified in TOML config
for idom in $(seq 1 ${DOMAINS_NESTS}); do
    echo "Processing domain $idom of ${DOMAINS_NESTS}"
    
    # Run in background for parallel execution
    (
        OUTGRID="${OUTPUT_GRID_BASEDIR}/${DOMNAME}/${PROJECT_NAME}-seg${iseg_string}_dom${idom}_DOM01.nc"

        # Set up output name for initial conditions
        OUTNAME="${TIMESTAMP}_DOM0${idom}_ini.nc"

        OUT_IC_DIR="${OUTPUT_ICBC_BASEDIR}/${DOMNAME}"

        if [ ! -d "${OUT_IC_DIR}" ]; then
            mkdir -p "${OUT_IC_DIR}"
        fi
        FULL_OUTNAME="${OUT_IC_DIR}/${OUTNAME}"


        #=============================================================================
        # Start remapping
        #=============================================================================

        echo "Starting remapping for domain $idom"
        cdo -P $ncpus remapcon,${OUTGRID} -setgrid,${INGRID} -delname,vn ${INFILE} ${FULL_OUTNAME}

        if [ $? -ne 0 ]; then
            echo "Error: Remapping failed for domain $idom"
            exit 1
        fi
    ) &
done

# Wait for all domain processing to complete
wait

echo "All domains processed successfully"

#-----------------------------------------------------------------------------
exit
#-----------------------------------------------------------------------------


