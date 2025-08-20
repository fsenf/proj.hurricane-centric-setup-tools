#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/ic-bc/icon2icon_offline_lam_lbc.bash
#=============================================================================
# DESCRIPTION:
#   Boundary condition processing for hurricane segments using ICON tools.
#   Converts boundary conditions from coarse to nested hurricane-centric grids.
#
# USAGE:
#   ./icon2icon_offline_lam_lbc.bash [segment_number] [-c|--config config_file]
#
# ARGUMENTS:
#   segment_number  - Hurricane segment number to process
#
# OPTIONS:
#   -c, --config    - Path to TOML configuration file (optional)
#                     Default: ../../config/hurricane_config.toml
#                     Relative paths are resolved from script directory
#   -h, --help      - Show this help message
#
# DEPENDENCIES:
#   - ../../utilities/config_handler.sh
#   - ../../utilities/toml_reader.sh
#   - ../../utilities/find_icbc_file.py
#   - ../../config/hurricane_config.toml
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
# Parallel job control settings
#=============================================================================
Njob_parallel=2  # Maximum number of parallel background jobs

ncpus=${SLURM_CPUS_PER_TASK}
cd $PROJECT_WORKING_DIR


# Find BC files using Python utility
DATAFILELIST=($(python "${ORIGINAL_SCRIPT_DIR}/../../utilities/find_icbc_file.py" "$CONFIG_FILE" "$iseg" "BC"))
if [ $? -ne 0 ] || [ ${#DATAFILELIST[@]} -eq 0 ]; then
    echo "Error: No BC files found for segment $iseg"
    exit 1
fi
echo "Found ${#DATAFILELIST[@]} BC files for segment $iseg"

#=============================================================================
# Define path to binaries and for input/output 
#=============================================================================

# Set up input grid
INGRID="$REFERENCE_INPUT_GRID"

# Set up domain name and grids
DOMNAME="${PROJECT_NAME}/seg${iseg_string}_${PROJECT_WIDTH_CONFIG}"
OUTGRID="${OUTPUT_GRID_BASEDIR}/${DOMNAME}/${PROJECT_NAME}-seg${iseg_string}_dom1_DOM01.nc"

# Output directory for boundary data
OUTDIR="${OUTPUT_ICBC_BASEDIR}/${DOMNAME}"
if [ ! -d "${OUTDIR}" ]; then
    mkdir -p "${OUTDIR}"
fi

# Name of boundary grid
BOUNDGRID=${OUTGRID%.*}'_lbc.nc'

# Geometry file from reference dataset
GEODIR="${REFERENCE_INPUT_ICBC_DIR}/${REFERENCE_INPUT_ICBC_SUBDIR}"
GEOFILE="${GEODIR}/lam_input_geo_DOM02_ML.nc"

#=============================================================================
# PART I: Create auxiliary grid file which contains only the cells of the 
#         boundary zone.
#=============================================================================

# Create temporary namelist file for iconsub
TEMP_NAMELIST_SUB=$(mktemp --suffix=_NAMELIST_ICONSUB_LBC)

cat > ${TEMP_NAMELIST_SUB} << EOF_1
&iconsub_nml
  grid_filename     = '${OUTGRID}',
  output_type       = 4,
  lwrite_grid       = .TRUE.,
/
&subarea_nml
  out_grid_filename = '${BOUNDGRID}',
  min_refin_c_ctrl  = 1
  max_refin_c_ctrl  = 21
/
EOF_1

${START} ${TOOLS_ICONTOOLS_DIR}/iconsub -vv --nml ${TEMP_NAMELIST_SUB}

# Clean up temporary file
rm ${TEMP_NAMELIST_SUB}

#=============================================================================
# PART II: Get Weight file for remapping
#=============================================================================

# Set up input grid
source_grid_file=$(mktemp --tmpdir=${PROJECT_WORKING_DIR})
cdo -selname,cell_area ${REFERENCE_INPUT_GRID} ${source_grid_file}

target_grid_file=$(mktemp --tmpdir=${PROJECT_WORKING_DIR})
cdo -selname,cell_area ${BOUNDGRID} ${target_grid_file}

# Create weights file for remapping
weights_file=$(mktemp --tmpdir=${PROJECT_WORKING_DIR})
cdo -P ${SLURM_JOB_CPUS_PER_NODE} gencon,${target_grid_file} ${source_grid_file} ${weights_file}

# Finally set the remap command
remap_cmd="cdo -P ${ncpus} remap,${target_grid_file},${weights_file}"

#=============================================================================
# PART III: Remap geofile
#=============================================================================
z_ifc_output_file=$(mktemp --tmpdir=${PROJECT_WORKING_DIR})
${remap_cmd} -selname,z_ifc ${GEOFILE} ${z_ifc_output_file}


# Initialize job counter for parallel processing
njobs=0

# Loop over all BC files
for datafilename in "${DATAFILELIST[@]}" ; do
    echo "Processing BC file: $datafilename"

    # Get filename without path
    datafile="${datafilename##*/}"

    # Get filename without suffix
    outdatafile=${datafile%.*}
    outdatafile=${outdatafile#*ML_}

    FULL_OUTNAME="${OUTDIR}/${outdatafile}_lbc.nc"

    if [ -f "${FULL_OUTNAME}" ]; then
        echo "Output file ${FULL_OUTNAME} already exists. Removing ${FULL_OUTNAME}."
        rm -f "${FULL_OUTNAME}"
    fi

    # Run in background for parallel execution
    (
        #=============================================================================
        # Start remapping
        #=============================================================================

        echo "Starting remapping for file:" "$datafile"
        novn_output_file=$(mktemp --tmpdir=${PROJECT_WORKING_DIR})
        ${remap_cmd} -delname,vn ${datafilename} ${novn_output_file}
        
        
        if [ $? -ne 0 ]; then
            echo "Error: Remapping failed for file: $datafile"
            exit 1
        fi

        #=============================================================================
        # Final variable merging
        #=============================================================================

        cdo merge ${novn_output_file} ${z_ifc_output_file} ${FULL_OUTNAME}

        # Clean up temporary files for this iteration
        rm ${novn_output_file}
    ) &
    
    # Increment job counter
    echo "Started background job for $datafile for $njobs" 
    njobs=$((njobs + 1))
    
    # Wait for jobs if we've reached the parallel limit
    if (( njobs % Njob_parallel == 0 )); then
        echo "Waiting for background jobs to complete..."
        echo "Current job count: $njobs"
        # Wait for any background job to complete
        wait -n  # wait for any background job to complete
    fi
done

# Wait for all background jobs to complete
wait

# ==============================================================================
# PART III: Finalize and clean up
# ==============================================================================

echo "All background jobs completed. Finalizing..."
# Clean up temporary files
rm -f ${source_grid_file}
rm -f ${target_grid_file}
rm -f ${weights_file}
rm -f ${z_ifc_output_file}


#-----------------------------------------------------------------------------
exit
#-----------------------------------------------------------------------------
