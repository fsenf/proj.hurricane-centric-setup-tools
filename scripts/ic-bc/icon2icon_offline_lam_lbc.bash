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
#
# Levante cpu batch job parameters
#
#SBATCH --account=bb1376
#SBATCH --job-name=ifs2icon_lbc
#SBATCH --partition=compute
#SBATCH --nodes=1
#SBATCH --cpus-per-task=4
#SBATCH --exclusive
#SBATCH --time=04:00:00
#SBATCH --mem=0
#SBATCH --output=../LOG/slurm-%x-%j.out
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
Njob_parallel=7  # Maximum number of parallel background jobs

#=============================================================================
# OpenMP environment variables
#=============================================================================
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}
export KMP_AFFINITY=verbose,granularity=fine,scatter
export OMP_STACKSIZE=128M

# Environment variables for the experiment and the target system
export OMPI_MCA_pml="ucx"
export OMPI_MCA_btl=self
export OMPI_MCA_osc="pt2pt"
export UCX_IB_ADDR_TYPE=ib_global
export OMPI_MCA_coll="^ml,hcoll"
export OMPI_MCA_coll_hcoll_enable="0"
export HCOLL_ENABLE_MCAST_ALL="0"
export HCOLL_MAIN_IB=mlx5_0:1
export UCX_NET_DEVICES=mlx5_0:1
export UCX_TLS=mm,knem,cma,dc_mlx5,dc_x,self
export UCX_UNIFIED_MODE=y
export HDF5_USE_FILE_LOCKING=FALSE
export OMPI_MCA_io="romio321"
export UCX_HANDLE_ERRORS=bt

export START="srun -l --cpu_bind=verbose --distribution=block:cyclic --ntasks=8 --cpus-per-task=${OMP_NUM_THREADS}"

cd $PROJECT_WORKING_DIR

# Load python and cdo modules
module load python3
module load cdo

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
# PART II: Extract boundary data
#=============================================================================

# Create directory for weights
WEIGHTDIR=${OUTDIR}/lbc_weights
[ ! -d ${WEIGHTDIR} ] && mkdir -p ${WEIGHTDIR}

# Create temporary namelist for field definitions
TEMP_NAMELIST_FIELDS=$(mktemp --suffix=_NAMELIST_ICONREMAP_FIELDS)

cat > ${TEMP_NAMELIST_FIELDS} << EOF_2A
&input_field_nml
inputname = "rho"
outputname = "rho"
intp_method = 3
/
&input_field_nml
inputname = "qc"
outputname = "qc"
intp_method = 3
/
&input_field_nml
inputname = "qi"
outputname = "qi"
intp_method = 3
/
&input_field_nml
inputname = "qr"
outputname = "qr"
intp_method = 3
/
&input_field_nml
inputname = "qv"
outputname = "qv"
intp_method = 3
/
&input_field_nml
inputname = "qs"
outputname = "qs"
intp_method = 3
/
&input_field_nml
inputname = "theta_v"
outputname = "theta_v"
intp_method = 3
/
&input_field_nml
inputname = "vn"
outputname = "vn"
intp_method = 3
/
&input_field_nml
inputname = "w"
outputname = "w"
intp_method = 3
/
&input_field_nml
inputname = "z_ifc"
outputname = "z_ifc"
intp_method = 3
/
EOF_2A

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

    # Run in background for parallel execution
    (
        # Add geo reference - create temporary merged file
        temp_file=$(mktemp --tmpdir=${PROJECT_WORKING_DIR})
        cdo -P 4 merge ${datafilename} ${GEOFILE} ${temp_file}
        
        # Create temporary namelist for this file
        TEMP_NAMELIST_LBC=$(mktemp --suffix=_NAMELIST_ICONREMAP_LBC)
        
        # Create unique ncstorage file to avoid conflicts in parallel execution
        ncstorage_file="${WEIGHTDIR}/ncstorage_lbc_${outdatafile}.tmp"
        
        cat > ${TEMP_NAMELIST_LBC} << EOF_2C
&remap_nml
 in_grid_filename  = '${INGRID}'                         ! file containing grid information of input data
 in_filename       = '${temp_file}'                      ! input data file name
 in_type           = 2                                   ! type of input grid (2: triangular)
 out_grid_filename = '${BOUNDGRID}'                      ! output lateral boundary grid
 out_filename      = '${OUTDIR}/${outdatafile}_lbc.nc'   ! output file name
 out_type          = 2                                   ! type of output grid (2: triangular)
 out_filetype      = 4                                   ! output filetype (4: NetCDF)
 l_have3dbuffer    = .FALSE.                             ! buffer option
 ncstorage_file    = "${ncstorage_file}"
/
EOF_2C

        ${START} ${TOOLS_ICONTOOLS_DIR}/iconremap -q --remap_nml ${TEMP_NAMELIST_LBC} \
                 --input_field_nml ${TEMP_NAMELIST_FIELDS} 2>&1
        
        # Clean up temporary files for this iteration
        rm ${TEMP_NAMELIST_LBC}
        rm ${temp_file}
        rm -f ${ncstorage_file}
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

echo "All BC files processed successfully"

# Clean up field namelist
rm ${TEMP_NAMELIST_FIELDS}

#-----------------------------------------------------------------------------
exit
#-----------------------------------------------------------------------------
