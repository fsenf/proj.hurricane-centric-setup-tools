#!/bin/bash
#=============================================================================
# DESCRIPTION:
#   Initial condition processing for hurricane segments using ICON tools.
#
# USAGE:
#   ./icon2icon_offline_lam_ini.bash [segment_number] [-c|--config config_file]
#
#=============================================================================
#
# Levante cpu batch job parameters
#
#SBATCH --account=bb1376
#SBATCH --job-name=ifs2icon_ini
#SBATCH --partition=compute
#SBATCH --nodes=1
#SBATCH --cpus-per-task=4
#SBATCH --exclusive
#SBATCH --time=06:00:00
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




# Load python module
module load python3

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

# Set up input grid
INGRID="$REFERENCE_INPUT_GRID"

# Set up domain name and output grid
DOMNAME="${PROJECT_NAME}/seg${iseg}_${PROJECT_WIDTH_CONFIG}"

# Loop through domains as specified in TOML config
for idom in $(seq 1 ${DOMAINS_NESTS}); do
    echo "Processing domain $idom of ${DOMAINS_NESTS}"
    
    # Run in background for parallel execution
    (
        OUTGRID="${OUTPUT_GRID_BASEDIR}/${DOMNAME}/${PROJECT_NAME}-seg${iseg}_dom${idom}_DOM01.nc"

        # Set up output name for initial conditions
        OUTNAME="${TIMESTAMP}_DOM0${idom}_ini.nc"

        OUT_IC_DIR="${OUTPUT_ICBC_BASEDIR}/${DOMNAME}"

        if [ ! -d "${OUT_IC_DIR}" ]; then
            mkdir -p "${OUT_IC_DIR}"
        fi
        FULL_OUTNAME="${OUT_IC_DIR}/${OUTNAME}"

        # Create directory for weights
        WEIGHTDIR=`mktemp -d -p ${PROJECT_WORKING_DIR}`
        NAMELIST_FILE=${WEIGHTDIR}/NAMELIST_ICONREMAP_INI

        [ ! -d ${WEIGHTDIR} ] && mkdir -p ${WEIGHTDIR}

        # Generate namelist and run remap for this domain
        cat > ${NAMELIST_FILE} << REMAP_NML_EOF
! REMAPPING NAMELIST FILE
!
&remap_nml
 in_grid_filename   = "${INGRID}"                           ! file containing grid information of input data
 in_filename        = "${INFILE}"                           ! input data file name
 in_type            = 2                                     ! type of input grid (2: triangular)
 out_grid_filename  = "${OUTGRID}"                          ! containing output grid
 out_filename       = "${FULL_OUTNAME}"                          ! output file name
 out_type           = 2                                     ! type of output grid (2: triangular)
 out_filetype       = 4                                     ! output filetype (4: NetCDF)
 lsynthetic_grid    = .FALSE.                               ! .TRUE. if output grid shall be created from scratch
 ncstorage_file     = "${WEIGHTDIR}/ncstorage_ini.tmp"
/
! DEFINITION FOR INPUT DATA FIELD
&input_field_nml
inputname = "rho"
outputname = "rho"
intp_method = 3
/
&input_field_nml
inputname = "pres"
outputname = "pres"
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
inputname = "qg"
outputname = "qg"
intp_method = 3
/
&input_field_nml
inputname = "qh"
outputname = "qh"
intp_method = 3
/
&input_field_nml
inputname = "qnc"
outputname = "qnc"
intp_method = 3
/
&input_field_nml
inputname = "qns"
outputname = "qns"
intp_method = 3
/
&input_field_nml
inputname = "qnr"
outputname = "qnr"
intp_method = 3
/
&input_field_nml
inputname = "qni"
outputname = "qni"
intp_method = 3
/
&input_field_nml
inputname = "qng"
outputname = "qng"
intp_method = 3
/
&input_field_nml
inputname = "qnh"
outputname = "qnh"
intp_method = 3
/
&input_field_nml
inputname = "temp"
outputname = "temp"
intp_method = 3
/
&input_field_nml
inputname = "theta_v"
outputname = "theta_v"
intp_method = 3
/
&input_field_nml
inputname = "tke" 
outputname = "tke"
intp_method = 3
/
&input_field_nml
inputname = "u"
outputname = "u"
intp_method = 3
/
&input_field_nml
inputname = "v"
outputname = "v"
intp_method = 3
/
&input_field_nml
inputname = "w"
outputname = "w"
intp_method = 3
/
&input_field_nml
inputname = "c_t_lk"
outputname = "c_t_lk"
intp_method = 3
/
&input_field_nml
inputname = "freshsnow"
outputname = "freshsnow"
intp_method = 3
/
&input_field_nml
inputname = "fr_seaice"
outputname = "fr_seaice"
intp_method = 3
/
&input_field_nml
inputname = "h_ice"
outputname = "h_ice"
intp_method = 3
/
&input_field_nml
inputname = "h_ml_lk"
outputname = "h_ml_lk"
intp_method = 3
/
&input_field_nml
inputname = "h_snow"
outputname = "h_snow"
intp_method = 3
/
&input_field_nml
inputname = "hsnow_max"
outputname = "hsnow_max"
intp_method = 3
/
&input_field_nml
inputname = "qv_s"
outputname = "qv_s"
intp_method = 3
/
&input_field_nml
inputname = "rho_snow"
outputname = "rho_snow"
intp_method = 3
/
&input_field_nml
inputname = "t_sk"
outputname = "t_sk"
intp_method = 3
/
&input_field_nml
inputname = "snowfrac"
outputname = "snowfrac"
intp_method = 3
/
&input_field_nml
inputname = "t_bot_lk"
outputname = "t_bot_lk"
intp_method = 3
/
&input_field_nml
inputname = "t_g"
outputname = "t_g"
intp_method = 3
/
&input_field_nml
inputname = "t_ice"
outputname = "t_ice"
intp_method = 3
/
&input_field_nml
inputname = "t_mnw_lk"
outputname = "t_mnw_lk"
intp_method = 3
/
&input_field_nml
inputname = "t_seasfc"
outputname = "t_seasfc"
intp_method = 3
/
&input_field_nml
inputname = "t_snow"
outputname = "t_snow"
intp_method = 3
/
&input_field_nml
inputname = "t_wml_lk"
outputname = "t_wml_lk"
intp_method = 3
/
&input_field_nml
inputname = "w_i"
outputname = "w_i"
intp_method = 3
/
&input_field_nml
inputname = "gz0"
outputname = "gz0"
intp_method = 3
/
&input_field_nml
inputname = "t_so"
outputname = "t_so"
intp_method = 3
loptional = .TRUE.
/
&input_field_nml
inputname = "t_2m"
outputname = "t_2m"
intp_method = 3
/
&input_field_nml
inputname = "w_so"
outputname = "w_so"
intp_method = 3
/
&input_field_nml
inputname = "w_so_ice"
outputname = "w_so_ice"
intp_method = 3
/
&input_field_nml
inputname = "w_snow"
outputname = "w_snow"
intp_method = 3
/
&input_field_nml
inputname = "z_ifc"
outputname = "z_ifc"
intp_method = 2
loptional=.TRUE.
/
&input_field_nml
inputname = "z_mc"
outputname = "z_mc"
intp_method = 2
loptional=.TRUE.
/
REMAP_NML_EOF

        #=============================================================================
        # Start remapping
        #=============================================================================

        ${START} ${ICONTOOLS_DIR}/iconremap --remap_nml ${NAMELIST_FILE}
        rm ${NAMELIST_FILE}
        rm -rf ${WEIGHTDIR}
    ) &
    

done

# Wait for all domain processing to complete
wait

echo "All domains processed successfully"

#-----------------------------------------------------------------------------
exit
#-----------------------------------------------------------------------------


