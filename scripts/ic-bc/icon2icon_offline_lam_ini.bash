#!/bin/bash
#=============================================================================
# DESCRIPTION:
#   Initial condition processing for hurricane segments using ICON tools.
#   Converts initial conditions from coarse to nested hurricane-centric grids.
#
# USAGE:
#   ./icon2icon_offline_lam_ini.bash [config_name] [additional_args...]
#
# DEPENDENCIES:
#   - ../../utilities/toml_reader.sh
#   - ../../utilities/ic_bc_utils.sh
#   - ../../config/hurricane_config.toml
#
#=============================================================================
#
# Levante cpu batch job parameters
#
#SBATCH --account=bb1376
#SBATCH --job-name=ifs2icon_ini
#SBATCH --partition=compute
#SBATCH --nodes=1
#SBATCH --cpus-per-task=16
#SBATCH --exclusive
#SBATCH --time=02:00:00
#SBATCH --chdir=/scratch/b/b380352/icontools
#SBATCH --mem=0
#=============================================================================
set -eu
ulimit -s unlimited
ulimit -c 0

# Get script directory - use SLURM_SUBMIT_DIR when submitted via SLURM
ORIGINAL_SCRIPT_DIR="${SLURM_SUBMIT_DIR}"
echo "Script directory: ${ORIGINAL_SCRIPT_DIR}"

# Load TOML reader and configuration
source "${ORIGINAL_SCRIPT_DIR}/../../utilities/toml_reader.sh"
source "${ORIGINAL_SCRIPT_DIR}/../../utilities/ic_bc_utils.sh"
CONFIG_FILE="${ORIGINAL_SCRIPT_DIR}/../../config/hurricane_config.toml"
read_toml_config "$CONFIG_FILE"

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

# Parse arguments
args=("$@")
config_name=${args[0]}
add_args=("${args[@]:1}")

# For backward compatibility: if config_name is a known config file, use legacy approach
# Otherwise, extract segment number from arguments
if [[ "$config_name" == "ic_config.sh" ]]; then
    # Legacy mode: extract segment from add_args
    iseg=${add_args[0]}
    setup_ic_bc_config "$iseg"
elif [[ "$config_name" == "warmstart-config.sh" ]]; then
    # Warmstart mode: use different utility function (to be implemented)
    echo "Warmstart mode not yet implemented with TOML config"
    exit 1
else
    # Direct segment mode: config_name is actually the segment number
    iseg="$config_name"
    setup_ic_bc_config "$iseg"
fi

#=============================================================================
# Define path to binaries and for input/output 
#=============================================================================

# Directory containing dwd_icon_tool binaries
ICONTOOLS_DIR="$PATHS_ICONTOOLS_DIR"

# File name of input grid/file to be remapped without path
DATAFILE="${INFILE##*/}"

# Create directory for weights
WEIGHTDIR=`mktemp -d -p /scratch/b/b380352/icontools`
NAMELIST_FILE=${WEIGHTDIR}/NAMELIST_ICONREMAP_INI

[ ! -d ${WEIGHTDIR} ] && mkdir -p ${WEIGHTDIR}

cat > ${NAMELIST_FILE} << REMAP_NML_EOF
! REMAPPING NAMELIST FILE
!
&remap_nml
 in_grid_filename   = "${INGRID}"                           ! file containing grid information of input data
 in_filename        = "${INFILE}"                           ! input data file name
 in_type            = 2                                     ! type of input grid (2: triangular)
 out_grid_filename  = "${OUTGRID}"                          ! containing output grid
 out_filename       = "${OUTNAME}"                          ! output file name
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

REMAP_NML_EOF

#=============================================================================
# Start remapping
#=============================================================================

${START} ${ICONTOOLS_DIR}/iconremap --remap_nml ${NAMELIST_FILE}
rm ${NAMELIST_FILE}

#-----------------------------------------------------------------------------
exit
#-----------------------------------------------------------------------------


