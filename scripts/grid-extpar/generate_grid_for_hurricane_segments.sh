#!/bin/bash
#=============================================================================
# DESCRIPTION:
#   Grid generation script for hurricane segments. Creates nested grids
#   centered on hurricane trajectories using ICON tools and segment masks.
#
# USAGE:
#   ./generate_grid_for_hurricane_segments.sh [segment_number]
#
# DEPENDENCIES:
#   This script calls:
#   - ../../utilities/35-Create-a-Segment-Mask-for-Hurricane-Centric-Runs.py
#   - ../../utilities/toml_reader.sh
#   - ../../config/hurricane_config.toml
#
#=============================================================================
#
# Levante cpu batch job parameters
#
#SBATCH --account=bb1376
#SBATCH --job-name=gridgen
#SBATCH --partition=compute
#SBATCH --nodes=1
#SBATCH --cpus-per-task=16
#SBATCH --exclusive
#SBATCH --time=02:30:00
#=============================================================================


set -eux
ulimit -s unlimited
ulimit -c 0

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

export START="srun -l --cpu_bind=verbose --distribution=block:cyclic --ntasks-per-node=8 --cpus-per-task=${OMP_NUM_THREADS}"

#=============================================================================
# Get Configuration and Script Directory
#=============================================================================
ORIGINAL_SCRIPT_DIR="${SLURM_SUBMIT_DIR}"
echo "Script directory: ${ORIGINAL_SCRIPT_DIR}"

# Load TOML reader and configuration
source "${ORIGINAL_SCRIPT_DIR}/../../utilities/toml_reader.sh"
CONFIG_FILE="${ORIGINAL_SCRIPT_DIR}/../../config/hurricane_config.toml"
read_toml_config "$CONFIG_FILE"

cd $PROJECT_WORKING_DIR

# INPUT ARGUMENT
iseg=$1

module load python3

#-----------------------------------------------------------------------------
# PART I: Create auxiliary grid file which contains only the cells of the 
#         boundary zone.
#-----------------------------------------------------------------------------

# Use configuration values with new variable names
DOMNAME="${PROJECT_NAME}/seg${iseg}_${PROJECT_WIDTH_CONFIG}"
maskdir=${OUTPUT_GRID_BASEDIR}/${DOMNAME}
maskname=${maskdir}/'${PROJECT_NAME}_mask_${REFERENCE_EXPNAME}_seg${iseg}_dom${idom}.nc'

if [ ! -d ${maskdir} ]; then
    mkdir -p ${maskdir}
fi

outputdir="${OUTPUT_GRID_BASEDIR}/${DOMNAME}"
outfile=${outputdir}/'${PROJECT_NAME}-seg${iseg}_dom${idom}'

idom=0
fname=`eval echo $outfile`"_DOM01.nc"
ln -s $REFERENCE_INPUT_GRID $fname

for ((idom = 1 ; idom <= $DOMAINS_NESTS ; idom++)); do

    # (1) Create the Mask based on the new Grid
    # =========================================
    cd "${ORIGINAL_SCRIPT_DIR}/../../utilities"
    python 35-Create-a-Segment-Mask-for-Hurricane-Centric-Runs.py $iseg $idom "$CONFIG_FILE"
    cd -

    # (2) Prepare Grid Config
    # =======================
    mname=`eval echo $maskname`
    ofile=`eval echo $outfile`

    cat > NAMELIST_ICONGRIDGEN << EOF_1
&gridgen_nml
 filename="${fname}" 
  centre=78
  subcentre=255
  lspring_dynamics= .TRUE.
  maxit=2000
  beta_spring=0.9
  lfixed_boundary= .TRUE.
  initial_refinement= .TRUE.
  max_ncells=-1
  bdy_indexing_depth=21  ! default is 14
   dom(1)%region_type=4
   dom(1)%outfile="${ofile}"
   dom(1)%number_of_grid_used=99
   dom(1)%pole_lon=0.0
   dom(1)%pole_lat=90.0
   dom(1)%lwrite_parent= .FALSE.
   dom(1)%expression="if([mask] > 0, 1, 0)"
   dom(1)%expression_var="${mname}","mask"
  parent_id= 0
/
EOF_1

    # (3) Grid Creation
    # =================
    ${START} ${TOOLS_ICONTOOLS_DIR}/icongridgen -vv --nml NAMELIST_ICONGRIDGEN
    

    # (4) Set the New Basefile
    # ========================
    fname=`eval echo $outfile`"_DOM01.nc"

done

exit

