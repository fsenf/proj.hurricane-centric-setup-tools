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
#SBATCH --chdir=/scratch/b/b380352/icontools
#SBATCH --time=02:30:00
#=============================================================================

# Use SLURM_SUBMIT_DIR to get the directory where sbatch was called from
SCRIPT_DIR="${SLURM_SUBMIT_DIR}"
echo "Script submit directory: ${SCRIPT_DIR}"

set +x
ulimit -s unlimited
ulimit -c 0
#=============================================================================
#
# OpenMP environment variables
#
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}
export KMP_AFFINITY=verbose,granularity=fine,scatter
export OMP_STACKSIZE=128M

# #=============================================================================
# #
# # Environment variables for the experiment and the target system
# #
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


# LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
# INPUT ARGUMENT

iseg=$1

# TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT



module load python3

#-----------------------------------------------------------------------------
# PART I: Create auxiliary grid file which contains only the cells of the 
#         boundary zone.
#-----------------------------------------------------------------------------

# Directory containing dwd_icon_tool binaries
ICONTOOLS_DIR=/work/bb1174/models/icon/dwd_icon_tools/icontools

width_config = 'width20km'
project_subpath = 'paulette-segments' 

DOMNAME="${project_subpath}/seg${iseg}_${width_config}"

maskdir=/work/bb1376/data/icon/grids-extpar/${DOMNAME}
maskname=${maskdir}/'paulette_segment_mask_ifces2-atlanXL-20200907-exp021_seg${iseg}_dom${idom}.nc'

if [ ! -d ${maskdir} ]; then
    mkdir -p ${maskdir}
fi

outputdir="/work/bb1376/data/icon/grids-extpar/"${DOMNAME}
outfile=${outputdir}/'paulette-seg${iseg}_dom${idom}'

basegrid="/work/bb1376/data/icon/grids-extpar/atlanXL/atlanXL_R02B10_DOM02.nc"

nests=3

idom=0
fname=`eval echo $outfile`"_DOM01.nc"
ln -s $basegrid $fname

for ((idom = 1 ; idom <= $nests  ; idom++)); do

    # (1) Create the Mask based on the new Grid
    # =========================================
    cd "${SCRIPT_DIR}/../../utilities"
    python 35-Create-a-Segment-Mask-for-Hurricane-Centric-Runs.py $iseg $idom 
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
    ${START} ${ICONTOOLS_DIR}/icongridgen -vv --nml NAMELIST_ICONGRIDGEN
    

    # (4) Set the New Basefile
    # ========================
    fname=`eval echo $outfile`"_DOM01.nc"


    # Clean up
    #rm ${OUTDIR}/NAMELIST*

done

#-----------------------------------------------------------------------------
exit
#-----------------------------------------------------------------------------

