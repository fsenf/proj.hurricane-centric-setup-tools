#!/usr/bin/bash
#=============================================================================
# DESCRIPTION:
#   External parameter processing for hurricane segments using ICON ExtPar tools.
#   Processes topography, land use, soil properties, and other surface parameters
#   for hurricane-centric grids.
#
# USAGE:
#   ./run_extpar_levante.bash [segment_number] [-c|--config config_file]
#
#=============================================================================

# Levante cpu batch job parameters
#
#SBATCH --account=bb1376
#SBATCH --job-name=extpar
#SBATCH --partition=shared
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --distribution=block:block
#SBATCH --mem=20G
#SBATCH --time=01:00:00
#SBATCH --output=../LOG/slurm-%x-%j.out
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

module purge
module load python3 


./run_extpar_generic.bash $@