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

# Load python module
module load python3

./icon2icon_offline_lam_ini_generic.bash $@
