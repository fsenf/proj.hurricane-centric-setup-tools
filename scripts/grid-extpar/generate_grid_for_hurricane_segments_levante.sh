#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/grid-extpar/generate_grid_for_hurricane_segments.sh
#=============================================================================
# DESCRIPTION:
#   Grid generation script for hurricane segments. Creates nested grids
#   centered on hurricane trajectories using ICON tools and segment masks.
#
# USAGE:
#   ./generate_grid_for_hurricane_segments.sh [segment_number] [-c|--config config_file]
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
#=============================================================================
#
# Levante cpu batch job parameters
#
#SBATCH --account=bb1376
#SBATCH --job-name=gridgen
#SBATCH --partition=compute
#SBATCH --nodes=1
#SBATCH --cpus-per-task=128
#SBATCH --exclusive
#SBATCH --time=02:30:00
#SBATCH --output=../LOG/slurm-%x-%j.out
#=============================================================================

set -eux
ulimit -s unlimited
ulimit -c 0


#=============================================================================
# Environment Setup
#=============================================================================

# OpenMP environment variables
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

export START="srun -l --cpu_bind=verbose --distribution=block:cyclic --ntasks-per-node=1 --cpus-per-task=${OMP_NUM_THREADS}"

module load python3

./generate_grid_for_hurricane_segments_generic.sh