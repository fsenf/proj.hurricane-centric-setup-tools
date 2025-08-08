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
#SBATCH --account=ifces2-scalexa
#SBATCH --job-name=gridgen
#SBATCH --partition=batch
#SBATCH --nodes=1
#SBATCH --cpus-per-task=48
#SBATCH --time=02:30:00
#SBATCH --output=../LOG/slurm-%x-%j.out
#=============================================================================

set -eux
ulimit -s unlimited
ulimit -c 0

#=============================================================================
# Environment Setup
#=============================================================================

module load Stages/2025 GCC OpenMPI Python

# OpenMP environment variables
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}
export KMP_AFFINITY=verbose,granularity=fine,scatter
export OMP_STACKSIZE=128M

export START="srun -l --cpu_bind=verbose --distribution=block:cyclic --ntasks-per-node=1 --cpus-per-task=${OMP_NUM_THREADS}"

./generate_grid_for_hurricane_segments_generic.sh