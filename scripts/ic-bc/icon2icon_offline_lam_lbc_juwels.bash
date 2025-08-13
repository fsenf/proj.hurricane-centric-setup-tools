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
#SBATCH --account=ifces2-scalexa
#SBATCH --job-name=ifs2icon_lbc
#SBATCH --partition=batch
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
# Parallel job control settings
#=============================================================================
Njob_parallel=7  # Maximum number of parallel background jobs

#=============================================================================
# OpenMP environment variables
#=============================================================================
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}
export KMP_AFFINITY=verbose,granularity=fine,scatter
export OMP_STACKSIZE=128M

export START="srun -l --cpu_bind=verbose --distribution=block:cyclic --ntasks=8 --cpus-per-task=${OMP_NUM_THREADS}"

# Load python and cdo modules
module load GCC OpenMPI
module load python3
module load cdo

./icon2icon_offline_lam_lbc_generic.bash
