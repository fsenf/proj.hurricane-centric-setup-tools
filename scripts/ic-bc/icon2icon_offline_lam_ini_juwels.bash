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
#SBATCH --account=ifces2-scalexa
#SBATCH --job-name=ifs2icon_ini
#SBATCH --partition=batch
#SBATCH --nodes=1
#SBATCH --cpus-per-task=4
#SBATCH --time=06:00:00
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

# Load python module
module load GCC OpenMPI Python

export START="srun -l --cpu_bind=verbose --distribution=block:cyclic --ntasks=8 --cpus-per-task=${OMP_NUM_THREADS}"

./icon2icon_offine_lam_ini_generic.bash $@
