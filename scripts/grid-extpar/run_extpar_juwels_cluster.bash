#!/usr/bin/bash
#=============================================================================
# DESCRIPTION:
#   External parameter processing for hurricane segments using ICON ExtPar tools.
#   Processes topography, land use, soil properties, and other surface parameters
#   for hurricane-centric grids.
#
# USAGE:
#   ./run_extpar_juwels.bash [segment_number] [-c|--config config_file]
#
#=============================================================================

# Levante cpu batch job parameters
#
#SBATCH --account=ifces2-scalexa
#SBATCH --job-name=extpar
#SBATCH --partition=batch
#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --distribution=block:block
#SBATCH --time=01:00:00
#SBATCH --output=../LOG/slurm-%x-%j.out
#=============================================================================

set -eux
ulimit -s unlimited
ulimit -c 0

module purge
module load Stages/2025 StdEnv Python

export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}
export KMP_AFFINITY=verbose,granularity=fine,scatter
export OMP_STACKSIZE=128M

./run_extpar_generic.bash $@