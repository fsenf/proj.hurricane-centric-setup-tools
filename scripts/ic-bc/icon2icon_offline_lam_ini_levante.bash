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


# Load python module
module load python3
module load cdo

./icon2icon_offline_lam_ini_generic.bash $@
