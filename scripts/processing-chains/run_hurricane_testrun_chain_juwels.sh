#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/processing-chains/run_hurricane_testrun_chain.sh
#=============================================================================
# DESCRIPTION:
#   This script checks if all required files for a hurricane segment have been
#   properly created before allowing runscript creation. It verifies:
#   1. Grid files
#   2. External parameter files (extpar)
#   3. Initial condition files (IC)
#   4. Boundary condition files (BC)
#
# USAGE:
#   ./run_hurricane_testrun_chain.sh [segment_number] [-c|--config config_file]
#
# ARGUMENTS:
#   segment_number  - The hurricane segment number to check
#
# OPTIONS:
#   -c, --config    - Path to TOML configuration file (required)
#   -h, --help      - Show this help message
#
# EXIT CODES:
#   0 - Success, all files exist and are valid
#   1 - Invalid arguments
#   2 - Missing or invalid files
#   3 - Insufficient file count
#
# AUTHOR:
#   Fabian Senf (senf@tropos.de)
#
# DATE:
#   June 2025
#=============================================================================
#SBATCH --job-name=testrun_chain  # Specify job name
#SBATCH --partition=batch        # Specify partition name
#SBATCH --nodes=1
#SBATCH --time=00:05:00           # Set a limit on the total run time
#SBATCH --account=ifces2-scalexa          # Charge resources on this project account
#SBATCH --output=../LOG/slurm-%x-%j.out

module load StdEnv Python SciPy-Stack netcdf4-python Cartopy

./run_hurricane_testrun_chain_generic.sh $@
