#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/config/juwels/sbatch_env_setter.sh
#=============================================================================
# DESCRIPTION:
#   SBATCH environment variable setter for Juwels platform
#   Sets platform and script-specific SBATCH parameters
#
# USAGE:
#   source config/juwels/sbatch_env_setter.sh <TYPE>
#
# ARGUMENTS:
#   TYPE - Script type (e.g., grid_gen, extpar, testrun, production, lbc, ini)
#
# EXPORTS:
#   SBATCH_* variables for dynamic header generation
#=============================================================================

TYPE=${1:-default}

# Platform-specific defaults for Juwels
export SBATCH_ACCOUNT="ifces2-scalexa"
export SBATCH_PARTITION="batch"
export SBATCH_OUTPUT="../LOG/slurm-%x-%j.out"
export SBATCH_EXCLUSIVE="true"
export SBATCH_MEM="0"

# Script-specific configurations
case "$TYPE" in
    grid_gen|generate_grid)
        export SBATCH_JOB_NAME="grid_gen"
        export SBATCH_NODES="1"
        export SBATCH_CPUS_PER_TASK="8"
        export SBATCH_TIME="04:00:00"
        ;;
    
    extpar|run_extpar)
        export SBATCH_JOB_NAME="extpar"
        export SBATCH_NODES="1"
        export SBATCH_CPUS_PER_TASK="8"
        export SBATCH_TIME="06:00:00"
        ;;
    
    testrun|testrun_chain)
        export SBATCH_JOB_NAME="testrun_chain"
        export SBATCH_MEM="2G"
        export SBATCH_TIME="00:05:00"
        export SBATCH_EXCLUSIVE="false"
        ;;
    
    production|production_chain)
        export SBATCH_JOB_NAME="production_chain"
        export SBATCH_NODES="64"
        export SBATCH_TIME="08:00:00"
        ;;
    
    lbc|icon2icon_lbc)
        export SBATCH_JOB_NAME="ifs2icon_lbc"
        export SBATCH_NODES="1"
        export SBATCH_CPUS_PER_TASK="4"
        export SBATCH_TIME="04:00:00"
        ;;
    
    ini|icon2icon_ini)
        export SBATCH_JOB_NAME="ifs2icon_ini"
        export SBATCH_NODES="1"
        export SBATCH_CPUS_PER_TASK="4"
        export SBATCH_TIME="06:00:00"
        ;;
    
    default|*)
        export SBATCH_JOB_NAME="hurricane_job"
        export SBATCH_NODES="1"
        export SBATCH_TIME="01:00:00"
        ;;
esac

# Debug output if requested
if [[ "${HURRICANE_DEBUG_SBATCH:-false}" == "true" ]]; then
    echo "SBATCH configuration for type '$TYPE':"
    env | grep "^SBATCH_" | sort
fi
