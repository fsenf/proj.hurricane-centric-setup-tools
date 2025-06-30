#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/processing-chains/run_hurricane_segments_preproc_chain.sh
#=============================================================================
# DESCRIPTION:
#   This script executes the complete preprocessing chain for hurricane-centric
#   simulations. It processes a specific hurricane segment by:
#   1. Creating the computational grid
#   2. Processing external parameters (extpar)
#   3. Generating initial conditions (IC)
#   4. Creating boundary conditions (BC)
#
# USAGE:
#   ./run_hurricane_segments_preproc_chain.sh [segment_number] [sbatch_options] [-c|--config config_file]
#
# ARGUMENTS:
#   segment_number  - The hurricane segment number to process
#   sbatch_options  - Additional options to pass to sbatch, typically dependencies
#
# OPTIONS:
#   -c, --config    - Path to TOML configuration file (optional)
#                     Default: ../../config/hurricane_config.toml
#                     Relative paths are resolved from script directory
#   -h, --help      - Show this help message
#
# DEPENDENCIES:
#   This script calls the following bash/sh scripts:
#   - ../grid-extpar/generate_grid_for_hurricane_segments.sh
#   - ../grid-extpar/run_extpar_levante.bash
#   - ../ic-bc/icon2icon_offline_lam_ini.bash
#   - ../ic-bc/icon2icon_offline_lam_lbc.bash
#
# AUTHOR:
#   Fabian Senf (senf@tropos.de)
#
# DATE:
#   May 2025
#=============================================================================

#=============================================================================
# Configuration and Argument Parsing
#=============================================================================

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
echo "Script directory: ${SCRIPT_DIR}"

# Load shared configuration handler
source "${SCRIPT_DIR}/../../utilities/config_handler.sh"

# Parse config argument
CONFIG_ARG=$(parse_config_argument "$@")

# Handle configuration loading (optional - only if config is provided)
CONFIG_OPTION=""
if [[ -n "$CONFIG_ARG" ]]; then
    handle_config "$SCRIPT_DIR" \
                  "${SCRIPT_DIR}/../../config/hurricane_config.toml" \
                  "$CONFIG_ARG"
    CONFIG_OPTION="-c $CONFIG_FILE"
    echo "Config will be passed to child scripts: $CONFIG_OPTION"
fi

# Remove config arguments and parse remaining arguments
REMAINING_ARGS=($(remove_config_args "$@"))

# Parse remaining arguments
iseg=""
ADDED_ARG=""
arg_count=0

for arg in "${REMAINING_ARGS[@]}"; do
    case $arg in
        -h|--help)
            echo "Usage: $0 [segment_number] [sbatch_options] [options]"
            echo ""
            echo "Arguments:"
            echo "  segment_number    Hurricane segment number to process"
            echo "  sbatch_options    Additional sbatch options (e.g. --dependency=afterok:12345)"
            echo ""
            show_config_help
            echo ""
            echo "Examples:"
            echo "  $0 5"
            echo "  $0 5 --dependency=afterok:12345"
            echo "  $0 5 --dependency=afterok:12345 -c my_config.toml"
            echo ""
            echo "Dependencies:"
            echo "  This script submits jobs that call:"
            echo "  - ../grid-extpar/generate_grid_for_hurricane_segments.sh"
            echo "  - ../grid-extpar/run_extpar_levante.bash"
            echo "  - ../ic-bc/icon2icon_offline_lam_ini.bash"
            echo "  - ../ic-bc/icon2icon_offline_lam_lbc.bash"
            exit 0
            ;;
        --dependency=*|--*=*)
            # This is an sbatch option
            if [[ -z "$ADDED_ARG" ]]; then
                ADDED_ARG="$arg"
            else
                ADDED_ARG="$ADDED_ARG $arg"
            fi
            ;;
        -*)
            echo "Error: Unknown option $arg"
            echo "Use --help for usage information"
            exit 1
            ;;
        *)
            # Positional arguments
            if [[ $arg_count -eq 0 ]]; then
                iseg="$arg"
            elif [[ $arg_count -eq 1 ]]; then
                # Second positional argument is additional sbatch args
                if [[ -z "$ADDED_ARG" ]]; then
                    ADDED_ARG="$arg"
                else
                    ADDED_ARG="$ADDED_ARG $arg"
                fi
            else
                echo "Error: Too many positional arguments"
                exit 1
            fi
            ((arg_count++))
            ;;
    esac
done

#------------------------------------------------------------------------------
# Validation
#------------------------------------------------------------------------------
if [[ -z "$iseg" ]]; then
    echo "Error: segment_number is required"
    echo "Usage: $0 [segment_number] [sbatch_options] [-c|--config config_file]"
    exit 1
fi

if ! [[ "$iseg" =~ ^[0-9]+$ ]]; then
    echo "Error: segment_number (iseg) must be a number."
    exit 1
fi

echo "Processing segment: $iseg"
if [[ -n "$ADDED_ARG" ]]; then
    echo "Additional sbatch arguments: $ADDED_ARG"
fi
if [[ -n "$CONFIG_OPTION" ]]; then
    echo "Config option for child scripts: $CONFIG_OPTION"
fi

pp_path=".."

#-----------------------------------------------------------------------------
# PART I: Create Grid
#-----------------------------------------------------------------------------
echo "Submitting grid generation job..."
cd ${pp_path}/grid-extpar
if [[ -n "$CONFIG_OPTION" ]]; then
    grid_job=$(sbatch --parsable $ADDED_ARG ./generate_grid_for_hurricane_segments.sh $iseg $CONFIG_OPTION)
else
    grid_job=$(sbatch --parsable $ADDED_ARG ./generate_grid_for_hurricane_segments.sh $iseg)
fi
printf "... Grid job submitted with ID: $grid_job\n\n"

#-----------------------------------------------------------------------------
# PART II: Process Extpar
#-----------------------------------------------------------------------------
echo "Submitting extpar job..."
if [[ -n "$CONFIG_OPTION" ]]; then
    extpar_job=$(sbatch --parsable --dependency=afterok:$grid_job ./run_extpar_levante.bash $iseg $CONFIG_OPTION)
else
    extpar_job=$(sbatch --parsable --dependency=afterok:$grid_job ./run_extpar_levante.bash $iseg)
fi
printf "... Extpar job submitted with ID: $extpar_job\n\n"

#-----------------------------------------------------------------------------
# PART III: IC for Hurricane Segments
#-----------------------------------------------------------------------------
echo "Submitting IC job..."
cd ${pp_path}/ic-bc
if [[ -n "$CONFIG_OPTION" ]]; then
    ic_job=$(sbatch --parsable --dependency=afterok:$grid_job ./icon2icon_offline_lam_ini.bash $iseg $CONFIG_OPTION)
else
    ic_job=$(sbatch --parsable --dependency=afterok:$grid_job ./icon2icon_offline_lam_ini.bash $iseg)
fi
printf "... IC job submitted with ID: $ic_job\n\n"

#-----------------------------------------------------------------------------
# PART IV: BC for Hurricane Segments
#-----------------------------------------------------------------------------
echo "Submitting BC job..."
if [[ -n "$CONFIG_OPTION" ]]; then
    bc_job=$(sbatch --parsable --dependency=afterany:$ic_job ./icon2icon_offline_lam_lbc.bash $iseg $CONFIG_OPTION)
else
    bc_job=$(sbatch --parsable --dependency=afterany:$ic_job ./icon2icon_offline_lam_lbc.bash $iseg)
fi
printf "... BC job submitted with ID: $bc_job\n\n"

echo "All jobs submitted successfully!"
echo "Grid: $grid_job | Extpar: $extpar_job | IC: $ic_job | BC: $bc_job\n\n"

#------------------------------------------------------------------------------
# PART V: Final Tesrun
#------------------------------------------------------------------------------
echo "Submitting final testrun job..."
cd ${pp_path}/processing-chains
if [[ -n "$CONFIG_OPTION" ]]; then
    testrun_job=$(sbatch --parsable --dependency=afterany:$bc_job ./run_hurricane_testrun_chain.sh $iseg $CONFIG_OPTION)
else
    testrun_job=$(sbatch --parsable --dependency=afterany:$bc_job ./run_hurricane_testrun_chain.sh $iseg)
fi
printf "... Testrun job submitted with ID: $testrun_job\n\n"
echo "All preprocessing and testrun jobs submitted successfully!"
echo "Final testrun job ID: $testrun_job"