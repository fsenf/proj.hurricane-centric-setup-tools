#!/bin/bash

#==============================================================================
# HEADER
#==============================================================================

# Description: This script configures the warm start for ICON model runs.
# Author: Fabian Senf
# Date: 2025-03-10

#==============================================================================
# INPUT ARGUMENTS
#==============================================================================

iseg=$1

indom=$2

outdom=$3

#==============================================================================
# FUNCTIONS
#==============================================================================

#------------------------------------------------------------------------------
# Function: get_init_date
# Description: Get initialization date based on iseg
#------------------------------------------------------------------------------
get_init_date() {
    case $1 in
        0) echo '20200907' ;;
        1) echo '20200908' ;;
        2) echo '20200909' ;;
        3) echo '20200910' ;;
        4) echo '20200911' ;;
        5) echo '20200912' ;;
        6) echo '20200913' ;;
        7) echo '20200914' ;;
        *) echo 'Invalid segment' ;;
    esac
}

#------------------------------------------------------------------------------
# Function: get_datafilelist
# Description: Get DATAFILELIST based on iseg
#------------------------------------------------------------------------------
get_datafilelist() {
    case $1 in
        0) find ${DATADIR} -iname 'lam_input_BC_DOM02_ML_2020090[7-8]T??0000Z.nc' | sort ;;
        1) find ${DATADIR} -iname 'lam_input_BC_DOM02_ML_2020090[8-9]T??0000Z.nc' | sort ;;
        2) find ${DATADIR} -iname 'lam_input_BC_DOM02_ML_202009[01][09]T??0000Z.nc' | sort ;;
        3) find ${DATADIR} -iname 'lam_input_BC_DOM02_ML_2020091[0-1]T??0000Z.nc' | sort ;;
        4) find ${DATADIR} -iname 'lam_input_BC_DOM02_ML_2020091[1-2]T??0000Z.nc' | sort ;;
        5) find ${DATADIR} -iname 'lam_input_BC_DOM02_ML_2020091[2-3]T??0000Z.nc' | sort ;;
        6) find ${DATADIR} -iname 'lam_input_BC_DOM02_ML_2020091[3-4]T??0000Z.nc' | sort ;;
        7) find ${DATADIR} -iname 'lam_input_BC_DOM02_ML_2020091[4-5]T??0000Z.nc' | sort ;;
        *) echo 'Invalid segment' ;;
    esac
}

#------------------------------------------------------------------------------
# Function: transform_dom
# Description: Transform 'dom1' into 'DOM01', 'dom2' into 'DOM02', etc.
#------------------------------------------------------------------------------
transform_dom() {
    local dom=$1
    local num=${dom:3}
    printf "DOM%02d\n" $num
}

#==============================================================================
# MAIN SCRIPT
#==============================================================================

#------------------------------------------------------------------------------
# Subsection: Get initialization dates
#------------------------------------------------------------------------------
isegm1=$((iseg - 1))

prior_date=$(get_init_date $isegm1)
init_date=$(get_init_date $iseg)

#------------------------------------------------------------------------------
# Subsection: Define grids
#------------------------------------------------------------------------------
DOMNAME=paulette-segments/seg${iseg}_width100km
priorDOMNAME=paulette-segments/seg${isegm1}_width100km

INDOM='dom'${indom}
OUTDOM='dom'${outdom}

MAIN_GRID_DIR=/work/bb1376/data/icon/grids-extpar
INGRID=${MAIN_GRID_DIR}/${priorDOMNAME}/paulette-seg${isegm1}_${INDOM}_DOM01.nc
OUTGRID=${MAIN_GRID_DIR}/${DOMNAME}/paulette-seg${iseg}_${OUTDOM}_DOM01.nc


#------------------------------------------------------------------------------
# Subsection: input datafiles
#------------------------------------------------------------------------------
IN_EXPDIR=/work/bb1376/user/fabian/model/icon/icon-builds/icon-release-2024.07/experiments

DATADIR=${IN_EXPDIR}/ifces2-atlanLEM-segment${isegm1}-${prior_date}-exp106

# Input file
INDOM_big=$(transform_dom $INDOM)
INFILE=${DATADIR}/lam_input_IC_${INDOM_big}_ML_${init_date}T000000Z.nc


#------------------------------------------------------------------------------
# Subsection: output settings
#------------------------------------------------------------------------------
# Output directory for initial data
OUTDIR=${SCRATCH}/icontools #/work/bb1376/data/icon/bc-init/${DOMNAME}

# File name of input grid/file to be remapped without path
OUTNAME=${OUTDIR}/ifces2-atlanLEM-segment${isegm1}-exp105_${init_date}T000000Z_${INDOM}-to-${OUTDOM}_warmini.nc

### leftover from BC  DATAFILELIST=$(get_datafilelist $iseg)

#==============================================================================
# END OF SCRIPT
#==============================================================================




