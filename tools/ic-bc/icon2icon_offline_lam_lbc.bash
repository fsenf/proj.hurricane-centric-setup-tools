#!/bin/bash
#=============================================================================
#
# Levante cpu batch job parameters
#
#SBATCH --account=bb1376
#SBATCH --job-name=ifs2icon_lbc
#SBATCH --partition=compute
#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --exclusive
#SBATCH --chdir=/scratch/b/b380352/icontools
#SBATCH --time=04:00:00
#=============================================================================
set -eu
ulimit -s unlimited
ulimit -c 0
#=============================================================================
#
# OpenMP environment variables
#
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}
export KMP_AFFINITY=verbose,granularity=fine,scatter
export OMP_STACKSIZE=128M

# #=============================================================================
# #
# # Environment variables for the experiment and the target system
# #
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

export START="srun -l --cpu_bind=verbose --distribution=block:cyclic"

# LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
# INPUT ARGUMENT

iseg=$1

# TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT


#-----------------------------------------------------------------------------
# PART I: Create auxiliary grid file which contains only the cells of the 
#         boundary zone.
#-----------------------------------------------------------------------------

module load cdo


# Directory containing dwd_icon_tool binaries
ICONTOOLS_DIR=/work/bb1174/models/icon/dwd_icon_tools/icontools


SCRIPT_DIR=/work/bb1376/user/fabian/tools/pre-processing/ic-bc
source ${SCRIPT_DIR}/ic_config.sh ${iseg}


# Output directory for initial data
OUTDIR=/work/bb1376/data/icon/bc-init/${DOMNAME}

# Name of boundary grid
BOUNDGRID=${OUTGRID%.*}'_lbc.nc'



GEODIR=${DATADIR}
GEOFILE=${GEODIR}/lam_input_geo_DOM02_ML.nc




## should be the same for all files in order to use the weights

cat > ${OUTDIR}/NAMELIST_ICONSUB_LBC << EOF_1
&iconsub_nml
  grid_filename     = '${OUTGRID}',
  output_type       = 4,
  lwrite_grid       = .TRUE.,
/
&subarea_nml
  out_grid_filename = '${BOUNDGRID}',
  min_refin_c_ctrl  = 1
  max_refin_c_ctrl  = 21
/
EOF_1

${START} ${ICONTOOLS_DIR}/iconsub -vv --nml ${OUTDIR}/NAMELIST_ICONSUB_LBC

# Clean up
rm ${OUTDIR}/NAMELIST*

#-----------------------------------------------------------------------------
# PART II: Extract boundary data
#-----------------------------------------------------------------------------

# Create directory for weights
WEIGHTDIR=${OUTDIR}/lbc_weights
[ ! -d ${WEIGHTDIR} ] && mkdir -p ${WEIGHTDIR}


cat >  ${OUTDIR}/NAMELIST_ICONREMAP_FIELDS << EOF_2A
&input_field_nml
inputname = "rho"
outputname = "rho"
intp_method = 3
/
&input_field_nml
inputname = "qc"
outputname = "qc"
intp_method = 3
/
&input_field_nml
inputname = "qi"
outputname = "qi"
intp_method = 3
/
&input_field_nml
inputname = "qr"
outputname = "qr"
intp_method = 3
/
&input_field_nml
inputname = "qv"
outputname = "qv"
intp_method = 3
/
&input_field_nml
inputname = "qs"
outputname = "qs"
intp_method = 3
/
&input_field_nml
inputname = "theta_v"
outputname = "theta_v"
intp_method = 3
/
&input_field_nml
inputname = "vn"
outputname = "vn"
intp_method = 3
/
&input_field_nml
inputname = "w"
outputname = "w"
intp_method = 3
/
&input_field_nml
inputname = "z_ifc"
outputname = "z_ifc"
intp_method = 3
/
EOF_2A


# Loop over all IFS files
for datafilename in ${DATAFILELIST} ; do

    # Get filename without path
    datafile="${datafilename##*/}"

    # Get filename without suffix
    outdatafile=${datafile%.*}
    outdatafile=${outdatafile#*ML_}

    # add geo reference
    temp_file=$(mktemp  --tmpdir=${HOME}/scratch/icon)
    cdo merge ${datafilename} ${GEOFILE} ${temp_file} 

    
    cat >  ${OUTDIR}/NAMELIST_ICONREMAP_LBC << EOF_2C
&remap_nml
 in_grid_filename  = '${INGRID}'                         ! '${INGRID}' for weights! ! file containing grid information of input data
 in_filename       = '${temp_file}'           ! input data file name
 in_type           = 2                                  ! type of input grid (1: triangular)
 out_grid_filename = '${BOUNDGRID}'                     ! output lateral boundary grid
 out_filename      = '${OUTDIR}/${outdatafile}_lbc.nc'  ! output file name
 out_type          = 2                                  ! type of output grid (2: triangular)
 out_filetype      = 4                                  ! output filetype (4: NetCDF)
 l_have3dbuffer    = .FALSE.                            !.TRUE. if output grid shall be created from scratch
 ncstorage_file   = "${WEIGHTDIR}/ncstorage_lbc.tmp"
/
EOF_2C

    ${START} ${ICONTOOLS_DIR}/iconremap -q --remap_nml ${OUTDIR}/NAMELIST_ICONREMAP_LBC \
	     --input_field_nml  ${OUTDIR}/NAMELIST_ICONREMAP_FIELDS 2>&1
    
    rm ${OUTDIR}/NAMELIST_ICONREMAP_LBC
done

# Clean up
rm ${OUTDIR}/NAMELIST*

#-----------------------------------------------------------------------------
exit
#-----------------------------------------------------------------------------

echo 'fertig'
