#!/usr/bin/bash
#=============================================================================
# DESCRIPTION:
#   External parameter processing for hurricane segments using ICON ExtPar tools.
#   Processes topography, land use, soil properties, and other surface parameters
#   for hurricane-centric grids.
#
# USAGE:
#   ./run_extpar_levante.bash [segment_number]
#
# DEPENDENCIES:
#   This script calls:
#   - ../../utilities/toml_reader.sh
#   - ../../config/hurricane_config.toml
#   - ExtPar Fortran binaries and Python scripts
#
#=============================================================================
####### EXECUTE SCRIPT FROM ENVIRONMENT THAT LOADS PYTHON AND CDO ######

# Levante cpu batch job parameters
#
#SBATCH --account=bb1376
#SBATCH --job-name=extpar
#SBATCH --partition=shared
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --distribution=block:block
#SBATCH --chdir=/scratch/b/b380352/icontools
#SBATCH --mem=10G
#SBATCH --time=00:20:00

#=============================================================================
set -eux
ulimit -s unlimited
ulimit -c 0

# Get script directory
ORIGINAL_SCRIPT_DIR="${SLURM_SUBMIT_DIR}"
echo "Script directory: ${ORIGINAL_SCRIPT_DIR}"

# Load TOML reader and configuration
source "${ORIGINAL_SCRIPT_DIR}/../../utilities/toml_reader.sh"
CONFIG_FILE="${ORIGINAL_SCRIPT_DIR}/../../config/hurricane_config.toml"
read_toml_config "$CONFIG_FILE"

#=============================================================================
# OpenMP environment variables
#=============================================================================
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}
export KMP_AFFINITY=verbose,granularity=fine,scatter
export OMP_STACKSIZE=128M

# Environment variables for the experiment and the target system
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

# INPUT ARGUMENT
iseg=$1

module purge
module load python3 

# Set variables from TOML config instead of sourcing extpar_config.sh
DOMNAME="${PROJECT_NAME}/seg${iseg}_${PROJECT_WIDTH_CONFIG}"
grid_dir="${OUTPUT_GRID_BASEDIR}/${DOMNAME}"

# Create icon_grid_files array based on TOML config
declare -a icon_grid_files
for ((idom = 1; idom <= DOMAINS_NESTS; idom++)); do
    icon_grid_files+=("paulette-seg${iseg}_dom${idom}_DOM01.nc")
done

# Directory with input data for generation of external parameters
extpar_input_dir="$TOOLS_EXTPAR_INPUT_DIR"

# Directory of ExtPar
extpar_dir="$TOOLS_EXTPAR_DIR"

# Output directory
out_dir=${grid_dir}

# Set PYTHONPATH to point to ExtPar libraries and namelist.py, which is located in out_dir
export PYTHONPATH=${extpar_dir}/python/lib:${out_dir}

# Create and change into out_dir
if [ ! -d ${out_dir} ] ; then
       mkdir -p ${out_dir}
fi
cd ${out_dir}

##### Create dictionaries/namelists for python scripts 

    cat <<NAMELIST_PYTHON > namelist.py

input_era = {
        'iera_type': 1,
        'raw_data_era_path': '${extpar_input_dir}/era',
        'raw_data_era_ORO': 'ERA5_ORO_1990.nc',
        'raw_data_era_T2M': 'ERA5_T2M_1990_2019.nc',
        'raw_data_era_SST': 'ERA5_SST_1990_2019.nc',
        'raw_data_era_SD': 'ERA5_SD_1990_2019.nc',
        'era_buffer_file': 'era_buffer.nc',
        }

input_alb = {
        'ialb_type': 1,
        'raw_data_alb_path':  '${extpar_input_dir}/albedo',
        'raw_data_alb_filename': 'alb_new.nc',
        'raw_data_alnid_filename': 'alnid_new.nc',
        'raw_data_aluvd_filename': 'aluvd_new.nc',
        'alb_buffer_file': 'albedo_buffer.nc',
        'alb_output_file': 'albedo_cosmo.nc',
        }

input_ndvi = {
        'raw_data_ndvi_path':  '${extpar_input_dir}/ndvi',
        'raw_data_ndvi_filename': 'NDVI_1998_2003.nc',
        'ndvi_buffer_file': 'ndvi_buffer.nc',
        'ndvi_output_file': 'ndvi_extpar_cosmo.nc'
        }

input_emiss = {
        'iemiss_type': 1, 
        'raw_data_emiss_path':  '${extpar_input_dir}/emiss',
        'raw_data_emiss_filename': 'CAMEL_bbe_full_2010-2015.nc',
        'emiss_buffer_file': 'emiss_buffer.nc',
        'emiss_output_file': 'emiss_icon.nc'
        }

input_tclim = {
        'raw_data_t_clim_path': '${extpar_input_dir}/cru',
        'raw_data_tclim_coarse': 'absolute_hadcrut3.nc',
        'raw_data_tclim_fine': 'CRU_T2M_SURF_clim.nc',
        't_clim_buffer_file': 'cru_buffer.nc',
        'it_cl_type': 2
        }

NAMELIST_PYTHON

##### Input files and namelist for AOT 
cat > INPUT_AOT << EOF
&aerosol_raw_data
  raw_data_aot_path='${extpar_input_dir}/aerosols',
  raw_data_aot_filename='aod_MACC_2003-2012.nc'
  iaot_type=3
/  
&aerosol_io_extpar
  aot_buffer_file='aot_buffer.nc',
/
EOF

### OROGRAPHY ###
cat > INPUT_ORO << EOF
&oro_runcontrol
  lcompute_sgsl=.FALSE.
/
&orography_io_extpar
  orography_buffer_file='topography_buffer.nc',
  orography_output_file='topography_ICON.nc'
/
&orography_raw_data
 itopo_type = 1
 lsso_param = .TRUE.,
 lsubtract_mean_slope = .FALSE.,
 raw_data_orography_path = '${extpar_input_dir}/topo/globe',
 ntiles_column = 4,
 ntiles_row = 4,
 topo_files = 'GLOBE_A10.nc' 'GLOBE_B10.nc'  'GLOBE_C10.nc'  'GLOBE_D10.nc'  'GLOBE_E10.nc'  'GLOBE_F10.nc'  'GLOBE_G10.nc'  'GLOBE_H10.nc'  'GLOBE_I10.nc'  'GLOBE_J10.nc'  'GLOBE_K10.nc'  'GLOBE_L10.nc'  'GLOBE_M10.nc'  'GLOBE_N10.nc'  'GLOBE_O10.nc'  'GLOBE_P10.nc'
/
EOF

cat > INPUT_OROSMOOTH << EOF
&orography_smoothing
  lfilter_oro=.FALSE.
  ! lfilter_oro=.TRUE., 
  numfilt_oro= 1, 
  ilow_pass_oro = 1, 
  lxso_first=F, 
  numfilt_xso= 1, 
  ilow_pass_xso= 0, 
  ifill_valley= 1, 
  eps_filter= 10.0, 
  rfill_valley=  0.0, 
  rxso_mask=   0.0
/
EOF

cat > INPUT_RADTOPO << EOF
&radtopo
  lradtopo=.FALSE.,
  nhori=24,
/
EOF

### FRESHWATER LAKE DATA ###
cat > INPUT_FLAKE << EOF
&flake_raw_data
   raw_data_flake_path='${extpar_input_dir}',
   raw_data_flake_filename='flake/GLDB_lakedepth.nc'
/
&flake_io_extpar
   flake_buffer_file='flake_buffer.nc'
/
EOF

### LAND USE ###
cat > INPUT_LU << EOF
&lu_raw_data
   raw_data_lu_path='${extpar_input_dir}/landuse',
   raw_data_lu_filename='GLOBCOVER_0_16bit.nc' 'GLOBCOVER_1_16bit.nc' 'GLOBCOVER_2_16bit.nc' 'GLOBCOVER_3_16bit.nc' 'GLOBCOVER_4_16bit.nc' 'GLOBCOVER_5_16bit.nc',
   i_landuse_data=1,
   ilookup_table_lu=1,
   ntiles_globcover=6
/
&lu_io_extpar
   lu_buffer_file='landuse_buffer.nc',
/
&glcc_raw_data
   raw_data_glcc_path='${extpar_input_dir}/landuse',
   raw_data_glcc_filename='GLCC_usgs_class_byte.nc'
/
&glcc_io_extpar
   glcc_buffer_file='glcc_landuse_buffer.nc'
/
EOF

## SOIL DATA ###
cat > INPUT_SOIL << EOF
&soil_raw_data
 isoil_data = 1,
 ldeep_soil = .FALSE.,
 raw_data_soil_path='${extpar_input_dir}/soil',
 raw_data_soil_filename='FAO_DSMW_double.nc'
/
&soil_io_extpar
  soil_buffer_file='soil_buffer.nc',
/
EOF

##### Loop over all gridfiles for which external parameters should be produced 
for icon_grid_file in ${icon_grid_files[@]}
do

    touch ${icon_grid_file%.*}.log
    ##### Create namelists pointing to the ICON grid

    cat > INPUT_grid_org << EOF
&GRID_DEF
 igrid_type = 1,
 domain_def_namelist='INPUT_ICON_GRID'
/
EOF

    cat > INPUT_ICON_GRID << EOF
&icon_grid_info
 icon_grid_dir='${grid_dir}'
 icon_grid_nc_file ='${icon_grid_file}'
/
EOF

    # OUTPUT
    netcdf_output_filename="extpar_${icon_grid_file%.*}_tiles.nc"
    
    ### CONSISTENCY CHECK ### ---> not checked!!
    cat > INPUT_CHECK << EOF
&extpar_consistency_check_io
 netcdf_output_filename='${netcdf_output_filename}'
 i_lsm_data=1
 land_sea_mask_file=""
 tile_mode=1
/
EOF

    ##### Run set of Fortran binaries for extpar preprocessing
    for script_name_specifier in topo landuse flake soil aot
    do
        script_name=extpar_${script_name_specifier}_to_buffer.exe
        echo 'Running '${script_name}
        ${extpar_dir}/bin/${script_name}
        cat ${script_name%.*}.log >> ${icon_grid_file%.*}.log
    done

    ##### Run set of python scripts for extpar preprocessing
    for script_name_specifier in era alb ndvi emiss cru
    do
        script_name=extpar_${script_name_specifier}_to_buffer.py
        echo 'Running '${script_name}
        python ${extpar_dir}/python/${script_name}
        cat ${script_name%.*}.log >> ${icon_grid_file%.*}.log
    done

    ##### Run consistency check
    echo 'Running 'extpar_consistency_check.exe
    ${extpar_dir}/bin/extpar_consistency_check.exe

done
exit



