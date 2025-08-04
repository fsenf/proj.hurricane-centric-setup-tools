# Getting Started with Hurricane-Centric Setup Tools

This guide will help you get up and running with the Hurricane-Centric Setup Tools quickly.

## Prerequisites

### System Requirements
- **Linux environment** (tested on Levante/DKRZ)
- **SLURM workload manager** for job submission
- **Module system** for environment management

### Software Dependencies
- **ICON Model** with complete installation and tools
- **Python 3.7+** with required packages (tomllib, numpy, xarray, netCDF4)
- **CDO** (Climate Data Operators)
- **ICON Tools** from https://gitlab.dkrz.de/dwd-sw/dwd_icon_tools for grid generation and regridding
- **ExtPar** from https://github.com/C2SM/extpar for external parameter processing

### Input Data Requirements
You'll need access to:
- **External parameter datasets**: GLOBE topography, GLOBCOVER/GLCC land use, FAO soil, etc.
- **ERA5 reanalysis data** for initial/boundary conditions
- **Hurricane track data** for trajectory-based grid generation

## Installation

### 1. Clone the Repository
```bash
git clone https://github.com/fsenf/proj.hurricane-centric-setup-tools.git hurricane-centric-setup-tools
cd hurricane-centric-setup-tools
```

### 2. Set up Environment
```bash
# Example for Levante/DKRZ
module load python3 cdo

# Verify your environment (for Python version <= 3.10)
python -c "import tomli, numpy, xarray, netCDF4" && echo "✅ Python packages OK"
which cdo && echo "✅ CDO found"
```

### 3. Configure Your Setup
```bash
# Copy the default configuration
cp config/hurricane_config.toml config/my_config.toml

# Edit the configuration file to match your environment
vim config/my_config.toml
```

#### Key Configuration Parameters to Update:
```toml
[tools]
# Update these paths to match your system
icon_build_dir = "/your/path/to/icon/build/"

[project]
# Customize your project name
name = "your-hurricane-project"
working_dir = "/your/path/to/working/dir/"

[output]
# Output directories
grid_basedir = "/your/path/to/grids"
icbc_basedir = "/your/path/to/icbc"
```
If you do not change `[reference]` and `[track]` parts than you start to test the software with existing data from hurricane Paulette 2020. For setting up own hurricane simulations, please read [Preparing New Hurricane Cases](preparing_new_hurricane_cases.md) carefully.


### 4. Test Your Configuration
```bash
# Test configuration parsing
cd test
bash test_toml_reader.sh ../config/my_config.toml
cd ..
```

## Your First Hurricane Simulation

### Step 1: Single Segment Preprocessing
Start with processing one hurricane segment to verify everything works:

```bash
cd scripts/processing-chains

# Process initial segment 2 with your configuration (assuming reinit=12h and one day spinup)
bash run_hurricane_segments_preproc_chain.sh 2 -c ../../config/my_config.toml
```

This will submit 5 SLURM jobs in sequence:
1. **Grid generation** - Creates hurricane-centered grids
2. **External parameters** - Processes topography, land use, etc.
3. **Initial conditions** - Generates IC from global data
4. **Boundary conditions** - Creates BC files
5. **Test run** - Validates the setup with a short simulation

### Step 2: Monitor Progress
```bash
# Check job status
squeue -u $USER

# Monitor log files
tail -f ../LOG/slurm-*.out
```

### Step 3: Validate Results
```bash
# Check if all files were created successfully
cd ../../utilities
python check_preprocessing_files.py ../config/my_config.toml 2 all
```
### Step 4: Set Initial Segment
```bash
# prepare segment 2 as first segment
cd ../scripts/ic-bc
bash set_initial_segment.sh 2 -c ../../config/my_config.toml
cd ..
```

### Step 5: Run Production Simulation
Once preprocessing is complete:

```bash
cd scripts/processing-chains
bash run_hurricane_production_chain.sh 2 -c ../../config/my_config.toml
```

## Common First-Time Issues

### Configuration Problems
- **Config file not found**: Use absolute paths or ensure relative paths are correct
- **Path errors**: Verify all directory paths exist and are accessible
- **Permission issues**: Check write permissions for output directories

### SLURM Issues
- **Job failures**: Check `scripts/LOG/slurm-*.out` files for error messages
- **Resource limits**: Adjust `--nodes` and `--time` parameters if needed
- **Module loading**: Ensure required modules are available in your SLURM environment

### Data Issues
- **Missing input data**: Verify ERA5 and static datasets are available
- **Hurricane track data**: Ensure track files are in the correct format and location
- **Grid generation failures**: Check hurricane track data and domain specifications

## Next Steps

Once you have successfully run your first simulation:

1. **Try multiple segments**: Use `preproc_chain_looper.sh` for parallel processing
2. **Explore configurations**: Try different domain sizes and reinitialization intervals
3. **Run production workflows**: Use `production_looper.sh` for multi-segment production
4. **Customize**: Modify configurations for your specific hurricane case

## Getting Help

- **Detailed workflows**: See [Detailed Workflows](detailed_workflows.md)
- **Configuration options**: See [Configuration Reference](configuration_reference.md)
- **Troubleshooting**: Check log files in `scripts/LOG/`
- **Dependencies**: Review [dependency graphs](production_looper_dependencies.md)

## Quick Reference Commands

```bash
# Single segment preprocessing
./run_hurricane_segments_preproc_chain.sh [segment] -c [config]

# Multiple segments preprocessing (parallel)
./preproc_chain_looper.sh [start] [end] -c [config]

# Single segment production
./run_hurricane_production_chain.sh [segment] -c [config] [--nodes N] [--time HH:MM:SS] [--dependency TYPE] [--initial]

# Multiple segments production (sequential)
./production_looper.sh [start] [end] -c [config] [--nodes N] [--time HH:MM:SS] [--dependency TYPE] [--initial]

# Validate files
python check_preprocessing_files.py [config] [segment] [type]

# Monitor jobs
squeue -u $USER
```

## Related Documentation

- **[Detailed Workflows](detailed_workflows.md)**: Complete technical reference for all workflows
- **[Configuration Reference](configuration_reference.md)**: Complete TOML parameter documentation
- **[Preparing New Hurricane Cases](preparing_new_hurricane_cases.md)**: Advanced guide for setting up different hurricanes
- **[Grid Generation](generate_grid_for_hurricane_segments.md)**: Detailed grid generation documentation
- **[ExtPar Processing](run_extpar_levante.md)**: External parameter processing guide
- **[Main README](../README.md)**: Project overview and quick start
