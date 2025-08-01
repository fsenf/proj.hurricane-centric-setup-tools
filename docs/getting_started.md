# Getting Started with Hurricane-Centric Setup Tools

This guide will help you get up and running with the Hurricane-Centric Setup Tools quickly.

## Prerequisites

### System Requirements
- **Linux environment** (tested on Levante/DKRZ)
- **SLURM workload manager** for job submission
- **Module system** for environment management

### Software Dependencies
- **ICON Model** with complete installation and tools
- **Python 3.7+** with required packages:
  ```bash
  # Check if you have the required packages
  python -c "import tomllib, numpy, xarray, netCDF4"
  # If missing, set up a python user environment and install: pip install tomli numpy xarray netCDF4
  ```
- **CDO** (Climate Data Operators)
- **ICON Tools** from https://gitlab.dkrz.de/dwd-sw/dwd_icon_tools
- **ExtPar** for external parameter processing

### Input Data Requirements
You'll need access to:
- **External parameter datasets**: GLOBE topography, GLOBCOVER/GLCC land use, FAO soil, etc.
- **ERA5 reanalysis data** for initial/boundary conditions
- **Hurricane track data** for trajectory-based grid generation

## Installation

### 1. Clone the Repository
```bash
git clone <repository_url> hurricane-centric-setup-tools
cd hurricane-centric-setup-tools
```

### 2. Set up Environment
```bash
# Example for Levante/DKRZ
module load python3 cdo netcdf

# Verify your environment
python -c "import tomllib, numpy, xarray, netCDF4" && echo "✅ Python packages OK"
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
[paths]
# Update these paths to match your system
output_grid_basedir = "/your/path/to/grids"
output_icbc_basedir = "/your/path/to/icbc"
tools_icon_build_dir = "/your/path/to/icon/build"
extpar_input_dir = "/your/path/to/input/data"

[project]
# Customize your project name
name = "your-hurricane-project"
```

### 4. Test Your Installation
```bash
# Test configuration parsing
cd test
./test_toml_reader.sh

# Run a basic validation
cd ../utilities
python check_preprocessing_files.py ../config/my_config.toml 1 grid
```

## Your First Hurricane Simulation

### Step 1: Single Segment Preprocessing
Start with processing one hurricane segment to verify everything works:

```bash
cd scripts/processing-chains

# Process segment 1 with your configuration
./run_hurricane_segments_preproc_chain.sh 1 -c ../../config/my_config.toml
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
python check_preprocessing_files.py ../config/my_config.toml 1 all
```

### Step 4: Run Production Simulation
Once preprocessing is complete:

```bash
cd ../scripts/processing-chains
./run_hurricane_production_chain.sh 1 -c ../../config/my_config.toml
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
./run_hurricane_production_chain.sh [segment] -c [config]

# Multiple segments production (sequential)
./production_looper.sh [start] [end] -c [config]

# Validate files
python check_preprocessing_files.py [config] [segment] [type]

# Monitor jobs
squeue -u $USER
```
