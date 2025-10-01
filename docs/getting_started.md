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

### 3. Platform Detection and Configuration Setup

The system automatically detects your platform and uses appropriate configurations:

```bash
# Check platform detection (from project root directory)
cd /path/to/hurricane-centric-setup-tools
./utilities/detect_platform.sh
echo "Detected platform: $(./utilities/detect_platform.sh)"
```

#### Platform-Specific Configuration Selection

**Option A: Use Platform-Specific Configuration (Recommended)**
```bash
# For Levante users
cp config/levante/hurricane_config_width20km_reinit12h.toml config/my_config.toml

# For JUWELS users  
cp config/juwels/hurricane_config_width20km_reinit12h_JUWELS.toml config/my_config.toml

# For other systems
cp config/generic/hurricane_config.toml config/my_config.toml
```

**Option B: Create Custom Configuration**
```bash
# Start with the generic template
cp config/generic/hurricane_config.toml config/my_config.toml
vim config/my_config.toml
```

#### Key Configuration Parameters to Update:
```toml
[tools]
# Update these paths to match your system
icon_build_dir = "/your/path/to/icon/build/"
icontools_dir = "/your/path/to/icontools/"
extpar_dir = "/your/path/to/extpar/"

[project] 
# Customize your project name and working directory
name = "your-hurricane-project"
working_dir = "/your/path/to/working/dir/"

[output]
# Output directories (must have write permissions)
grid_basedir = "/your/path/to/grids"
icbc_basedir = "/your/path/to/icbc"
```

**Note**: If you keep the default `[reference]` and `[track]` sections unchanged, you can test the software with existing Hurricane Paulette 2020 data. For setting up your own hurricane simulations, see [Preparing New Hurricane Cases](preparing_new_hurricane_cases.md).


### 4. Test Your Configuration
```bash
# Test configuration parsing (from project root)
cd /path/to/hurricane-centric-setup-tools/test
./test_toml_reader.sh ../config/my_config.toml
```

## Your First Hurricane Simulation

### Step 1: Single Segment Preprocessing
Start with processing one hurricane segment to verify everything works:

```bash
# Change to the appropriate script directory
cd /path/to/hurricane-centric-setup-tools/scripts/processing-chains

# Process initial segment 2 with your configuration (assuming reinit=12h and one day spinup)
# The system automatically detects your platform and applies appropriate settings
./run_hurricane_segments_preproc_chain.sh 2 -c ../../config/my_config.toml
```

This preprocessing chain uses the platform-independent `utilities/submit.sh` wrapper to submit 5 SLURM jobs in sequence:

1. **Grid generation** - Creates hurricane-centered grids
2. **External parameters** - Processes topography, land use, etc.
3. **Initial conditions** - Generates IC from global data
4. **Boundary conditions** - Creates BC files
5. **Test run** - Validates the setup with a short simulation

**Platform Benefits**: Each job automatically uses optimal resources (CPU count, time limits, partitions) for your detected platform without manual configuration.

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
### Step 4a: Set Initial Segment by Hand
```bash
# prepare segment 2 as first segment
cd /path/to/hurricane-centric-setup-tools/scripts/ic-bc
./set_initial_segment.sh 2 -c ../../config/my_config.toml
```
This provides some check informations.

### Step 4b: Run Production Simulation
Once preprocessing is complete and checks are done:

```bash
cd /path/to/hurricane-centric-setup-tools/scripts/processing-chains
./run_hurricane_production_chain.sh 2 -c ../../config/my_config.toml --initial
```
This redoes segment initialization as part of the production chain using platform-optimal settings.

## Common First-Time Issues

### Platform and Configuration Problems
- **Platform not detected**: Check `/path/to/hurricane-centric-setup-tools/utilities/detect_platform.sh` output
- **Config file not found**: Use absolute paths or ensure relative paths are correct
- **Path errors**: Verify all directory paths exist and are accessible
- **Permission issues**: Check write permissions for output directories
- **Module loading failures**: Verify platform-specific modules are available

### Job Submission Issues
- **Job failures**: Check `scripts/LOG/slurm-*.out` files for error messages
- **Resource limits**: Platform defaults should work, but you can override with `--nodes` and `--time`
- **Platform-specific errors**: Set `HURRICANE_DEBUG_SUBMIT=true` for job submission debugging
- **Account/partition issues**: Update your platform's `config/{platform}/sbatch_env_setter.sh`

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

### Platform Detection and Configuration
```bash
# Check detected platform (from project root)
cd /path/to/hurricane-centric-setup-tools
./utilities/detect_platform.sh

# Test platform-specific modules
source config/$(./utilities/detect_platform.sh)/module_loader.sh
```

### Workflow Execution (Platform-Independent)
```bash
# From appropriate script directory: /path/to/hurricane-centric-setup-tools/scripts/processing-chains

# Single segment preprocessing (uses platform-optimal settings automatically)
./run_hurricane_segments_preproc_chain.sh [segment] -c [config]

# Multiple segments preprocessing (parallel)
./preproc_chain_looper.sh [start] [end] -c [config]

# Single segment production (automatically detects platform resources)
./run_hurricane_production_chain.sh [segment] -c [config] [--nodes N] [--time HH:MM:SS] [--dependency TYPE] [--initial]

# Multiple segments production (sequential)
./production_looper.sh [start] [end] -c [config] [--nodes N] [--time HH:MM:SS] [--dependency TYPE] [--initial]

# Validate files (from project root)
cd /path/to/hurricane-centric-setup-tools
python utilities/check_preprocessing_files.py [config] [segment] [type]

# Monitor jobs
squeue -u $USER

# Debug job submission (from appropriate script directory)
cd /path/to/hurricane-centric-setup-tools/scripts/grid-extpar  # or other script directory
HURRICANE_DEBUG_SUBMIT=true ${sbatch_wrapper} script.sh -c [config] [args]
```

## Related Documentation

- **[Configuration Reference](configuration_reference.md)**: Complete TOML parameter documentation with platform-aware features
- **[Platform Configuration](platform_configuration.md)**: Platform-specific setup and job submission guide
- **[Detailed Workflows](detailed_workflows.md)**: Complete technical reference for all workflows
- **[Preparing New Hurricane Cases](preparing_new_hurricane_cases.md)**: Advanced guide for setting up different hurricanes
- **[Grid Generation](generate_grid_for_hurricane_segments.md)**: Detailed grid generation documentation
- **[ExtPar Processing](run_extpar_levante.md)**: External parameter processing guide
- **[Main README](../README.md)**: Project overview and quick start
