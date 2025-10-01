# Platform Configuration Guide

This document explains how to configure the Hurricane-Centric Setup Tools for different computing platforms with automatic platform detection and optimization.

## Overview

The system supports multiple HPC platforms through automatic platform detection and platform-specific configurations. Each platform has its own:

- **Module loading scripts** - Load required software modules
- **SLURM configuration** - Platform-specific job parameters
- **Configuration files** - Optimized settings for the platform

## Supported Platforms

### DKRZ Levante (`levante`)
- **Detection**: Hostnames matching `*.levante.dkrz.de`
- **Configuration Directory**: `config/levante/`
- **Partition**: `compute` (default), `shared` (for small jobs)
- **Module System**: Traditional `module load` commands

### JSC JUWELS (`juwels`)  
- **Detection**: Hostnames containing `juwels`
- **Configuration Directory**: `config/juwels/`
- **Partition**: `batch` (default)
- **Module System**: Stages-based module system

### Generic Systems (`generic`)
- **Detection**: All other systems (fallback)
- **Configuration Directory**: `config/generic/`
- **Purpose**: Template for custom platform configurations

## Platform Configuration Files

Each platform directory contains:

```bash
config/{platform}/
├── hurricane_config*.toml     # Platform-specific configurations
├── module_loader.sh           # Module loading script
└── sbatch_env_setter.sh       # SLURM parameter settings
```

### Module Loader Scripts

#### Levante (`config/levante/module_loader.sh`)
```bash
#!/bin/bash
# Module loading for DKRZ Levante

module load python3
module load cdo
```

#### JUWELS (`config/juwels/module_loader.sh`)
```bash
#!/bin/bash
# Module loading for JSC JUWELS

module load Stages/2025
module load GCC OpenMPI
module load Python
module load CDO NCO
```

#### Generic (`config/generic/module_loader.sh`)
```bash
#!/bin/bash
# Generic module loading - customize for your system

# Uncomment and modify for your system:
# module load python/3.9
# module load cdo
# module load netcdf

echo "Generic platform - please customize module loading"
```

### Platform-Independent Job Submission System

The system uses `utilities/submit.sh` as a universal job submission wrapper that handles platform detection and SBATCH parameter configuration automatically.

#### Universal Submit Wrapper
Instead of calling `sbatch` directly, an sbatch wrapper needs to be defined with absolute path

```bash
sbatch_wrapper="/path/to/hurricane-centric-setup-tools/utilities/submit.sh"
```


All jobs are submitted through:
```bash
${sbatch_wrapper} <script> [sbatch_options] [script_arguments]
```

**Key Benefits**:
- **Platform independence**: Automatically detects platform and applies appropriate settings
- **Script type detection**: Recognizes script type from filename and applies optimal resources
- **Consistent interface**: Same command syntax across all platforms
- **Job ID capture**: Returns parsable job IDs for dependency chains

#### Script Types and Resource Requirements

The submit system automatically detects script types and applies optimized resources:

| Script Type | Detection Pattern | Purpose | Typical Resources |
|------------|------------------|---------|-------------------|
| `grid_gen` | `*grid*`, `generate_grid*` | Grid generation | High CPU, moderate time |
| `extpar` | `*extpar*`, `run_extpar*` | External parameters | Moderate CPU, short time |
| `testchain` | `*testrun*`, `test*` | Test runs | Few nodes, short time |
| `production` | `*production*`, `*starter*` | Model runs | High nodes, very long time |
| `lbc` | `*lbc*`, `*icon2icon*lbc*` | Boundary conditions | Moderate CPU, long time |
| `remapmerge` | `*remap*merge*` | Remap/merge operations | Low CPU, moderate time |
| `ini` | `*ini*`, `*icon2icon*ini*` | Initial conditions | High CPU, long time |

#### SBATCH Environment Configuration

Platform-specific settings are configured in `config/{platform}/sbatch_env_setter.sh` files.

**Configuration Questions to Consider**:
- How many cores per node on your platform?
- What is the maximum walltime allowed?
- What is your compute partition name?
- What is your compute account?
- How many nodes do you need for production runs within walltime constraints?

**Example Platform Settings**:

**Levante Configuration**:
```bash
# Machine and account settings
SBATCH_CPUS_PER_TASK="128"      # Cores per node on Levante
SBATCH_PARTITION="compute"       # Compute partition name
SBATCH_ACCOUNT="your-account"    # Your account name

...

# Script-type specific settings
case "$1" in
    "grid_gen")
        SBATCH_TIME="02:30:00"
        ;;
    "production") 
        SBATCH_NODES="64"
        SBATCH_TIME="08:00:00"
        ;;
    ...
esac
```

**JUWELS Configuration**:
```bash
# Machine and account settings
SBATCH_CPUS_PER_TASK="48"       # Cores per node on JUWELS
SBATCH_PARTITION="batch"         # Compute partition name  
SBATCH_ACCOUNT="your-account"    # Your account name

...

# Script-type specific settings
case "$1" in
    "grid_gen")
        SBATCH_TIME="03:00:00"    # JUWELS needs more time
        ;;
    "production")
        SBATCH_NODES="172"        # More nodes available on JUWELS
        SBATCH_TIME="08:00:00"
        ;;
    ...
esac
```

#### Usage Examples
```bash
# Define sbatch wrapper with absolute path (required for all examples)
sbatch_wrapper="/path/to/hurricane-centric-setup-tools/utilities/submit.sh"

# Submit grid generation from appropriate directory (automatically detects platform and grid_gen type)
cd /path/to/hurricane-centric-setup-tools/scripts/grid-extpar
JOB1=$(${sbatch_wrapper} generate_grid_for_hurricane_segments.sh -c ../../config/config.toml 2)

# Submit extpar with dependency (automatically detects platform and extpar type)
JOB2=$(${sbatch_wrapper} run_extpar.bash --dependency=afterok:$JOB1 -c ../../config/config.toml 2)

# Platform-specific configuration (recommended)
JOB1=$(${sbatch_wrapper} generate_grid_for_hurricane_segments.sh -c ../../config/levante/hurricane_config.toml 2)
JOB1=$(${sbatch_wrapper} generate_grid_for_hurricane_segments.sh -c ../../config/juwels/hurricane_config.toml 2)

# Override platform defaults (advanced usage)
${sbatch_wrapper} generate_grid_for_hurricane_segments.sh --nodes=128 --time=12:00:00 -c ../../config/config.toml 1

# Debug job submission
HURRICANE_DEBUG_SUBMIT=true ${sbatch_wrapper} generate_grid_for_hurricane_segments.sh -c ../../config/config.toml 1
```

## Adding a New Platform

To add support for a new platform:

### 1. Create Platform Directory
```bash
mkdir config/your-platform
```

### 2. Create Module Loader
```bash
cat > config/your-platform/module_loader.sh << 'EOF'
#!/bin/bash
# Module loading for Your Platform

module load your-python-module
module load your-cdo-module
# Add other required modules
EOF

chmod +x config/your-platform/module_loader.sh
```

### 3. Create SBATCH Environment Setter
```bash
cp config/generic/sbatch_env_setter.sh config/your-platform/
# Edit the file to customize for your platform
```

### 4. Update Platform Detection
Edit `utilities/detect_platform.sh`:
```bash
# Add your platform detection logic
elif [[ "$HOSTNAME" == *your-pattern* ]]; then
    PLATFORM="your-platform"
```

### 5. Create Platform-Specific Configurations
```bash
cp config/generic/hurricane_config.toml config/your-platform/
# Edit paths and settings for your platform
```

## Testing Platform Configuration

### 1. Test Platform Detection
```bash
source utilities/detect_platform.sh
echo "Detected platform: $PLATFORM"
```

### 2. Test Module Loading
```bash
source config/$PLATFORM/module_loader.sh
python --version
cdo --version
```

### 3. Test SBATCH Environment
```bash
source config/$PLATFORM/sbatch_env_setter.sh grid_gen
echo "SBATCH settings:"
env | grep SBATCH_
```

### 4. Test Full Integration
```bash
# Define sbatch wrapper with absolute path
sbatch_wrapper="/path/to/hurricane-centric-setup-tools/utilities/submit.sh"

# Change to appropriate script directory
cd /path/to/hurricane-centric-setup-tools/scripts/grid-extpar/

# Test platform-aware submission with automatic detection
${sbatch_wrapper} generate_grid_for_hurricane_segments.sh 1 -c ../../config/hurricane_config.toml

# Test with platform-specific configuration
${sbatch_wrapper} generate_grid_for_hurricane_segments.sh 1 -c ../../config/$PLATFORM/hurricane_config.toml

# Debug mode for troubleshooting
HURRICANE_DEBUG_SUBMIT=true ${sbatch_wrapper} generate_grid_for_hurricane_segments.sh 1 -c ../../config/$PLATFORM/hurricane_config.toml
```

## Troubleshooting

### Platform Not Detected
- Check hostname pattern in `utilities/detect_platform.sh`
- Verify your system's hostname: `echo $HOSTNAME`
- Add custom detection logic if needed

### Module Loading Fails
- Check available modules: `module avail`
- Update module names in `config/$PLATFORM/module_loader.sh`
- Check module dependencies and load order

### SLURM Jobs Fail
- Verify account name: `sacctmgr list assoc user=$USER`
- Check partition availability: `sinfo`
- Adjust resource requirements in `sbatch_env_setter.sh`

### Configuration Issues
- Validate TOML syntax: `python -c "import tomllib; tomllib.load(open('config.toml', 'rb'))"`
- Check file paths exist and are accessible
- Verify directory permissions for working directories
- Test platform-specific configurations: `/path/to/hurricane-centric-setup-tools/utilities/submit.sh --help` to verify submit.sh is working
- Validate platform detection: `/path/to/hurricane-centric-setup-tools/utilities/detect_platform.sh` should return your platform

## Best Practices

### Resource Allocation
- **Start conservative**: Use smaller resources for testing
- **Monitor usage**: Check actual resource utilization with `sacct`
- **Scale appropriately**: Adjust based on your data and timeline

### Configuration Management
- **Version control**: Keep platform configs in git
- **Document changes**: Comment your customizations
- **Test thoroughly**: Verify changes with test cases

### Maintenance
- **Regular updates**: Keep module versions current
- **Monitor performance**: Track job completion times
- **User feedback**: Collect experience from other users

## See Also

- **[Configuration Reference](configuration_reference.md)** - Complete TOML configuration options
- **[Getting Started Guide](getting_started.md)** - Initial setup instructions
- **[Detailed Workflows](detailed_workflows.md)** - Platform-specific workflow examples
