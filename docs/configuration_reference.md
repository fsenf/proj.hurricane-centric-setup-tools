# Configuration Reference

This document provides a comprehensive reference for all TOML configuration options used in the Hurricane-Centric Setup Tools.

## Configuration File Structure

The system uses TOML (Tom's Obvious Minimal Language) configuration files. The main configuration file is `config/hurricane_config.toml`.

### Configuration Sections

```toml
[project]        # Project identification and naming
[paths]          # File system paths and directories  
[domains]        # Grid and domain specifications
[reference]      # Reference data and timing
[simulation]     # Simulation parameters
```

## Section Details

### [project] - Project Settings

Controls project naming and identification used throughout the workflow.

```toml
[project]
name = "hurricane-paulette2020-segments"    # Project identifier for directories
width_config = "width200km_reinit24h"       # Configuration variant identifier
```

**Parameters**:
- **`name`** (string): Base name for output directories and experiment names
  - Used in: `${OUTPUT_GRID_BASEDIR}/${name}/`, experiment names
  - Format: lowercase, hyphens preferred, no spaces
  - Example: `"hurricane-paulette2020-segments"`

- **`width_config`** (string): Configuration variant identifier
  - Used in: Directory naming, experiment identification
  - Format: descriptive of grid width and reinit timing
  - Examples: `"width100km_reinit12h"`, `"width200km_reinit24h"`

### [paths] - File System Paths

Defines all input and output directory locations.

```toml
[paths]
output_grid_basedir = "/work/bb1376/data/icon/grids-extpar"
output_icbc_basedir = "/work/bb1376/data/icon/bc-init"
tools_icon_build_dir = "/work/bb1376/user/fabian/model/icon/icon-builds/icon-release-2024.07"
extpar_input_dir = "/work/pd1167/extpar-input-data"
reference_input_grid = "/work/bb1376/data/icon/grids-extpar/ifces2-atlanLEM/iconR2B06-atl-nest-multlayer.nc"
reference_input_icbc_dir = "/work/bb1376/data/icon/bc-init/ifces2-atlanLEM"
reference_input_icbc_subdir = "2020090700/BC-IC_DOM02_ML_20200907T000000Z"
hurricane_track_dir = "/work/bb1376/data/hurricane-tracks"
```

**Parameters**:
- **`output_grid_basedir`** (string): Base directory for generated grids
  - Structure: `${output_grid_basedir}/${project.name}/seg${N}_${project.width_config}/`
  - Must have write permissions

- **`output_icbc_basedir`** (string): Base directory for IC/BC files
  - Structure: `${output_icbc_basedir}/${project.name}/seg${N}_${project.width_config}/`
  - Must have write permissions

- **`tools_icon_build_dir`** (string): ICON model installation directory
  - Contains: `bin/`, `run/`, `experiments/`
  - Must contain compiled ICON binaries

- **`extpar_input_dir`** (string): ExtPar input datasets directory
  - Contains: `era/`, `topo/globe/`, `landuse/`, `soil/`, etc.
  - Read-only access required

- **`reference_input_grid`** (string): Reference grid file for interpolation
  - Format: NetCDF grid file with ICON mesh information
  - Used for: IC/BC generation

- **`reference_input_icbc_dir`** (string): Directory containing reference IC/BC data
  - Contains: ERA5 or other global model data
  - Format: Organized by initialization date

- **`reference_input_icbc_subdir`** (string): Subdirectory within reference IC/BC dir
  - Format: Typically `${YYYYMMDDHH}/BC-IC_DOM02_ML_${YYYYMMDDHH}T000000Z`
  - Contains: Actual IC/BC files

- **`hurricane_track_dir`** (string): Hurricane track data directory
  - Contains: Track files for grid center positioning
  - Format: Depends on track data source

### [domains] - Grid and Domain Configuration

Defines the nested domain structure and grid specifications.

```toml
[domains]
nests = 3                                    # Number of nested domains
dom_width = [60.0, 40.0, 20.0]             # Domain widths in km
segment_reinit_hours = 24                    # Hours between segment reinitializations
segment_length_added = 12                    # Additional hours beyond reinit interval
```

**Parameters**:
- **`nests`** (integer): Number of nested domains
  - Range: 1-5 (typically 3)
  - Creates: Domain 1 (coarsest) to Domain N (finest)

- **`dom_width`** (array of floats): Domain widths in kilometers
  - Length: Must match `nests` parameter
  - Order: Coarsest to finest domain
  - Example: `[60.0, 40.0, 20.0]` for 3 domains

- **`segment_reinit_hours`** (integer): Hours between segment reinitializations
  - Common values: 12, 24, 48
  - Controls: Temporal overlap between segments
  - Example: 24 = new segment every 24 hours

- **`segment_length_added`** (integer): Additional simulation hours beyond reinit
  - Purpose: Overlap for warmstart processing
  - Typical: 12 hours
  - Total segment length: `segment_reinit_hours + segment_length_added`

### [reference] - Reference Data and Timing

Specifies reference initialization and experiment parameters.

```toml
[reference]
init_time = "2020-09-07T00:00:00"
expname = "ifces2-atlanXL-20200907-exp021"
```

**Parameters**:
- **`init_time`** (string): Reference initialization time
  - Format: ISO 8601 datetime string
  - Used for: Calculating segment start times
  - Example: `"2020-09-07T00:00:00"`

- **`expname`** (string): Reference experiment name
  - Used for: Template and naming patterns
  - Format: Typically includes date and experiment number
  - Example: `"ifces2-atlanXL-20200907-exp021"`

### [simulation] - Simulation Parameters

Controls ICON model simulation settings.

```toml
[simulation]
# Additional simulation parameters can be added here
# Currently most parameters are handled by runscript templates
```

## Configuration Examples

### High-Resolution Configuration
```toml
[project]
name = "hurricane-ida2021-hires"
width_config = "width50km_reinit12h"

[domains]
nests = 4
dom_width = [80.0, 60.0, 40.0, 20.0]
segment_reinit_hours = 12
segment_length_added = 12
```

### Low-Resolution Configuration
```toml
[project]
name = "hurricane-katrina2005-lowres"
width_config = "width100km_reinit48h"

[domains]
nests = 2
dom_width = [100.0, 60.0]
segment_reinit_hours = 48
segment_length_added = 24
```

## Environment Variables

When configuration is loaded, TOML sections and keys are converted to uppercase environment variables with section prefixes:

```bash
# [project] section
PROJECT_NAME="hurricane-paulette2020-segments"
PROJECT_WIDTH_CONFIG="width200km_reinit24h"

# [paths] section  
PATHS_OUTPUT_GRID_BASEDIR="/work/bb1376/data/icon/grids-extpar"
PATHS_OUTPUT_ICBC_BASEDIR="/work/bb1376/data/icon/bc-init"

# [domains] section
DOMAINS_NESTS="3"
DOMAINS_DOM_WIDTH="60.0 40.0 20.0"
```

## Configuration Validation

### Required Parameters
The following parameters are required and must be set:
- `project.name`
- `project.width_config`
- `paths.output_grid_basedir`
- `paths.output_icbc_basedir`
- `paths.tools_icon_build_dir`
- `domains.nests`
- `domains.dom_width`

### Validation Scripts
Test your configuration:

```bash
# Test TOML parsing
cd test
./test_toml_reader.sh

# Validate paths exist
python ../utilities/check_preprocessing_files.py ../config/my_config.toml 1 grid
```

## Configuration Best Practices

### 1. Path Management
- Use absolute paths for all directory specifications
- Ensure write permissions for output directories
- Verify input data availability before running workflows

### 2. Naming Conventions
- Use descriptive project names with hyphens
- Include key parameters in `width_config` (resolution, timing)
- Keep names filesystem-safe (no spaces, special characters)

### 3. Domain Configuration
- Start with 3 nested domains for most applications
- Ensure sufficient resolution jump between domains (factor of 1.5-2)
- Match domain widths to your hurricane size and computational resources

### 4. Timing Configuration
- Use 24-hour reinit for most applications
- Reduce to 12 hours for rapidly evolving hurricanes
- Ensure adequate overlap (`segment_length_added`) for warmstart

### 5. Version Control
- Keep configuration files in version control
- Use descriptive names for different configuration variants
- Document configuration changes in commit messages

## Troubleshooting Configuration Issues

### Common Problems

1. **Path not found errors**
   - Check all paths exist and are accessible
   - Verify write permissions for output directories
   - Use absolute paths instead of relative paths

2. **TOML parsing errors**
   - Validate TOML syntax with online validators
   - Check for missing quotes around string values
   - Ensure array syntax is correct `[item1, item2]`

3. **Environment variable issues**
   - Use `test_toml_reader.sh` to debug variable conversion
   - Check for special characters in values
   - Verify array values are space-separated in environment

4. **Domain configuration problems**
   - Ensure `dom_width` array length matches `nests`
   - Check domain widths are decreasing (coarse to fine)
   - Verify computational resources can handle finest domain

For additional help, see the [Getting Started](getting_started.md) guide or [Detailed Workflows](detailed_workflows.md) documentation.
