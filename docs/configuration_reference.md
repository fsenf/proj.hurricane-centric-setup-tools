# Configuration Reference

This document provides a comprehensive reference for all TOML configuration options used in the Hurricane-Centric Setup Tools.

## Overview

### Configuration File Format

The system uses TOML (Tom's Obvious Minimal Language) configuration files. The main configuration file is `config/hurricane_config.toml`.

### Configuration Sections

```toml
[tools]          # Tool directories and executables
[project]        # Project identification and naming  
[reference]      # Reference grids and datasets
[track]          # Hurricane track data
[domains]        # Grid and domain specifications
[output]         # Output directories
```

## Section Details

### [tools] - Tool Directories and Executables

Specifies paths to required tools and software installations.

```toml
[tools]
icontools_dir = "/work/bb1376/tools/dwd_icon_tools/icontools"
extpar_input_dir = "/work/pd1167/extpar-input-data"
extpar_dir = "/work/bb1376/tools/extpar"
icon_build_dir = "/work/bb1376/user/fabian/model/icon/icon-builds/icon-release-2024.07/"
```

**Parameters**:
- **`icontools_dir`** (string): Path to ICON tools installation
  - Contains: Grid generation and processing tools
  - Required for: Grid generation workflows

- **`extpar_input_dir`** (string): External parameter input datasets directory
  - Contains: `era/`, `topo/globe/`, `landuse/`, `soil/`, etc.
  - Used by: ExtPar processing for static field generation

- **`extpar_dir`** (string): ExtPar tool installation directory
  - Contains: ExtPar executable and configuration files
  - Used for: Processing static datasets (topography, land use, etc.)

- **`icon_build_dir`** (string): ICON model installation directory
  - Contains: `bin/`, `run/`, `experiments/`
  - Must contain: Compiled ICON binaries and runscript templates

### [project] - Project Settings

Controls project naming and identification used throughout the workflow.

```toml
[project]
name = "hurricane-paulette2020-segments"
width_config = "width20km_reinit12h"
working_dir = "/scratch/b/b380352/icontools"
```

**Parameters**:
- **`name`** (string): Base name for output directories and experiment names
  - Used in: `${OUTPUT_GRID_BASEDIR}/${name}/`, experiment names
  - Format: lowercase, hyphens preferred, no spaces
  - Example: `"hurricane-paulette2020-segments"`

- **`width_config`** (string): Configuration variant identifier
  - Used in: Directory naming, experiment identification
  - Format: descriptive of grid width and reinit timing
  - Examples: `"width20km_reinit12h"`, `"width100km_reinit24h"`

- **`working_dir`** (string): Working directory for temporary files
  - Used for: Intermediate processing files and scratch space
  - Must have: Write permissions and sufficient disk space

### [reference] - Reference Grids and Datasets

Specifies reference initialization data and experiment parameters.

```toml
[reference]
init_time = "20200907T000000Z"
expname = "ifces2-atlanXL-20200907-exp021"
input_grid = "/work/bb1376/data/icon/grids-extpar/atlanXL/atlanXL_R02B10_DOM02.nc"
input_icbc_dir = "/work/bb1376/data/icon/atlantic-cases/paulette/ifces2-atlanXL-20200907-exp021"
input_icbc_subdir = "NESTING"
```

**Parameters**:
- **`init_time`** (string): Reference initialization time
  - Format: ISO 8601 datetime string (YYYYMMDDTHHMMSSZ)
  - Used for: Calculating segment start times
  - Example: `"20200907T000000Z"`

- **`expname`** (string): Reference experiment name
  - Used for: Template and naming patterns
  - Format: Typically includes date and experiment number
  - Example: `"ifces2-atlanXL-20200907-exp021"`

- **`input_grid`** (string): Reference grid file for interpolation
  - Format: NetCDF grid file with ICON mesh information
  - Used for: IC/BC generation and grid templates

- **`input_icbc_dir`** (string): Main directory containing reference ICON data
  - Contains: reference ICON data (large domain ICOn simulations)
  - Format: Typically includes initialization date and experiment name

- **`input_icbc_subdir`** (string): Subdirectory within reference IC/BC dir
  - Format: Typically subdirectory name within the input_icbc_dir
  - Contains: 
    - actual IC/BC files for initialization and boundary conditions
    - files for IC `lam_input_IC_DOM0?_ML_{init_time}.nc` and BC `lam_input_BC_DOM0?_ML_{init_time}.nc`
    - should also contain one `lam_input_geo_DOM02_ML.nc` file with variable `z_ifc`


### [track] - Hurricane Track Data

Defines hurricane track files for grid center positioning.

```toml
[track]
track_dir = "/work/bb1376/data/icon/atlantic-cases/derived-data/tracks/"
track_file = "ifces2-atlanXL-20200907-exp021-DOM02_paulette_best-fit.nc"
```

**Parameters**:
- **`track_dir`** (string): Hurricane track data directory
  - Contains: Track files for grid center positioning

- **`track_file`** (string): Hurricane track filename
  - Format: NetCDF file with time-dependent hurricane position
  - Used for: Dynamic grid centering for each segment

### [domains] - Grid and Domain Configuration

Defines the nested domain structure and grid specifications.

```toml
[domains]
segment_reinit_hours = 12.0
segment_length_added = 3.0
nests = 3
dom_width = [60.0, 40.0, 20.0]  # in km
```

**Parameters**:
- **`segment_reinit_hours`** (float): Hours between segment reinitializations
  - Common values: 12.0, 24.0, 48.0
  - Controls: Temporal extent of segments
  - Example: 12.0 = new segment every 12 hours

- **`segment_length_added`** (float): Additional simulation hours beyond reinit
  - Purpose: Overlap before and after the actual segment for warmstart processing
  - Typical: 3.0 hours
  - Total segment length: `segment_reinit_hours + 2 * segment_length_added`

- **`nests`** (integer): Number of nested domains
  - Range: 1-3 (typically 3)
  - Creates: Domain 1 (coarsest) to Domain N (finest)

- **`dom_width`** (array of floats): Domain cross-track widths in kilometers
  - Length: Must match `nests` parameter
  - Order: Coarsest to finest domain
  - Example: `[60.0, 40.0, 20.0]` for 3 domains

### [output] - Output Directories

Specifies where generated files will be stored.

```toml
[output]
grid_basedir = "/work/bb1376/data/icon/grids-extpar"
icbc_basedir = "/work/bb1376/data/icon/bc-init"
```

**Parameters**:
- **`grid_basedir`** (string): Base directory for generated grids
  - Structure: `${grid_basedir}/${project.name}/seg${kk}_${project.width_config}/`
  - Must have: Write permissions and sufficient disk space

- **`icbc_basedir`** (string): Base directory for IC/BC files
  - Structure: `${icbc_basedir}/${project.name}/seg${kk}_${project.width_config}/`
  - Must have: Write permissions and sufficient disk space

## Configuration Examples

```toml
[tools]
icontools_dir = "/work/bb1376/tools/dwd_icon_tools/icontools"
extpar_input_dir = "/work/pd1167/extpar-input-data"
extpar_dir = "/work/bb1376/tools/extpar"
icon_build_dir = "/work/bb1376/user/fabian/model/icon/icon-builds/icon-release-2024.07/"

[project]
name = "hurricane-paulette2020-segments"
width_config = "width20km_reinit12h"
working_dir = "/scratch/b/b380352/icontools"

[reference]
init_time = "20200907T000000Z"
expname = "ifces2-atlanXL-20200907-exp021"
input_grid = "/work/bb1376/data/icon/grids-extpar/atlanXL/atlanXL_R02B10_DOM02.nc"
input_icbc_dir = "/work/bb1376/data/icon/atlantic-cases/paulette/ifces2-atlanXL-20200907-exp021"
input_icbc_subdir = "NESTING"

[track]
track_dir = "/work/bb1376/data/icon/atlantic-cases/derived-data/tracks/"
track_file = "ifces2-atlanXL-20200907-exp021-DOM02_paulette_best-fit.nc"

[domains]
segment_reinit_hours = 12.0
segment_length_added = 3.0
nests = 3
dom_width = [60.0, 40.0, 20.0]

[output]
grid_basedir = "/work/bb1376/data/icon/grids-extpar"
icbc_basedir = "/work/bb1376/data/icon/bc-init"
```

## Environment Variables

When configuration is loaded, TOML sections and keys are converted to uppercase environment variables with section prefixes:

```bash
# [tools] section
TOOLS_ICONTOOLS_DIR="/work/bb1376/tools/dwd_icon_tools/icontools"
TOOLS_EXTPAR_INPUT_DIR="/work/pd1167/extpar-input-data"
TOOLS_EXTPAR_DIR="/work/bb1376/tools/extpar"
TOOLS_ICON_BUILD_DIR="/work/bb1376/user/fabian/model/icon/icon-builds/icon-release-2024.07/"

# [project] section
PROJECT_NAME="hurricane-paulette2020-segments"
PROJECT_WIDTH_CONFIG="width20km_reinit12h"
PROJECT_WORKING_DIR="/scratch/b/b380352/icontools"

# [reference] section
REFERENCE_INIT_TIME="20200907T000000Z"
REFERENCE_EXPNAME="ifces2-atlanXL-20200907-exp021"
REFERENCE_INPUT_GRID="/work/bb1376/data/icon/grids-extpar/atlanXL/atlanXL_R02B10_DOM02.nc"
REFERENCE_INPUT_ICBC_DIR="/work/bb1376/data/icon/atlantic-cases/paulette/ifces2-atlanXL-20200907-exp021"
REFERENCE_INPUT_ICBC_SUBDIR="NESTING"

# [track] section
TRACK_TRACK_DIR="/work/bb1376/data/icon/atlantic-cases/derived-data/tracks/"
TRACK_TRACK_FILE="ifces2-atlanXL-20200907-exp021-DOM02_paulette_best-fit.nc"

# [domains] section
DOMAINS_SEGMENT_REINIT_HOURS="12.0"
DOMAINS_SEGMENT_LENGTH_ADDED="3.0"
DOMAINS_NESTS="3"
DOMAINS_DOM_WIDTH="60.0 40.0 20.0"

# [output] section
OUTPUT_GRID_BASEDIR="/work/bb1376/data/icon/grids-extpar"
OUTPUT_ICBC_BASEDIR="/work/bb1376/data/icon/bc-init"
```

## Configuration Validation

### Required Parameters
The following parameters are required and must be set:
- `tools.icontools_dir`
- `tools.extpar_input_dir`
- `tools.extpar_dir`
- `tools.icon_build_dir`
- `project.name`
- `project.width_config`
- `project.working_dir`
- `reference.init_time`
- `reference.expname`
- `reference.input_grid`
- `reference.input_icbc_dir`
- `reference.input_icbc_subdir`
- `track.track_dir`
- `track.track_file`
- `domains.segment_reinit_hours`
- `domains.segment_length_added`
- `domains.nests`
- `domains.dom_width`
- `output.grid_basedir`
- `output.icbc_basedir`

### Validation Scripts
Test your configuration:

```bash
# Test TOML parsing
cd test
./test_toml_reader.sh

# Validate paths exist
python ../utilities/check_preprocessing_files.py ../config/hurricane_config.toml 1 grid
```

## Configuration Best Practices

### 1. Path Management
- Use absolute paths for all directory specifications
- Ensure write permissions for output directories (`output.grid_basedir`, `output.icbc_basedir`, `project.working_dir`)
- Verify input data availability before running workflows
- Check that tool directories contain required executables

### 2. Naming Conventions
- Use descriptive project names with hyphens
- Include key parameters in `width_config` (resolution, timing)
- Keep names filesystem-safe (no spaces, special characters)
- Use consistent hurricane case naming across projects

### 3. Domain Configuration
- Start with 3 nested domains for most applications
- Grid spacing of the coarsest nest is half of the reference spacing
- Match domain widths to your hurricane size and computational resources
- sufficient cross-track width `dom_width` helps to keep the hurricane laterally
- sufficient `segment_length_added` help to keep slower or faster progressing hurricanes

### 4. Timing Configuration
- Use 24-hour reinit for most hurricane applications
- Reduce to 12 hours if computational constraints are important
- Ensure adequate overlap for warmstart processing

### 5. Version Control
- Keep configuration files in version control
- Use descriptive names for different configuration variants
- Document configuration changes in commit messages
- Test configurations on small segments before production runs

## Troubleshooting Configuration Issues

### Common Problems

1. **Path not found errors**
   - Check all paths exist and are accessible
   - Verify write permissions for output directories (`output.grid_basedir`, `output.icbc_basedir`)
   - Use absolute paths instead of relative paths
   - Ensure tool directories contain required executables

2. **TOML parsing errors**
   - Validate TOML syntax with online validators
   - Check for missing quotes around string values
   - Ensure array syntax is correct `[item1, item2]`
   - Verify numeric values don't have quotes

3. **Environment variable issues**
   - Use `test_toml_reader.sh` to debug variable conversion
   - Check for special characters in values
   - Verify array values are space-separated in environment
   - Ensure section names match expected format

4. **Domain configuration problems**
   - Ensure `dom_width` array length matches `nests`
   - Check domain widths are decreasing (coarse to fine)
   - Verify computational resources can handle finest domain
   - Check `segment_reinit_hours` and `segment_length_added` are reasonable

5. **Hurricane track issues**
   - Verify track file exists and is readable
   - Check track file format matches expected NetCDF structure
   - Ensure track covers the simulation time period
   - Validate hurricane position data quality

### File Structure Generated

The configuration generates the following directory structure:

```
${output.grid_basedir}/
└── ${project.name}/
    └── seg${kk}_${project.width_config}/
        ├── ${project.name}-seg${kk}_dom1_DOM01.nc
        ├── ${project.name}-seg${kk}_dom2_DOM01.nc
        └── ${project.name}-seg${kk}_dom3_DOM01.nc

${output.icbc_basedir}/
└── ${project.name}/
    └── seg${kk}_${project.width_config}/
        ├── ifces2-atlanXL-ML_${project.name}_seg${kk}_DOM01_ic.nc
        ├── ifces2-atlanXL-ML_${project.name}_seg${kk}_DOM01_lbc.nc
        └── ifces2-atlanXL-ML_${project.name}_seg${kk}_warmini.nc

${tools.icon_build_dir}/experiments/
└── ${project.name}-segment${kk}-${DATE}-exp111/
    ├── exp.${project.name}-segment${kk}-${DATE}-exp111.run
    ├── ICON_master.namelist
    └── output/
```

For additional help, see the [Getting Started](getting_started.md) guide, [Preparing New Hurricane Cases](preparing_new_hurricane_cases.md) for advanced setups, or [Detailed Workflows](detailed_workflows.md) documentation.
