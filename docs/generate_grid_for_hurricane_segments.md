# Grid Generation for Hurricane Segments

## Overview

The `generate_grid_for_hurricane_segments.sh` script creates nested computational grids centered on hurricane trajectories for hurricane-centric ICON simulations. It generates segment-specific grids that follow the hurricane path with configurable nested domains.

## Purpose

This script addresses the need for dynamic, hurricane-following grids by:

- Creating segment-specific computational grids based on hurricane track data
- Generating nested domains with increasing resolution toward the hurricane center
- Producing mask files that define the active computational region for each segment
- Ensuring proper grid connectivity between nested domains

## Prerequisites

### System Requirements
- Linux environment with SLURM workload manager
- Access to Levante supercomputer (or similar HPC system)
- Python 3.6+ with scientific packages (numpy, xarray, scipy)
- ICON model tools and libraries

### Required Files
- Hurricane track data in NetCDF format
- Base grid file for the coarsest domain
- Configuration file (`hurricane_config.toml`)

## Configuration

The script reads configuration from `../../config/hurricane_config.toml`. Key configuration sections:

### Project Settings
```toml
[project]
name = "paulette-segments"           # Project name for output directories
width_config = "width20km"           # Configuration identifier
```

### Paths
```toml
[paths]
icontools_dir = "/path/to/icontools"      # ICON tools installation
base_grid = "/path/to/base_grid.nc"       # Coarsest domain grid file
output_base = "/path/to/output"           # Output directory base
track_dir = "/path/to/track/data"         # Hurricane track data
```

### Simulation Parameters
```toml
[simulation]
expname = "ifces2-atlanXL-20200907-exp021"  # Experiment name
nests = 3                                    # Number of nested domains
```

### Domain Configuration
```toml
[domains]
dom_width = [60.0, 40.0, 20.0]  # Domain widths in km for each nest level
```

## Usage

### Basic Usage
```bash
cd /path/to/hurricane-centric-setup-tools/scripts/grid-extpar
sbatch generate_grid_for_hurricane_segments.sh [segment_number]
```

### Parameters
- `segment_number`: Integer specifying which hurricane segment to process (e.g., 1, 2, 3, ...)

### Example
```bash
# Generate grid for segment 5
sbatch generate_grid_for_hurricane_segments.sh 5
```

## Workflow

The script executes the following steps for each nested domain:

### 1. Mask Generation
- Calls Python utility to create segment masks based on hurricane track
- Masks define the active computational region around the hurricane path
- Uses KDTree algorithm for efficient distance calculations

### 2. Grid Configuration
- Creates ICON grid generation namelist with segment-specific parameters
- Configures nested domain relationships and boundaries
- Sets refinement criteria based on the hurricane mask

### 3. Grid Creation
- Executes ICON grid generation tool (`icongridgen`)
- Creates nested grids with proper connectivity
- Outputs grid files in NetCDF format

### 4. Domain Linking
- Links newly created grids as base grids for subsequent nesting levels
- Ensures proper parent-child relationships between domains

## Output Files

### Grid Files
```
output_base/project_name/seg{N}_{width_config}/
├── paulette-seg{N}_dom1_DOM01.nc    # First nested domain
├── paulette-seg{N}_dom2_DOM01.nc    # Second nested domain
└── paulette-seg{N}_dom3_DOM01.nc    # Third nested domain (if configured)
```

### Mask Files
```
output_base/project_name/seg{N}_{width_config}/
├── paulette_segment_mask_expname_seg{N}_dom1.nc
├── paulette_segment_mask_expname_seg{N}_dom2.nc
└── paulette_segment_mask_expname_seg{N}_dom3.nc
```

## SLURM Job Configuration

The script is configured for the Levante supercomputer with the following resource requirements:

```bash
#SBATCH --account=bb1376
#SBATCH --job-name=gridgen
#SBATCH --partition=compute
#SBATCH --nodes=1
#SBATCH --cpus-per-task=16
#SBATCH --exclusive
#SBATCH --time=02:30:00
```

### Resource Usage
- **Cores**: 16 CPUs per task
- **Memory**: Exclusive node access
- **Runtime**: Up to 2.5 hours
- **Working Directory**: `/scratch/b/b380352/icontools`

## Error Handling

### Common Issues

1. **Missing Configuration File**
   - Error: "Config file not found"
   - Solution: Ensure `hurricane_config.toml` exists in `../../config/`

2. **Invalid Segment Number**
   - Error: Track data not available for segment
   - Solution: Check that hurricane track data covers the requested time period

3. **Grid Generation Failure**
   - Error: `icongridgen` execution fails
   - Solution: Check mask file validity and ICON tools availability

4. **SLURM Resource Errors**
   - Error: "More processors requested than permitted"
   - Solution: Adjust SLURM parameters or check resource availability


## Performance Considerations

- Grid generation is computationally intensive
- Runtime scales with domain size and number of grid cells
- Nested domains require sequential processing
- Consider running multiple segments in parallel for different hurricane tracks

## Integration

This script is part of the hurricane-centric preprocessing chain:

1. **Grid Generation** ← (This script)
2. External Parameter Processing (`run_extpar_levante.bash`)
3. Initial Conditions (`icon2icon_offline_lam_ini.bash`)
4. Boundary Conditions (`icon2icon_offline_lam_lbc.bash`)

## Dependencies

### Python Scripts
- `35-Create-a-Segment-Mask-for-Hurricane-Centric-Runs.py`: Creates hurricane segment masks

### Utilities
- `toml_reader.sh`: Configuration file parser
- `hurricane_config.toml`: Central configuration file

### External Tools
- ICON grid generation tools (`icongridgen`)
- SLURM workload manager
- NetCDF libraries

## Troubleshooting

For issues or questions:
- Check log files for detailed error messages
- Verify configuration file syntax
- Ensure all required input files are available


### Log Files
Check SLURM output files for detailed error messages:
```bash
# View job output
cat slurm-{job_id}.out

# Check job status
squeue -u $USER
```

### Validation
Verify grid file integrity:
```bash
ncdump -h output_file.nc  # Check NetCDF header
cdo info output_file.nc   # Check with CDO tools
```

## See Also

- **[Getting Started Guide](getting_started.md)** - Setup and first steps
- **[Detailed Workflows](detailed_workflows.md)** - Complete workflow documentation  
- **[Configuration Reference](configuration_reference.md)** - TOML configuration options
- **[ExtPar Processing](run_extpar_levante.md)** - Next step in preprocessing chain
- **[Preprocessing Dependencies](preproc_chain_looper_dependencies.md)** - Dependency analysis


