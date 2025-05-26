# Hurricane-Centric Setup Tools

![Hurricane Simulation](docs/images/hurri.jpg)

## Overview

Hurricane-Centric Setup Tools is a comprehensive toolkit designed for preprocessing and setting up hurricane-centric numerical simulations with the atmospheric model ICON. This toolset streamlines the workflow for creating hurricane-centric simulations by automating grid generation, parameter processing, and initial/boundary condition preparation.

## Purpose

This toolset addresses the specific challenges of hurricane modeling by:

- Creating computational grids centered on hurricane trajectories
- Processing external parameters for hurricane-centric grids
- Generating appropriate initial conditions (ICs) and boundary conditions (BCs)
- Supporting warmstart simulation capabilities for improved hurricane forecasting

## Directory Structure

    hurricane-centric-setup-tools/
    ├── tools/
    │   ├── processing-chains/   # Main workflow scripts
    │   ├── grid-extpar/         # Grid and external parameter tools
    │   ├── ic-bc/               # Initial and boundary condition tools
    │   └── utilities/           # Helper scripts and utilities
    ├── docs/
    │   └── images/              # Documentation images
    └── test/                    # Test cases and validation scripts

## Usage

### Basic Preprocessing Chain

To run the complete preprocessing chain for a hurricane segment:

    cd tools/processing-chains
    ./run_hurricane_segments_preproc_chain.sh [segment_number] [sbatch_options]

This will execute the following steps:
1. Create a computational grid centered on the hurricane
2. Process external parameters with extpar
3. Generate initial conditions
4. Create boundary conditions

### Warmstart Simulation Setup

For warmstart simulations with specific hurricane segments:

    cd tools/processing-chains
    ./run_inits_for_warmstart_hurri-segments.sh [segment_number] [sbatch_options]

This will:
1. Create initial conditions through icon2icon transformations
2. Generate a weighted combination of the processed IC data

## Dependencies

This toolset requires:

- Bash/Shell environment (Linux-based system)
- SLURM workload manager for job submission
- DWD ICON tools hosted under: https://gitlab.dkrz.de/dwd-sw/dwd_icon_tools
- NetCDF libraries
- Python 3.6+ with scientific packages (numpy, xarray)
- CDO (Climate Data Operators)

## Installation

Clone the repository:

    git clone https://github.com/yourusername/hurricane-centric-setup-tools.git
    cd hurricane-centric-setup-tools

No additional installation is required as the toolset consists of scripts that run within your computing environment.

## Contributing

Contributions to the Hurricane-Centric Setup Tools are welcome:

1. Fork the repository
2. Create a feature branch (git checkout -b feature/amazing-feature)
3. Commit your changes (git commit -m 'Add amazing feature')
4. Push to the branch (git push origin feature/amazing-feature)
5. Open a Pull Request

Please ensure your code adheres to our coding standards and includes appropriate documentation.

## License

This project is licensed under the MIT License - see the LICENSE file for details.



## Acknowledgments

- This toolset was developed with support from BMBF-funded project IFCES2 under funding line SCALEXA with underlying support from European Union - NextGenerationEU. 
- Special thanks to contributors and testers who helped refine these tools
