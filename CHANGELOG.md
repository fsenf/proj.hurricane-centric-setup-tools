# Changelog

All notable changes to the Hurricane-Centric Setup Tools project will be documented in this file.

## [v2026.01] - 2026-01-16

### Added
- **Common Initialization System**: Centralized `utilities/common_inits.sh` script that standardizes platform environment setup and makes all sbatch variables available across all scripts
- **Comprehensive Microphysics Templates**: Added 10 new runscript templates for microphysics perturbation studies:
  - Terminal velocity perturbations (fast/slow variants for GRAUPEL, ICE, SNOW)
  - Rime density variations (dense/soft RIMED configurations)
  - Ice crystal size variations (large/small ICE parameters)
  - CCN concentration perturbations (high/low CCN scenarios)
- **Hurricane Humberto Experiment Configurations**: New configuration files for perturbation experiments (exp120-exp129) on Levante platform
- **Extended JUWELS Configurations**: Additional experimental configurations for CCN and rime density perturbations (exp112-exp113)
- **Template Overview Documentation**: Comprehensive documentation for all available runscript templates
- **Enhanced Module Loading**: Added purge functionality for cleaner module environments on Levante

### Changed
- **Grid Generation Workflow**: Enhanced `generate_grid_for_hurricane_segments.sh` with automatic working directory creation and improved error handling
- **Dependency Management**: Improved preprocessing chain dependency handling for more robust workflow execution
- **Script Standardization**: All workflow scripts now use common initialization patterns with enhanced debug capabilities

### Fixed
- **Levante sbatch Variables**: Resolved critical bug where sbatch environment variables were not properly available in Levante job submissions
- **Working Directory Creation**: Fixed automatic creation of working directories in grid generation processes
- **IC/LBC File Handling**: Corrected configuration file references from LBC to IC files for proper initial condition processing
- **Humberto Experiment References**: Fixed test experiment references for Humberto perturbation experiments
- **Debug Functionality**: Enhanced error handling and debug output across all processing scripts

### Infrastructure
- **Cross-Platform Compatibility**: Maintained compatibility for both JUWELS and Levante HPC systems
- **Enhanced Error Handling**: Improved robustness across grid generation, IC/BC processing, and production chains
- **Workflow Reliability**: Consolidated multiple experimental enhancements for more reliable hurricane simulation workflows

## [v2025.10] - 2025-10-01

### Added
- Platform-aware configuration system with automatic detection for Levante, JUWELS, and generic HPC systems
- Universal job submission wrapper (`utilities/submit.sh`) for cross-platform compatibility
- Platform-specific configuration hierarchies under `config/{platform}/`
- Platform detection utility (`utilities/detect_platform.sh`)
- New Platform Configuration Guide (`docs/platform_configuration.md`)

### Changed
- Restructured configuration system for platform awareness
- Updated all workflow scripts to use platform-aware job submission
- Enhanced documentation with platform-specific examples and proper working directory usage

### Fixed
- Working directory dependencies in all script execution examples
- Cross-platform compatibility issues with module loading and file paths

## [v2025.08] - 2025-08-04

### Features
- Hurricane-centric grid generation and processing workflows
- TOML-based configuration system
- Multi-segment processing with SLURM job orchestration
- Basic documentation and examples
