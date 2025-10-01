# Changelog

All notable changes to the Hurricane-Centric Setup Tools project will be documented in this file.

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
