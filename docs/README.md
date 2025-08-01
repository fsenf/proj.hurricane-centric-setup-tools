# Hurricane-Centric Setup Tools - Documentation Index

Welcome to the comprehensive documentation for the Hurricane-Centric Setup Tools. This index will help you find the information you need.

## 📚 Main Documentation

### 🚀 **Getting Started**
- **[Getting Started Guide](getting_started.md)** - Your first steps with the tools
  - Prerequisites and installation
  - Basic configuration setup  
  - Running your first hurricane simulation
  - Common first-time issues and solutions

### 🔧 **Detailed Technical Documentation**
- **[Detailed Workflows](detailed_workflows.md)** - Complete workflow reference
  - Single and multi-segment preprocessing
  - Production workflows with warmstart capabilities
  - Validation and testing procedures
  - Advanced SLURM job management

- **[Configuration Reference](configuration_reference.md)** - Complete TOML configuration guide
  - All configuration sections and parameters
  - Environment variable mapping
  - Configuration examples and best practices
  - Troubleshooting configuration issues

## 🛠️ **Component-Specific Guides**

### Grid and Parameter Processing
- **[Grid Generation](generate_grid_for_hurricane_segments.md)** - Hurricane-centered grid creation
  - Nested domain setup
  - Hurricane trajectory following
  - Mask generation for segments

- **[ExtPar Processing](run_extpar_levante.md)** - External parameter processing
  - Topography and land use processing
  - Static dataset integration
  - Multi-domain parameter generation

## 📊 **Workflow Analysis**

### Dependency Documentation
- **[Production Looper Dependencies](production_looper_dependencies.md)** - Production workflow dependencies
  - Complete dependency graph with Mermaid diagrams
  - Critical path analysis
  - Resource management considerations

- **[Preprocessing Dependencies](preproc_chain_looper_dependencies.md)** - Preprocessing workflow dependencies
  - Multi-segment preprocessing dependencies
  - SLURM job orchestration
  - Parallel processing considerations

## 📋 **Additional Resources**

- **[Project Status](STATUS.md)** - Development status and known issues

## 🗺️ **Navigation Guide**

### **New Users**: Start Here
1. **[Getting Started Guide](getting_started.md)** - Set up your environment and run your first simulation
2. **[Configuration Reference](configuration_reference.md)** - Customize for your specific setup
3. **[Detailed Workflows](detailed_workflows.md)** - Learn about advanced features

### **Experienced Users**: Quick Reference  
- **[Detailed Workflows](detailed_workflows.md)** - Complete command reference and advanced usage
- **[Configuration Reference](configuration_reference.md)** - Parameter tuning and optimization
- **[Dependency Analysis](production_looper_dependencies.md)** - Understanding the workflow structure

### **Troubleshooting**
1. **[Getting Started - Common Issues](getting_started.md#common-first-time-issues)** - First-time setup problems
2. **[Detailed Workflows - Monitoring & Debugging](detailed_workflows.md#monitoring-and-debugging)** - Runtime issues
3. **[Configuration Reference - Troubleshooting](configuration_reference.md#troubleshooting-configuration-issues)** - Configuration problems

### **System Administrators**
- **[Detailed Workflows](detailed_workflows.md)** - Resource requirements and SLURM configuration
- **[Dependency Analysis](production_looper_dependencies.md)** - System integration requirements
- **[ExtPar Processing](run_extpar_levante.md)** - Input dataset requirements

## 🔗 **Cross-References**

### Workflow Stages
1. **Grid Generation** → [Grid Generation Guide](generate_grid_for_hurricane_segments.md)
2. **External Parameters** → [ExtPar Processing Guide](run_extpar_levante.md)  
3. **Initial/Boundary Conditions** → [Detailed Workflows - IC/BC Section](detailed_workflows.md#preprocessing-workflows)
4. **Production Runs** → [Detailed Workflows - Production Section](detailed_workflows.md#production-workflows)

### Configuration Topics
- **Project Setup** → [Configuration Reference - Project Settings](configuration_reference.md#project---project-settings)
- **Path Configuration** → [Configuration Reference - Paths](configuration_reference.md#paths---file-system-paths)
- **Domain Setup** → [Configuration Reference - Domains](configuration_reference.md#domains---grid-and-domain-configuration)

## 📞 **Getting Help**

### Documentation Issues
If you find broken links, missing information, or unclear instructions:
1. Check this documentation index for alternative pages
2. Review the [Getting Started Guide](getting_started.md) for basic troubleshooting
3. Consult the [Detailed Workflows](detailed_workflows.md) for comprehensive information

### Technical Support
- Check log files in `scripts/LOG/` for error messages
- Review configuration with [Configuration Reference](configuration_reference.md)
- Validate setup using guides in [Getting Started](getting_started.md)

---

**Quick Navigation**: [⬆️ Back to Top](#hurricane-centric-setup-tools---documentation-index) | [📖 Main README](../README.md)
