#!/bin/bash
# filepath: utilities/common_inits.sh
#=============================================================================
# PLATFORM ENVIRONMENT SETUP
# Shared platform detection, module loading, and environment configuration
# for hurricane-centric setup tools
#=============================================================================

# Setup platform environment including detection, modules, and sbatch configuration
setup_platform_environment() {
    local script_type="${1:-default}"  # Accept type argument, default to 'default'
    
    # ORIGINAL_SCRIPT_DIR should already be set by calling script
    # Map to SCRIPT_DIR for compatibility with existing scripts
    SCRIPT_DIR=${ORIGINAL_SCRIPT_DIR}
    export SCRIPT_DIR
    echo "Script directory: ${ORIGINAL_SCRIPT_DIR}"
    
    # Detect platform and load platform-specific modules
    PLATFORM=$("${ORIGINAL_SCRIPT_DIR}/../../utilities/detect_platform.sh")
    echo "Detected platform: ${PLATFORM}"
    echo "Hostname: $(hostname)"
    
    # Load platform-specific modules
    module_loader_path="${ORIGINAL_SCRIPT_DIR}/../../config/${PLATFORM}/module_loader.sh"
    if [[ -f "$module_loader_path" ]]; then
        echo "Loading modules for platform: ${PLATFORM}"
        source "$module_loader_path"
    else
        echo "Warning: No module loader found for platform ${PLATFORM} at ${module_loader_path}"
    fi
    
    # Load platform-dependent sbatch environment setter
    sbatch_env_setter_path="${ORIGINAL_SCRIPT_DIR}/../../config/${PLATFORM}/sbatch_env_setter.sh"
    if [[ -f "$sbatch_env_setter_path" ]]; then
        echo "Loading sbatch environment setter for platform: ${PLATFORM}, type: ${script_type}"
        source "$sbatch_env_setter_path" "$script_type"
    else
        echo "Warning: No sbatch environment setter found for platform ${PLATFORM} at ${sbatch_env_setter_path}"
    fi
}