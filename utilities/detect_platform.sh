#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/utilities/detect_platform.sh
#=============================================================================
# DESCRIPTION:
#   Platform detection script based on hostname patterns
#   Returns the detected platform name for use in configuration loading
#
# USAGE:
#   platform=$(detect_platform)
#   source "config/platforms/${platform}/platform.conf"
#
# SUPPORTED PLATFORMS:
#   - levante (DKRZ)
#   - juwels (JSC)
#   - generic (fallback)
#
# EXIT CODES:
#   0 - Success, platform detected
#   1 - Unknown platform, fallback to generic
#=============================================================================

detect_platform() {
    local hostname=$(hostname)
    local platform="generic"  # Default fallback
    
    # Check hostname patterns for known platforms
    case "$hostname" in
        *levante* | levante* | *dkrz* | *zmaw*)
            platform="levante"
            ;;
        *juwels* | juwels* | *jsc* | *fz-juelich*)
            platform="juwels"
            ;;
        *)
            platform="generic"
            ;;
    esac
    
    echo "$platform"
}

# If script is called directly (not sourced), run detection
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    detect_platform
fi
