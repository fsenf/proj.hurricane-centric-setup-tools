#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/test/test_toml_reader.sh
#=============================================================================
# Test script for TOML reader debugging
#=============================================================================

#set -eux  # Enable debugging and exit on error

CONFIG_FILE=$1

# Get script directory
ORIGINAL_SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
echo "Script directory: ${ORIGINAL_SCRIPT_DIR}"

# Load TOML reader
echo "Loading TOML reader..."
source "${ORIGINAL_SCRIPT_DIR}/../utilities/toml_reader.sh"

# Read configuration with debugging
echo "Reading TOML configuration..."
CONFIG_FILE="${ORIGINAL_SCRIPT_DIR}/$CONFIG_FILE"
read_toml_config "$CONFIG_FILE"

echo ""
echo "=== TOML Reader Results ==="
echo "All variables starting with OUTPUT_:"
env | grep "^OUTPUT_" | sort || echo "No OUTPUT_ variables found"

echo ""
echo "All variables starting with PROJECT_:"
env | grep "^PROJECT_" | sort || echo "No PROJECT_ variables found"

echo ""
echo "All variables starting with TOOLS_:"
env | grep "^TOOLS_" | sort || echo "No TOOLS_ variables found"

echo ""
echo "Checking specific variables:"
echo "OUTPUT_ICBC_BASEDIR='${OUTPUT_ICBC_BASEDIR:-NOT_SET}'"
echo "OUTPUT_GRID_BASEDIR='${OUTPUT_GRID_BASEDIR:-NOT_SET}'"
echo "PROJECT_NAME='${PROJECT_NAME:-NOT_SET}'"

echo ""
echo "=== Test Complete ==="