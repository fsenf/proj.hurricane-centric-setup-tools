#!/bin/bash
# Example usage in a bash script

module load python3

# go back to the project root directory
cd "$(dirname "$0")/.."

CONFIG_FILE="config/hurricane_config.toml"
ISEG=$1

# Find IC file (single file)
IC_FILE=$(python utilities/find_icbc_file.py "$CONFIG_FILE" "$ISEG" "IC")
if [ $? -eq 0 ] && [ -n "$IC_FILE" ]; then
    echo "Found IC file: $IC_FILE"
    export INFILE="$IC_FILE"
else
    echo "Error: IC file not found for segment $ISEG"
    exit 1
fi

# Find BC files (multiple files)
BC_FILES=($(python utilities/find_icbc_file.py "$CONFIG_FILE" "$ISEG" "BC"))
if [ $? -eq 0 ] && [ ${#BC_FILES[@]} -gt 0 ]; then
    echo "Found ${#BC_FILES[@]} BC files for segment $ISEG:"
    for bc_file in "${BC_FILES[@]}"; do
        echo "  $bc_file"
    done
    export DATAFILELIST="${BC_FILES[*]}"  # Space-separated list
else
    echo "Error: No BC files found for segment $ISEG"
    exit 1
fi

# Alternative: iterate over BC files
while IFS= read -r bc_file; do
    [ -n "$bc_file" ] && echo "Processing BC file: $bc_file"
done < <(python utilities/find_icbc_file.py "$CONFIG_FILE" "$ISEG" "BC")
