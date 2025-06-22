#!/usr/bin/env python3
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/utilities/find_icbc_file.py
"""
Find IC/BC file(s) based on segment number and configuration.

Usage:
    python find_icbc_file.py <config_file> <segment_number> <file_type>
    
Arguments:
    config_file: Path to hurricane_config.toml
    segment_number: Segment number (iseg)
    file_type: Either 'IC' or 'BC'
    
For IC: Returns single file path
For BC: Returns all files in segment time range, one per line
"""

import sys
from helpers import find_icbc_file


def main():
    if len(sys.argv) != 4:
        print("Usage: python find_icbc_file.py <config_file> <segment_number> <file_type>", file=sys.stderr)
        print("file_type should be 'IC' or 'BC'", file=sys.stderr)
        sys.exit(1)
    
    config_file = sys.argv[1]
    try:
        iseg = int(sys.argv[2])
    except ValueError:
        print(f"Error: segment_number must be an integer, got '{sys.argv[2]}'", file=sys.stderr)
        sys.exit(1)
    
    file_type = sys.argv[3]
    
    # Find file(s)
    file_paths = find_icbc_file(config_file, iseg, file_type)
    
    if file_paths:
        # Print each file path on a separate line
        for file_path in file_paths:
            print(file_path)
    else:
        sys.exit(1)


if __name__ == "__main__":
    main()