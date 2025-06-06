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
import os
import glob
from datetime import datetime, timedelta

try:
    import tomllib  # Python 3.11+
except ImportError:
    try:
        import tomli as tomllib  # fallback for older Python versions
    except ImportError:
        print("Error: Please install tomli: pip install tomli", file=sys.stderr)
        sys.exit(1)


def load_config(config_file):
    """Load configuration from TOML file."""
    with open(config_file, 'rb') as f:
        return tomllib.load(f)


def find_icbc_file(config_file, iseg, file_type):
    """
    Find IC/BC file(s) for given segment.
    
    Args:
        config_file: Path to TOML config file
        iseg: Segment number
        file_type: 'IC' or 'BC'
    
    Returns:
        For IC: Path to existing file or empty string if not found
        For BC: List of all BC files in segment time range
    """
    # Load configuration
    config = load_config(config_file)
    
    # Extract values from config
    init_time_str = config['reference']['init_time']
    segment_reinit_hours = config['domains']['segment_reinit_hours']
    input_dir = config['reference']['input_icbc_dir']
    subdir = config['reference']['input_icbc_subdir']
    
    # Calculate times
    init_time = datetime.strptime(init_time_str, '%Y%m%dT%H%M00Z')   # e.g. "20200907T000000Z"
    
    # Construct search directory
    search_dir = os.path.join(input_dir, subdir)
    
    if file_type.upper() == 'IC':
        # For IC: exact match at segment start time
        target_time = init_time + timedelta(hours=int(iseg * segment_reinit_hours))
        target_time_str = target_time.strftime('%Y%m%dT%H%M00Z')
        filename = f"lam_input_IC_DOM02_ML_{target_time_str}.nc"
        full_path = os.path.join(search_dir, filename)
        
        if os.path.isfile(full_path):
            return [full_path]
        else:
            print(f"Error: IC file not found: {full_path}", file=sys.stderr)
            return []
    
    elif file_type.upper() == 'BC':
        # For BC: all files in segment time range
        segment_start_time = init_time + timedelta(hours=int(iseg * segment_reinit_hours))
        segment_end_time = init_time + timedelta(hours=int((iseg + 1) * segment_reinit_hours))
        
        # Find all BC files in the directory
        bc_pattern = os.path.join(search_dir, "lam_input_BC_DOM02_ML_*.nc")
        all_bc_files = glob.glob(bc_pattern)
        
        # Filter files within time range
        matching_files = []
        for file_path in all_bc_files:
            filename = os.path.basename(file_path)
            # Extract timestamp from filename: lam_input_BC_DOM02_ML_YYYYMMDDTHHMMMMZ.nc
            try:
                time_part = filename.split('_')[5].replace('.nc', '')  # Get YYYYMMDDTHHMMMMZ part
                file_time = datetime.strptime(time_part, '%Y%m%dT%H%M00Z')
                
                # Check if file time is within segment range
                if segment_start_time <= file_time <= segment_end_time:
                    matching_files.append(file_path)
            except (IndexError, ValueError) as e:
                # Skip files that don't match expected naming pattern
                continue
        
        # Sort files by timestamp
        matching_files.sort()
        
        if matching_files:
            return matching_files
        else:
            print(f"Error: No BC files found in time range {segment_start_time} to {segment_end_time}", file=sys.stderr)
            return []
    
    else:
        print(f"Error: Unknown file_type '{file_type}'. Use 'IC' or 'BC'", file=sys.stderr)
        return []


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