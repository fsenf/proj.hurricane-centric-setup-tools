#!/usr/bin/env python3
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/utilities/check_preprocessing_files.py

"""
Check if all required hurricane simulation preprocessing files exist and are valid.

Usage:
    check_preprocessing_files.py <config_file> <segment_number> <file_type>

Arguments:
    config_file:    Path to hurricane_config.toml
    segment_number: Segment number to check (required)
    file_type:      Type of files to check: "grid", "extpar", "bc", "ic"
"""

import os
import sys
import glob
import argparse
from datetime import datetime, timedelta
import xarray as xr
from helpers import load_config, print_timings


def check_netcdf(filepath):
    """Check if file exists and is a readable NetCDF file."""
    if not os.path.exists(filepath):
        return False, f"File does not exist: {filepath}"
    
    try:
        # Only open the file without loading data into memory
        with xr.open_dataset(filepath, engine='netcdf4') as ds:
            # Just accessing attributes is enough to verify the file is valid
            _ = ds.attrs
        return True, f"✓ Valid NetCDF: {filepath}"
    except Exception as e:
        return False, f"Error reading NetCDF: {filepath} - {str(e)}"


def check_grid_files(config, segment):
    """Check if all grid files exist and are valid."""
    valid_count = 0
    invalid_count = 0
    
    print(f"\n=== Checking grid files for segment {segment} ===")
    
    project_name = config['project']['name']
    width_config = config['project']['width_config']
    domains_nests = config['domains']['nests']
    grid_basedir = config['output']['grid_basedir']
    
    # Construct domain path
    domain_path = f"{project_name}/seg{segment}_{width_config}"
    grid_dir = os.path.join(grid_basedir, domain_path)
    
    # Check if grid directory exists
    if not os.path.exists(grid_dir):
        print(f"Grid directory not found: {grid_dir}")
        return 0, domains_nests
    
    # Check each domain's grid file
    for idom in range(1, domains_nests + 1):
        grid_file = os.path.join(grid_dir, f"{project_name}-seg{segment}_dom{idom}_DOM01.nc")
        valid, msg = check_netcdf(grid_file)
        print(msg)
        
        if valid:
            valid_count += 1
        else:
            invalid_count += 1
    
    return valid_count, invalid_count


def check_extpar_files(config, segment):
    """Check if all extpar files exist and are valid."""
    valid_count = 0
    invalid_count = 0
    
    print(f"\n=== Checking extpar files for segment {segment} ===")
    
    project_name = config['project']['name']
    width_config = config['project']['width_config']
    domains_nests = config['domains']['nests']
    grid_basedir = config['output']['grid_basedir']
    
    # Construct domain path
    domain_path = f"{project_name}/seg{segment}_{width_config}"
    grid_dir = os.path.join(grid_basedir, domain_path)
    
    # Check if grid directory exists
    if not os.path.exists(grid_dir):
        print(f"Grid directory not found: {grid_dir}")
        return 0, domains_nests
    
    # Check each domain's extpar file
    for idom in range(1, domains_nests + 1):
        extpar_file = os.path.join(grid_dir, f"extpar_{project_name}-seg{segment}_dom{idom}_DOM01_tiles.nc")
        valid, msg = check_netcdf(extpar_file)
        print(msg)
        
        if valid:
            valid_count += 1
        else:
            invalid_count += 1
    
    return valid_count, invalid_count


def check_bc_files(config, segment):
    """Check if all boundary condition files exist and are valid."""
    valid_count = 0
    invalid_count = 0
    
    print(f"\n=== Checking boundary condition files for segment {segment} ===")
    
    project_name = config['project']['name']
    width_config = config['project']['width_config']
    icbc_basedir = config['output']['icbc_basedir']
    init_time_str = config['reference']['init_time']
    segment_reinit_hours = config['domains']['segment_reinit_hours']
    
    # Calculate segment time range
    init_time = datetime.strptime(init_time_str, '%Y%m%dT%H%M00Z')
    segment_start = init_time + timedelta(hours=float(segment * segment_reinit_hours))
    segment_end = init_time + timedelta(hours=float((segment + 1) * segment_reinit_hours))
    
    # Construct domain path
    domain_path = f"{project_name}/seg{segment}_{width_config}"
    bc_dir = os.path.join(icbc_basedir, domain_path)
    
    # Check if BC directory exists
    if not os.path.exists(bc_dir):
        print(f"BC directory not found: {bc_dir}")
        return 0, 1
    
    # Find all BC files in the directory
    bc_files = sorted( glob.glob(os.path.join(bc_dir, "[0-9]*_lbc.nc")) )
    
    if not bc_files:
        print(f"No BC files found in {bc_dir}")
        return 0, 1
    
    # Check each BC file
    for bc_file in bc_files:
        valid, msg = check_netcdf(bc_file)
        print(msg)
        
        if valid:
            valid_count += 1
        else:
            invalid_count += 1
    
    return valid_count, invalid_count


def check_ic_files(config, segment, config_file_path=None):
    """Check if all initial condition files exist and are valid."""
    valid_count = 0
    invalid_count = 0
    
    print(f"\n=== Checking initial condition files for segment {segment} ===")
    
    project_name = config['project']['name']
    width_config = config['project']['width_config']
    domains_nests = config['domains']['nests']
    icbc_basedir = config['output']['icbc_basedir']
    
    # Construct domain path
    domain_path = f"{project_name}/seg{segment}_{width_config}"
    ic_dir = os.path.join(icbc_basedir, domain_path)
    
    # Check if IC directory exists
    if not os.path.exists(ic_dir):
        print(f"IC directory not found: {ic_dir}")
        return 0, domains_nests
    
    # Check each domain's IC file
    for idom in range(1, domains_nests + 1):
        # Get the timestamp for this segment
        timestamp = print_timings(config_file_path, segment, "INIT_DATE")
        # The pattern from updated icon2icon_offline_lam_ini.bash
        ic_pattern = f"{timestamp}_DOM0{idom}_ini.nc"
        ic_files = glob.glob(os.path.join(ic_dir, ic_pattern))
        
        if not ic_files:
            print(f"No IC files found matching {ic_pattern} in {ic_dir}")
            invalid_count += 1
            continue
        
        # Check the first matching file
        ic_file = ic_files[0]
        valid, msg = check_netcdf(ic_file)
        print(msg)
        
        if valid:
            valid_count += 1
        else:
            invalid_count += 1
    
    return valid_count, invalid_count


def main():
    parser = argparse.ArgumentParser(description="Check hurricane simulation preprocessing files")
    parser.add_argument("config_file", help="Path to hurricane_config.toml")
    parser.add_argument("segment", type=int, help="Segment number to check")
    parser.add_argument("file_type", choices=["grid", "extpar", "bc", "ic", "all"], 
                        help="Type of files to check: grid, extpar, bc, ic, or all")
    
    args = parser.parse_args()
    
    try:
        # Load configuration
        config = load_config(args.config_file)
        
        # Track totals across all checks
        total_valid = 0
        total_invalid = 0
        
        if args.file_type == "all":
            # Run all checks and accumulate results
            print("\n=== Running all file checks for segment", args.segment, "===")
            
            file_types = ["grid", "extpar", "bc", "ic"]
            for file_type in file_types:
                if file_type == "grid":
                    valid, invalid = check_grid_files(config, args.segment)
                elif file_type == "extpar":
                    valid, invalid = check_extpar_files(config, args.segment)
                elif file_type == "bc":
                    valid, invalid = check_bc_files(config, args.segment)
                elif file_type == "ic":
                    valid, invalid = check_ic_files(config, args.segment, args.config_file)
                
                # Accumulate totals
                total_valid += valid
                total_invalid += invalid
                
                # Print sub-summary for this check
                print(f"\n--- {file_type.upper()} Summary ---")
                print(f"Valid files:   {valid}")
                print(f"Invalid files: {invalid}")
            
            # Set overall valid/invalid for the final summary
            valid = total_valid
            invalid = total_invalid
        else:
            # Run single check as before
            if args.file_type == "grid":
                valid, invalid = check_grid_files(config, args.segment)
            elif args.file_type == "extpar":
                valid, invalid = check_extpar_files(config, args.segment)
            elif args.file_type == "bc":
                valid, invalid = check_bc_files(config, args.segment)
            elif args.file_type == "ic":
                valid, invalid = check_ic_files(config, args.segment, args.config_file)
        
        # Print final summary
        if args.file_type == "all":
            print(f"\n=== Overall Summary for ALL files (Segment {args.segment}) ===")
        else:
            print(f"\n=== Summary for {args.file_type.upper()} files (Segment {args.segment}) ===")
        
        print(f"Valid files:   {valid}")
        print(f"Invalid files: {invalid}")
        print(f"Total files:   {valid + invalid}")
        
        # Set exit code based on validity
        if invalid > 0:
            print("\nERROR: Some files are missing or invalid")
            sys.exit(1)
        else:
            print("\nSUCCESS: All files exist and are valid")
            sys.exit(0)
            
    except Exception as e:
        print(f"Error: {str(e)}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()