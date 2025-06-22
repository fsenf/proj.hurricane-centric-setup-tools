#!/usr/bin/env python3
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/utilities/extract_region_bounds.py
"""
Extract region bounds from ICON grid file.

This script extracts the minimum and maximum longitude and latitude values from
an ICON grid file and formats them as region definition strings for use in ICON
experiment scripts.

Usage:
    extract_region_bounds.py <grid_file>
"""

import sys
import numpy as np
import xarray as xr
from pathlib import Path

def extract_region_bounds(grid_file):
    """
    Extract longitude and latitude bounds from a grid file.
    
    Args:
        grid_file (str): Path to the netCDF grid file
        
    Returns:
        tuple: (reg_lon_def, reg_lat_def) strings
    """
    try:
        # Open the grid file
        ds = xr.open_dataset(grid_file)
        
        # Check if clon/clat exist, these are typically in radians
        if 'clon' in ds and 'clat' in ds:
            # Convert from radians to degrees
            lon_deg = np.rad2deg(ds.clon.values)
            lat_deg = np.rad2deg(ds.clat.values)
            
            # Get min and max values
            lon_min = np.nanmin(lon_deg)
            lon_max = np.nanmax(lon_deg)
            lat_min = np.nanmin(lat_deg)
            lat_max = np.nanmax(lat_deg)
            
            # Fixed step size
            dx = 0.01
            
            # Format strings
            reg_lon_def = f"{lon_min:.2f},{dx:.2f},{lon_max:.2f}"
            reg_lat_def = f"{lat_min:.2f},{dx:.2f},{lat_max:.2f}"
            
            return reg_lon_def, reg_lat_def
            
        else:
            # Check for lon/lat variables (might be in degrees already)
            lon_var = None
            lat_var = None
            
            for var in ['lon', 'longitude', 'nav_lon']:
                if var in ds:
                    lon_var = var
                    break
                    
            for var in ['lat', 'latitude', 'nav_lat']:
                if var in ds:
                    lat_var = var
                    break
            
            if lon_var and lat_var:
                # These might already be in degrees
                lon_deg = ds[lon_var].values
                lat_deg = ds[lat_var].values
                
                # Check if values look like radians (between -π and π)
                if np.nanmax(np.abs(lon_deg)) <= np.pi:
                    lon_deg = np.rad2deg(lon_deg)
                if np.nanmax(np.abs(lat_deg)) <= np.pi/2:
                    lat_deg = np.rad2deg(lat_deg)
                    
                # Get min and max values
                lon_min = np.nanmin(lon_deg)
                lon_max = np.nanmax(lon_deg)
                lat_min = np.nanmin(lat_deg)
                lat_max = np.nanmax(lat_deg)
                
                # Fixed step size
                dx = 0.01
                
                # Format strings
                reg_lon_def = f"{lon_min:.2f},{dx:.2f},{lon_max:.2f}"
                reg_lat_def = f"{lat_min:.2f},{dx:.2f},{lat_max:.2f}"
                
                return reg_lon_def, reg_lat_def
            else:
                raise ValueError(f"Could not find longitude/latitude variables in {grid_file}")
                
    except Exception as e:
        print(f"Error processing grid file: {e}", file=sys.stderr)
        sys.exit(1)

def main():
    # Parse command line arguments
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <grid_file>", file=sys.stderr)
        sys.exit(1)
        
    grid_file = sys.argv[1]
    
    # Check if file exists
    if not Path(grid_file).is_file():
        print(f"Error: Grid file not found: {grid_file}", file=sys.stderr)
        sys.exit(1)
    
    # Extract region bounds
    reg_lon_def, reg_lat_def = extract_region_bounds(grid_file)
    
    # Print formatted output
    grid_name = Path(grid_file).stem
    print(f"# This is the definition of {grid_name}")
    print(f'reg_lon_def="{reg_lon_def}"')
    print(f'reg_lat_def="{reg_lat_def}"')

if __name__ == "__main__":
    main()