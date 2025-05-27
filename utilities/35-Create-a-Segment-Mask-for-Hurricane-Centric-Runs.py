#!/usr/bin/env python3
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/utilities/35-Create-a-Segment-Mask-for-Hurricane-Centric-Runs.py
"""
Create segment masks for hurricane-centric runs.

Usage:
    python 35-Create-a-Segment-Mask-for-Hurricane-Centric-Runs.py <segment_num> <domain_num> <config_file>
"""

import os
import sys
import datetime
import numpy as np
import xarray as xr
import matplotlib.pyplot as plt
from scipy.spatial import KDTree

try:
    import tomllib  # Python 3.11+
except ImportError:
    try:
        import tomli as tomllib  # fallback for older Python versions
    except ImportError:
        print("Error: Please install tomli: pip install tomli")
        sys.exit(1)

xr.set_options(keep_attrs=True)


def load_config(config_file):
    """Load configuration from TOML file."""
    with open(config_file, 'rb') as f:
        return tomllib.load(f)


def lonlat2xyz(lam, phi):
    """Convert longitude/latitude to cartesian coordinates."""
    cosphi = np.cos(phi)
    a_E = 6.371e3  # in kilometer
    return a_E * cosphi * np.cos(lam), a_E * cosphi * np.sin(lam), a_E * np.sin(phi)


def main():
    # Parse command line arguments
    if 'launcher' in sys.argv[0] or len(sys.argv) < 4:
        isegment = 5
        idom = 1
        config_file = "../config/hurricane_config.toml"
        interactive = True
    else:
        isegment = int(sys.argv[1])
        idom = int(sys.argv[2])
        config_file = sys.argv[3]
        interactive = False

    # Load configuration
    config = load_config(config_file)
    
    # Extract configuration values - updated to match new TOML structure
    project_name = config['project']['name']
    width_config = config['project']['width_config']
    expname = config['reference']['expname']
    segment_reinit_hours = config['domains']['segment_reinit_hours']
    segment_length_added = config['domains']['segment_length_added']
    dom_width = config['domains']['dom_width']
    output_base = config['output']['grid_basedir']
    track_dir = config['track']['track_dir']
    track_file = config['track']['track_file']
    input_grid = config['reference']['input_grid']

    # Calculate segment timing
    init_str = expname.split('-')[2]
    init_time = datetime.datetime.strptime(init_str, '%Y%m%d')
    
    segment_length_hours = segment_reinit_hours + 2 * segment_length_added
    segment_start_time = init_time + datetime.timedelta(
        hours=isegment * segment_reinit_hours - segment_length_added
    )
    segment_end_time = segment_start_time + datetime.timedelta(hours=segment_length_hours)

    # Determine grid file based on domain
    if idom == 1:
        geofile = input_grid  # Use input_grid instead of base_grid
    elif idom == 2:
        geofile = f'{output_base}/{project_name}/seg{isegment}_{width_config}/paulette-seg{isegment}_dom1_DOM01.nc'
    elif idom == 3:
        geofile = f'{output_base}/{project_name}/seg{isegment}_{width_config}/paulette-seg{isegment}_dom2_DOM01.nc'
    else:
        raise ValueError(f"Invalid domain number: {idom}")

    # Load geometry data
    geo = xr.open_dataset(geofile)

    # Load track data - use track_file directly and rename local variable
    full_track_filename = f'{track_dir}/{track_file}'
    paulette_full = xr.open_dataset(full_track_filename).swap_dims({'index': 'time'})
    paulette = paulette_full.sel(time=slice(segment_start_time, segment_end_time))

    # Convert to radians
    paulette['phi'] = np.deg2rad(paulette['latitude'])
    paulette['lam'] = np.deg2rad(paulette['longitude'])

    # Create diagnostic plots
    if interactive:
        fig, axs = plt.subplots(ncols=2, nrows=2, figsize=(14, 8))
        axs = axs.flatten()
        paulette['longitude'].plot(ax=axs[0])
        paulette['latitude'].plot(ax=axs[1])
        paulette.plot.scatter(x='longitude', y='latitude', hue='time', 
                            add_colorbar=False, ax=axs[2], add_legend=False)
        plt.show()
    else:
        plt.ioff()

    # Setup search tree for nearest neighbor search
    xyz = np.c_[lonlat2xyz(paulette.lam.data.ravel(), paulette.phi.data.ravel())]
    tree = KDTree(xyz)

    # Find nearest neighbor indices and distances
    gxyz = np.c_[lonlat2xyz(geo.clon.data.flatten(), geo.clat.data.flatten())]
    dd, ii = tree.query(gxyz, k=1)

    # Create distance field
    distance = xr.zeros_like(geo.clon)
    distance[:] = dd
    distance.attrs['units'] = 'km'
    distance.attrs['long_name'] = 'distance to track'

    # Create segment mask
    segment = xr.Dataset()
    w = dom_width[idom - 1]
    segment['mask'] = xr.where(distance < w, 1, 0)

    # Ensure output directory exists
    data_path = f'{output_base}/{project_name}/seg{isegment}_{width_config}'
    if not os.path.exists(data_path):
        os.makedirs(data_path)

    # Write output file
    outname = f'{data_path}/paulette_segment_mask_{expname}_seg{isegment}_dom{idom}.nc'
    print(f'...writing segment mask to {outname}')
    segment.to_netcdf(outname)


if __name__ == "__main__":
    main()
