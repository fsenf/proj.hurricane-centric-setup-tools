#!/usr/bin/env python
# coding: utf-8


import os
import sys
import glob
import argparse
import numpy as np
import xarray as xr
from scipy.spatial import KDTree
from dask.diagnostics import ProgressBar

xr.set_options(keep_attrs=True)

#=============================================================================
# ARGUMENT PARSING
#=============================================================================

def parse_arguments():
    """Parse command line arguments"""
    parser = argparse.ArgumentParser(
        description="Combine center grids from different hurricane segments",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    
    # Required arguments
    parser.add_argument("segment_file", 
                        help="Path to interpolated segment file")
    parser.add_argument("background_file", 
                        help="Path to interpolated background file")
    parser.add_argument("from_grid", 
                        help="Path to source grid file")
    parser.add_argument("to_grid", 
                        help="Path to target grid file")
    parser.add_argument("output_file", 
                        help="Path to output file")
    
    # Optional arguments
    parser.add_argument("--scale", type=float, default=20.0,
                        help="Scaling factor for distance weighting")
    parser.add_argument("--max-distance", type=float, default=0.5,
                        help="Maximum distance for overlap definition")
    parser.add_argument("--min-weight", type=float, default=0.05,
                        help="Minimum weight threshold")
    
    # Processing options
    parser.add_argument("--chunk-size", type=str, default="auto",
                        help="Chunk size for dask processing")
    parser.add_argument("--verbose", "-v", action="store_true",
                        help="Enable verbose output")
    
    args = parser.parse_args()
    
    # Validate file existence
    for file_arg in [args.segment_file, args.background_file, args.from_grid, args.to_grid]:
        if not os.path.exists(file_arg):
            parser.error(f"File does not exist: {file_arg}")
    
    # Ensure output directory exists
    output_dir = os.path.dirname(args.output_file)
    if output_dir and not os.path.exists(output_dir):
        if args.verbose:
            print(f"Creating output directory: {output_dir}")
        os.makedirs(output_dir, exist_ok=True)
    
    return args

#=============================================================================
# FUNCTIONS
#=============================================================================

def dim_rename(in_bg_data, names={"height": "height_2", "height_2": "height"}):
    """Rename dimensions to handle inconsistencies in height variable names"""
    ks = list(names.keys())
    k1 = ks[0]
    l1 = names[k1]

    k2 = ks[1]
    l2 = names[k2]

    dtemp = in_bg_data.rename_dims({k1: "htemp"}).rename_vars({k1: "htemp"})
    dtemp2 = dtemp.rename_dims({k2: l2}).rename_vars({k2: l2})

    return dtemp2.rename_dims({"htemp": l1}).rename_vars({"htemp": l1})


def cell_rename( d, ):

    if 'cell' in d.dims:
        return d.rename_dims({'cell':'ncells'})
    else:
        return d


def read_grid(grid_name, rad2deg=True, lonname="clon", latname="clat", return_mask=False):
    """Read grid data and optionally return with mask"""
    grid = xr.open_dataset(grid_name)
    grid = cell_rename( grid )

    if rad2deg:
        lon = np.rad2deg(grid[lonname])
        lat = np.rad2deg(grid[latname])
    else:
        lon = grid[lonname]
        lat = grid[latname]

    if return_mask:
        if lonname == "clon":
            v = grid["refin_c_ctrl"]
        elif lonname == "elon":
            v = grid["refin_e_ctrl"]
        mask = (v == 0) | (v > 8)

        return lon, lat, mask
    else:
        return lon, lat


def lonlat2xyz(lam, phi):
    """Convert spherical to Cartesian coordinates"""
    cosphi = np.cos(phi)
    a_E = 6.371e3  # in kilometer
    return a_E * cosphi * np.cos(lam), a_E * cosphi * np.sin(lam), a_E * np.sin(phi)


def measure_distance(in_lam, in_phi, out_lam, out_phi):
    """Calculate distance between points using a KDTree"""
    xyz = np.c_[lonlat2xyz(in_lam.data.ravel(), in_phi.data.ravel())]
    tree = KDTree(xyz)

    gxyz = np.c_[lonlat2xyz(out_lam.data.flatten(), out_phi.data.flatten())]
    dd, ii = tree.query(gxyz, k=1)

    return dd, ii


def calculate_total_weights(
    in_lam,
    in_phi,
    out_lam,
    out_phi,
    scale=20.0,
    max_distance=0.5,
    min_weight=0.05,
    add_mask=None,
    in2out_transform4mask=True,
):
    """Calculate blending weights between two grids"""
    # Find nearest neighbor indices and distances between segments
    dd, ii = measure_distance(in_lam, in_phi, out_lam, out_phi)

    if add_mask is not None:
        if in2out_transform4mask:
            add_mask = add_mask[ii]

    # Define overlapping part
    inner_mask = dd <= max_distance
    if add_mask is not None:
        inner_mask = np.logical_and(inner_mask, add_mask)
    outer_mask = np.logical_not(inner_mask)

    # Second distance measure between overlapping and non-overlapping part
    dist2outm, index2out = measure_distance(
        out_lam[outer_mask],
        out_phi[outer_mask],
        out_lam[inner_mask],
        out_phi[inner_mask],
    )

    # Define a weight that decays linearly in the overlapping part from the edge towards the inner part
    w = 1.0 - dist2outm / scale
    w[w < min_weight] = 0.0

    # Combine everything to total weights
    total_weights = np.ones_like(out_lam)
    total_weights[inner_mask] = w

    return total_weights


def main():
    """Main execution function"""
    # Parse command line arguments
    args = parse_arguments()
    
    if args.verbose:
        print(f"Processing files:")
        print(f"  Segment file:     {args.segment_file}")
        print(f"  Background file:  {args.background_file}")
        print(f"  From grid file:   {args.from_grid}")
        print(f"  To grid file:     {args.to_grid}")
        print(f"  Output file:      {args.output_file}")
    
    # Load and prepare data
    if args.verbose:
        print("Loading input files...")
    
    in_seg_data = xr.open_dataset(args.segment_file, chunks=args.chunk_size)
    in_bg_data = xr.open_dataset(args.background_file, chunks=args.chunk_size)
    
    # Fix dimension naming inconsistencies
    try:
        in_bg_data = dim_rename(in_bg_data)
    except:
        try:
            in_bg_data = dim_rename(in_bg_data, names={"height_2": "height", "height_2_2": "height_2"})
        except:
            in_bg_data = dim_rename(in_bg_data, names={"lev": "height", "lev_2": "height_2"})
        
    in_seg_data = cell_rename( in_seg_data )

    # Assign level coordinates
    h_fulllev = np.arange(1, len(in_seg_data.height) + 1)
    h_halflev = np.arange(1, len(in_seg_data.height_2) + 1)
    
    in_seg_data = in_seg_data.assign_coords({"height": h_fulllev, "height_2": h_halflev})
    in_bg_data = in_bg_data.assign_coords({"height": h_fulllev, "height_2": h_halflev})
    
    # Remove normal velocity component if present
    if "vn" in in_seg_data:
        del in_seg_data["vn"]
    
    if "vn" in in_bg_data:
        del in_bg_data["vn"]
    
    # Calculate blending weights
    if args.verbose:
        print("Calculating blending weights...")
        
    out_lam, out_phi, out_mask = read_grid(args.to_grid, rad2deg=False, return_mask=True)
    in_lam, in_phi, in_mask = read_grid(args.from_grid, rad2deg=False, return_mask=True)
    
    total_weights = calculate_total_weights(
        in_lam, in_phi, out_lam, out_phi, 
        scale=args.scale,
        max_distance=args.max_distance,
        min_weight=args.min_weight,
        add_mask=in_mask
    )
    
    # Create weights dataset
    wc = xr.zeros_like(in_seg_data["clon"])
    wc = wc.rename("total_weights")
    wc[:] = total_weights
    wc = wc.astype(np.float32)
    
    # Prepare for final combination
    del in_bg_data["z_mc"]
    z_ifc = in_bg_data["z_ifc"]
    del in_bg_data["z_ifc"]
    
    # Create combined dataset
    if args.verbose:
        print("Blending variables...")
        
    combined_ic_data = xr.zeros_like(in_bg_data)
    
    # Blend variables
    for vname in in_bg_data.data_vars:
        v1 = in_bg_data[vname]
    
        if "bnds" in vname:
            combined_ic_data[vname] = v1
            continue
    
        dims = v1.dims
        if "vertices" in dims or "vertices_2" in dims:
            if args.verbose:
                print(f"...{vname}; vertices in dims")
            continue
        elif "ncells" in dims:
            w = wc
            if args.verbose:
                print(f"...{vname}; interpolated with center weights")
        elif "ncells_2" in dims:
            if args.verbose:
                print(f"...{vname}; ncells_2 in dims")
            continue
        else:
            if args.verbose:
                print(f"...{vname}; no criterion fulfilled")
            continue
    
        v2 = in_seg_data[vname]
        combined_ic_data[vname] = v1 * w + v2 * (1 - w)
    
    # Add z_ifc back
    combined_ic_data["z_ifc"] = z_ifc
    
    # Write output
    if args.verbose:
        print(f"Writing output to {args.output_file}")
        
    delayed = combined_ic_data.to_netcdf(args.output_file, compute=False)
    with ProgressBar():
        results = delayed.compute()
        
    if args.verbose:
        print("Processing complete!")


if __name__ == "__main__":
    main()
