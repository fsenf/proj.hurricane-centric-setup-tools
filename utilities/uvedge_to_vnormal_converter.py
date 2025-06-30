#!/usr/bin/env python3

import sys, os

import xarray as xr

xr.set_options(keep_attrs=True)

# Fix the incomplete import and add progress bar
import dask
from dask.diagnostics import ProgressBar

# input arguments
# ==============
grid_file = sys.argv[1]
uvedge_file = sys.argv[2]

# open datasets
# ==============
grid = xr.open_dataset(grid_file)
uvedge = xr.open_dataset(uvedge_file)

# extract variables
# ==============
# components of normal edge vectors
en_x = grid["zonal_normal_primal_edge"]
en_y = grid["meridional_normal_primal_edge"]

# velocity components
u = uvedge["u"]
v = uvedge["v"]

# projection
# ==============
out = xr.Dataset()
out['vn'] = u * en_x + v * en_y

# output
# ==============
delayed = out.to_netcdf(
    uvedge_file.replace("uvedge", "vnormal"), mode="w", compute=False
)

with ProgressBar():
    dask.compute(delayed)
# close datasets
# ==============
grid.close()
uvedge.close()
