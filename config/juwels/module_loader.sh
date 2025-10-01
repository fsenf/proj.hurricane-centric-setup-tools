#!/bin/bash

module purge

# Load necessary modules for Juwels
module load Stages/2025  GCC/13.3.0  OpenMPI/5.0.5
module load CDO 
module load SciPy-Stack netcdf4-python dask
