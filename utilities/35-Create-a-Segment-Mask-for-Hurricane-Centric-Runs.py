#%matplotlib inline

import os, sys, glob
import pylab as plt
import numpy as np
import scipy.ndimage

import datetime
import xarray as xr
xr.set_options(keep_attrs=True)

from scipy.spatial import KDTree




if 'launcher' in sys.argv[0]:
    isegment = 5
    idom = 1
    interactive = True

else:
    isegment = int( sys.argv[1] )
    idom = int( sys.argv[2] )
    interactive = False

expname = 'ifces2-atlanXL-20200907-exp021'

init_str = expname.split('-')[2]
init_time = datetime.datetime.strptime( init_str, '%Y%m%d')

# dom_width = [1.4e3, 1.2e3, 1e3] # in km
dom_width = [60., 40., 20.] # in km
width_config = 'width20km'
project_subpath = 'paulette-segments' 

segment_reinit_hours = 6.
segment_length_added = 3.
segment_length_hours = segment_reinit_hours + 2*segment_length_added

segment_start_time = init_time + datetime.timedelta( hours = isegment * segment_reinit_hours - segment_length_added )
segment_end_time = segment_start_time + datetime.timedelta( hours = segment_length_hours )

ndom = len( dom_width )

if idom == 1:
    geofile = '/work/bb1376/data/icon/grids-extpar/atlanXL/atlanXL_R02B10_DOM02.nc'
elif idom == 2:
    geofile = f'/work/bb1376/data/icon/grids-extpar/{project_subpath}/seg{isegment}_{width_config}/paulette-seg{isegment}_dom1_DOM01.nc'
elif idom == 3:
    geofile = f'/work/bb1376/data/icon/grids-extpar/{project_subpath}/seg{isegment}_{width_config}/paulette-seg{isegment}_dom2_DOM01.nc'
    
geo = xr.open_dataset(geofile)

track_dir = '/work/bb1376/user/fabian/data/icon/paulette-statistics/'

trackfile = f'{track_dir}/{expname}-DOM02_paulette_best-fit.nc'


paulette_full = xr.open_dataset( trackfile ).swap_dims({'index':'time'})

paulette = paulette_full.sel( time = slice( segment_start_time, segment_end_time ))

paulette['phi'] = np.deg2rad( paulette['latitude'] )
paulette['lam'] = np.deg2rad( paulette['longitude'] )

fig, axs = plt.subplots( ncols = 2, nrows = 2, figsize = (14,8))
axs = axs.flatten()
paulette['longitude'].plot( ax = axs[0])
paulette['latitude'].plot( ax = axs[1])
paulette.plot.scatter( x = 'longitude', y = 'latitude', hue = 'time', add_colorbar = False, ax = axs[2], add_legend=False)

if interactive:
    plt.show()
else:
    plt.close()

def lonlat2xyz(lam, phi):
    cosphi = np.cos(phi)
    a_E = 6.371e3  # in kilometer
    return a_E *cosphi * np.cos(lam), a_E * cosphi * np.sin(lam), a_E * np.sin(phi)

# setup search tree
xyz = np.c_[ lonlat2xyz(paulette.lam.data.ravel(),paulette.phi.data.ravel())]
tree = KDTree(xyz)

# find nn indices and distances
gxyz = np.c_[ lonlat2xyz(geo.clon.data.flatten(),geo.clat.data.flatten())]

dd, ii = tree.query(gxyz, k=1)


distance = xr.zeros_like( geo.clon )
distance[:] = dd

distance.attrs['units'] = 'km'
distance.attrs['long_name'] = 'distance to track'


segment = xr.Dataset()

w = dom_width[idom-1]
segment[f'mask'] = xr.where( distance < w, 1, 0)

data_path = f'/work/bb1376/data/icon/grids-extpar/{project_subpath}/seg{isegment}_{width_config}'

if not os.path.exists(data_path):
    os.makedirs(data_path)

outname = f'{data_path}/paulette_segment_mask_{expname}_seg{isegment}_dom{idom}.nc'

print(f'...writing {outname}')
segment.to_netcdf(outname )
