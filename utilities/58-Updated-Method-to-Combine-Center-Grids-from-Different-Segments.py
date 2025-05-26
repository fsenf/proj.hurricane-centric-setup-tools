#!/usr/bin/env python
# coding: utf-8

# # Method to Combine Grids from Different Segments

# ## Libs

# In[1]:


import os, sys, glob
import pylab as plt
import numpy as np
import numpy

import seaborn as sns
sns.set_context('talk')


import xarray as xr
xr.set_options(keep_attrs=True)


# drawing onto a map
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import cartopy.io.shapereader as shpreader


from scipy.spatial import KDTree


SCRATCH=os.environ['SCRATCH']


# ## Arguments

# In[2]:


if 'ipykernel_launcher' in sys.argv[0]:
    interactive = True
    seg = 4
    dom = 3
else:
    interactive = False
    seg = int( sys.argv[1] )
    dom = int( sys.argv[2] )


# ## Derived Props

# In[3]:


from_seg = seg - 1
to_seg = seg
from_dom = dom
to_dom = dom


# In[4]:


interpolated_segment_file = f'{SCRATCH}/icontools/ifces2-atlanLEM-segment{from_seg}-exp105_202009??T000000Z_dom{dom}-to-dom{dom}_warmini.nc'
interpolated_background_file = f'{SCRATCH}/icontools/ifces2-atlanXL-exp021_202009??T000000Z_ref-to-segment{seg}-dom{dom}_warmini_L150.nc'

interpolated_segment_file = glob.glob( interpolated_segment_file )[0]
interpolated_background_file = glob.glob( interpolated_background_file )[0]


# In[5]:


time_str = interpolated_background_file.split('_')[1]


# ## Data Input

# In[6]:


def dim_rename( in_bg_data, names = {'height':'height_2', 'height_2':'height'}):

    ks = list( names.keys() )
    k1 = ks[0]
    l1 = names[k1]

    k2 = ks[1]
    l2 = names[k2]
    
    dtemp = in_bg_data.rename_dims({k1:'htemp'}).rename_vars({k1:'htemp'})
    dtemp2 = dtemp.rename_dims({k2:l2}).rename_vars({k2:l2})
    
    return dtemp2.rename_dims({'htemp':l1}).rename_vars({'htemp':l1})


# ### Interpolated Data

# In[7]:


in_seg_data = xr.open_dataset( interpolated_segment_file, chunks = 'auto' )
in_bg_data = xr.open_dataset( interpolated_background_file, chunks = 'auto' )

# dimension swap
try:
    in_bg_data = dim_rename( in_bg_data, )
except:
    in_bg_data = dim_rename( in_bg_data, names = {'height_2':'height', 'height_2_2':'height_2'})
#dtemp = in_bg_data.rename_dims({'height':'htemp'}).rename_vars({'height':'htemp'})
#dtemp2 = dtemp.rename_dims({'height_2':'height'}).rename_vars({'height_2':'height'})
#in_bg_data = dtemp2.rename_dims({'htemp':'height_2'}).rename_vars({'htemp':'height_2'})


# In[8]:


h_fulllev =  np.arange( 1, len( in_seg_data.height) +1 )
h_halflev =  np.arange( 1, len( in_seg_data.height_2) +1 )

in_seg_data = in_seg_data.assign_coords({'height': h_fulllev, 'height_2': h_halflev})
in_bg_data = in_bg_data.assign_coords({'height': h_fulllev, 'height_2': h_halflev})


# In[10]:


if 'vn' in in_seg_data:
    del in_seg_data['vn']

if 'vn' in in_bg_data:
    del in_bg_data['vn']


# In[11]:


if interactive:
    for vname in in_bg_data.data_vars:
        print( in_bg_data[[vname]].data_vars )
        try:
            print( in_seg_data[[vname]].data_vars )
        except:
            print('not in in_seg_data')
        print()


# ### Grids

# In[12]:


def read_grid( grid_name, rad2deg = True, lonname = 'clon', latname = 'clat', return_mask = False ):

    grid = xr.open_dataset( grid_name )

    if rad2deg:
        lon = np.rad2deg( grid[lonname] )
        lat = np.rad2deg( grid[latname] )
    else:
        lon = grid[lonname]
        lat = grid[latname]

    if return_mask:
        if lonname == 'clon':
            v = grid['refin_c_ctrl']
        elif lonname == 'elon':
            v = grid['refin_e_ctrl']
        mask = (v == 0) | ( v > 8 )

        return lon, lat, mask
    else:
        return lon, lat


# ###  Segment Grids

# In[13]:


grid_dir = '/work/bb1376/data/icon/grids-extpar/paulette-segments/'

seg_grids = {}

for iseg in range(1,8):
    seg_grids[iseg ] = {}
    
    for dom_number in [1,2,3,]:

        domname = f'DOM0{dom_number}'
        icon_grid_file = f'{grid_dir}/seg{iseg}_width100km/paulette-seg{iseg}_dom{dom_number}_DOM01.nc'
        
        seg_grids[iseg][dom_number] = read_grid( icon_grid_file, rad2deg = False, return_mask=True )  # return as     lon_boundary, lat_boundary


# ## Method

# General Idea
# 
# 1. Map seg1 DOM0x --> seg2 DOM0y
# 2. Calculate dist of each seg2 DOM0y point to the nearest seg1 DOM0x point

# In[14]:


def lonlat2xyz(lam, phi):
    cosphi = np.cos(phi)
    a_E = 6.371e3  # in kilometer
    return a_E *cosphi * np.cos(lam), a_E * cosphi * np.sin(lam), a_E * np.sin(phi)


# In[15]:


# distance from search tree
def measure_distance(in_lam, in_phi, out_lam, out_phi):
    xyz = np.c_[ lonlat2xyz(in_lam.data.ravel(),in_phi.data.ravel())]
    tree = KDTree(xyz)

    gxyz = np.c_[ lonlat2xyz(out_lam.data.flatten(),out_phi.data.flatten())]
    dd, ii = tree.query(gxyz, k=1)

    return dd, ii


# In[16]:


def calculate_total_weights( in_lam, in_phi, out_lam, out_phi, scale = 20., add_mask = None, in2out_transform4mask = True ):
    
    # find nn indices and distances between segments
    dd,ii =  measure_distance(in_lam, in_phi, out_lam, out_phi)

    if add_mask is not None:
        if in2out_transform4mask:
            add_mask = add_mask[ii]
            
    # define part that overlapp
    max_distance_for_overlapp = 0.5
    inner_mask = (dd <= max_distance_for_overlapp)
    if add_mask is not None:
        inner_mask = np.logical_and( inner_mask, add_mask )
    outer_mask = np.logical_not( inner_mask )
    
    # 2nd distance measure between overlapping an dnon-overlapping part
    dist2outm, index2out = measure_distance( out_lam[outer_mask], out_phi[outer_mask], out_lam[inner_mask], out_phi[inner_mask])

    # define a weight that decays linearily in the overlapping part from the edge towards the inner part 
    
    w = 1. - dist2outm / scale
    w[w<0.05] = 0.

    # combine everything to total weights
    total_weights = np.ones_like( out_lam )
    total_weights[inner_mask] = w

    return total_weights


# In[17]:


out_lam, out_phi, out_mask = seg_grids[to_seg][to_dom]
in_lam, in_phi, in_mask = seg_grids[from_seg][from_dom]

total_weights = calculate_total_weights( in_lam, in_phi, out_lam, out_phi, add_mask = in_mask )


# In[18]:


if interactive:
    get_ipython().run_line_magic('matplotlib', 'inline')
    plt.scatter(out_lam, out_phi, c = total_weights, s = 1 )


# ## Final Combination

# In[19]:


wc = xr.zeros_like( in_seg_data['clon'] )
wc = wc.rename('total_weights')
wc[:] = total_weights
wc = wc.astype(np.float32)


# In[20]:


del in_bg_data['z_mc']


# In[21]:


z_ifc = in_bg_data['z_ifc']
del in_bg_data['z_ifc']


# In[22]:


combined_ic_data = xr.zeros_like( in_bg_data )

for vname in in_bg_data.data_vars:

    v1 = in_bg_data[vname]

    if 'bnds' in vname:
        combined_ic_data[vname] = v1
    
    dims = v1.dims
    if 'vertices' in dims or 'vertices_2' in dims:
        print(f'...{vname}; vertices in dims')
        continue
    elif 'ncells' in dims:
        w = wc
        print(f'...{vname}; interpolated with center weights')
    elif 'ncells_2' in dims:
        print(f'...{vname}; ncells_2 in dims')
        continue
    else:
        print(f'...{vname};no criterion fullfilled')
        continue
    v2 = in_seg_data[vname]
    combined_ic_data[vname] = v1*w + v2*(1 - w)



# In[23]:


if interactive:
    in_bg_data


# In[24]:


combined_ic_data['z_ifc'] = z_ifc


# In[25]:


if interactive:
    for vname in in_bg_data.data_vars:
        print( in_bg_data[[vname]].data_vars )
        try:
            print( combined_ic_data[[vname]].data_vars )
        except:
            print('not in in_seg_data')
        print()


# ## Final Output

# In[27]:


#outname = f'~/scratch/icontools/combined_seg{seg}_dom{dom}.nc'

outdir = f'/work/bb1376/data/icon/bc-init/paulette-segments/seg{seg}_width100km/'
outname = f'{outdir}/lam_input_IC_ML_{time_str}_warmini-novn_dom{dom}.nc'


# In[28]:


delayed = combined_ic_data.to_netcdf(outname , compute = False)


# In[29]:


from dask.diagnostics import ProgressBar

print(f'... write to output file {outname}')
with ProgressBar():
    results = delayed.compute()

