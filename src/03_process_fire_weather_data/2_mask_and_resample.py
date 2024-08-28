"""

Apply dataset specific masks, and convert data to tifs.
Use GDAL to resample tifs to common 0.1deg spatial resolution.

NOTE: this script was run on linux, GDAL throws an error if run on windows.

"""

import sys
import numpy as np
import pandas as pd
import netCDF4 as nc
from pathlib import Path

#custom
sys.path.append('./../../lib')
import paths as paths
import utils as utils

# OPTIONS ------------------------
#mpl.use('Agg')  # prevent figures popping up as interactive plots - useful when running from terminal
#mpl.use('Qt5Agg')
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)
#gdal.UseExceptions()

# directories to preprocessed netcdf data, first set of tifs, resampled tifs
dir_base = Path(paths.dir_main)
dir_raw = dir_base / "03_fire_weather/01_preprocessed/"
dir_tif = dir_base / "03_fire_weather/02_tif/"
dir_resamp = dir_base / "03_fire_weather/03_resampled/"
# -------------------------------

# the min/max data files are masked (NaNs) but annual FWSL files do not. 
# Need to load the max files to to mask the annual files
# NOTE!: the mask for the geff indices are not the same for each fire weather index!
fp = dir_raw / 'fwi_cpc_maximum.nc'
fh = nc.Dataset(fp)
cpc_max = fh.variables['var1'][0, 0, :, :]
fp = dir_raw / 'fwi_merra2corrected_maximum.nc'
fh = nc.Dataset(fp)
merra_max = fh.variables['var1'][0, 0, :, :]
fp = dir_raw / 'geff_fwi_maximum.nc'
fh = nc.Dataset(fp)
geff_fwi_max = fh.variables['var1'][0, 0, :, :]
fp = dir_raw / 'geff_fdi_maximum.nc'
fh = nc.Dataset(fp)
geff_fdi_max = fh.variables['var1'][0, 0, :, :]
fp = dir_raw / 'geff_bi_maximum.nc'
fh = nc.Dataset(fp)
geff_bi_max = fh.variables['var1'][0, 0, :, :]



## convert .nc to .tif files, and apply appropriate mask based on the max arrays
fp_list = list(Path.glob(dir_raw, '*.nc'))

for i, fp in enumerate(fp_list):
    fn_out = str(fp).split('\\')[-1].split('.')[0] + '.tif'
    print(fn_out)
    fp_out = dir_tif / fn_out
    print(fp_out)
    
    # get data
    fh = nc.Dataset(fp)
    arr = fh.variables['var1'][0, 0, :, :]
    lat = fh.variables['lat'][:]
    lon = fh.variables['lon'][:]
    fh.close()
    
    height = len(lat)
    width = len(lon)
    hsize = abs(lon[0]-lon[1])
    vsize = abs(lat[0]-lat[1])
    
    # apply appropriate mask
    if 'cpc' in fp:
        arr[np.isnan(cpc_max)] = np.nan
    elif 'merra' in fp:
        arr[np.isnan(merra_max)] = np.nan
    elif 'geff_fwi' in fp:
        arr[np.isnan(geff_fwi_max)] = np.nan
    elif 'geff_fdi' in fp:
        arr[np.isnan(geff_fdi_max)] = np.nan
    elif 'geff_bi' in fp:
        arr[np.isnan(geff_bi_max)] = np.nan
    else:
        print('file name error?')
        sys.exit(1)
    
    # output to tif
    # note, transform has gdal element ordering
    if 'geff' in fp:
        trans=(0, hsize, 0, 90.0, 0, -vsize) 
    else:
        # GFWED       
        trans=(-180.0, hsize, 0, 75.0, 0, -vsize) 
        print('gfwed', trans)       
        # raw files are stored 'upside down'. Need to reflip to N aligned to export as tif
        arr=np.flipud(arr)
        
    utils.nparray2raster(arr=arr,
                   fp=fp_out, 
                   trans_type='gdal', 
                   trans=trans, 
                   arr_rows=height, 
                   arr_cols=width, 
                   driver='GTiff', count=1, dtype='float64',
                   compress='lzw', predictor=3, nodata=np.nan, tile=False, crs='EPSG:4326')    
print('Export to tif done')


# resample to tif files to a common (0.1deg) spatial resolution.
# NOTE: this also shifts the projection of the GEFF data from [0,360] to [-180,180]
fp_list = list(Path.glob(dir_tif, '*.tif'))

for i, fp in enumerate(fp_list):
    fn_out = str(fp).split('\\')[-1].split('.')[0] + 'resampled_0p1deg.tif'
    fp_out = dir_resamp / fn_out
    print(fn_out) 
    print(fp_out) 
    utils.gdalwarpcompress_v1(fp_in=fp, 
                            fp_out=fp_out,
                            target_extent=' '.join([str(x) for x in [-180.0, -90.0, 180.0, 90.0]]), 
                            target_res=' '.join([str(x) for x in [0.1, -0.1]]), 
                            proj='EPSG:4326', 
                            dtype='float')    

print('Resample done')

print('finished')