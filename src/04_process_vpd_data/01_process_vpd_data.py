"""
Prepare VPD data for analysis

Prepare gridded data for mapped trend analysis:
# 1) generate 'raw' annual statistic grids for VPD from input higher temporal res data
# 2) convert netcdf files to .tifs
# 3) resample tif files to 0.1deg spatial resolution for consistancy with fire danger data
# 4) generate a multiyear raster stack for mean, median, and max VPD and export

Prepare GFED region and tropical/temporate/boreal aggregated data for regional timeseries analysis:
+ calculate grid cell area weighted avergage for region, and output a pandas dataframe to csv

"""

import sys
import numpy as np
import pandas as pd
import netCDF4 as nc
from osgeo import gdal 
from pathlib import Path

#custom
sys.path.append('./../../lib')
import paths as paths
import utils as utils

#options ---------------------
# directories
dir_base = Path(paths.dir_main)
dir_input = dir_base / "04_vpd/00_vpd/"
dir_raw = dir_base / "04_vpd/01_preprocessed/"
dir_tif = dir_base / "04_vpd/02_tif/"
dir_resamp = dir_base / "04_vpd/03_resampled/"

years = np.arange(1986, 2016 + 1)

# common mask generated from the fire weather datasets; use here for consistency
fw_mask = np.load(dir_base / '03_fire_weather/fdi_commonmask_0p1deg.npy')
#-----------------------------


# 1) generate 'raw' annual statistic grids for VPD from input higher temporal res data

for i, year in enumerate(years):
    
    fn = f"vapor_pressure_deficit_{year}.nc"
    fp = dir_input / fn
    
    fh = nc.Dataset(str(fp))
    lat = np.ma.filled(np.squeeze(fh.variables['Latitude'][:]), fill_value=np.NaN)
    lon = np.ma.filled(np.squeeze(fh.variables['Longitude'][:]), fill_value=np.NaN)
    vpd = np.ma.filled(np.squeeze(fh.variables['VPD'][:]), fill_value=np.NaN)

    vpd_mean = np.mean(vpd, axis=0)
    vpd_median = np.median(vpd, axis=0)
    vpd_max = np.max(vpd, axis=0)
    vpd_min = np.max(vpd, axis=0)
    
    outdict = {'mean': vpd_mean, 
               'median': vpd_median, 
               'max': vpd_max,
               'min': vpd_min}
    
    #output as netcdfs
    for var in ['mean', 'median', 'max', 'min']:
        
        dict_of_arr = outdict.get(var)
        utils.output_2d_nc(mygrid=dict_of_arr, mylats=lat, mylons=lon,
                           fp_out=str(dir_raw / f"vpd_{var}_0p25_{year}.nc"), 
                           do_zlib=1)
    
    fh.close()    
print('Done preprocessing statistics grids')


# 2) convert netcdf files to .tifs

fp_list = list(Path.glob(dir_raw, '*.nc'))

for i, fp in enumerate(fp_list):
    fn_out = str(fp).split('\\')[-1].split('.')[0] + '.tif'
    fp_out = str(dir_tif / fn_out)
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
    
    # output to tif
    # note, trans has gdal element ordering
    trans=(0, hsize, 0, 90.0, 0, -vsize) #for geff
       
    utils.nparray2raster(arr=arr, 
                         fp=fp_out, 
                         trans_type='gdal', 
                         trans=trans, 
                         arr_rows=height, 
                         arr_cols=width, 
                         driver='GTiff', count=1, dtype='float64',
                         compress='lzw', predictor=3, nodata=np.nan, 
                         tile=False, crs='EPSG:4326')    
print('Done netcdf -> .tif')



# 3) resample tif files to 0.1deg spatial resolution for consistancy with fire danger data
# NOTE: this also shifts the projection of the VPD data from [0,360] to [-180,180]

fp_list = list(Path.glob(dir_tif, '*.tif'))

for i, fp in enumerate(fp_list):
    
    fn_out = str(fp).split('\\')[-1].split('.')[0] + 'resampled_0p1deg.tif'
    fp_out = str(dir_resamp / fn_out)
        
    utils.gdalwarpcompress_v1(fp_in=fp, 
                              fp_out=fp_out,
                              target_extent=' '.join([str(x) for x in [-180.0, -90.0, 180.0, 90.0]]), 
                              target_res=' '.join([str(x) for x in [0.1, -0.1]]), 
                              proj='EPSG:4326', 
                              dtype='float')    
print('Done GDAL resample')



# 4) generate a multiyear raster stack for mean, median, and max VPD
# also applies common mask used for fire weather data

# load vpd mean
#fp_list = utils.get_sorted_paths(dir_resamp, '/*vpd_mean*tif')
fp_list = list(Path.glob(dir_resamp, '*vpd_mean*tif'))
nyears = len(fp_list)
vpd_mean = np.zeros((nyears, 1800, 3600)) * np.nan

for i, fp in enumerate(fp_list):

    fh = gdal.Open(str(fp))
    vpd_mean[i, :, :] = fh.GetRasterBand(1).ReadAsArray().astype(float)
    fh = None

    
# load vpd median    
#fp_list = utils.get_sorted_paths(dir_resamp, '/*vpd_median*tif')
fp_list = list(Path.glob(dir_resamp, '*vpd_median*tif'))
nyears = len(fp_list)
vpd_median = np.zeros((nyears, 1800, 3600)) * np.nan

for i, fp in enumerate(fp_list):

    fh = gdal.Open(str(fp))
    vpd_median[i, :, :] = fh.GetRasterBand(1).ReadAsArray().astype(float)
    fh = None


# load vpd max
#fp_list = utils.get_sorted_paths(dir_resamp, '/*vpd_max*tif')
fp_list = list(Path.glob(dir_resamp, '*vpd_max*tif'))
nyears = len(fp_list)
vpd_max = np.zeros((nyears, 1800, 3600)) * np.nan

for i, fp in enumerate(fp_list):

    fh = gdal.Open(str(fp))
    vpd_max[i, :, :] = fh.GetRasterBand(1).ReadAsArray().astype(float)
    fh = None  

# apply the mask
vpd_mean = vpd_mean * fw_mask
vpd_median = vpd_median * fw_mask
vpd_max = vpd_max * fw_mask

print('Done raster stacking and masking')

# Export the 3D arrays for each variant as a numpy binary for ease of use in later analysis
np.savez_compressed(str(dir_resamp / "vpd_mean_1986_2016_resampled_0p1deg.npz"), vpd_mean)
np.savez_compressed(str(dir_resamp / "vpd_median_1986_2016_resampled_0p1deg.npz"), vpd_median)
np.savez_compressed(str(dir_resamp / "vpd_max_1986_2016_resampled_0p1deg.npz"), vpd_max)

print('Done outputting files to disk')

#----------------------
### uncomment line below if just running the final bit of the script without all the preprocessing stels 
#vpd_mean = np.load(str(dir_resamp / "vpd_mean_1986_2016_resampled_0p1deg.npz"))['arr_0']
#----------------------

# Prepare regionally averaged data for GFED and T/T/B regional timeseries analysis
# this uses the gridded data, and calculates regional averages accounting for grid cell area

# calculate 1d array of latitudinal grid cell area based on grid cell latitude and longitude
res = 0.1   # grid cell spatial resolution in degrees
lat1d = np.linspace(90. - res / 2., -90. + res / 2., num=int(180 / res))
lon1d = np.linspace(0. + res / 2., 360. - res / 2., num=int(360 / res)) - 180
area = utils.calc_grid_cell_area(lat1d, lon1d)

# load gfed regions
fp_gfed = dir_base / "aux_data/gfed/gfed_regions_0_1_deg.npy"
gfed = np.load(str(fp_gfed))
# realign longitudinally and mask oceans
gfed = np.roll(gfed, 1800)
gfed[gfed == 0] = np.nan

# specify broad areas for temperate, tropical and boreal regions
# 1 = temperate, 2 = boreal, 3 = tropics
ttb = np.ones_like(gfed)
ttb[(lat1d < 20) & (lat1d > -20),:] = 2
ttb[(lat1d > 50), :] = 3
# same ocean mask as gfed
ttb[np.isnan(gfed)] = np.nan  

# high latitude mask (> 60N)
arctic_mask = np.zeros_like(gfed)
arctic_mask[0:300,:] = 1

# setup variables needed for aggregation
gfed_regions = np.arange(1,15)
gfed_names = ['BONA', 'TENA', 'CEAM', 'NHSA', 'SHSA', 'EURO', 'MIDE',
              'NHAF', 'SHAF', 'BOAS', 'CEAS', 'SOAS', 'EQAS', 'AUST']

ttb_regions = np.arange(1, 4)
ttb_names = ['Temperate', 'Tropics', 'Boreal']


# do averaging by GFED region
df_out = pd.DataFrame()
for k, region in enumerate(gfed_regions):
    
    name = gfed_names[k]
    print(f"doing {region} {name}")
    
    #mask data to the current gfed region
    mask = (gfed == region).astype(float)
    mask[mask==0] = np.nan

    # mask out the arctic
    mask[arctic_mask == 1] = np.nan

    #calculate regional weighted mean and store alongside metadata
    for i, year in enumerate(years):
        
        df_tmp = pd.DataFrame()
        df_tmp['gfed_name'] = pd.Series(name)
        df_tmp['gfed'] = region
        df_tmp['year'] = year
        df_tmp['vpd_mean'] = utils.calc_weighted_avg(vpd_mean[i,:,:], area, mask=mask)
        df_out = pd.concat([df_out,df_tmp])

print('completed GFED regional aggregation')


# do averaging by trop/temp/boreal
df_out2 = pd.DataFrame()
for k, region in enumerate(ttb_regions):
    
    name = ttb_names[k]
    print(f"doing {region} {name}")
    mask = (ttb == region).astype(float)
    mask[mask == 0] = np.nan
    
    # mask out the arctic
    mask[arctic_mask == 1] = np.nan

    for i, year in enumerate(years):
        
        df_tmp = pd.DataFrame()
        df_tmp['ttb_name'] = pd.Series(name)
        df_tmp['ttb'] = region
        df_tmp['year'] = year
        df_tmp['vpd_mean'] = utils.calc_weighted_avg(vpd_mean[i,:,:], area, mask=mask)
        df_out2 = pd.concat([df_out2,df_tmp])

print('completed Tropical/Temporate/Boreal regional aggregation')

# now calculate global totals
mask = np.ones_like(gfed).astype(float)
mask[arctic_mask == 1] = np.nan
df_global = pd.DataFrame()

for i, year in enumerate(years):   
    
    df_tmp = pd.DataFrame()
    df_tmp['gfed_name'] = pd.Series('Global')
    df_tmp['gfed'] = 99
    df_tmp['year'] = year
    df_tmp['vpd_mean'] = utils.calc_weighted_avg(vpd_mean[i,:,:], area, mask=mask)
    df_global = pd.concat([df_global, df_tmp])

print('completed global aggregation')
print(df_global.head)

# add global results to both the gfed and ttb dataframes and output to file
# GFED
df_out = pd.concat([df_out, df_global])
df_out.to_csv(str(dir_base / "gfed_region_mean_vpd.csv"), index=False)

# TTB
df_global2 = df_global.copy().rename(columns={'gfed':'ttb','gfed_name':'ttb_name'})
#df_global2.loc[df_global2.ttb_name=='global', 'ttb_name'] = 'Global'
df_out2 = pd.concat([df_out2, df_global2])
df_out2.to_csv(str(dir_base / f"TemperateTropicalBoreal_region_mean_vpd.csv"), index=False)

print('Finished')
