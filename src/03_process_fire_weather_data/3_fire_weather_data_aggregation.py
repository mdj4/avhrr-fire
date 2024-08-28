"""
apply a common mask to all the FWI datasets, and then output in formats for analysis

Prepare gridded data for mapped trend analysis:
+ create aggregated gridded outputs for (1) GWED data, (2) GEFF data, (3) all data combined 
+ export all as stacked 3D rasters (.npz) for trend calculation and mapping

Prepare GFED region and tropical/temporate/boreal aggregated data for regional timeseries analysis:
+ calculate grid cell area weighted avergage for each climate dataset and region, and output a pandas dataframe to csv

# NOTE: prior versions of this script erroneously omitted the high latitude mask on the global average values

"""
import sys
import numpy as np
import pandas as pd
from osgeo import gdal 
from pathlib import Path

#custom
sys.path.append('./../../lib')
import paths as paths
import utils as utils


#options ---------------------
# directories
dir_base = Path(paths.dir_main)
dir_resamp = dir_base / "03_fire_weather/03_resampled/"

years = np.arange(1986, 2016 + 1)
#------------------------------


# 1) Prepare gridded data for mapped trend analyses
# load gfwed merra corrected
fp_fwsl = list(Path.glob(dir_resamp, '*fwi_days_merra*tif'))
nyears = len(fp_fwsl)
print(f"nyears: {nyears}")


merra = np.zeros((nyears, 1800, 3600)) * np.nan 
for i,fp in enumerate(fp_fwsl):
    #print(fp)
    fh = gdal.Open(str(fp))
    merra[i, :, :] = fh.GetRasterBand(1).ReadAsArray().astype(float)
    fh = None


# load gfwed cpc
fp_fwsl = list(Path.glob(dir_resamp, '*fwi_days_cpc*tif'))
nyears = len(fp_fwsl)
print(f"nyears: {nyears}")

cpc = np.zeros((nyears, 1800, 3600)) * np.nan
for i,fp in enumerate(fp_fwsl):
    #print(fp)
    fh = gdal.Open(str(fp))
    cpc[i, :, :] = fh.GetRasterBand(1).ReadAsArray().astype(float)
    fh = None


# load geff fwi
fp_fwsl = list(Path.glob(dir_resamp, '*geff_fwi_days*tif'))
nyears = len(fp_fwsl)
print(f"nyears: {nyears}")

gfwi = np.zeros((nyears, 1800, 3600)) * np.nan
for i,fp in enumerate(fp_fwsl):
    #print(fp)
    fh = gdal.Open(str(fp))
    gfwi[i, :, :] = fh.GetRasterBand(1).ReadAsArray().astype(float)
    fh = None


# load geff fdi (mark5)
fp_fwsl = list(Path.glob(dir_resamp, '*geff_fdi_days*tif'))
nyears = len(fp_fwsl)
print(f"nyears: {nyears}")

mark5 = np.zeros((nyears, 1800, 3600)) * np.nan
for i,fp in enumerate(fp_fwsl):
    #print(fp)
    fh = gdal.Open(str(fp))
    mark5[i, :, :] = fh.GetRasterBand(1).ReadAsArray().astype(float)
    fh = None


# load geff bi (USFDRS)
fp_fwsl = list(Path.glob(dir_resamp, '*geff_bi_days*tif'))
nyears = len(fp_fwsl)
print(f"nyears: {nyears}")

bi = np.zeros((nyears, 1800, 3600)) * np.nan
for i,fp in enumerate(fp_fwsl):
    #print(fp)
    fh = gdal.Open(str(fp))
    bi[i, :, :] = fh.GetRasterBand(1).ReadAsArray().astype(float)
    fh = None


# calc mean of ensemble for each year for:
# + all GEFF and GFWED data combined
# + GFWED metrics
# + GEFF metrics
efwsl_both = np.mean([merra,cpc,gfwi,mark5,bi], axis=0)
efwsl_geff = np.mean([gfwi,mark5,bi], axis=0)
efwsl_gfwed = np.mean([merra,cpc], axis=0)

# make a common mask from 'efwsl_both' to ensure all data are masked the same
fw_mask = efwsl_both[0] * 0 + 1

merra = merra * fw_mask
cpc = cpc * fw_mask
gfwi = gfwi * fw_mask
mark5 = mark5 * fw_mask
bi = bi * fw_mask
efwsl_both = efwsl_both * fw_mask
efwsl_geff = efwsl_geff * fw_mask
efwsl_gfwed = efwsl_gfwed * fw_mask

print('AFTER MASKING (all NaNs should match)')
print('combined nans: ', np.where(np.isnan(efwsl_both[0]))[0].shape)
print('geff nans: ', np.where(np.isnan(efwsl_geff[0]))[0].shape)
print('gfwed nans: ', np.where(np.isnan(efwsl_gfwed[0]))[0].shape)


# Export the 3D arrays for each variant as a numpy binary for ease of use in later analysis

# combined
np.savez_compressed(str(dir_resamp / "fwi_days_ensemble_gfwed_1986_2016_resampled_0p1deg.npz"), efwsl_gfwed)
np.savez_compressed(str(dir_resamp / "fwi_days_ensemble_geff_1986_2016_resampled_0p1deg.npz"), efwsl_geff)
np.savez_compressed(str(dir_resamp / "fwi_days_ensemble_gfwed_and_geff_1986_2016_resampled_0p1deg.npz"), efwsl_both)

# gfwed only
np.savez_compressed(str(dir_resamp / "fwi_days_ensemble_gfwed_fwi_merra_1986_2016_resampled_0p1deg.npz"), merra)
np.savez_compressed(str(dir_resamp / "fwi_days_ensemble_gfwed_fwi_cpc_1986_2016_resampled_0p1deg.npz"), cpc)

# geff only
np.savez_compressed(str(dir_resamp / "fwi_days_ensemble_geff_fwi_1986_2016_resampled_0p1deg.npz"), gfwi)
np.savez_compressed(str(dir_resamp / "fwi_days_ensemble_geff_bi_1986_2016_resampled_0p1deg.npz"), bi)
np.savez_compressed(str(dir_resamp / "fwi_days_ensemble_geff_fdi_1986_2016_resampled_0p1deg.npz"), mark5)

# export the mask for use with VPD data later
np.save(str(dir_base / "03_fire_weather/fdi_commonmask_0p1deg.npy"), fw_mask)

print('all gridded files saved to disk')




## 2) Prepare regionally averaged data for GFED and T/T/B regional timeseries analysis
# this uses the gridded data, and calculates regional averages accounting for grid cell area

# calculate 1d array of latitudinal grid cell area based on grid cell latitude and longitude
res = 0.1   # grid cell spatial resolution in degrees
lat1d = np.linspace(90. - res / 2., -90. + res / 2., num=int(180 / res))
lon1d = np.linspace(0. + res / 2., 360. - res / 2., num=int(360 / res)) - 180
area = utils.calc_grid_cell_area(lat1d, lon1d)

# load gfed regions
fp_gfed = dir_base / "aux_data/gfed/gfed_regions_0_1_deg.npy"
gfed = np.load(fp_gfed)
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
arr_list = [merra, cpc, bi, mark5, gfwi]
arr_label_list = ['gfwed_merra_fwi', 'gfwed_cpc_fwi', 'geff_bi', 'geff_fdi', 'geff_fwi']

gfed_regions = np.arange(1,15)
gfed_names = ['BONA', 'TENA', 'CEAM', 'NHSA', 'SHSA', 'EURO', 'MIDE',
              'NHAF', 'SHAF', 'BOAS', 'CEAS', 'SOAS', 'EQAS', 'AUST']

ttb_regions = np.arange(1, 4)
ttb_names = ['Temperate', 'Tropics', 'Boreal']

# do averaging by GFED region
df_out = pd.DataFrame()
for m, arr in enumerate(arr_list):
    
    arr_name = arr_label_list[m]
    
    for k, region in enumerate(gfed_regions):
        
        name = gfed_names[k]
        print(f"doing {arr_name} - {region} {name}")
        
        #mask data to the current gfed region
        mask = (gfed == region).astype(float)
        mask[mask==0] = np.nan

        # mask out the arctic
        mask[arctic_mask == 1] = np.nan

        #calculate regional weighted mean and store alongside metadata
        for i, year in enumerate(years):
            
            df_tmp = pd.DataFrame()
            df_tmp['dataset'] = pd.Series(arr_name)
            df_tmp['gfed_name'] = name
            df_tmp['gfed'] = region
            df_tmp['year'] = year
            df_tmp['fwsl_mean'] = utils.calc_weighted_avg(arr[i,:,:], area, mask=mask)
            df_out = pd.concat([df_out,df_tmp])

print('completed GFED regional aggregation')


# do averaging by trop/temp/boreal
df_out2 = pd.DataFrame()
for m, arr in enumerate(arr_list):
    
    arr_name = arr_label_list[m]
    
    for k, region in enumerate(ttb_regions):
        
        name = ttb_names[k]
        print(f"doing {arr_name} - {region} {name}")
        mask = (ttb == region).astype(float)
        mask[mask == 0] = np.nan
        
        # mask out the arctic
        mask[arctic_mask == 1] = np.nan

        for i, year in enumerate(years):
            
            df_tmp = pd.DataFrame()
            df_tmp['dataset'] = pd.Series(arr_name)
            df_tmp['ttb_name'] = name
            df_tmp['ttb'] = region
            df_tmp['year'] = year
            df_tmp['fwsl_mean'] = utils.calc_weighted_avg(arr[i,:,:], area, mask=mask)
            df_out2 = pd.concat([df_out2,df_tmp])

print('completed Tropical/Temporate/Boreal regional aggregation')


# now calculate global totals
mask = np.ones_like(gfed).astype(float)
mask[arctic_mask == 1] = np.nan
df_global = pd.DataFrame()

for m, arr in enumerate(arr_list):
    
    arr_name = arr_label_list[m]
    print(f"doing {arr_name}")
    
    for i, year in enumerate(years):
        
        df_tmp = pd.DataFrame()
        df_tmp['dataset'] = pd.Series(arr_name)
        df_tmp['gfed_name'] = 'Global'
        df_tmp['gfed'] = 99
        df_tmp['year'] = year
        df_tmp['fwsl_mean'] = utils.calc_weighted_avg(arr[i,:,:], area, mask=mask)
        df_global = pd.concat([df_global, df_tmp])
print('completed global aggregation')

# add global results to both the gfed and ttb dataframes and output to file

# GFED
df_out = pd.concat([df_out, df_global])
df_out.to_csv(str(dir_base / "gfed_region_mean_fwsl.csv"), index=False)

# TTB
df_global2 = df_global.copy().rename(columns={'gfed':'ttb','gfed_name':'ttb_name'})
df_out2 = pd.concat([df_out2, df_global2])
df_out2.to_csv(str(dir_base / "TemperateTropicalBoreal_region_mean_fwsl.csv"), index=False)

print('Finished')
