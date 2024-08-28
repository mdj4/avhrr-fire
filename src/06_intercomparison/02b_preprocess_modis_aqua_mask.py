""" 
Mask and compile the Aqua MODIS auxiliary data so that it is as comparable to AVHRR data as possible.

Masking: 
* persistent thermal activity
* volc/geothermal activity
* industrial heat sources
* high latitude mask (60deg N)
* non-flammable landcovers

GFED regions are also assigned at this stage.

"""

import sys
import numpy as np
import pandas as pd
from osgeo import gdal 
from pathlib import Path

#custom
#sys.path.append(str(Path(__file__).resolve().parent.parent))
sys.path.append('./../../lib')
import paths as paths
import utils as utils



# USER INPUTS -----------
# grid spatial resolution (degs) 
res = 0.1  

dir_base = Path(paths.dir_main)
dir_in = Path(dir_base / "aux_data/modis/mcd14ml_lst")
dir_out = Path(dir_base / "aux_data/modis/")

# persistent thermal activity detected from AVHRR
fp_persist = Path(dir_base / "aux_data/spurious/persistent_thermal_activity_gt20days_in_3yrs_0p1deg_cells.csv")

# csv of all volc/geothermal activity between 0-2019CE from the global volcanism program: https://volcano.si.edu/gvp_votw.cfm
fp_volc = Path(dir_base / "aux_data/spurious/GVP_Volcano_List_0CE_2019CE.csv")

# csv of industrial heat sources from liu et al 2018 https://doi.org/10.1016/j.rse.2017.10.019
fp_industry = Path(dir_base / "aux_data/spurious/liu_et_al_2018_industrial_heat_sources.csv")

# raster of GFED basis regions available from: https://www.globalfiredata.org/data.html
fp_gfed = Path(dir_base / "aux_data/gfed/gfed_regions_0_1_deg.npy")

# raster of modal land cover at 0.1deg resolution, derived from the modis MCD12C1 product
fp_lc = Path(dir_base / 'aux_data/modis/MCD12C1.A2016001.006.0_1deg.modal_pixel_resample.tif')

# -------------------------

# 1) load and format all masking data 

# first get the csv based masking data and clean it up
persistent = pd.read_csv(str(fp_persist))[['lat', 'lon180']]
volc = pd.read_csv(str(fp_volc))
industry = pd.read_csv(str(fp_industry))

# VOLCANOES
volc = volc[['Latitude', 'Longitude', 'Last Known Eruption2']]
volc = volc.rename(index=str, columns={"Latitude": "lat", 
                                       "Longitude": "lon180", 
                                       "Last Known Eruption2": "last_eruption"})
volc = volc.astype({"last_eruption": 'int'})
volc = volc.round({'lat': 1, 'lon180': 1})
volc['lat_x10_int'] = np.round(volc['lat'] * 10, 0).astype('int')
volc['lon180_x10_int'] = np.round(volc['lon180'] * 10, 0).astype('int')

# PERSISTENT HOTSPOTS
persistent = persistent.round({'lat': 1, 'lon180': 1})
persistent['lat_x10_int'] = np.round(persistent['lat'] * 10, 0).astype('int')
persistent['lon180_x10_int'] = np.round(persistent['lon180'] * 10, 0).astype('int')

# NON BIOMASS BURNING INDUSTRIAL HEAT SOURCES
industry = industry.round({'Y': 1, 'X': 1})
industry['lat_x10_int'] = np.round(industry['Y'] * 10, 0).astype('int')
industry['lon180_x10_int'] = np.round(industry['X'] * 10, 0).astype('int')

# LANDCOVER
# make a landcover mask based on the coarsened MODIS LC dataset
fh = gdal.Open(str(fp_lc))
lc = fh.ReadAsArray()  # first layer is the UMD classification
lc = np.roll(lc, int(lc.shape[1]/2), axis=1)  # shift the x-axis 
lc_mask = np.ones_like(lc)

# mask classes: water (0), urban (13), perm. snow/ice (15), non-vegetated/barren (16), water bodies (17)
lc_mask[(lc == 0) | (lc == 13) | (lc == 15) | (lc == 16) | (lc == 17)] = 0

# generate 2d lat and lon arrays for use with the array data
lat = np.linspace(90. - res / 2., -90. + res / 2., num=int(180 / res))
lon = np.linspace(0. + res / 2., 360. - res / 2., num=int(360 / res))
lon_array, lat_array = np.meshgrid(lon, lat)

# Convert the LC mask to a dataframe
df_mask = pd.DataFrame()
df_mask['lon'] = lon_array.flatten()
df_mask['lat'] = lat_array.flatten()
df_mask['mask'] = lc_mask.flatten()
df_mask['lon360_ix_lc'] = np.round((df_mask['lon'] * 100)).astype(int)
df_mask['lat_ix_lc'] = np.round((df_mask['lat'] * 100)).astype(int)
df_mask = df_mask[df_mask['mask'] == 1]  # keep only locs that are NOT masked

# Load the gfed region array and convert to a dataframe
gfed = np.load(str(fp_gfed))
df_gfed = pd.DataFrame()
df_gfed['lon'] = lon_array.flatten()
df_gfed['lat'] = lat_array.flatten()
df_gfed['gfed'] = gfed.flatten().astype(int)
df_gfed['lon360_ix_lc'] = np.round((df_gfed['lon'] * 100)).astype(int)
df_gfed['lat_ix_lc'] = np.round((df_gfed['lat'] * 100)).astype(int)



# 2) load MODIS data files and mask iteratively.
# processes both the 'am' and 'pm' aqua data sequentially

# am/pm loop
for period in ['am','pm']: 
    fn_list = pd.Series(list(Path.glob(dir_in, '*')))
    fn_list = [i for i in fn_list if period in i]

    df_out = pd.DataFrame()
    
    # year file loop
    for infile in fn_list:
        print("\nDOIING FILE: ",infile)
        df_full = pd.read_csv(str(infile))
        
        print("START SIZE: ",df_full.shape)

        # add some useful columns
        df_full['dt_lst'] = pd.to_datetime(df_full['dt_lst'])
        df_full['date_lst'] = df_full['dt_lst'].dt.date
        df_full['month'] = df_full['dt_lst'].dt.month
        df_full['year'] = df_full['dt_lst'].dt.year
        df_full['doy'] = df_full['dt_lst'].dt.dayofyear
        # For volcano and spurious signal masking, round lat/lons to nearest 0.1 deg
        # these are converted to integers so dataframes can be joined on these indices
        df_full['lat_x10_int'] = np.round(df_full['lat'] * 10, 0).astype('int')
        df_full['lon180_x10_int'] = np.round(df_full['lon180'] * 10, 0).astype('int')
        # For landcover masking, add an ix that matches the LC mask indices
        # note: use of lon360 for the LC mask, and rounded to nearest 0.1 offset to 5 digit
        df_full['lat_ix_lc'] = np.round(utils.round_with_5_offset(df_full['lat'], prec=2) * 100).astype('int')
        df_full['lon360_ix_lc'] = np.round(utils.round_with_5_offset(df_full['lon360'], prec=2) * 100).astype('int')

        
        # 3) Apply the various masks to the AVHRR data
        
        # HIGH LATITUDE MASK
        # remove all data > 60N
        df_full = df_full.loc[df_full.lat <= 60]
        
        # VOLCANOES
        # merge, then drop recs where (a) only in volcs (i.e. sub satellite LOD),
        # or (b) in both (volc detected by sat)
        df_full = df_full.merge(volc[['lat_x10_int','lon180_x10_int']],
                                on=['lat_x10_int','lon180_x10_int'],
                                how='outer', indicator=True)
        df_full = df_full[df_full['_merge'] == 'left_only']
        df_full = df_full.drop(['_merge'], axis=1)

        #print(df_full.head())
        print("AFTER VOLC:", df_full.shape)
        
        # NON-FIRE PERSISTENT HOTSPOTS
        # merge, then only retain records not present in both (i.e. keep 'left')
        df_full = df_full.merge(persistent[['lat_x10_int','lon180_x10_int']],
                                on=['lat_x10_int','lon180_x10_int'],
                                how='outer', indicator=True)
        df_full = df_full[df_full['_merge'] == 'left_only']
        df_full = df_full.drop(['_merge'], axis=1)
        print("AFTER PERSISTENT:", df_full.shape)
        
        # NON BIOMASS BURNING INDUSTRIAL HEAT SOURCES
        # merge, then only retain records not present in both (i.e. keep 'left')
        df_full = df_full.merge(industry[['lat_x10_int','lon180_x10_int']],
                                on=['lat_x10_int','lon180_x10_int'],
                                how='outer', indicator=True)
        df_full = df_full[df_full['_merge'] == 'left_only']
        df_full = df_full.drop(['_merge'], axis=1)

        print("AFTER INDUSTRIAL MASK:", df_full.shape) 

        # LANDCOVER
        df_full = df_full.merge(df_mask[['lon360_ix_lc', 'lat_ix_lc']],
                                on=['lon360_ix_lc', 'lat_ix_lc'],
                                how='inner')

        print("AFTER LC MASK:", df_full.shape)

        # ASSIGN GFED REGION IDS AND NAMES 
        df_full = df_full.merge(df_gfed[['lon360_ix_lc', 'lat_ix_lc','gfed']],
                                on=['lon360_ix_lc', 'lat_ix_lc'],
                                how='inner')
        print('shape after ADD GFAS REGIONS: ',df_full.shape)
        
        # indices no longer needed, drop
        df_full = df_full.drop(['lon360_ix_lc', 'lat_ix_lc'], axis=1)
        df_full = df_full.drop(['lat_x10_int','lon180_x10_int'], axis=1)
        
        # add region labels
        df_full['gfed_name'] = ''
        df_full.loc[df_full.gfed == 0, 'gfed_name'] = 'Mask'
        df_full.loc[df_full.gfed == 1, 'gfed_name'] = 'BONA'
        df_full.loc[df_full.gfed == 2, 'gfed_name'] = 'TENA'
        df_full.loc[df_full.gfed == 3, 'gfed_name'] = 'CEAM'
        df_full.loc[df_full.gfed == 4, 'gfed_name'] = 'NHSA'
        df_full.loc[df_full.gfed == 5, 'gfed_name'] = 'SHSA'
        df_full.loc[df_full.gfed == 6, 'gfed_name'] = 'EURO'
        df_full.loc[df_full.gfed == 7, 'gfed_name'] = 'MIDE'
        df_full.loc[df_full.gfed == 8, 'gfed_name'] = 'NHAF'
        df_full.loc[df_full.gfed == 9, 'gfed_name'] = 'SHAF'
        df_full.loc[df_full.gfed == 10, 'gfed_name'] = 'BOAS'
        df_full.loc[df_full.gfed == 11, 'gfed_name'] = 'CEAS'
        df_full.loc[df_full.gfed == 12, 'gfed_name'] = 'SOAS'
        df_full.loc[df_full.gfed == 13, 'gfed_name'] = 'EQAS'
        df_full.loc[df_full.gfed == 14, 'gfed_name'] = 'AUST'
        
        #drop some excess columns to save space
        df_full = df_full[['lat', 'lon180', 'lon360', #'acq_time_str', 
                        'dt_utc', 'dt_lst', 'hours_lst', 'date_lst',
                        'month', 'year', 'doy', 'gfed', 'gfed_name']]
        
        df_out = pd.concat([df_out, df_full], ignore_index=True)
    
    # SAVE MASKED MODIS DATA TO DISK
    print(f'saving {period} data to disk..')
    # for some reason, the merge operations seem to convert month and year to floats - need to convert back
    df_out = df_out.astype({"year": 'int', "month": 'int', "doy": 'int'})
    # compress default is 'infer' so ext .gz will result in compressed
    df_out.to_csv(Path(dir_out / f'MODIS_aqua_hotspots_masked_{period}.csv.gz'), index=False)
print('\nfinished')