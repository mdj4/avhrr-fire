"""
## Extreme fire weather days from GFWED MERRA2.CORRECTED

Step 1) make min/max/mid arrays
* Read over all days in timeseries
* create a min value array
* create a max value array
* create a mid point array np.mean([min_arr, max_arr])

Step 2) calc no of days > mid point for each calendar year
* for each year and grid cell, determine how many days in that year are 'fire weather days' 
  (FWI value > midpoint value)

"""

import sys
import numpy as np
#import pandas as pd
import netCDF4 as nc
from pathlib import Path

#custom
sys.path.append('./../../lib')
import paths as paths
import utils as utils

# options-----------------------------------
#pd.set_option('display.max_columns', 500)
#pd.set_option('display.width', 1000)
#gdal.UseExceptions()

dir_base = Path(paths.dir_main)
dir_in = dir_base / "03_fire_weather/00_raw_merra/"
dir_out = dir_base / "03_fire_weather/01_preprocessed/"
start_year = 1986
end_year = 2016
do_plot = 0  # 0=no, 1=yes
#--------------------------------------------


# get list of directories for each year in analysis period
year_dir_list = [str(dir_in / str(year)) for year in np.arange(start_year, end_year + 1)]

# get list of all *Daily* files in all year directories
fp_list = []
for i, year_dir in enumerate(year_dir_list):
    fp_list = fp_list + list(Path.glob(year_dir, '*Daily*'))
len(fp_list)
print(fp_list)

#########
# ITERATE OVER ALL FWI FILES AND GET LOCAL MIN MAX VALUES
# note: all values rounded to 0.1 here, as decimal FWI not very meaningful
for i, fp in enumerate(fp_list):
    print(f"doing {i} of {len(fp_list)}: {fp}")
    fh = nc.Dataset(str(fp), 'r')
    if i == 0: # setup
        lat = np.ma.filled(np.squeeze(fh.variables['lat'][:]), fill_value=np.NaN)
        lon = np.ma.filled(np.squeeze(fh.variables['lon'][:]), fill_value=np.NaN)
        min_arr = np.round(np.ma.filled(np.squeeze(fh.variables['MERRA2.CORRECTED_FWI'][:]), fill_value=np.NaN),1)
        max_arr = min_arr.copy()
    else:
        fwi = np.round(np.ma.filled(np.squeeze(fh.variables['MERRA2.CORRECTED_FWI'][:]), fill_value=np.NaN),1)
        min_arr = np.where((fwi < min_arr) | (np.isnan(min_arr) & ~np.isnan(fwi)), fwi, min_arr)
        max_arr = np.where((fwi > max_arr) | (np.isnan(max_arr) & ~np.isnan(fwi)), fwi, max_arr)
    fh.close()

# get the FWI value half way between min and max
mid_arr = np.mean([min_arr, max_arr],axis=0)

#output as netcdfs
utils.output_2d_nc(mygrid=min_arr, mylats=lat, mylons=lon,
                       fp_out=str(dir_out / "fwi_merra2corrected_minimum.nc"), do_zlib=1)
utils.output_2d_nc(mygrid=mid_arr, mylats=lat, mylons=lon,
                       fp_out=str(dir_out / "fwi_merra2corrected_midpoint.nc"), do_zlib=1)
utils.output_2d_nc(mygrid=max_arr, mylats=lat, mylons=lon,
                       fp_out=str(dir_out / "fwi_merra2corrected_maximum.nc"), do_zlib=1)

print("CALC N. DAYS BEYOND MIDPOINT")
for i, year_dir in enumerate(year_dir_list):
    #print(year_dir)
    year = year_dir.split('/')[-1]
    fp_list = list(Path.glob(year_dir, '*Daily*'))
    print(year, len(fp_list))
    # reset the fwi days array
    fwi_days = np.zeros_like(mid_arr)
    for k, fp in enumerate(fp_list):
        fh = nc.Dataset(str(fp), 'r')
        fwi = np.round(np.ma.filled(np.squeeze(fh.variables['MERRA2.CORRECTED_FWI'][:]), fill_value=np.NaN),1)
        # increment by 1 when fwi value exceeds local mid point
        fwi_days[fwi > mid_arr] += 1
        fh.close()
    if len(fp_list) > 0:
        utils.output_2d_nc(mygrid=fwi_days, mylats=lat, mylons=lon,
                           fp_out=str(dir_out / f"fwi_days_merra2corrected_{year}.nc"), do_zlib=1)


print('finished')
