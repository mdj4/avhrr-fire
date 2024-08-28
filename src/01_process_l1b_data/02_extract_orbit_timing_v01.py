'''
for a given lat lon, extract all orbit information if intersects,
 and write to csv
'''
import sys, os
import os
import numpy as np
import pandas as pd
from netCDF4 import Dataset

import paths as paths

def load_and_fill_bad(fp):
    fh = Dataset(fp, mode='r')

    b1 = fh.variables['ch1'][:]
    b2 = fh.variables['ch2'][:] 
    b3 = fh.variables['ch3'][:]
    b4 = fh.variables['ch4'][:]
    # avhrr/1 doesn't have b5
    try:
        b5 = fh.variables['ch5'][:]
    except:
        b5 = np.ones_like(b4) * np.nan

    lat = fh.variables['latitude'][:]
    lon = fh.variables['longitude'][:]
    satza = fh.variables['satZA'][:]
    year = fh.variables['Year'][:]     
    month = fh.variables['Month'][:]   
    day = fh.variables['Day'][:]       
    hour = fh.variables['Hours'][:]    
    time = fh.variables['Time'][:]     
    fh.close()

    # bad values (-1.0e+30 for floats, -32768 for int)
    # set to np.nan
    b1[b1 < -1e+10] = np.nan
    b2[b2 < -1e+10] = np.nan
    b3[b3 < -1e+10] = np.nan
    b4[b4 < -1e+10] = np.nan
    b5[b5 < -1e+10] = np.nan
    lat[lat < -1e+10] = np.nan
    lon[lon < -1e+10] = np.nan
    satza[satza < -1e+10] = np.nan
    hour[hour < -1e+10] = np.nan
    time[time < -1e+10] = np.nan

    orbit_dict = {
        'b1':b1, 'b3':b3, 'b2':b2, 'b4':b4, 'b5':b5,
        'lat':lat, 'lon':lon,
        'satza':satza,
        'year':year, 'month':month, 'day':day, 'hour':hour, 'time': time
    }

    return orbit_dict


def find_nearest_md(array, value):
    # returns idx of multidim array
    X = np.abs(array - value)
    idx = np.where(X == np.nanmin(X))
    return idx


def extract_timing(fp, dir_out, location, N, E, radius):
    '''
    for an orbit, check if a location(s) is within the lat lon extent
    if not, exit
    else, get ix of nearest grid cell lat lon, extract UTC time, zenith angle, band values
    
    '''
    print("extract_timing(): hello!")
    print("extract_timing(): loading orbit")
    d = load_and_fill_bad(fp)

    # only pixels meeting lat/long range will have an area_mask value = 2
    lat_mask = (d['lat'] > N - radius) & (d['lat'] < N + radius).astype(int)
    lon_mask = (d['lon'] > E - radius) & (d['lon'] < E + radius).astype(int)
    area_mask = lat_mask + lon_mask

    if np.nanmax(area_mask) == 2:
        print('extract_timing(): ROI present in orbit, processing')
        fn = fp.split('/')[-1]
        fnparts = fn.split('.')
        
        # mask lat to avoid multiple ix being returned
        masked_lat = d['lat'].copy()
        masked_lat[area_mask != 2] = np.nan
        ix = find_nearest_md(masked_lat, N)

        # extract possibly useful values for this location
        df = pd.DataFrame()
        df['fn'] = pd.Series(fn)
        df['lat'] = d['lat'][ix[0], ix[1]]
        df['lon'] = d['lon'][ix[0], ix[1]]
        df['year'] = d['year'][ix[0]]
        df['month'] = d['month'][ix[0]]
        df['day'] = d['day'][ix[0]]
        df['hour'] = d['hour'][ix[0]]
        df['time'] = d['time'][ix[0]]
        df['satza'] = d['satza'][ix[0], ix[1]]
        df['b1'] = d['b1'][ix[0], ix[1]]
        df['b2'] = d['b2'][ix[0], ix[1]]
        df['b3'] = d['b3'][ix[0], ix[1]]
        df['b4'] = d['b4'][ix[0], ix[1]]
        df['b5'] = d['b5'][ix[0], ix[1]]

        # ensure column order 
        df = df[['fn', 'lat', 'lon', 'year', 'month', 'day', 'hour', 'time',
                 'satza', 'b1', 'b2', 'b3', 'b4', 'b5']]

        # output
        dir_out_full = dir_out
        if not os.path.exists(dir_out_full):
            os.makedirs(dir_out_full)
        
        df.to_csv(os.path.join(dir_out_full, fn[:-3] + '_orbit_drift_ana_' + location + '.csv.gz'), compression='gzip',
                  index=False)
    else:
        pass
    
    print('extract_timing(): bye!')
    return


if __name__ == "__main__":


    fp = '/your/input/file/path/file.nc'
    dir_out = paths.dir_jasmin_main

    dir_out_full = os.path.join(dir_out,'raw_orbit_timing_data')

    location = '0N0E'
    N = 0
    E = 0  # lon must be within [0, 360]
    radius = 0.05

    extract_timing(fp=fp,
                   dir_out=dir_out_full,
                   location=location,
                   N=N,
                   E=E,
                   radius=radius)