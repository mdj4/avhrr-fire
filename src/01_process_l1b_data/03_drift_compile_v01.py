"""
script for combining orbit drift files by satellite.
"""

import os
import glob
import pandas as pd

import paths as paths

dir_in = os.path.join(paths.dir_jasmin_main,'raw_orbit_timing_data')
dir_out = dir_in

if not os.path.exists(dir_out):
    os.makedirs(dir_out)

for root, dirs, files in os.walk(dir_in):

    for name in files:
       
        # load small files
        df_pixel = pd.read_csv(os.path.join(root, name))
        
        # make sure all columns are same order
        df_pixel = df_pixel[['fn', 'lat', 'lon', 'year', 'month', 'day', 'hour', 'time',
                             'satza', 'b1', 'b2', 'b3', 'b4', 'b5']]
        
        # create output file name
        rootparts = root.split('/')
        platform = rootparts[-2]
        year = rootparts[-1]
        fnout = os.path.join(dir_out, platform + year + '_overpass_time_data_0lat0lon.csv')	
        
        if not os.path.exists(fnout):
            
            with open(fnout, 'wb') as oFile:
                print('make new file ' + fnout)
                df_pixel.to_csv(oFile, header=True)
        
        else:
            
            with open(fnout, 'a') as oFile:
                df_pixel.to_csv(oFile, header=False)	


# Combine individual satellite files into a single file for all outputs   
list_fp = glob.glob(dir_out + '*')

df_combined = pd.DataFrame()
for fp in list_fp:
    df_tmp = pd.read_csv(fp)
    df_combined = pd.concat([df_combined, df_tmp])

df_combined.to_csv(os.path.join(dir_out,'overpass_time_data_0lat0lon.csv'), index=False)
