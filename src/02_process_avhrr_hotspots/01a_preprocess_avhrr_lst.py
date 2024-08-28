"""
Add local solar time to the AVHRR hotspot data
"""
import sys
from pathlib import Path
import numpy as np
import pandas as pd
from datetime import datetime, time, timedelta

# load custom functions
sys.path.append('./../../lib')
import paths as paths
import utils as utils


def local_solar_time_single(dt, lon):
    """
    Calculate local solar time. Adapted from https://stackoverflow.com/a/13424528, and 
    ultimately NOAA https://www.esrl.noaa.gov/gmd/grad/solcalc/solareqns.PDF

    Args:
        dt: a datetime object
        lon: float, longitude value between [-180, 180] 
    
    Returns: 
        solar_time: float, local solar time of observation
    """
    
    
    def leap_year(y):
        if y % 400 == 0:
            return True
        if y % 100 == 0:
            return False
        if y % 4 == 0:
            return True
        else:
            return False
    
    if leap_year(dt.year):
        denominator = 366
    else:
        denominator = 365

    gamma = 2 * np.pi / denominator * (dt.timetuple().tm_yday - 1 + float(dt.hour - 12) / 24)
    
    eqtime = 229.18 * (0.000075 + 0.001868 * np.cos(gamma) - 0.032077 * np.sin(gamma) \
             - 0.014615 * np.cos(2 * gamma) - 0.040849 * np.sin(2 * gamma))
    
    time_offset = eqtime + 4 * lon
    
    tst = dt.hour * 60 + dt.minute + dt.second / 60 + time_offset
    
    solar_time = datetime.combine(dt.date(), time(0)) + timedelta(minutes=tst)
    
    return solar_time

# USER INPUTS------------------------------------
dir_base = Path(paths.dir_main)
dir_in = dir_base / "01_avhrr_data_raw"
dir_out = dir_base / "02_avhrr_data_with_lst"
#------------------------------------------------
print(dir_in)
print(dir_out)

# get list of paths to raw input data
fp_list = list(Path.glob(dir_in, '*pixel*'))

#print(fp_list)

# add local solar times to raw data
for i, infile in enumerate(fp_list):
    print(f'doing {infile}')
    
    fn = str(infile).split('\\')[-1].split('.')[0]
    
    df = pd.read_csv(infile)

    # adjust longitude
    df = df.rename(columns={"lon": "lon360"})
    df['lon180'] = df['lon360']
    df.loc[df.lon360 >= 180, 'lon180'] = df.loc[df.lon360 >= 180, 'lon180'] - 360

    # convert to dt
    df['dt_utc'] = pd.to_datetime(df[['year', 'month', 'day']]) + pd.to_timedelta(df.hour, unit='h')
    
    # apply NOAA LST eq
    df['dt_lst'] = df.apply(lambda x: local_solar_time_single(x['dt_utc'], x['lon180']), axis=1)
    
    # drop excess columns
    df = df[['fn', 'lat', 'lon360', 'b1', 'b2', 'b3', 'b34', 'b4', 'b45', 'b5',
         'satza', 'lon180', 'dt_utc', 'dt_lst']]

    # drop hours outside 23:00 & 04:00
    df = df.loc[(df.dt_lst.dt.hour < 4) | (df.dt_lst.dt.hour > 23)]
    
    # output
    if len(df) > 0:
        df.to_csv(#os.path.join(dir_out, f'{fn}_lst_2300_0400.csv.gz'), 
                  dir_out / f'{fn}_lst_2300_0400.csv.gz',
                  header=True,
                  index=False,
                  compression='gzip')  
print('finished')