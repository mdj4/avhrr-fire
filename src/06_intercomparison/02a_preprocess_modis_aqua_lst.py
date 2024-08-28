"""
Preprocess MODIS AQUA data so that it is as comparable as possible with AVHRR data (masking etc)

"""
import sys
import numpy as np
import pandas as pd
from datetime import datetime, time, timedelta
from pathlib import Path

#custom
#sys.path.append(str(Path(__file__).resolve().parent.parent))
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
dir_raw_modis = Path(dir_base / "aux_data/modis/mcd14ml_firms_raw")
dir_out = Path(dir_base / "aux_data/modis/mcd14ml_lst")
#------------------------------------------------

# get list of paths to raw input data
fp_list = list(Path.glob(dir_raw_modis, '*gz'))

# add local solar times to raw data
for i, infile in enumerate(fp_list):
    print('doing {0} of {1}: {2}'.format(i+1, len(fp_list), infile))
    
    fn = str(infile).split('\\')[-1].split('.')[0]
    print(fn)
    df = pd.read_csv(str(infile))
    
    # just keep Aqua data
    df = df.loc[df.satellite == 'Aqua']

    # adjust longitude
    df = df.rename(columns={"longitude": "lon180", "latitude": "lat"})
    df['lon360'] = df['lon180']
    df.loc[df.lon360 < 0, 'lon360'] = df.loc[df.lon360 < 0, 'lon360'] + 360

    # convert to dt
    df['dt_utc'] = pd.to_datetime(df['acq_date'] + ' ' + df['acq_time'].astype(str).str.zfill(4), format='%Y-%m-%d %H%M')

    # apply NOAA LST eq
    df['dt_lst'] = df.apply(lambda x: local_solar_time_single(x['dt_utc'], x['lon180']), axis=1)
    
    # get decimal lst hours and date
    df['hours_lst'] = df['dt_lst'].dt.hour
    df['date_lst'] = df['dt_lst'].dt.date

    # filter out low confidence fires
    df = df.loc[df.confidence >= 30]
      
    # roughly split into an AM and PM dataset
    aqua_am = df.loc[df.hours_lst < 6]
    aqua_pm = df.loc[df.hours_lst >= 6]
    
    # output
    aqua_am.to_csv(str(dir_out / f'aqua_am_{fn}.csv.gz'), 
                header=True,
                index=False,
                compression='gzip') 
    
    aqua_pm.to_csv(str(dir_out / f'aqua_pm_{fn}.csv.gz'), 
                header=True,
                index=False,
                compression='gzip')

print('\n finished') 
  