"""
Aggregate the hotspot data to a global grid to generate gridded annual number of fires (ANF) metric 
used for mapping. 

Default is to aggregate to 0.1 degree grid at this stage. 

"""

import sys
import numpy as np
import pandas as pd
from pathlib import Path

#custom
sys.path.append('./../../lib')
import paths as paths
import utils as utils

# options-----------------------------------
# working directory
dir_base = Path(paths.dir_main)

fn_in = 'avhrr_hotspots_masked.csv'

# list of years to compile
all_years = range(1986, 2016 + 1)

# define grid res
res = 0.1

# make sure only the following satellite/years are being processed
list_process = [
               'NF1986',
               'NH1989', 'NH1990', 'NH1991',
               'NJ1996', 'NJ1997', 'NJ1998',
               'NL2001', 'NL2002', 'NL2003', 'NL2004', 'NL2005',
               'NN2006', 'NN2007', 'NN2008', 'NN2009', 'NN2010', 'NN2011', 'NN2012',
               'NP2013', 'NP2014', 'NP2015', 'NP2016'
               ]
#--------------------------------------------

# generate grid
lat = np.linspace(90. - res / 2., -90. + res / 2., num=int(180 / res))
lon = np.linspace(0. + res / 2., 360. - res / 2., num=int(360 / res))
lon_array, lat_array = np.meshgrid(lon, lat)
ybin = np.linspace(90., -90., num=int(180 / res)+1)
xbin = np.linspace(0., 360., num=int(360 / res)+1)

# load & filter
df = pd.read_csv(dir_base / fn_in)
df['sat_yr'] = df['sat'] + df['year'].astype('str')
df = df[df['sat_yr'].isin(list_process)]

# setup params for gridding
av = np.zeros((len(all_years), lat.shape[0], lon.shape[0])) * np.nan
offset = all_years[0]
myiter = df[['sat', 'sat_yr', 'year']].drop_duplicates().sort_values(by='year').reset_index(drop=True)

# do gridding
for row in myiter.itertuples():
    print(row)
    sat = getattr(row, "sat")
    sat_yr = getattr(row, "sat_yr")
    year = getattr(row, "year")

    sub = df[(df['sat_yr'] == sat_yr)]

    # grid the counts
    x = sub['lon360']
    y = sub['lat'] + 90  # lat must be positive with np.histogram2d
    z0, x0, y0 = np.histogram2d(x, y, bins=(xbin, np.flipud(ybin + 90)))
    z0 = np.rot90(z0)
    
    # add current year to 3D array
    av[(year - offset)] = z0

# add sums to df - this is just useful for checking totals 
tmp = pd.DataFrame()
tmp['year'] = pd.Series(all_years)
tmp['av'] = np.nansum(av, axis=(1,2))
print(f'totals:\n{tmp}')

np.save(dir_base / 'avhrr_gridded_0_1_deg_annual_1986_2016', av)

print('Finished')

