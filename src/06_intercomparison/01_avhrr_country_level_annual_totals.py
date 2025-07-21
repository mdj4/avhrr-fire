"""
Intersect fires by country boundaries using GIS shapefile, and output annual total data for each country.
"""

import sys
import numpy as np
import pandas as pd
import geopandas as gpd
from shapely.geometry import Point
from pathlib import Path

#custom
sys.path.append('./../../lib')
sys.path.append(str(Path(__file__).resolve().parent.parent.parent / 'lib'))
print(sys.path)

import paths as paths
import utils as utils


#options --------------------
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)
#gdal.UseExceptions()

# directories
dir_base = Path(paths.dir_main)
dir_in = dir_base
dir_out = dir_base / "05_avhrr_by_country"

# avhrr fire dataset
fn_fires = "avhrr_hotspots_masked.csv"

# ## GADM dataset country boundaries
# dir_poly = dir_base = Path(paths.dir_main)
# fn_polygons = 'gadm36_0.shp'
# fn_out = 'gadm36_0_yr{0}'

## natural earth country boundaries
dir_poly = dir_base / "aux_data/ne_50m_admin_0_countries"
fn_polygons = "ne_50m_admin_0_countries.shp"
fn_out = "ne_50m_admin_0_countries_yr{0}"

# should points use geometry based on 0-360 or -180-180 longitude?
#geom_lon360 = True
geom_lon360 = False
# ----------------------------


# load shp file
print('processing ' + fn_polygons)

mypoly = gpd.read_file(str(dir_poly / fn_polygons))

df_full = pd.read_csv(str(dir_in / fn_fires))

# only keep calendar years of interest
df_full['sat_yr'] = df_full['sat'] + df_full['year'].astype('str')

# TODO: consider filtering all these at an earlier stage
list_process = [
               'NF1986',
               'NH1989', 'NH1990', 'NH1991',
               'NJ1996', 'NJ1997', 'NJ1998',
               'NL2001', 'NL2002', 'NL2003', 'NL2004', 'NL2005',
               'NN2006', 'NN2007', 'NN2008', 'NN2009', 'NN2010', 'NN2011', 'NN2012',
               'NP2013', 'NP2014', 'NP2015', 'NP2016'
               ]

df_full = df_full[df_full['sat_yr'].isin(list_process)]


# convert df_full to xy points with dates of detection
# https://gis.stackexchange.com/questions/174159/convert-a-pandas-dataframe-to-a-geodataframe
print('converting df to geodf')
if geom_lon360:
    geometry = [Point(xy) for xy in zip(df_full.lon360, df_full.lat)]
else:
    geometry = [Point(xy) for xy in zip(df_full.lon180, df_full.lat)]

crs = 'epsg:4326'
gdf = gpd.GeoDataFrame(df_full, crs=crs, geometry=geometry)


#gdf = gdf.sample(100)  # for debugging!

# do intersection year by year to break up processing if using large file
print("begin intersect with {0}".format(fn_polygons))

for yr in np.unique(gdf['year']):

    print("doing {0} {1}: {2} years".format(fn_polygons, yr, np.shape(np.unique(gdf['year']))[0]))
    sub = gdf[gdf['year'] == yr]

    # intersect points with poly
    # https://gis.stackexchange.com/questions/287243/intersect-points-and-polygons-in-shapely
    tmp = gpd.sjoin(sub, mypoly, how='left', op='intersects')
    print(tmp.shape)
    
    # convert back to standard df
    df1 = pd.DataFrame(tmp)
    
    # filter columns to remove unneeded cols from natural earth shp
    col_names = [x for x in df_full.columns] + ['ADM0_A3', 'ADMIN']
    df1 = df1[col_names]

    df1.to_pickle(str(dir_out / fn_out.format(yr)))


# now stitch all the year files together and store on disk for future use
fp_list = list(Path.glob(dir_out, '*yr*'))

df = pd.DataFrame()
for fp in fp_list:
    print('combining: {0}'.format(fp))
    tmp = pd.read_pickle(str(fp))
    df = pd.concat([df, tmp], ignore_index=True)
df.to_pickle(str(dir_out / '{0}_all_years'.format(fn_polygons.split('.')[0])))
print('\n finished shapefile intersection')


# aggregate to get annual totals
print('\n aggregating data to annual totals')
ann_totals = df.groupby(['ADM0_A3', 'ADMIN', 'sat', 'sat_yr', 'year'])\
    .count()['fn']\
    .reset_index()\
    .rename(columns={'fn': 'count'})\
    .sort_values('count', ascending=False)
# ann_totals.shape ##(3843, 6)


# sometimes there are years/area combinations with no fire, currently with no records.
# need to pad with zeros here if using later for any trend work
a = ann_totals[['sat_yr', 'sat', 'year']].drop_duplicates().reset_index(drop=True)
b = ann_totals[['ADM0_A3', 'ADMIN']].drop_duplicates().reset_index(drop=True)
a['joincol'] = 1
b['joincol'] = 1
c = a.merge(b, how='outer', on='joincol')
c = c.drop(['joincol'], axis=1)
c['dummy'] = 0
ann_totals = ann_totals.merge(c, how='outer', on=['sat_yr', 'sat', 'year', 'ADM0_A3', 'ADMIN'])
ann_totals.loc[np.isnan(ann_totals['count']), 'count'] = 0
ann_totals = ann_totals.drop(['dummy'], axis=1)

# output to disk.
# note: utf-8 is required for special characters in some country names
ann_totals.to_csv(str(dir_base / 'NE_50m_ADM0_country_annual_fire_count_totals.csv'), 
                  index=False, encoding='utf-8') 

print('\n ~~~ FINISHED ~~~')