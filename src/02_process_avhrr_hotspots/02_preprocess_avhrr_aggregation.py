"""
Aggregate AVHRR count data by GFED regions and broad biomes over different temporal periods
+ calendar year
+ 8 day compositing period (cp)

Also produces 'average profiles' of fire activity by GFED region, and 'profile_summary' which 
contains the values used to determine the start of the fire year for the GFED regions 

"""
import sys
from pathlib import Path
import numpy as np
import pandas as pd

#custom
sys.path.append('./../../lib')
import paths as paths
import utils as utils


def fire_season_calc(d, colA, colB, prop, override={}):
    """
    Defines core fire seasons by ordering starting from the period with fewest fires, then designates fire season where
    0.2 < [cumulative proportion of fires] < 0.8

    # NOTE: if there are non-unique fire count minima, the first CP in ascending order is used as the yearly minimum

    :param d:
    :param colA:
    :param colB:
    :return:
    """
    # get minimum fire month in terms of colA, and then reorder df starting with this month
    d = d.sort_values([colB], ascending=[True])  # bug fix, 06/03/2020: sort on 'comp_period' as might not be sorted
    d = d.reset_index(drop=True)
    #print d
    current_region = np.unique(d['gfed_name2'])[0]
    #print current_region
    if current_region in override.keys():
        year_min = override[current_region]
    else:
        year_min = d[d[colA] == np.min(d[colA])].sort_values(colB, ascending=True).iloc[0][colB]

    ix = d.index[d[colB] == year_min].tolist()[0]
    f = pd.concat([d.loc[ix:], d.loc[:ix - 1]]).reset_index(drop=True)  # new reordered df from year_min
    # identify the fire season
    f['cumsum'] = f[colA].cumsum()
    f['prop'] = f['cumsum'] / np.sum(f[colA])
    f['season'] = 0
    f.loc[(f['prop'] >= prop[0]) & (f['prop'] <= prop[1]), 'season'] = 1
    f.reset_index(inplace=True)
    f.rename(columns={'index': 'period_rank'}, inplace=True)
    return f


def fire_season_summary(d, colA, colB, colC, colD, override={}):
    """
    get summary statistics of fire season data
    year_min is tricky to define: often might be non-unique minima!
    stats produced:
    year_min:       compositing period matching min(count). in case of ties, keep first)
    year_max        compositing period matching max(count). in case of ties, keep first)
    season_start    first CP flagged as fire season
    season_end      last CP flagged as fire season
    season_length   count of CPs flagged as fire season (doesn't include gaps e.g. for [1,0,1] length = 2 not 3)

    :param d:
    :param colA: str, float, measure of fire activity e.g. median monthly count
    :param colB: str, (float?), compounding interval e.g. numbered 8 day MODIS periods
    :param colC: str, float(int?), index of compounding periods starting at first period in fire season ('period_rank').
                 used for sorting
    :param colD: str, col name of 'season' (binary 0/1)
    :return: stats
    """
    d = d.sort_values([colC], ascending=[True])
    current_region = np.unique(d['gfed_name2'])[0]
    if current_region in override.keys():
        year_min = override[current_region]
    else:
        year_min = d[d[colA] == np.min(d[colA])].iloc[0][colB]

    year_max = d[d[colA] == np.max(d[colA])].iloc[0][colB]

    # subset only the fire season
    sub = d[d[colD] == 1].sort_values(colC, ascending=True)
    season_len = len(sub)
    if season_len < 1:
        season_start = -9999
        season_end = -9999
    else:
        season_start = sub.iloc[0][colB]
        season_end = sub.iloc[-1][colB]
    # make dataframe and arrange nicely
    stats = {'year_min': int(year_min), 'year_max': int(year_max),
             'season_start': int(season_start), 'season_end': int(season_end), 'season_len': int(season_len)}
    stats = pd.DataFrame(stats, index=[0])
    stats = stats[['season_start', 'season_end', 'year_min', 'year_max', 'season_len']]
    return stats


# INPUTS---------------------------------
# working directory
dir_base = Path(paths.dir_main)

# input data file name
fn_in = 'avhrr_hotspots_masked.csv'

len_cp = 8  # length of an individual compositing period 

n_cp = int(368 / len_cp)  # n CPs in a year; 368 days gives whole number for n_cp when cp is 8 (n_cp = 46 cps per year)

season_def = (0.1, 0.9) # percentiles used to define start/end of fire season in a given year

# 'forced_regions' used to override year_min for certain regions when determining start of fire year.
# BONA, TENA & EURO all end in cp 46, so more intuitive to just use cp 1 as the start of year
# EQAS has bimondal fire season; more sense to start at local min after the 2nd smaller fire season 
# than split season
forced_regions = {'EQAS': 16}

avg_type = 'mean_SMA_3' # used for smoothing the fire seasons

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

# LOAD
df = pd.read_csv(dir_base / fn_in)

#  modify/add input fields
df['dt_utc'] = pd.to_datetime(df['dt_utc'])

df['dt_lst'] = pd.to_datetime(df['dt_lst'])

df['date_lst'] = pd.to_datetime(df['date_lst'])

df['sat_yr'] = df['sat'] + df['year'].astype('str')

# filter unsuitable data
df = df[df['sat_yr'].isin(list_process)]

# AGG DATA BY GFED REGIONS AND CALENDAR YEARS
df_cal_yr = df.groupby(['sat_yr', 'sat', 'year',
                         'gfed', 'gfed_name'])['fn'].agg(['count']).reset_index()

# add in global totals
df_cal_yr_globe = df.groupby(['sat_yr', 'sat', 'year'])['fn'].agg(['count']).reset_index()

df_cal_yr_globe['gfed'] = 99

df_cal_yr_globe['gfed_name'] = 'global'

df_cal_yr = pd.concat([df_cal_yr, df_cal_yr_globe])

# filter unsuitable data
#df_cal_yr = df_cal_yr[df_cal_yr['sat_yr'].isin(list_process)]

# AGGREGATE DATA BY BROAD TEMPERATE/TROPICAL/BOREAL BANDS
df['tropflag'] = 'Temperate'

df.loc[(df.lat < 20) & (df.lat > -20), 'tropflag'] = 'Tropics'

df.loc[(df.lat > 50), 'tropflag'] = 'Boreal'

df_ttb = df.groupby(['sat_yr', 'sat', 
                     'year', 'tropflag'])['fn'].agg(['count']).reset_index()

#df_ttb = df_ttb[df_ttb['sat_yr'].isin(list_process)]

# normalise all years to 1986
df86 = df_ttb.loc[df_ttb.year == 1986].rename(columns={'count': 'count86'})

df_ttb = df_ttb.merge(df86[['tropflag', 'count86']], how='left',
                      on=['tropflag'])

df_ttb['ncount'] = df_ttb['count'] / df_ttb['count86']

df_ttb = df_ttb.drop('count86', axis=1)



# AGGREGATE DATA BY COMPOSITING PERIODS
# (1) First create a date to compositing period lookup table that accounts for leap years
cp_label = np.linspace(start=1, stop=n_cp, num=n_cp)

# repeat each cp_label cp times
cp_label2 = np.repeat(cp_label, len_cp).astype(int)  

df_cp1 = pd.DataFrame()

for year in range(1979, 2019):
    
    dates = pd.date_range(start='1/1/{0}'.format(year), periods=368)
    
    tmp = pd.DataFrame()
    
    tmp['date_lst'] = dates
    
    tmp['comp_period'] = cp_label2
    
    df_cp1 = pd.concat([df_cp1, tmp], ignore_index=True)

# (2) add composite periods to the main df.
# NOTE: Due to final period overlapping calendar years by some days, this increases df size 
# through the addition of duplicates. This is also how the MODIS 8 day compositing period products work.
df2 = df.merge(df_cp1,
               on=['date_lst'],
               how='inner')

# (3) aggregate fires by CP, and expand any NaNs
df_cp2 = df2.groupby(['sat_yr', 'sat', 'year', 
                     'comp_period', 'gfed', 
                     'gfed_name'])['fn'].agg(['count']).reset_index()

# add in global
df_cp_globe = df2.groupby(['sat_yr', 'sat', 'year', 'comp_period'])['fn'].agg(['count']).reset_index()

df_cp_globe['gfed'] = 99

df_cp_globe['gfed_name'] = 'global'

df_cp2 = pd.concat([df_cp2, df_cp_globe])

#df_cp_narrow = df_cp2[df_cp2['sat_yr'].isin(list_process)]  # used for 'df_profiles', avg. regional seas profiles

# some compositing periods / region combinations have no fire.
# need to add zeros here to correctly calculate average fires per cp
# (3a) create a df of all possible regions, years and CPs for df_cp_narrow
a = df_cp2[['sat_yr', 'sat', 'year', 'gfed', 'gfed_name']].drop_duplicates().reset_index(drop=True)

b = df_cp2[['comp_period']].drop_duplicates().reset_index(drop=True)

a['joincol'] = 1

b['joincol'] = 1

c = a.merge(b, how='outer', on='joincol')

c = c.drop(['joincol'], axis=1)

c['dummy'] = 0

# 16 * 23 * 46  (regions[inc mask & global] * narrow years * cps) = 16928
print('expected no. of CP combinations = 16928')
print('actual no. of CP combinations = ', c.shape[0])

# (3b) expand df_cp2 to ensure zeros for cases where no fires detected
df_cp2 = df_cp2.merge(c, how='outer', 
                         on=['sat_yr', 'sat', 'year', 
                         'comp_period', 'gfed', 'gfed_name'])

df_cp2.loc[np.isnan(df_cp2['count']), 'count'] = 0

df_cp2 = df_cp2.drop(['dummy'], axis=1)

print('rows in df_cp2 = ', df_cp2.shape[0])



# CREATE SEASONAL PROFILES BASED ON CP DATA
# (1a) generate 'average' cp profiles used to determine the fire season
df_profiles = df_cp2.groupby(['gfed', 'gfed_name', 'comp_period'])['count']\
                    .agg(['mean', 'median']).reset_index()

# (2b) smooth seasonal profiles
# Averages for CPs of 8 days are quite spikey; smooth using moving average with a window width of 3 cps.
# To avoid NaNs in final smooth output caused by window edges, as data are circular, manually alter the 
# seasonal profiles by adding a comp period 0 and period max(cp)+1 based on cp 1 & max(cp) before smooth, 
# and drop these at the end.
upper_cp = np.max(df_profiles['comp_period'])

extra = df_profiles.loc[(df_profiles['comp_period'] == 1) |
                        (df_profiles['comp_period'] == upper_cp)]\
                            .sort_values(['gfed', 'comp_period'], 
                                         ascending=[True, True])

extra.loc[extra['comp_period'] == 1, 'comp_period'] = upper_cp + 1

extra.loc[extra['comp_period'] == upper_cp, 'comp_period'] = 0

df_profiles = pd.concat([df_profiles, extra])\
                        .sort_values(['gfed', 'comp_period'], 
                                     ascending=[True, True]).reset_index(drop=True)

df_profiles['median_SMA_3'] = df_profiles['median'].rolling(window=3,
                                                            min_periods=None,
                                                            center=True,
                                                            win_type=None,
                                                            on=None, axis=0,
                                                            closed=None).mean()

df_profiles['mean_SMA_3'] = df_profiles['mean'].rolling(window=3,
                                                        min_periods=None,
                                                        center=True,
                                                        win_type=None,
                                                        on=None, axis=0,
                                                        closed=None).mean()

# now remove the 'extra' cps; drop cp=0 & cp=max(cp)+1
df_profiles = df_profiles[(df_profiles['comp_period'] > 0) &
                          (df_profiles['comp_period'] < upper_cp + 1)]



# CONSTRUCT SEASONAL PROFILES FOR GFED REGIONS

# (1) Define fire season for each region
# average (profile derived) fire year starts at comp_period where 'avg_type' is min. 
# e.g. NHAF cp=30 using mean_SMA_3. then calculates average (profile derived) fire season 
# based on season_def[0] < [cumulative proportion of fires] < season_def[1]

# order CPs starting from the period with fewest fires (or CP defined in forced_regions 
# if overriding minima), then designate fire season where:
#     season_def[0] < [cumulative proportion of fires] < season_def[1]

# temporarily duplicate gfed_name for use in apply func below
df_profiles['gfed_name2'] = df_profiles['gfed_name']  

profile_seas = df_profiles.groupby(['gfed', 'gfed_name'])[['gfed_name2', 'comp_period', avg_type]]\
                    .apply(fire_season_calc, colA=avg_type, colB='comp_period',
                           prop=season_def, override=forced_regions)\
                    .reset_index()\
                    .drop(['level_2'], axis=1)

# (2) Get summary stats for the 'average' fire season in each GFED region
profile_summary = profile_seas.groupby(['gfed', 'gfed_name'])

profile_summary = profile_summary[['comp_period', avg_type, 'period_rank', 
                                   'season', 'gfed_name2']].apply(fire_season_summary, 
                                                                  colA=avg_type, 
                                                                  colB='comp_period', 
                                                                  colC='period_rank', 
                                                                  colD='season',
                                                                  override=forced_regions)\
                                                           .reset_index()\
                                                           .drop(['level_2'], axis=1)



# OUTPUT TO FILE
f = (len_cp, avg_type, int(season_def[0] * 100), int(season_def[1] * 100))

"""
GFED_annual_counts_calendaryr.csv                                    df_cal_yr               fire counts aggr annually, for all used calendar years and GFED regions
GFED_{0}day_cp_counts_calendaryr_{1}_season_{2}_{3}.csv              df_cp_narrow            fire counts aggr to (8 day) cps, for all used calendar years and GFED regions
GFED_{0}day_cp_average_profile_{1}_season_{2}_{3}.csv                profile_seas            average fire activity profile for each GFED region (from df_cp_narrow)
GFED_{0}day_cp_average_profile_summary_{1}_season_{2}_{3}.csv        profile_summary         summary stats of average fire activity profile for each GFED region (from df_cp_narrow)
TempTropBoreal_annual_counts_calendaryr                              df_ttb                  fire counts aggr annually, for all used calendar years and trop/temp/boreal
"""

df_cal_yr.to_csv(dir_base / 'GFED_annual_counts_calendaryr.csv', index=False)

df_ttb.to_csv(dir_base / f'TempTropBoreal_annual_counts_calendaryr.csv', index=False)

df_cp2.to_csv(dir_base / 'GFED_{0}day_cp_counts_calendaryr_{1}_season_{2}_{3}.csv'.format(f[0], f[1], f[2], f[3]), index=False)

# this output not used right now
# profile_seas.to_csv(dir_base / 'GFED_{0}day_cp_average_profile_{1}_season_{2}_{3}.csv'.format(f[0], f[1], f[2], f[3]), index=False)

profile_summary.to_csv(dir_base / 'GFED_{0}day_cp_average_profile_summary_{1}_season_{2}_{3}.csv'.format(f[0], f[1], f[2], f[3]), index=False)

print('Finished')
