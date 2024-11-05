# avhrr-fire

AVHRR L1B imagery fire detection algorithms and data analysis.

Project programming languages: Python, R, and FORTRAN

The processed active fire 'hotspot' data generated and analysed by this code are available to reviewers from the weblink in the manuscript. Upon publication, a link to the dataset and DOI will be made available here.

## Data requirements
All raw data used in this study are publicly available. 
- AVHRR GAC Level 1B data are available at: http://catalogue.ceda.ac.uk/uuid/b0e6eb5efa623c837d10a233256cc9ce.
- Daily maximum VPD data from Jain et al., (2021) are available at: 560 https://zenodo.org/record/5567062#.Yt_VeHbMJD8.
- Fire weather data analysed from the NASA Global Fire Weather Database (GFWED) (v2.0) are available at: https://data.giss.nasa.gov/impacts/gfwed/ and the Copernicus Emergency Management Service for the European Forest Fire Information System (CEMS-EFFIS) fire danger indices historical dataset (v3.1) is available at https://doi.org/10.24381/cds.0e89c522.
- Study regions (aka ‘GFED basis regions’) are available at: https://www.globalfiredata.org/.
- The masking process used to screen out non-fire activity uses the MODIS landcover 2016 dataset available at: https://doi.org/10.5067/MODIS/MCD12C1.006 and the Global Volcanism Program database (v4.11.0) available at: https://doi.org/10.5479/si.GVP.VOTW4-2013 

## Structure of this repository
## /lib
common python scripts containing paths and functions used throught the project.

- paths.py: a file containing paths to all directories on the user’s system. For security reasons, this file is not uploaded to the repository, and should be created and configured by the user based on the file ‘paths_DUMMY.py’.
- paths_DUMMY.py: an example ‘paths.py’ file that should be used to generate ‘paths.py’ by configuring all file paths directories to match the user’s local system. 
- utils.py: contains functions called repeatedly by scripts in this repository. 

## /src
### /00_fortran
- FORTRAN code in this directory is used to preprocess, perform quality control, and converted AVHRR L1B orbit files to netcdf files. This code needs to be compiled before running, with the paths to NetCDF-4 libraries in the Makefile modified to match the user’s system. The resulting executable file ‘extract_l1b_data.exe’ is then called later by python in ‘01_avhrr_process_jasmin_v01.py’ and ‘02_extract_orbit_timing_v01.py.’

### /01_process_l1b_data
scripts in this directory deal with the raw AVHRR L1B orbit files. These should be run on a high performance computer, such as JASMIN (https://www.ceda.ac.uk/services/jasmin/)
- 01_avhrr_process_jasmin_v01.py: used for batch processing AVHRR L1B files. It first converts the input L1B AVHRR files to netcdf format using the previously compiled FORTRAN executable, and then calls the fire detection code that identifies active fires (identify_fires_jasmin_v01.py).
- 02_extract_orbit_timing_v01.py: extracts single pixel information for every orbit that intersects a specified lat long point for later analysis.
- 03_drift_compile_v01.py: compiles all the single orbit file outputs from 02_extract_orbit_timing_v01.py into a more manageable file output. This information is used later to do orbital drift analysis in script 04_overpass_times_and_orbital_drift.ipynb.
- identify_fires_jasmin_v01.py: contains the code implementation of the fire detection algorithm described in the methods section of the manuscript.

### /02_process_avhrr_hotspots
Scripts in this directory preprocess the hotspot data prior to data analysis.
- 01a_preprocess_avhrr_lst.py: add local solar time information to the AVHRR hotspot data and perform some basic data cleaning steps.
- 01b_preprocess_avhrr_mask.py: mask the AVHRR hotspot data using a variety of criteria and auxiliary datasets described in the manuscript, and outputs all data as  a single file 'avhrr_hotspots_masked.csv'.
- 02_preprocess_avhrr_aggregation.py: Aggregate AVHRR hotspot data produced in ‘01b_preprocess_avhrr_mask.py’ by GFED regions and broad biomes for calendar years and 8-day compositing periods.
- 03_grid_avhrr.py: aggregate the hotspot data produced in ‘01b_preprocess_avhrr_mask.py’ to a global grid to generate gridded annual number of fires (ANF) metric used for map plotting. Default is to aggregate to 0.1-degree grid at this stage. 

### /03_process_fire_weather_data
Scripts in this directory are used to process raw fire weather  data. Note: code in this directory needs running on linux for the utils.nparray2raster function (based on GDAL)  to work correctly.
- 1a_preprocess_cpc.py: preprocess the GFWED CPC dataset outputting min, max, and mid point arrays, and the number of days per calendar year beyond the midpoint. 
- 1b_preprocess_merra.py: preprocess the GFWED MERRA2 dataset outputting min, max, and mid point arrays, and the number of days per calendar year beyond the midpoint. 
- 1c_preprocess_geff.py: preprocess the ECMWF GEFF dataset outputting min, max, and mid point arrays, and the number of days per calendar year beyond the midpoint. 
- 2_mask_and_resample.py: mask all the fire weather datasets  from the previous three ‘preprocess’ scripts and convert to .tif outputs. 
- 3_fire_weather_data_aggregation.py: combine the annual files for each fire weather dataset into 3d arrays used for gridded trend calculation and apply a single unified mask (masks for each individual dataset differ in extent). The common mask generated here is also applied to VPD data later in ‘01_process_vpd_data.py’. This script also calculates area weighted regional means using the gridded data for both the GFED regions and biomes (tropical, temperate, boreal) that are used extensively in the ‘05_data_analysis’ scripts. 

### /04_process_vpd_data
Script in this directory is used to process raw VPD data. Note: code in this directory needs running on linux for the utils.nparray2raster function (based on GDAL)  to work correctly.
- 01_process_vpd_data.py: process the VPD dataset to get gridded and regional outputs for data analysis. Get min/mean/median/max statistics from raw data, convert to tifs, resample, and apply a unified mask before outputting  a 3d array of VPD for trend analysis. This script also calculates area weighted regional means using the gridded data for both the GFED regions and biomes (tropical, temperate, boreal) that are used extensively in the ‘05_data_analysis’ scripts. 

### /05_data_analysis
scripts in this directory produce the figures, tables and statistics used in the manuscript.
- 01_mapped_analyses.ipynb: Figures/Tables generated:
  - Figure 1 - Trends in ANF 
  - Figures 2A & 2B - Trends in FWSL and VPD 
  - Figures 2C & 2D - (Dis)agreement between ANF and FWSL / VPD 
  - Extended Data Figure 3A - Mapped GFED regions 
  - Extended Data Figure 3B - Mean annual AVHRR fire counts  
  - Extended Data Figure 3C & 3D - Correlation between ANF and FWSL / VPD 
- 02_timeseries_trends_vpd.ipynb: Figures/Tables generated:
  - Figure 1 (inset) - VPD vs ANF plot (global timeseries)
  - Figure 3 - VPD vs ANF trend plots for 6 regions
  - Extended Data Figure 4 - VPD vs ANF trend plots for all GFED regions and biomes
- 03_timeseries_trends_FWSL.ipynb: Figures/Tables generated:
  - Extended Data Figure 5 - FWSL vs ANF trend plots for all GFED regions and biomes
- 04_seasonality_and_boxplots.ipynb: Figures/Tables generated:
  - Figure 4 - boxplots for annual and seasonal metrics
  - Figure 5 - Seasonal profiles
  - Extended Data Figure 6 - regional trends in seasonal metrics
- 05_assemble_tables.ipynb: Figures/Tables generated:
  - Extended data table 1 – global and regional scale trends in fire and climate metrics
  - Extended data table 2 – pre-MODIS vs MODIS era differences in fire and climate metrics

- change_point_identification.r: Uses intermediate outputs from the following data analysis scripts to identify any potential change points in time series data. The output change point plots are examined and compared against the criteria in the manuscript to identify ‘valid’ change points. Where present, the years identified as a change point are used in the analysis scripts below to incorporate the change points in the timeseries plots:
  - 02_timeseries_trends_vpd.ipynb ('avhrr_and_vpd_fulljoin_1986-2016.csv’, 'avhrr_and_vpd_fulljoin_1986-2016_TropTempBoreal.csv'),  
  - 03_timeseries_trends_FWSL.ipynb ('avhrr_and_fire_weather_fulljoin_1986-2016.csv' & 'avhrr_and_fire_weather_fulljoin_1986-2016_TropTempBoreal.csv'), 
  - 04_seasonality_and_boxplots.ipynb ('GFED_fire_season_length.csv' & 'GFED_peak_fire_activity_magnitude.csv')

### /06_intercomparison
scripts in this directory are used to (1) preprocess data for, and perform, intercomparison between AVHRR data and data from other sources, and (2) explore the effects of orbital drift

- 01_avhrr_country_level_annual_totals.py: aggregates annual hotspots by country, using a shapefile "ne_50m_admin_0_countries.shp"of country boundaries downloaded from ‘natural earth’. 
- 02a_preprocess_modis_aqua_lst.py: Preprocess MODIS AQUA data so that it is as comparable as possible with AVHRR data by applying the same preprocessing.
- 02b_preprocess_modis_aqua_mask.py: Mask and compile the Aqua MODIS auxiliary data so that it is as comparable to AVHRR data as possible.
- 03_intercomparison_vs_other_data.ipynb: Generates Extended Data Figures 1 & 2. Intercomparison of AVHRR annual fire count data vs.:
  - USA NIFC fire terrestrial records
  - Canada NFDB terrestrial records
  - European EFFIS terrestrial records 
  - MODIS Aqua active fire data (nighttime and daytime)
- 04_overpass_times_and_orbital_drift.ipynb: analyses the outputs of 02_extract_orbit_timing_v01.py and 03_drift_compile_v01.py to produce extended data figures 7 and 8. Extended data figure 7 illustrates the creation of the AVHRR active fire dataset from the full AVHRR record. Extended data figure 8 presents an analysis of the residual impact of orbital drift on the AVHRR active fire dataset.


