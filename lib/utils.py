"""
Common functions for data analysis
"""
import sys
import os
import glob
import csv
import numpy as np
import math
import matplotlib as mpl
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
import scipy
from scipy import stats
import netCDF4 as nc
import fnmatch
from osgeo import gdal 
import cartopy
import cartopy.crs as ccrs
import rasterio
from affine import Affine
from datetime import datetime

def get_sorted_paths(d, pattern):
    """
    sorted glob.glob full path output

    Args:
        d: str, path to directory containing a L1B input files
        pattern: str, pattern used to filter input file list by glob

    Returns:
        subdirs: list, of sorted file paths
    """
    subdirs = glob.glob(d + pattern)    
    list.sort(subdirs)
    return subdirs


def output_2d_nc(mygrid, mylats, mylons, fp_out, do_zlib):
    """
    output a simple netcdf file with a single 2d variable named 'var1', with 1d lat lon fields, 
    no time and only 1 level dim
    compression level=4.
    CAUTION! data precision is hard coded... consider changing this depending on application
    
    :param mygrid: 2d np.array of parameter of interest e.g. frp (output prec=float64)
    :param mylats: 1d np.array of latitudes
    :param mylons: 1d np.array of longitudes
    :param fp_out: str, full file path inc fn
    :param do_zlib: boolean, if true, applies compression: complevel=4
    :returns: None
    """
    ##create nc file
    dataset = nc.Dataset(fp_out, 'w', format='NETCDF4')
    #create dims
    level = dataset.createDimension('level', 1) 
    lat = dataset.createDimension('lat', len(mylats))
    lon = dataset.createDimension('lon', len(mylons)) 
    time = dataset.createDimension('time', None)
    #create var
    times = dataset.createVariable('time', np.float64,('time',), zlib=do_zlib, complevel=4) 
    levels = dataset.createVariable('level', np.int32,('level',), zlib=do_zlib, complevel=4) 
    latitudes = dataset.createVariable('lat', np.float32,('lat',), zlib=do_zlib, complevel=4)
    longitudes = dataset.createVariable('lon', np.float32,('lon',), zlib=do_zlib, complevel=4) 
    var1 = dataset.createVariable('var1', np.float64, ('time','level','lat','lon'), zlib=do_zlib, complevel=4)
    ##set values
    latitudes[:] = mylats
    longitudes[:] = mylons
    var1[0,0,:,:] = mygrid
    dataset.close()
    return


def nparray2raster(arr, fp, trans_type, trans, arr_rows, arr_cols, driver='GTiff', count=1, dtype='uint16',
                   compress='lzw', predictor=2, nodata=np.nan, tile=False, crs='EPSG:4326'):
    """
    Write a raster (default=geotif) to disk using the Rasterio library

    NOTE: not tested this with multiband rasters (count > 1)

    for rasterio.open() options see: https://rasterio.readthedocs.io/en/latest/api/rasterio.html#
    several of these are kwargs that i think get passed to gdal e.g. 'tile', 'compress', 'predictor'

    DATA TYPES
    https://gdal.org/drivers/raster/gtiff.html
    "Currently band types of Byte, UInt16, Int16, UInt32, Int32, Float32,
    Float64, CInt16, CInt32, CFloat32 and CFloat64 are supported for reading and writing."

    COMPRESSION
    https://kokoalberti.com/articles/geotiff-compression-optimization-guide/
    I normally use compress='lzw', then predictor=2 for int, and predictor=3 for floats.


    CAREFUL WITH GEOTRANSFORM ELEMENT ORDER!
    https://www.perrygeo.com/python-affine-transforms.html

    element order for trans_type='affine' input:
    1 = width of a pixel
    2 = row rotation (typically zero)
    3 = x-coordinate of the upper-left corner of the upper-left pixel
    4 = column rotation (typically zero)
    5 = height of a pixel (typically negative)
    6 = y-coordinate of the of the upper-left corner of the upper-left pixel

    element order for trans_type='gdal' - input can be from fh.GetGeoTransform():
    1 = x-coordinate of the upper-left corner of the upper-left pixel
    2 = width of a pixel
    3 = row rotation (typically zero)
    4 = y-coordinate of the of the upper-left corner of the upper-left pixel
    5 = column rotation (typically zero)
    6 = height of a pixel (typically negative)

    PARAMS
    :arr: np.array(), probably 2D - might need to modify func for 3D
    :trans_type: str, flag type of input geotransform
    :trans: 6-tuple, used to construct geotransform.
             if trans_type = 'gdal', can be a gdal 'gt' 6-tuple.
             if trans_type = 'affine', must be a 6-tuple with Affine element ordering
    :rows: int, n rows in arr
    :cols: int, n cols in arr
    """

    if trans_type == 'gdal':
        trans = Affine.from_gdal(trans[0], trans[1], trans[2], trans[3], trans[4], trans[5])
    elif trans_type == 'affine':
        trans = Affine(trans[0], trans[1], trans[2], trans[3], trans[4], trans[5])
    else:
        print("error: must specify transform type as either 'gdal' or 'affine")
        sys.exit(1)

    with rasterio.open(
        fp,
        mode='w',
        driver=driver,
        width=arr_cols,
        height=arr_rows,
        count=count,
        dtype=dtype,
        compress=compress,
        predictor=predictor,
        crs=crs,
        nodata=nodata,
        tile=tile,
        transform=trans) as dst:
            dst.write(arr, indexes=1)
    return


def gdalwarpcompress_v1(fp_in, fp_out, target_extent, target_res, proj, dtype, verbose=False):
    """
    Warp and compress a file using commandline GDAL.
    gdalwarp doesn't seem to work with compression; so gdal_translate is needed as 
    the second part of the workflow. To avoid two lengthy operations (and storing 
    a large uncompressed warped file), more efficient to gdalwarp to a VRT then 
    gdal_translate to file.
    As of GDAL2.X this can be achieved in a single operation using piping.
    
    Adapted from here: 
    https://gis.stackexchange.com/questions/89444/file-size-inflation-normal-with-gdalwarp
    https://gdal.org/user/virtual_file_systems.html
    
    USAGE ::::
    gdalwarpcompress_v1(fp_in=fp_in, 
                    fp_out=fp_out, 
                    target_extent=' '.join([str(x) for x in [-180.0, -90.0, 180.0, 90.0]]), 
                    target_res=' '.join([str(x) for x in [0.1, -0.1]]), 
                    proj='EPSG:4326', 
                    dtype='float')
    
    
    PARAMS ::::
    :fp_in: str
    :fp_out: str
    :target_extent: str, in format '<xmin ymin xmax ymax>' e.g. '-180.0 -90.0 180.0 90.0'
    :target_res: str, in format '<xres> <yres>' e.g. '0.1 -0.1'
    :proj: str, EPSG format e.g. 'EPSG:4326'
    :dtype: str, 'int' or 'float'. needed to select compression predictor.
    :verbose: bool, determines whether the GDAL cmd is printed
    """

    if dtype == 'int':
        pred = 2
    elif dtype == 'float':
        pred = 3
    else:
        print('error in data type choice "int" or "float"')
        sys.exit(1)


    command = (
        "gdalwarp -t_srs {0} "
        "-te {1} -tr {2} -of vrt "
        "{3} /vsistdout/ | gdal_translate "
        "-co COMPRESS=LZW -co NUM_THREADS=ALL_CPUS -co PREDICTOR={4} "
        "/vsistdin/ {5}".format(proj, target_extent, target_res, fp_in, pred, fp_out)
        )
    
    if verbose:
        print(command)
    print("Start Time =", datetime.now().strftime("%H:%M:%S"))
    result = os.system(command)
    print("Finish Time =", datetime.now().strftime("%H:%M:%S"))
    return result


def calc_grid_cell_area(lat1d, lon1d):
    """
    Calculates the grid cell area for each grid cell in the latitudinal direction

    ::params::
    :lat1d: np.array, 1d array of grid cell latitudes
    :lon1d: np.array, 1d array of grid cell longitudes
    ::returns:: 1d np.array of grid cell area (in m^2) between [-90,90] degrees latitude
    """
    earth_radius = 6367470.0 # m
    # all equatorial grid cells have this area
    equatorial_cell_area = (2. * np.pi * earth_radius / 360.)**2 \
        * abs(lat1d[1]-lat1d[0]) \
        * abs(lon1d[1]-lon1d[0])
    # area of grid cells at all latitude intervals
    area = equatorial_cell_area * np.cos(lat1d * np.pi / 180.)
    return area


def calc_weighted_avg(arr, w1d, mask=None):
    """
    Calculate weighted mean from a 2d array of values and a 1d array of weights, 
    properly (i think!) accounting for NaNs.
    Intended to be applied to a 2d global grid with output of calc_grid_cell_area() 
    as latitudinal grid cell weights (i.e. grid cell areas in m^2).
      
    [0,1 ] or [nan,1] mask can be optionally used to subset to specific ROIs

    weighted mean = sum(weight_i * value_i) / sum(weight_i)
    
    ::params::
    arr: 2d np.array of values to be averaged
    w1d: 1d np.array of weights (grid cell areas) corresponding to the 1st (latitudinal) dim of arr
    mask: 2d np.array of binary (or 1 and np.nan) values. arr is retained where mask = 1. 
    ::returns::
    wmean: float, weighted mean value
    """
    # generate 2d weights 
    w = np.ones((arr.shape[0], arr.shape[1])) * w1d[:, np.newaxis]
    # mask weights where arr is nan otherwise 'w_sum' is wrong
    w[np.isnan(arr)] = np.nan
    
    if mask is not None:
        w = w * mask
    
    # calc weighted mean
    w_x_arr = w * arr 
    w_x_arr_sum = np.nansum(w_x_arr) 
    w_sum = np.nansum(w)
    wmean = w_x_arr_sum / w_sum 
    
    return wmean


def grid_coarsen(arr, c):
    """
    create new array with x,y based on coarsening factor, and sum counts of arr to new grid cell res
    applies to each level of z (ax=0)
    :param arr: 3d array
    :param c: factor by which to coarsen x and y
    :return: arr_new, the coarsened array
    """
    arr_new = np.zeros([arr.shape[0], int(arr.shape[1]/c), int(arr.shape[2]/c)])
    for z in range(arr.shape[0]):
        for ii in range(c):
            for jj in range(c):
                arr_new[z] += arr[z, ii::c, jj::c]
    return arr_new


def geo_plot_v2(lons=None, lats=None, arr=None, extent=None,
             in_proj=ccrs.PlateCarree(), out_proj=ccrs.PlateCarree(), 
             cblab=None,
             stock_fill=False, 
             grid_options={'color': 'lightgrey', 'linestyle': ':', 'alpha': 0.7, 'linewidth': 0.25, 'draw_labels': False},
             mesh_options={'norm': mpl.colors.Normalize()}, 
             feature_res='110m',
             draw_features=['land', 'lakes', 'coast', 'countries'],
             ocean_ops={'linewidth': 0.25, 'edgecolor': 'black', 'facecolor': cartopy.feature.COLORS.get('water')},
             coast_ops={'linewidth': 0.25, 'edgecolor': 'black'}, 
             land_ops={'linewidth': 0.25, 'edgecolor': 'black', 'facecolor': cartopy.feature.COLORS.get('land_alt1')},
             country_ops={'linewidth': 0.25, 'edgecolor': 'black', 'facecolor': 'None'},
             state_ops={'linewidth': 0.25, 'edgecolor': 'black', 'facecolor': 'None'},
             lake_ops={'linewidth': 0.25, 'edgecolor': 'black', 'facecolor': 'w'},
             river_ops={'linewidth': 0.25, 'edgecolor': cartopy.feature.COLORS.get('water'), 'facecolor': 'None'},
             figsize=(11.69, 8.27)
             ):
    """
    plot cartopy map with (optional) pcolormesh overlay
    can convert a series of WGS84 inputs to a specified output projection
    NOTE: not all projections are compatible with the default global extent and cartopy.features
    
    v2: + added better cartopy feature functionality and control e.g. oceans, borders etc 
    
    mesh_options:
        'norm': mpl.colors.LogNorm()     #log colorscale
        'norm': mpl.colors.Normalize()   #linear colorscale
    
    drawing various vector features:
    'coast' can be useful even if 'land' and/or 'ocean' plotted, as I think the artist has a higher z-order and will plot over pcolormesh 
    cartopy.feature.COLORS is a dict of useful colors for plotting, can use like this:
        cartopy.feature.COLORS.get('water')     # blue
        cartopy.feature.COLORS.get('land')      # beige
        cartopy.feature.COLORS.get('land_alt1') # grey (shows up white things better)

    helpful tutorials on cartopy:
    > https://scitools.org.uk/cartopy/docs/v0.14/matplotlib/feature_interface.html
    > https://rabernat.github.io/research_computing_2018/maps-with-cartopy.html
    > https://clouds.eos.ubc.ca/~phil/courses/eosc582/html/cartopy_1.html

    TODO: modify to:
        (2) option to do imshow version instead of pcolormesh
        (3) add way to pass more options to artists e.g. for changing axis font sizes

    :param lons:          nparray, 2d, of longitudes
    :param lats:          nparray, 2d, of latitudes
    :param arr:           nparray, 2d, z values
    :param extent:        list, [left, right, bottom, top] extents of output map (provide in WGS84 degrees only?)
    :param in_proj:       ccrs projection, e.g. ccrs.PlateCarree()
    :param out_proj:      ccrs projection, e.g. ccrs.NorthPolarStereo()
    :param cblab:         str, label for cb, don't plot colorbar if None   
    :param stock_fill:    bool, flag to determine whether to fill background with ax.stock_img()
    :param grid_options:  dict, kwargs for ax.gridlines(**grid_options)
    :param mesh_options:  dict, ax.pcolormesh(**mesh_options)
    :param feature_res:   str, '110m', '50m' or '10m'. defines resolution of background vector features
    :param draw_features: list of str, determines whether to draw any/all of ['countries', 'states', 'coast', 'land', 'lakes', 'rivers', 'ocean']
    :param ocean_ops:     dict of kwargs for oceans {'linewidth': 0.25, 'edgecolor': 'black', 'facecolor': cartopy.feature.COLORS.get('water')},
    :param coast_ops:     dict of kwargs for coast {'linewidth': 0.25, 'edgecolor': 'black'}, 
    :param land_ops:      dict of kwargs for land {'linewidth': 0.25, 'edgecolor': 'black', 'facecolor': cartopy.feature.COLORS.get('land_alt1')},
    :param country_ops:   dict of kwargs for countries {'linewidth': 0.25, 'edgecolor': 'black', 'facecolor': 'None'},
    :param state_ops:     dict of kwargs for states {'linewidth': 0.25, 'edgecolor': 'black', 'facecolor': 'None'},
    :param lake_ops:      dict of kwargs for lakes {'linewidth': 0.25, 'edgecolor': 'black', 'facecolor': 'w'},
    :param river_ops:     dict of kwargs for rivers {'linewidth': 0.25, 'edgecolor': cartopy.feature.COLORS.get('water'), 'facecolor': 'None'}
    :param figsize:       tuple of floats, determines figure size
    :return: fig, ax, mesh
    """

    fig = plt.figure(figsize=figsize)
    ax = plt.axes(projection=out_proj)
    if extent:
        ax.set_extent(extent, crs=in_proj)
    else:
        ax.set_global()

    # background options
    if stock_fill:
        ax.stock_img()
    
    if 'ocean' in draw_features:
        ax.add_feature(cartopy.feature.OCEAN.with_scale(feature_res), **ocean_ops)  

    if 'coast' in draw_features:
        ax.add_feature(cartopy.feature.COASTLINE.with_scale(feature_res), **coast_ops)

    if 'land' in draw_features:
        ax.add_feature(cartopy.feature.LAND.with_scale(feature_res), **land_ops)

    if 'countries' in draw_features:
        ax.add_feature(cartopy.feature.BORDERS.with_scale(feature_res), **country_ops)

    if 'states' in draw_features:
        states = cartopy.feature.NaturalEarthFeature('cultural', 'admin_1_states_provinces_lines', feature_res)
        ax.add_feature(states, **state_ops)

    if 'lakes' in draw_features:
        ax.add_feature(cartopy.feature.LAKES.with_scale(feature_res), **lake_ops)

    if 'rivers' in draw_features:
        ax.add_feature(cartopy.feature.RIVERS.with_scale(feature_res), **river_ops)    

    if grid_options:
        gl = ax.gridlines(crs=in_proj, **grid_options)
        if grid_options.get('draw_labels'):
            gl.top_labels = False     #gl.xlabels_top = False
            gl.right_labels = False   #gl.ylabels_right = False

    if arr is not None:
        im = ax.pcolormesh(lons, lats, arr, transform=in_proj, **mesh_options)
        # make a simple colorbar - often better to set 'cblab=None' & use dedicated cbar function e.g. colorbar_v3
        if cblab:
            cax, kw = mpl.colorbar.make_axes(ax, location='right', pad=0.05, shrink=0.7)
            out = fig.colorbar(im, cax=cax, extend='both', **kw)
            label = out.set_label(cblab, size=10, rotation=-90, verticalalignment='bottom')
    else:
        im = None
    return fig, ax, im


def colorbar_v4(fig, ax, mesh, fontsize=8, ticks=None,
                ticklab=None, cblab=None, 
                cbax_options={'location': 'right', 'pad': 0.05, 'shrink': 1, 'aspect': 50}, 
                cb_options={'orientation': 'vertical', 'extend': 'neither'}):
    """
    A nice way to deal with colorbar sizing on mpl plots
    
    v2: + works on cartopy 'geoaxes' objects as well as regular matplotlib objects, 
          so probably superceeds 'colorbar' func
    v3: + uses 'set_clim' to place ticks at patch mid-points on discrete CBs. 
          see: https://jakevdp.github.io/PythonDataScienceHandbook/04.07-customizing-colorbars.html
          Doesn't seem to negatively impact continuous CBs 
        + added arg to control scale of cbar, as best option varies depending on specific fig/subplot setup 
    v4: + revised to make better use of kwargs:
          cbax_options: kwargs for make_axis.
          NOTE: if 'location': 'right', probably want 'orientation': 'vertical'

    ** ticks could possibly be better in kwargs?
	
    NOTE: take care when making a 'banded' discrete map of continuous values (e.g. 0.1, ... 1 with 0.79 etc).
    extent of each class will be abs(ticks[0] - ticks[1]) / 2 CENTRED on the tick values!
          
    :param fig:  mpl fig containing ax, mesh
    :param ax:   mpl axes containing mesh
    :param mesh:  matplotlib.cm.ScalarMappable (i.e., Image, ContourSet, etc.)
    :param fontsize: int
    :param ticks: list of floats, e.g. [0.5, 0.8, 0.16]
    :param ticklab: list of str, used as labels on cb - only used if 'ticks' not None. must be same length as 'ticks'
    :param cbax_options: dict, **kwargs for mpl.colorbar.make_axes(). 'shrink' scales cbar relative to ax, aspect changes width/height ratio
    :param cb_options: dict, **kwargs for fig.colorbar()
    :return: colorbar object
    """
    #cax, kw = mpl.colorbar.make_axes(ax, location='right', pad=0.05, shrink=cbar_scale)
    cax, kw = mpl.colorbar.make_axes(ax, **cbax_options)    
    if ticks:        
        out = fig.colorbar(mesh, cax=cax, ticks=ticks, **cb_options)
        # ~~~~~ new for v3 ~~~~~
        # need 'set_clim' as default behaviour doesn't allow segments 
        # and ticks to line up properly. 
        offset = abs(ticks[0] - ticks[1]) / 2 
        out.mappable.set_clim(np.min(ticks) - offset, np.max(ticks) + offset)
        print(f'NOTE: discrete colorbar specified. ensure that ticks match ticklabs! tickrange={np.min(ticks)}-{np.max(ticks)} clim={np.min(ticks)-offset}-{np.max(ticks)+offset}, offset={offset}')
        #~~~~~~~~~~~~~~~~~~~~~~~
        if cb_options.get('orientation') == 'vertical':
            out.ax.set_yticklabels(ticklab, fontsize=fontsize)
        elif cb_options.get('orientation') == 'horizontal':
            out.ax.set_xticklabels(ticklab, fontsize=fontsize)
        else:
            print('error with cb_options - orientation')	
    else:
        out = fig.colorbar(mesh, cax=cax, **cb_options)
        out.ax.tick_params(labelsize=fontsize)

    if cblab:
        if cbax_options.get('location') == 'right':
            label = out.set_label(cblab, size=fontsize, rotation=-90, verticalalignment='bottom')
        elif cbax_options.get('location') == 'bottom':
            label = out.set_label(cblab, size=fontsize, rotation=0, verticalalignment='top')
        else:
            pass
    return out


def grid_kendall_no_nan_v2(arr, all_years, norm, thresh):
    """
    + for each grid cell in arr, get the non-nan values and corresponding years.
    + norm fire counts to first year
    + calculate correlation parameters and sen slope
    
    v2: 
     + normalising is now controlled by arg
     + extreme_prop threshold is now controlled by arg
     + if fires[0] == 0, norm using fires = fires + 1
            
    :param arr: fire count array
    :param all_years: list of years relating to z dim of array
    :norm: boolean, if true, normalise the fire counts to first non-nan value
    :return: kendall's tau, p value, sen slope
    """
    t = np.zeros_like(arr[0])
    p = np.zeros_like(arr[0])
    s = np.zeros_like(arr[0])
    for y in range(arr.shape[1]):
        for x in range(arr.shape[2]):
            #print(x)
            depth_mask = ~np.isnan(arr[:, y, x])
            yrs = np.array(all_years)[depth_mask]
            fires = arr[:, y, x][depth_mask]
            
            # very low fire areas often incorrectly flagged as sig trend. filter these out.
            # calculate 'extremes' prop where fire count = 0 or 1
            extremes = np.sum(fires == 0) + np.sum(fires == 1)
            extreme_prop = extremes / len(fires)
            # norm to first value
            if norm:
                if fires[0] == 0:
                    fires = fires + 1
                else:
                    fires = fires / fires[0]
            
            #calc slope, intercept etc
            if (len(fires) > 0) & (extreme_prop < thresh):
                tau, p_value = stats.kendalltau(x=yrs, y=fires)
                slope, intercept, _, _ = stats.theilslopes(x=yrs, y=fires)
                t[y, x] = tau
                p[y, x] = p_value
                s[y, x] = slope
            else:
                t[y, x] = np.nan
                p[y, x] = np.nan
                s[y, x] = np.nan
    return t, p, s



def geo_plot_ax(fig, ax, lons=None, lats=None, arr=None, extent=None,
                in_proj=ccrs.PlateCarree(), out_proj=ccrs.PlateCarree(), cblab=None,
                stock_fill=False, coast_options={'linewidth': 0.75, 'resolution': '110m'},
                grid_options={'color': 'lightgrey', 'linestyle': '-', 'alpha': 0.7, 'draw_labels': False},
                mesh_options={}):
    """
    
    modified version of 'geo_plot' func to work with an existing axis object.

    plot cartopy map with (optional) pcolormesh overlay
    can convert a series of WGS84 inputs to a specified output projection
    testing suggests not all projections are compatible with the default global extent

    have to pass parent fig to do a colorbar 
    ax must have been created as a geoaxis (e.g. cartopy.mpl.geoaxes.GeoAxesSubplot ):
    
    usage: 
    fig, axes = plt.subplots(figsize=(8.27, 11.69), nrows=4, ncols=2, 
                             subplot_kw={'projection': ccrs.PlateCarree()})  
   
    helpful tutorials on cartopy:
    > https://rabernat.github.io/research_computing_2018/maps-with-cartopy.html
    > https://clouds.eos.ubc.ca/~phil/courses/eosc582/html/cartopy_1.html

    TODO: modify to:
        (2) option to do imshow version instead of pcolormesh
        (3) add way to pass more options to artists e.g. for changing axis font sizes

    :param fig: parent fig to ax   
    :param ax: cartopy.mpl.geoaxes.GeoAxesSubplot 
    :param lons: nparray, 2d, of longitudes
    :param lats: nparray, 2d, of latitudes
    :param arr:  nparray, 2d, z values
    :param extent: list, [left, right, bottom, top] extents of output map (provide in WGS84 degrees only?)
    :param in_proj: ccrs projection, e.g. ccrs.PlateCarree()
    :param out_proj: ccrs projection, e.g. ccrs.NorthPolarStereo()
    :param cblab: str, label for cb, don't plot colorbar if None
    :param stock_fill: bool, flag to determine whether to fill background with ax.stock_img()
    :param coast_options: dict, kwargs for ax.coastlines(**coast_options)
    :param grid_options: dict, kwargs for ax.gridlines(**grid_options)
    :param mesh_options: dict, ax.pcolormesh(**mesh_options)
    :return: ax, mesh
    """

    #ax = plt.axes(projection=out_proj)
    if extent:
        ax.set_extent(extent, crs=in_proj) # should this be out_proj?
    else:
        ax.set_global()

    if coast_options:
        ax.coastlines(**coast_options)

    if grid_options:
        gl = ax.gridlines(crs=in_proj, **grid_options)
        if grid_options.get('draw_labels'):
            gl.top_labels = False     #gl.xlabels_top = False
            gl.right_labels = False   #gl.ylabels_right = False

    if stock_fill:
        ax.stock_img()

    if arr is not None:
        im = ax.pcolormesh(lons, lats, arr, transform=in_proj, **mesh_options)
        # make a simple colorbar
        if cblab:
            cax, kw = mpl.colorbar.make_axes(ax, location='right', pad=0.05, shrink=0.7)
            out = fig.colorbar(im, cax=cax, extend='both', **kw)
            label = out.set_label(cblab, size=10, rotation=-90, verticalalignment='bottom')
    else:
        im = None
    return ax, im


def grid_coarsen_v2(arr, lat1d, lon1d, factor, algo):
    """
    + Coarsen a 2d or 3d array by a 'factor', using algo choice of 'sum', 
      'mean', or 'area_mean'.

    :: Inputs ::
    arr: 2d or 3d np.array, to be coarsened
    lat1d:  1d np.array(), of latitudes matching arr
    lon1d:  1d np.array(), of longitudes matching arr
    factor: int, number of grid cells of arr to be coarsened in both x & y direction
    algo:   str, 'sum', 'mean', or 'area_mean' to calculate grid cell weighted avg (latter should
            work with e.g. FRP density)
    
    :: Returns ::
    arr_new: 2d or 3d np.array of values
    newLat:  1d np.array of new arr latitude
    newLon:  1d np.array of new arr latitude
    """
    earth_radius = 6367470.0 # m

    if lat1d.shape[0] % factor or lon1d.shape[0] % factor:
        sys.exit(f'Cannot coarsen {lat1d.shape[0]} x {lon1d.shape[0]} field with factor {factor}.')

    # for flexibility, works on 3d arrays. Extra dim removed later if necc.
    input_length = len(arr.shape)
    if input_length < 3:
        arr = arr[np.newaxis,:,:]

    # 1) area for new grid ================
    # all equatorial grid cells have this area
    old_equatorial_cell_area = (2. * np.pi * earth_radius / 360.)**2 \
                                * abs(lat1d[1]-lat1d[0]) \
                                * abs(lon1d[1]-lon1d[0])
    # area of grid cells at all latitude intervals
    old_area = old_equatorial_cell_area * np.cos(lat1d * np.pi / 180.)

    # 2) lat/lon/area for new grid ========
    newLat = np.linspace(lat1d[:factor].mean(), 
                         lat1d[-factor:].mean(), 
                         int(lat1d.shape[0]/factor)) 
    newLon = np.linspace(lon1d[:factor].mean(), 
                         lon1d[-factor:].mean(), 
                         int(lon1d.shape[0]/factor))
    # all equatorial grid cells have this area
    new_equatorial_cell_area = (2. * np.pi * earth_radius / 360.)**2 \
                                * abs(newLat[1]-newLat[0]) \
                                * abs(newLon[1]-newLon[0])
    # area of grid cells at all latitude intervals
    new_area = new_equatorial_cell_area * np.cos(newLat * np.pi / 180.)

    #3) area weighting =====================
    if algo=='area_mean':
        arr_old = arr * old_area[:,np.newaxis]
    else:
        arr_old = arr

    #4) coarsen to new grid ================
    arr_new = np.zeros([arr_old.shape[0], int(arr_old.shape[1]/factor), int(arr_old.shape[2]/factor)])   
    # sum the old values 
    for z in range(arr_old.shape[0]):
        for i in range(factor):
            for j in range(factor):
                arr_new[z] += arr_old[z, i::factor,j::factor]
    # transform
    if algo == 'sum':
        arr_new = arr_new
    elif algo == 'mean':
        arr_new = arr_new / factor**2
    elif algo == 'area_mean':
        arr_new = arr_new / new_area[np.newaxis, :, np.newaxis]
    else:
        sys.exit("ERROR: wrong algo type! use 'sum', 'mean', or 'area_mean'")
    if input_length < 3:
        arr_new = arr_new[0,:,:]
        
    return arr_new, newLat, newLon


def get_qual_cmap(cmap_str, n):
    """
    returns a LinearSegmentedColormap object useful for generating plots with discrete data
    resulting cmap has n colours evenly spread along an existing mpl cmap
    
    ****
    is this actually superceded by just doing the below?:
    plt.cm.get_cmap('Blues', 6)
    https://jakevdp.github.io/PythonDataScienceHandbook/04.07-customizing-colorbars.html
    ****

    usage:
    qual_cmap = get_qual_cmap('nipy_spectral', 11)

    :param cmap_str: str, a viable matplotlib colormap name
    :param n: float/int, number of discrete categories for the new cmap
    :return: LinearSegmentedColormap obj
    """

    cmap = plt.cm.get_cmap(cmap_str)
    cmaplist = [cmap(i) for i in range(cmap.N)]
    ix = np.round(np.linspace(0, len(cmaplist) - 1, n)).astype(int)
    cmaplist = [cmaplist[xx] for xx in ix]
    out_cmap = mpl.colors.LinearSegmentedColormap.from_list('Custom cmap', cmaplist, n)
    return out_cmap


def list_to_listed_cmap(d):
    """
    very specific func to create a ListedColormap from a list of 3 element lists
    where each 3 element sub-list represents an rgb color from 0-255
    e.g. d = [[255,255,100], ..., [0,100,0]]
    
    this was created because to generate an rgb ListedColormap, 
    need tuple input between 0-1 (i.e. div by 255)
    and it is fiddly to apply operators to tuples or lists 
    :param: d, a list of 3 element sub-lists, where elements are in [0,255]
    :return: mpl.colors.ListedColormap() object
    """
    tmp = [tuple(np.divide(np.array(x), 255)) for x in d]
    return mpl.colors.ListedColormap(tmp)


def grid_corr_stat(arr_a, arr_b, statistic, do_first_differencing=None):
    """ 
    use scipy.stats.linregress to calculate pearsons r/kendalls tau and p-values at each (x,y) location for two 3d arrays
    if gaps (NaN) in a(x,y) or b(x,y), filter these data in both a & b
    optionally applies first differencing to both filtered datasets before calculating statistic
    
    """
    
    stat_arr = np.zeros_like(arr_a[0,:,:])
    p_arr = np.zeros_like(arr_a[0,:,:])

    for y in range(arr_a.shape[1]):
        for x in range(arr_a.shape[2]):
            depth_mask = ~np.isnan(arr_a[:,y,x]) & ~np.isnan(arr_b[:,y,x])
            a = arr_a[:, y, x][depth_mask]
            b = arr_b[:, y, x][depth_mask]            
            
            if (len(a) > 0): # handle locations with no data                
                if do_first_differencing:
                    a = a[1:] - a[0:-1]
                    b = b[1:] - b[0:-1]                
                
                if statistic == 'pearson':
                    slope, intercept, stat, p, std_err = scipy.stats.linregress(a, b)                
                elif statistic == 'kendall':
                    stat, p = scipy.stats.kendalltau(x=a, y=b)
                    #slope, intercept, _, _ = scipy.stats.theilslopes(x=yrs, y=fires)
                else:
                    print('incorrect statistic selected, options are "pearson","kendall"')
                    return                
                stat_arr[y, x] = stat
                p_arr[y, x] = p
                
            else:
                stat_arr[y, x] = np.nan
                p_arr[y, x] = np.nan   
    return stat_arr, p_arr


def kendall_theilsen(x, y):
    """
    Calculate Kendall's statistics and thiel sen slopes
    
    :param x: years
    :param y: fire count
    :return: kendall's tau, p value, sen slope, sen intercept, upper(lower) 95% slope CI
    """
    tau, p_value = stats.kendalltau(x=x, y=y)
    slope, intercept, lower, upper = stats.theilslopes(x=x, y=y, alpha=0.95)

    return tau, p_value, slope, intercept, lower, upper


def round_with_5_offset(n, prec): 
    """
    helper function to round numbers to given precision ensuring the final digit is a 5
    Args:
        n: float, value to round
        prec: int, precision to round to
    Returns:
        rounded value
    """
    offset = 5. / 10. ** prec
    return (np.round(n + offset, prec - 1) - offset)

