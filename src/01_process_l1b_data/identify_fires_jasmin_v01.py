import sys
import glob
import numpy as np
import pandas as pd
from netCDF4 import Dataset


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


def load_and_fill_bad(fp):
    """
    Load the relevant L1B data into a dictionary and mask bad values
    
    args:
        fp: str, file path to an L1B orbit file
    
    returns: 
        orbit_dict: dict, contains all relevant L1B data as arrays
    """

    fh = Dataset(fp, mode='r')

    b1 = fh.variables['ch1'][:]
    b2 = fh.variables['ch2'][:] #b2 isn't used for anything
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
    year = fh.variables['Year'][:]     # 1D
    month = fh.variables['Month'][:]   # 1D
    day = fh.variables['Day'][:]       # 1D
    hour = fh.variables['Hours'][:]    # 1D - use this to flag where different orbits overlap?
    time = fh.variables['Time'][:]     # 1D
    fh.close()

    # bad values (-1.0e+30 for floats, -32768 for int)
    # set these all to np.nan
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


def make_orbit_mask(orbit_dict, conf_vars):
    """
    Generate a 2D mask to reduce data processing in areas where it is not needed:
    + non-night time portions of the imagery (based on vis data)
    + data beyond latitudinal thresholds
    + data at the edges of the instrument swath

    args:
        orbit_dict: dict, contains all relevant L1B data as arrays
        conf_vars: dict, user defined variables set in config file
    
    returns: 
        mask: np.ndarray, containing invalid data (np.nan), valid data (1) matching  dim of input bands 
    """
    mask = np.ones_like(orbit_dict['b1'])
    mask[orbit_dict['b1'] > conf_vars['thresh_b1_max']] = np.nan
    mask[(orbit_dict['lat'] > conf_vars['thresh_lat_max']) |
         (orbit_dict['lat'] < conf_vars['thresh_lat_min'])] = np.nan
    mask[orbit_dict['satza'] > conf_vars['thresh_satza_max']] = np.nan
    return mask


def fire_and_cloud_detection(b3, b4, b5, b34, b45, conf_vars, debug=None):
    """
    Calculate cloud masks, potential fire pixels, and do the contextual fire detection.

    args:
        b3: np.ndarray, MWIR data
        b4: np.ndarray, LWIR1 data
        b5: np.ndarray, LWIR2 data
        b34: np.ndarray, MWIR - LWIR1 difference data
        b45: np.ndarray, LWIR1 - LWIR2 difference data
        conf_vars: dict, user defined variables set in config file
        debug: bool, 1 == run verbose mode

    returns:
        d: dict, contains np.ndarrays with locations of PFPs, cloud, and various fire states
    """
    
    print('fire_and_cloud_detection(): hello!')
    # CONF VARS USED
    b3_minus_b4_thresh = conf_vars['b3_minus_b4_thresh']

    # cloud masking
    cloud_mask_no_buff, cloud_mask = make_cloud_mask(b3=b3, b34=b34, b4=b4, b5=b5, b45=b45, conf_vars=conf_vars)

    # PFP identification for b3 and b34 using percentiles
    pfp_array_b3 = pfp_screening(b3, conf_vars=conf_vars)
    pfp_array_b34 = pfp_screening(b34, conf_vars=conf_vars)

    # Final PFPs are those flagged in b3 AND b3-b4. Cloud pixels are also omitted.
    #  doing a sum here (so PFP b3 OR b34) results in too many PFPs
    pfp_array_final = pfp_array_b3 * pfp_array_b34
    pfp_array_final[np.isfinite(pfp_array_final) & (cloud_mask == 1)] = 0  # isfinite preserves 'nans'

    # THRESHOLDING OF BAND3-4
    pfp_array_final[np.isfinite(pfp_array_final) & (b34 < b3_minus_b4_thresh)] = 0  # isfinite preserves 'nans'

    # CONTEXTUAL DETECTION
    [true_fire_array, not_fire_array,
     not_determined_fire_array] = contextual_detection(pfp_array=pfp_array_final,
                                                       b3=b3,
                                                       b34=b34,
                                                       cloud_mask=cloud_mask,
                                                       conf_vars=conf_vars,
                                                       debug=debug)

    d={}
    d['pfp_array_final'] = pfp_array_final
    d['cloud_mask'] = cloud_mask
    d['cloud_mask_no_buff'] = cloud_mask_no_buff
    d['true_fire_array'] = true_fire_array
    d['not_fire_array'] = not_fire_array
    d['not_determined_fire_array'] = not_determined_fire_array

    print('fire_and_cloud_detection(): bye!')
    return d


def cloud_buffer(array, n=1):  
    '''
    Custom cloud buffering function. 
    There is probably a package that does the equivalent of this more succinctly, but this works.
    The 2D input 'array' is 'shifted' in all directions by n grid cells i.e. effectively 
    drawing an n-cell wide buffer around a binary mask.

    NOTE: currently this only works properly when n = 1!
    data 'jumps', rather than 'expands' so if n > 1, depending on the cloud pattern, 
    this can result in gaps in the buffered cloud mask that should not be there.

    args:
        array: np.ndarray, boolean cloud mask
        n: int, number of cells to buffer. this MUST be = 1 for this function to work correctly!
    returns:
        buffer_image: np.ndarray, a buffered cloud mask (boolean)
    '''
    a = np.copy(array)
    
    shapey, shapex = a.shape
    
    Yzeros = np.zeros((shapey, n))
    
    Xzeros = np.zeros((n, shapex))
    
    # shift up
    b = np.concatenate((a[n:, :], Xzeros), axis=0)
    
    # shift down
    c = np.concatenate((Xzeros, a[:-n, :]), axis=0)
    
    # shift left
    d = np.concatenate((a[:, n:], Yzeros), axis=1)
    
    # shift right
    e = np.concatenate((Yzeros, a[:, :-n]), axis=1)
    
    # shift diagonal up-right
    f = np.concatenate((a[n:, :], Xzeros), axis=0)
    f = np.concatenate((Yzeros, f[:, :-n]), axis=1)
    
    # shift diagonal up-left
    g = np.concatenate((a[n:, :], Xzeros), axis=0)
    g = np.concatenate((g[:, n:], Yzeros), axis=1)
    
    # shift diagonal down-left
    h = np.concatenate((Xzeros, a[:-n, :]), axis=0)
    h = np.concatenate((h[:, n:], Yzeros), axis=1)
    
    # shift diagonal down-right
    i = np.concatenate((Xzeros, a[:-n, :]), axis=0)
    i = np.concatenate((Yzeros, i[:, :-n]), axis=1)
    
    # combine each shifted layer to get final multi-directional buffer
    buffer_image = a + b + c + d + e + f + g + h + i
    
    # convert back to mask of 0 & 1s
    buffer_image[(buffer_image > 0)] = 1
    
    return buffer_image


def make_cloud_mask(b3, b34, b4, b5, b45, conf_vars):
    '''
    Cloud detection for creating a cloud mask. This is currently fairly basic:
        + screens B4, B34 with a loose threshold (B5 and B45 are currently not in use)
        + then adds a 1 grid cell buffer around all pixels determined as cloud
    args:
        b3: np.ndarray, MWIR data
        b4: np.ndarray, LWIR1 data
        b5: np.ndarray, LWIR2 data
        b34: np.ndarray, MWIR - LWIR1 difference data
        b45: np.ndarray, LWIR1 - LWIR2 difference data
        conf_vars: dict, user defined variables set in config file

    returns: 
        cloud_mask: np.ndarray, unbuffered cloud mask
        cloud_mask_buffered: np.ndarray, buffered cloud mask
    '''
    print('make_cloud_mask(): hello!')

    # CONF VARS USED
    b4_thresh = conf_vars['b4_thresh']
    b5_thresh = conf_vars['b5_thresh']
    b4_minus_b5_thresh = conf_vars['b4_minus_b5_thresh']
    IR_cloud_test_b34_thresh = conf_vars['IR_cloud_test_b34_thresh']
    IR_cloud_test_b3_thresh = conf_vars['IR_cloud_test_b3_thresh']      

    # cold areas in b4 are likely to be cloud
    cloud_array_b4 = b4 < b4_thresh

    # cold areas  in b5 are likely to be cloud - but AVHRR/1 doesnt have ch5, so not currently in use.
    #cloud_array_b5 = b5 < b5_thresh

    # 'Cirrus, or non-uniform low cloud test' Saunders & Kriebel - but AVHRR/1 doesnt have ch5, so not currently in use.
    #cloud_array_b45 = b45 > b4_minus_b5_thresh

    # IR diff test (Xu et al 2010)
    cloud_array_IR = ((b34 > IR_cloud_test_b34_thresh) & (b3 < IR_cloud_test_b3_thresh))

    # make final cloud mask: sum individual masks, then set all values > 0 to 1
    cloud_mask = (cloud_array_b4 + cloud_array_IR).astype(int)
    cloud_mask[cloud_mask > 0] = 1

    # now apply buffering function
    cloud_mask_buffered = cloud_buffer(cloud_mask, n=1)

    # mask the cloud mask based on how existing data is masked.
    swath_mask = np.copy(b4)
    swath_mask[np.isfinite(swath_mask)] = 0
    cloud_mask = cloud_mask + swath_mask
    cloud_mask_buffered = cloud_mask_buffered + swath_mask

    print('make_cloud_mask(): bye!')

    return cloud_mask, cloud_mask_buffered


def pfp_screening(b, conf_vars):
    """
    Identify potential fire pixels (PFPs) in an image, first by dividing image into
    segments, then flagging values in each segment above pfp_percentile.

    Args:
        b: np.ndarray, band of an image        
        conf_vars: dict, user defined variables set in config file. makes use of 'pfp_percentile' 
            the percentile threshold used identify PFPs in the image, and 'pfp_num_segments', which
            determines the n. subsets an orbit segment should be split into for percentile analysis.
            max value for pfp_num_segments = 10.
    Returns:
        pfp_array: boolean array of PFPs identified in b

    """

    print('pfp_screening(): hello!')

    # CONF VARS USED
    pfp_num_segments = conf_vars['pfp_num_segments']
    pfp_percentile = conf_vars['pfp_percentile']

    nrow = np.shape(b)[0]
    ncol = np.shape(b)[1]
    pfp_array = np.empty(np.shape(b)) * np.nan #filled later with locs of pfps

    if pfp_num_segments > 10.0:
        pfp_num_segments = 10.0
        print('pfp_screening(): pfp_num_segments > 10, setting to 10')

    # create 1d arrays of indices to use to divide up the image
    # image segments are roughly the same size, but not exactly, due to rounding
    # need a seq from zero to end 1 longer than pfp_num_segments
    # remove left edge value, as left edge of first segment set as 0 in loop below
    # round, and force back to int for indexing
    rows = np.round(np.linspace(start=0, stop=nrow, num=pfp_num_segments + 1)[1:]).astype(int)
    cols = np.round(np.linspace(start=0, stop=ncol, num=pfp_num_segments + 1)[1:]).astype(int)

    # divide image using indices, and extract subsets
    for c in range(len(cols)):
        minc = 0 if c == 0 else cols[c-1]
        maxc = cols[c]

        for r in range(len(rows)):
            minr = 0 if r == 0 else rows[r-1]
            maxr = rows[r]

            # extract image subset and set pfp threshold based on being above image percentile value
            subset = b[minr:maxr, minc:maxc]
            subset_percentile = np.nanpercentile(subset, pfp_percentile)

            # identify subset pfps, and add to final pfp array
            subset_pfps = subset > subset_percentile
            pfp_array[minr:maxr, minc:maxc] = subset_pfps
    
    # mask the pfp image with the swath
    swath_mask = np.copy(b)
    swath_mask[np.isfinite(swath_mask)] = 0
    pfp_array = pfp_array + swath_mask

    print('pfp_screening(): bye!')
    return pfp_array


def contextual_detection(pfp_array, b3, b34, cloud_mask, conf_vars, debug=None):
    """
    For each potential fire pixel, this function determines whether that pfp should be considered
    a genuine fire or not, based upon the properties of the surrounding background pixels.

    The surrounding ambient background land surface pixels are defined as a 5 x 5 (minimum) window centred
    on the PFP. If the number of these surrounding pixels that are 'actual' background surface pixels
    (tests are carried out to omit cloud/other fires etc) exceeds a given proportion ('ctx_background_pix_thresh'),
    statistics of these pixels (mean, mean absolute deviation; MAD) are taken and used to assess whether 
    the PFP is a genuine fire, according to the tests below. If there are not enough background pixels for stat
    calculation, the window is expanded 1 pixel in all directions and re-checked. Window expansion
    continues until either enough background pixels are identified, or the window > 15x15 pixels.
    Note that the 3 x 3 area imediately surrounding the PFP is masked to NaN, to avoid the possible
    inclusion of pixels neighbouring a fire (that a fire may be influencing) in the background
    statistic calculation process.
    Additionally, if a background window should expand beyond the extent of the array, the extent is
    truncated to the maximum possible in that direction.

    When enough background pixels have been identified, the following tests are performed on the MidIR
    (BT3) and MidIR-LWIR1 (BT3-4) data for each PFP:
            Test1: BT3-4(fire pixel) > BT3-4(bkgrd mean) + (fixedvalue1 * MAD)
            Test2: BT3-4(fire pixel) > BT3-4(bkgrd mean) + fixedvalue2
    Then one of the following, choosing the more conservative option:
            Test3a: BT3(fire pixel) > BT3(mean) + fixedvalue3 + MAD
            Test3a: BT3(fire pixel) > BT3(mean) + fixedvalue3 * MAD
    (if MAD is < 1, using an addition is more conservative than a multiplication)
    Here MAD = mean absolute deviation, and fixedvalues (ff1,ff2,ff3) are determined by the config file.

    Args:
        pfp_array: np.ndarray, a boolean array of potential fire pixels to evaluate in the contextual analysis.
        b3: np.ndarray, MWIR data
        b34: np.ndarray, MWIR - LWIR1 difference data
        cloud_mask: np.ndarray, a boolean cloud mask 
        conf_vars: dict, user defined variables set in config file. 
        debug: int, default value = None. 1 = some debug print statements, 2 = more verbose debug print statements.

    Returns:
        true_fire_array: np.ndarray, boolean array of PFP pixels confirmed as fire pixels
        not_fire_array: np.ndarray, boolean array of PFP pixels confirmed as not fire pixels
        not_determined_fire_array: np.ndarray, boolean array of PFP pixels that could not be classified definitively 
            as fire or non-fire pixels, due to too few ambient clear sky background pixels to perform contextual tests
    """

    print('contextual_detection(): hello!')

    # CONF VARS USED
    background_pix_thresh = conf_vars['ctx_background_pix_thresh']
    ff1 = conf_vars['ctx_ff1']
    ff2 = conf_vars['ctx_ff2']
    ff3a = conf_vars['ctx_ff3a']
    ff3b = conf_vars['ctx_ff3b']

    nrow = np.shape(b3)[0]
    ncol = np.shape(b3)[1]

    #mask new arrays using swath outline
    swath_mask = np.copy(b3)
    swath_mask[np.isfinite(swath_mask)] = 0    #set non-nan values = 0
    true_fire_array = np.zeros(np.shape(b3)) + swath_mask
    not_fire_array = np.zeros(np.shape(b3)) + swath_mask
    not_determined_fire_array = np.zeros(np.shape(b3)) + swath_mask

    #loop over each potential fire pixel
    y_vals, x_vals = np.where(pfp_array == 1)
    pfp_ix = zip(y_vals,x_vals)
    print('contextual_detection(): # PFPs in scene:', len(pfp_ix))

    for i, indices in enumerate(pfp_ix):
        if (debug == 1) or (debug == 2):
            print('testing:', i + 1, 'of', len(pfp_ix), 'potential fires, located at:', indices)
        y = indices[0]
        x = indices[1]
        #get b3 and b3-4 values of the pfp
        pfp_val_b3  = b3[y,x]
        pfp_val_b34 = b34[y,x]

        ##reset variable determining whether bg window needs to keep expanding
        got_enough_background_pix = 0
        expansion = 2 #initial window expansion size
        while (got_enough_background_pix == 0 and expansion <= 7):
            if debug == 2:
                if expansion < 7:
                    print("Current window size is", expansion*2+1, "x", expansion*2+1, "pixels")
                else:
                    print("Current window size is", expansion*2+1, "x", expansion*2+1, 
                          "pixels (maximum expansion to be attempted)")
            #define window extent
            winx = max([0, x - expansion])
            winx1= min([ncol - 1, x + expansion])
            winy = max([0, y - expansion])
            winy1= min([nrow - 1, y + expansion])
            if debug == 2:
                print(' max possible array indices:',nrow-1,',', ncol-1)
                print('fire pixel index:',y,',',x,' col min:',winx,' col max:',winx1,
                      ' row min:',winy,' row max:',winy1)

            #subset each of the datasets using the window indices,
            #and blank the 3x3 area immediately surrounding the pfp
            win_pfp =   pfp_array[winy:winy1 + 1, winx:winx1 + 1]
            win_cloud = cloud_mask[winy:winy1 + 1, winx:winx1 + 1]
            win_b3 =    b3[winy:winy1 + 1, winx:winx1 + 1]
            win_b34 =   b34[winy:winy1 + 1, winx:winx1 + 1]
            if debug == 2:
                print('win_pfp:', '\n', win_pfp)
                if np.shape(win_pfp)[0] < expansion*2+1:
                    print(np.shape(win_pfp), "window limit (row direction) out of bounds, resized to", 
                          np.shape(win_pfp)[0], "x", np.shape(win_pfp)[1], "pixels")
                if np.shape(win_pfp)[1] < expansion*2+1:
                    print(np.shape(win_pfp), "window limit (col direction) out of bounds, resized to", 
                          np.shape(win_pfp)[0], "x", np.shape(win_pfp)[1], "pixels")

            # MASK PIXELS IMMEDIATELY SURROUNDING CURRENT PFP
            # DON'T INCLUDE IN BACKGROUND ANALYSIS AS THEY MAY BE CONTAMINATED
            # each window iteration, make an array where 3x3 window of nans will be set 
            # (all other values = 1), and use to blank subsets
            # this is a fairly convoluted approach!
            # when at an edge case - the background window will not be centred on the pfp in these cases
            nan_array = np.ones(np.shape(b3)) #make a new array of ones
            blankx  = max([0,x-1])
            blankx1 = min([ncol-1,x+1])
            blanky  = max([0,y-1])
            blanky1 = min([nrow-1,y+1])
            nan_array[blanky:blanky1 + 1, blankx:blankx1 + 1] = np.nan # set upto 3x3 elements of nan_array to 'nan'
            win_nan =  nan_array[winy:winy1 + 1, winx:winx1 + 1] # subset nan_array to get

            # now use the NaN window to mask the other windows around the pfp
            win_pfp   = win_pfp * win_nan
            win_cloud = win_cloud * win_nan
            win_b3    = win_b3 * win_nan
            win_b34   = win_b34 * win_nan
            if debug == 2:
                print('win_pfp (masked):', win_pfp)
                print('win_cloud:', win_cloud)
                print('win_b3:', win_b3)
                print('win_b34:', win_b34)

            # determine if we have enough pixels to perform further analysis
            # count how many values are masked in the window - normally nan_values = 9
            # (3x3 around pfp), but will be less if pfp at edge of image
            num_nan_values = np.sum(np.isnan(win_pfp))
            
            # use nan_values to determine MAX POSSIBLE eligible bkgd pixels in the window
            possible_background_pix_count = float(np.size(win_pfp) - num_nan_values)
            
            # now identify how many pixels are ineligible as bkgd pixels by combining several subset arrays:
            # 'win_pix_bad' will >= 1 where a pixel is ineligible as bkgd pixel
            # ineligible pixel criteria: other pfp, cloudy pixels
            win_pix_bad = win_pfp + win_cloud
            
            actual_background_pix_count = float(np.sum(win_pix_bad == 0))
            
            if possible_background_pix_count == 0: # avoid 'divide by 0' errors
                actual_background_pix_prop = 0
            else:
                actual_background_pix_prop = actual_background_pix_count / possible_background_pix_count
            
            if debug == 2:
                print('bg pix:', actual_background_pix_count, 'max poss bg pix:', possible_background_pix_count, 'prop:', actual_background_pix_prop)

            # CONTEXTUAL FIRE TESTS -------------------------------------------
            # if n. eligible pixels exceeds required threshold, perform contextual tests
            if actual_background_pix_prop > background_pix_thresh:
                got_enough_background_pix = 1 #set condition to terminate the window expansion loop

                #calculate mean average deviation
                b3_vals = win_b3[win_pix_bad == 0]
                b34_vals = win_b34[win_pix_bad == 0]
                b3_mean = np.mean(b3_vals)
                b34_mean = np.mean(b34_vals)
                mad_b3 = np.mean(np.absolute(b3_vals - b3_mean))
                mad_b34 = np.mean(np.absolute(b34_vals - b34_mean))

                # apply the contextual tests
                # note: take care: the ff values are used out of order for b34
                if mad_b34 < 1:
                    test_1 = int(pfp_val_b34 >= b34_mean + ff2 + mad_b34)
                else:
                    test_1 = int(pfp_val_b34 >= b34_mean + ff1 * mad_b34)

                if mad_b3 < 1:
                    test_3 = int(pfp_val_b3 >= b3_mean + ff3a + mad_b3)
                else:
                    test_3 = int(pfp_val_b3 >= b3_mean + ff3b * mad_b3)

                # if all tests are passed, PFP is considered a true FP, so set output array to 1,
                # otherwise leave as 0 

                test_sum = 0
                test_sum = test_1 + test_3

                if test_sum == 2:
                    if (debug == 1) or (debug == 2):
                        print('fire pixel identified!', '\n')
                    true_fire_array[y,x] = 1
                else:
                    if (debug == 1) or (debug == 2):
                        print('not a fire pixel', '\n')
                    if debug == 2:
                        print("Current window size is", expansion*2+1, "x", expansion*2+1, "pixels")
                        print('test1:', test_1, 'test3:', test_3, '\n', '\n')
                    not_fire_array[y,x] = 1
            # ------------------------------------------------------------------------

            else:
                if debug == 2:
                    print("Not enough valid background pixels at window size ", np.shape(win_pfp)[0], "x", np.shape(win_pfp)[1])

            #make the background window 1 pixel wider in all directions
            expansion = expansion + 1

        # OUTSIDE WHILE LOOP
        if (true_fire_array[y,x] == 0) and (not_fire_array[y,x] == 0):
            ##if not enough eligible background pixels at max window size
            ## (i.e. actual bkgrnd pix prop is < pix thresh), both 'true fire
            ##array' and 'not fire array' will still be 0 at this pfp loc.
            ##In this case, this pixel needs flagging as 'not determined'
            not_determined_fire_array[y,x] = 1
            if (debug == 1) or (debug == 2):
                print("Not enough valid background pixels at window size ", np.shape(win_pfp)[0], "x", np.shape(win_pfp)[1], ", flagged as undetermined")

    #OUTSIDE CONTEXTUAL FOR LOOP
    print('contextual_detection(): # of non-fire pixels in scene:', int(np.nansum(not_fire_array)))
    print('contextual_detection(): # of not determined pixels in scene:', int(np.nansum(not_determined_fire_array)))
    print('contextual_detection(): # of true fire pixels in scene:', int(np.nansum(true_fire_array)))
    print('contextual_detection(): bye!')

    return true_fire_array, not_fire_array, not_determined_fire_array


def cloud_fire_extractor(cf, sub):
    """
    For all fire pixels, pull all array information into a dataframes
    
    Args:
        cf, dict. a dict of arrays containing the results of cloud and fire detection. 
        sub, dict. a dict of the input L1B arrays.

    Returns: 
        d, pd.DataFrame(). contains pixel level array information.
        d2, pd.DataFrame(). contains summary information for the entire sub scene.
    """

    print('cloud_fire_extractor(): hello!')
    d = pd.DataFrame()
    d2 = pd.DataFrame()
    
    # use true fire array to extract info from individual bands
    ys, xs = np.where(cf['true_fire_array'] == 1)
    d['sub_y_ix'] = pd.Series(ys)
    d['sub_x_ix'] = pd.Series(xs)
    for k in ['year', 'month', 'day', 'hour', 'time']:
        d[k] = pd.Series(sub[k][ys])
    for k in ['lat', 'lon', 'satza', 'b1', 'b2', 'b3', 'b4', 'b5', 'b34', 'b45']:
        d[k] = pd.Series(sub[k][ys, xs])

    # calc stats for entire subset and add to summary dataframe
    d2['subset_pfps'] = pd.Series(np.nansum(cf['pfp_array_final']))
    d2['subset_cloud'] = pd.Series(np.nansum(cf['cloud_mask']))
    d2['subset_cloud_no_buff'] = pd.Series(np.nansum(cf['cloud_mask_no_buff']))
    d2['subset_not_fire'] = pd.Series(np.nansum(cf['not_fire_array']))
    d2['subset_unconfirmed'] = pd.Series(np.nansum(cf['not_determined_fire_array']))
    d2['subset_true_fire'] = pd.Series(np.nansum(cf['true_fire_array']))
    print('cloud_fire_extractor(): bye!')
    return d, d2


def process_orbit(fp, conf_vars, do_fire_detection):
    """
    This function:
     + loads input file
     + does some preprocessing including application of a simple mask
     + segments the AVHRR orbit for processing with fire algorithm 
     + appends meta data to fire hotspot dataset
     + returns hotspot data and orbit file summary

    Args:
        fp: str, file path to an L1B orbit file
        conf_vars: dict, user defined variables set in config file.
        do_fire_detection: bool, if = 1, run the fire detection, otherwise skip. 
            this arg mostly just exists for debugging purposes.
    Returns:
        df_orbit_pixel: pd.DataFrame, contains all pixel information for detected fires in orbit file.
        df_orbit_summary: pd.DataFrame, contains a summary (no. fires, cloud pix etc) of all processed orbit file subsets
    """

    print('process_orbit(): hello!')
    o_splits = conf_vars['o_splits']
    o_seg_min_prop = conf_vars['o_seg_min_prop']

    fn_out = fp.split('/')[-1][:-3]

    # load and clean
    print('process_orbit(): load data')
    orbit_dict = load_and_fill_bad(fp=fp)

    # masking
    print('process_orbit(): mask')
    orbit_mask = make_orbit_mask(orbit_dict, conf_vars)
    orbit_dict['b1'] = orbit_dict['b1'] * orbit_mask
    orbit_dict['b2'] = orbit_dict['b2'] * orbit_mask
    orbit_dict['b3'] = orbit_dict['b3'] * orbit_mask
    orbit_dict['b4'] = orbit_dict['b4'] * orbit_mask
    orbit_dict['b5'] = orbit_dict['b5'] * orbit_mask
    orbit_dict['b34'] = orbit_dict['b3'] - orbit_dict['b4']
    orbit_dict['b45'] = orbit_dict['b4'] - orbit_dict['b5']

    # subset analysis
    # not all orbits are same dimensions, so need to do dynamic splitting
    # each subset is not exact same size due to rounding
    endval = orbit_dict['b3'].shape[0]
    seq = np.round(np.linspace(start=0, stop=endval, num=o_splits + 1)).astype(int)
    subsets = zip(seq[:-1], seq[1:])
    print('process_orbit(): begin orbit segment loop')
    for i in range(len(subsets)):
        print('process_orbit(): subset {0} of {1} orbit subsets'.format(i + 1, len(subsets)))

        ylim = subsets[i]

        # subset each orbit item
        subset_dict = {}
        for j in orbit_dict.keys():
            if len(np.shape(orbit_dict[j])) == 2:
                subset_dict[j] = orbit_dict[j][ylim[0]:ylim[1], :]
            else:
                subset_dict[j] = orbit_dict[j][ylim[0]:ylim[1]]

        # skip subset if > o_seg_min_prop of total pixels are masked
        subset_total_pixels = subset_dict['b3'].shape[0] * subset_dict['b3'].shape[1]
        subset_valid_pixels = np.sum(~np.isnan(subset_dict['b3']))

        if subset_valid_pixels > subset_total_pixels * o_seg_min_prop:
            print('process_orbit(): processing subset')

            # run cloud and fire detection algos
            if do_fire_detection:
                cloud_fire_dict = fire_and_cloud_detection(b3=subset_dict['b3'],
                                                           b4=subset_dict['b4'],
                                                           b5=subset_dict['b5'],
                                                           b45=subset_dict['b45'],
                                                           b34=subset_dict['b34'],
                                                           conf_vars=conf_vars)

                # extract info on fire and cloud to dataframes
                df_subset_pixel, df_subset_summary = cloud_fire_extractor(cf=cloud_fire_dict, 
                                                                          sub=subset_dict)

                # add some extra meta info to dataframes
                df_subset_pixel['subset_no'] = i + 1
                df_subset_pixel['subset_total_pxl'] = subset_total_pixels
                df_subset_pixel['subset_valid_pxl'] = subset_valid_pixels
                df_subset_pixel['subset_row_min'] = ylim[0]
                df_subset_pixel['subset_row_max'] = ylim[1]
                df_subset_summary['subset_no'] = i + 1
                df_subset_summary['subset_total_pxl'] = subset_total_pixels
                df_subset_summary['subset_valid_pxl'] = subset_valid_pixels
                df_subset_summary['subset_row_min'] = ylim[0]
                df_subset_summary['subset_row_max'] = ylim[1]

                # append these dfs to the orbit dfs
                try:
                    if np.shape(df_orbit_pixel)[0] < 1:
                        #sometimes no fires are detected. Can't concat to an empty df, so if len=0, just create a copy.
                        df_orbit_pixel = df_subset_pixel.copy()
                    else:
                        df_orbit_pixel = pd.concat([df_orbit_pixel, df_subset_pixel], ignore_index=True)
                except:
                    df_orbit_pixel = df_subset_pixel.copy()

                try:
                    df_orbit_summary = pd.concat([df_orbit_summary, df_subset_summary], ignore_index=True)
                except:
                    df_orbit_summary = df_subset_summary.copy()

                # add orbit filename to dataframes
                fp_parts = fp.split('/')
                fn_out_stub = fp_parts[-1][:-3]
                df_orbit_pixel['fn'] = fn_out_stub
                df_orbit_summary['fn'] = fn_out_stub
        else:
            print('process_orbit(): skipping subset')

    print('process_orbit(): bye!')
    return df_orbit_pixel, df_orbit_summary


if __name__ == "__main__":

    # input options ======================
    np.seterr(divide='ignore', invalid='ignore')  # Ignore NaN errors
    config_path = "/path/to/config/file 'config_file.py'/"
    dir_in = "/path/to/input/l1b .nc files/"
    dir_out = '/path/to/output/dir/'

    # flags 
    do_fire_detection = True
    # ====================================

    # import thresholds and test values for fire detection from config file
    sys.path.append(config_path)
    from config_file import conf_vars

    # get calibrated L1B files to process
    fp_list = get_sorted_paths(dir_in, '*NC*.nc')

    for i in range(len(fp_list)):
        fp = fp_list[i]
        print('')
        print ('id_fires(): doing orbit {0} of {1}'.format(i+1, len(fp_list)))
        print ('id_fires(): {0}'.format(fp))
        df_orbit_pixel, df_orbit_summary = process_orbit(fp=fp,
                                                         conf_vars=conf_vars,
                                                         do_fire_detection=do_fire_detection)
        # output fire data
        fn_out = fp.split('/')[-1][:-3]
        df_orbit_pixel.to_csv(dir_out + fn_out + '_pixel.csv', index=False)
        df_orbit_summary.to_csv(dir_out + fn_out + '_summary.csv', index=False)

    print('')
    print ('----------------------------')
    print ('  id_fires(): ~ finished ~  ')
    print ('----------------------------')
    print('')

