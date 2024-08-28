'''
config file for AVHRR GAC L1B data processing

'''
conf_vars = {"thresh_b1_max": 0.005, # b1 value used for day/night masking
             
             "thresh_lat_max": 70, # latitude threshold upper
             
             "thresh_lat_min": -60, # latitude threshold lower
             
             "thresh_satza_max": 35, # sat zenith angle threshold
             
             "o_splits": 24.0, # n. segments into which orbit should be divided for sub analysis
             
             "o_seg_min_prop": 0.01, # min prop of total grids observations that need to be != np.nan for orbit segment to be processed, otherwise skip
             
             "b4_thresh": 265, #cold cloud test
             
             "b5_thresh": -9999, #MODIS Collection 5/6 nightime cloud test (265K) NOT CURRENTLY IN USE.
             
             "b3_minus_b4_thresh": 3, # absolute difference scene test 
             
             "b4_minus_b5_thresh": 9999, # Wooster et al 2015 (LSA SAF) set to 1.5K. NOT CURRENTLY IN USE.
             
             "IR_cloud_test_b34_thresh": 5, # IR difference test for cloud #1
             
             "IR_cloud_test_b3_thresh": 285, # IR difference test for cloud #2
             
             "pfp_num_segments": 10.0, # how many subsections should the image be split into for identifying PFPs?
             
             "pfp_percentile": 90, # only pixels with values above 'pfp_percentile' get flagged as PFPs
             
             "ctx_background_pix_thresh": 0.65, #proportion of ambient bg pixels needed to perform contextual tests 
             
             "ctx_ff1": 4.0, # contextual test value 1: mean + ctx_ff1 * MAD
             
             "ctx_ff2": 4.0, # contextual test value 2: mean + ctx_ff2
             
             "ctx_ff3a": 4.0, # contextual test value 3a: mean + ctx_ff3a + MAD
             
             "ctx_ff3b": 4.0} # contextual test value 3b: mean + ctx_ff3b * MAD


