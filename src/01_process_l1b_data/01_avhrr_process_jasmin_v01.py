"""
This script calls code that calibrates AVHRR L1B files, and runs fire detection code on JASMIN 
"""

import os
import sys
import subprocess
import shutil
import csv
import glob

# custom
import paths as paths 
import identify_fires_jasmin_v01 as id_fire


def cal_l1b(str_fp_server, str_prog):
    """ 
    Call the compiled fortran code that calibrates L1B orbit files and 
    outputs a netCDF.

    Args:
        str_fp_server: str, full file path to raw L1B file on JASMIN/CEDA.
        str_prog: str, full file path to executable generated from fortran code.
    
    Returns:
        str_fp_out: str, full file path the the calibrated L1B file (netCDF)
        status: str, info indicating whether subprocess ran correctly or not.
    """
    
    # modify L1B input name to create output .nc file name and path
    # trims 'NSS.GHRR.' and '.gz'
    str_fn_in = str_fp_server.split("/")[-1]
    str_fn_out = str_fn_in[9:-3] + ".nc"
    str_fp_out = "./" + str_fn_out
    
    # set random pid
    str_pid = "1"

    # call L1B cal routine
    try:
        status = subprocess.call([str_prog, str_pid, str_fp_server, str_fp_out])
    except:
        status = 'L1B cal error with subprocess.call()'

    return str_fp_out, status


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


if __name__ == "__main__":

    # INPUTS ------------------------
    # location of raw L1B orbit files
    #dir_in = "/neodc/avhrr_gac/data/l1b/AVHRR18_G/v1/2014/01/01/"
    dir_in = paths.dir_jasmin_data

    # base working directory, here we will create and output files to tmp, archive dirs etc
    dir_base = paths.dir_jasmin_main

    # final output archive path - outputs get moved to here after processing
    dir_archive = os.path.join(dir_base, "01_avhrr_data_raw")

    # path to config file used for fire detection, contains thresholds etc
    dir_config = paths.dir_config

    # full file path to L1B calibration executable file compiled from fortran code
    fp_exe = paths.fp_exe

    # runid - determines dir name where archives are output
    runid = 'r001'

    # flags
    do_fire_detection = True          # actually run fire detection algo?


    # BEGIN PROCESSING --------------
    
    # import config settings as dictionary
    sys.path.append(dir_config)
    from config_file import conf_vars

    # get list of files to process
    fp_list = get_sorted_paths(dir_in, '*.gz')

    # process each file
    for i in range(len(fp_list)):
        fp_server = fp_list[i]

        print('avhrr_process():  {0} of {1} orbits'.format(i+1, len(fp_list)))

        fp_server_parts=fp_server.split('/')
        fn_out_stub = fp_server_parts[-1][9:-3]

        # create and cd to temp dir
        # calibrated .nc files processed here
        dir_temp = os.path.join(dir_base, 'temp', "temp_" + fn_out_stub)
        os.makedirs(dir_temp)
        os.chdir(dir_temp)

        try:
            # get data from server and calibrate
            print("avhrr_process(): copy and cal file: {0}".format(fp_server))
            #fp_nc, status = copy_and_cal(str_fp_server=fp_server, str_prog=fp_exe)
            fp_nc, status = cal_l1b(str_fp_server=fp_server, str_prog=fp_exe)

            # do fire processing
            print("avhrr_process(): do fire detection for file: {0}".format(fn_out_stub))
            df_orbit_pixel, df_orbit_summary = id_fire.process_orbit(fp=fp_nc,
                                                                    conf_vars=conf_vars,
                                                                    do_fire_detection=do_fire_detection)

            # output fires to tmp
            print("avhrr_process(): outputting to temp")
            df_orbit_pixel.to_csv(os.path.join(dir_temp, fn_out_stub + '_pixel.csv'), index=False)
            df_orbit_summary.to_csv(os.path.join(dir_temp, fn_out_stub + '_summary.csv'), index=False)

            # archive fire outputs
            print("avhrr_process(): archiving")
            str_instrument = fn_out_stub.split('.')[0]
            dir_archive_full = os.path.join(dir_archive, runid, str_instrument)
            if not os.path.exists(dir_archive_full):
                os.makedirs(dir_archive_full)
            archive_file_list = get_sorted_paths(dir_temp + '/', '*{0}*'.format(fn_out_stub))
            for f in archive_file_list:
                if os.path.isfile(f):
                    shutil.copy(f, dir_archive_full)

            # remove temp dir
            shutil.rmtree(dir_temp)

        except Exception as ex:
            # if for some reason orbit processing fails, rm dir_temp for that orbit, and log as a failure
            print("ERROR: couldn't process orbit")
            # remove temp dir
            shutil.rmtree(dir_temp)
            # log a failure
            fp_error_log = os.path.join(dir_archive, runid, 'orbits_failed_to_process.csv')
            with open(fp_error_log, 'a') as oFile:
                wr = csv.writer(oFile, delimiter=' ')
                wr.writerow([fp_server])
            raise

        print('')
        print('')
        print('')

    print('')
    print('-----------------------------')
    print('avhrr_process(): ~ finished ~')
    print('-----------------------------')
    print('')