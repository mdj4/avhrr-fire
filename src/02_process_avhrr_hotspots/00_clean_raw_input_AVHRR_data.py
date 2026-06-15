""" clean up a few intermediate columns in the raw data that should not be output for public consumption"""

import sys
from pathlib import Path
import numpy as np
import pandas as pd
from datetime import datetime, time, timedelta

# load custom functions
sys.path.append('./../../lib')
sys.path.append(str(Path(__file__).resolve().parent.parent.parent / 'lib'))
print(sys.path)
import paths as paths
import utils as utils

# USER INPUTS------------------------------------
dir_base = Path(paths.dir_main)
dir_in = dir_base / "00_avhrr_data_raw"
dir_out = dir_base / "01_avhrr_data_raw"
#------------------------------------------------
print(dir_in)
print(dir_out)

fp_list = list(Path.glob(dir_in, '*pixel*'))

#print(fp_list)

# get rid of unhelpful columns
for i, infile in enumerate(fp_list):
    print(f'doing {infile}')
    
    fn = str(infile).split('\\')[-1].split('.')[0]
    
    df = pd.read_csv(infile)

    # apply absolute b3 threshold of 285K
    df = df.loc[df['b3'] > 285]
    
    #subset
    df = df[['fn', 'lat', 'lon', 'year', 'month', 'day', 'hour',
       'b1', 'b2', 'b3', 'b4', 'b5', 'b34', 'b45', 'satza']]

    # output
    if len(df) > 0:
        df.to_csv(
                  dir_out / f'{fn}.csv.gz',
                  header=True,
                  index=False,
                  compression='gzip')  
print('finished')