#!/bin/python

import os,sys,stat,time
import subprocess
from subprocess import PIPE
import re
from os import path as osp

#in the section 8 tools, this uses S8_path instead
BASEDIR = os.environ.get('BASEDIR')
if BASEDIR is None:
    print("Error in python script calculation: $BASEDIR not set. Please source a path to the section 8 tools.")
    sys.exit(1)
sys.path.append(osp.join(BASEDIR, 'utils/shared_code'))
#from common_functions import get_nc_dim_lengths, tstamp
import env

def mirror_ptmp(source):
    '''
    Given a source file, provides the mirored location in the xtmp-based
    persistent temporary directory and copies to that location if a) the
    file is not present or b) the file in source has a later modification date
    than dest
    '''
    ptmp_dir = "/xtmp/" + env.USER + "/work/"
    dest = osp.join(ptmp_dir, source)
    if not osp.exists(dest):
        gcp(source, dest)
    else:
        source_mtime = osp.getmtime(source)
        dest_mtime = osp.getmtime(dest)
        if (dest_mtime > source_mtime):
            gcp(source, dest)
    return(dest)

def make_xtmp_path(experiment_name, gridname, create=False, var_list=[], rip_list=[]):
    xtmp_path = "/".join(["/xtmp",
                           env.USER,
                           "work/PROJECTS/DOWNSCALING/SUBPROJECTS",
                           gridname,
                           experiment_name])
    if create:
        if not osp.exists(xtmp_path):
            os.makedirs(xtmp_path)
        for var in var_list:
            for rip in rip_list:
                if not osp.exists(osp.join(xtmp_path, rip, var)):
                    os.makedirs(osp.join(xtmp_path, rip, var))
                    os.makedirs(osp.join(xtmp_path, rip, var, 'chunk'))
                #currently sticking with qcmask INSIDE outfile dirs b/c it simplfies some postproc stuff; may change later
                if not osp.exists(osp.join(xtmp_path, rip, var + "_qcmask")):
                    os.makedirs(osp.join(xtmp_path, rip, var + "_qcmask"))
                    os.makedirs(osp.join(xtmp_path, rip, var + "_qcmask", "chunk"))
        #and create output directories for the various kinds of processed output
        catdirs = [osp.join(xtmp_path, 'ens'), osp.join(xtmp_path, 'climanom'), osp.join(xtmp_path, 'trenddetr')]
        for c in catdirs:
            if not osp.exists(c):
                os.makedirs(c)
    return(xtmp_path)
