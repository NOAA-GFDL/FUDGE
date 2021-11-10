#!/bin/python
# Common functions for the ESD input generation scripts

#import fudgeUtils
#from fudgeUtils import gcp,tstamp
import os,sys,subprocess,shutil,time,copy,socket
from os import path as osp
import numpy as np
from subprocess import PIPE
import argparse
import shlex
import smtplib
import re
import traceback
import netCDF4
from netCDF4 import Dataset

#in the section 8 tools, this uses S8_path instead
BASEDIR = os.environ.get('BASEDIR')
if BASEDIR is None:
    print("Error in python script calculation: $BASEDIR not set. Please source a path to the section 8 tools.")
    sys.exit(1)
sys.path.append(osp.join(BASEDIR, 'utils/shared_code'))

import env

#These two functions are on loan from FudgeUtils unitl I add an addition to pythonpath
#within the section 8 environment setup
def gcp(infile, outfile="TMPDIR", timestamp=False, verbose=False, gcp_verbose=True):
    #Wrapper for GFDL's GCP function
    #for copying from /archive to /vftmp
    if (outfile=='TMPDIR'):
        tdir = os.environ.get("TMPDIR")
        outfile = osp.join(tdir,infile.lstrip("/"))
    if timestamp:
        outfile  = outfile +"." + tstamp()
    #Make sure that the output directory exists
    outdir = osp.dirname(osp.abspath(outfile)).rstrip("/") + "/"
    if not osp.isdir(outdir):
        os.makedirs(outdir)
    if gcp_verbose:
        command = "gcp -v " + infile + " " + outfile
    else:
        command = "gcp " + infile + " " + outfile
    if verbose:
        print(command)
    modulepath = "source /usr/local/Modules/default/init/sh"
    mod_command = modulepath + "; module load gcp; module load globus; "
    g = subprocess.Popen(mod_command + command, stdout=subprocess.PIPE, stderr=subprocess.PIPE,shell=True,env=os.environ)
    g_out,g_err = g.communicate()
    if(g.returncode != 0):
        print("Error in gcp: file " + infile + " did not copy to " + outfile)
        print(g_out)
        print(g_err)
        print("Trying again with cp")
        if not osp.exists(osp.dirname(outfile)):
            os.makedirs(osp.dirname(outfile))
        shutil.copy2(infile, outfile)
    return(outfile)

def get_nc_dim_lengths(nc_file, dimnames):
    #Gets the length of a named dimension from an input netcdf file.
    #Assumes that your system has ncdump available.
    #Yes, this is a bit faster than reading into Python
    # -C 8 is an attempt to get a newline behind every dim
    #
    #GFDL-specific code for checking tape migration
    dm_cmd = 'dmls -l ' + nc_file
    p0 = subprocess.Popen(dm_cmd, shell=True, stdout=PIPE, stdin=PIPE, stderr=PIPE)
    out0,err0 = p0.communicate()
    if p0.returncode == 0:
        if re.search('(OFL)', out0) is not None:
            p0 = subprocess.Popen("dmget " + nc_file,
                                 shell=True, stdout=PIPE, stdin=PIPE, stderr=PIPE)
            out0,err0 = p0.communicate()
    else:
        print("Error in get_nc_dim_length: file " + nc_file + " not found on disk or tape!")
        sys.exit(1)
    if isinstance(dimnames, basestring):
        dimnames = [dimnames]
    #system command with ncdump
    sys_cmd = "ncdump -h "+nc_file+" | grep -C 8 'dimensions:'"
    p1 = subprocess.Popen(sys_cmd, shell=True, stdout=PIPE, stdin=PIPE, stderr=PIPE)
    out1,err1=p1.communicate()
    if p1.returncode != 0:
        print(sys_cmd)
        print(out1)
        print(err1)
        sys.exit(1)
    #get dimnames
    coords = {}
    for d in dimnames:
        tmpstr = re.search(d + " \= (.*?)\n",out1).group(0)
        cdim = re.search(r'\d+',tmpstr).group(0)
        coords[d] = int(cdim)
    return(coords)

def tstamp():
    #Returns a simple timestamp for the output file
    #return(re.sub("\\.", "_", str(time.time())))
    return(time.strftime("%Y%m%d-%H%M%S-%S"))

def rcat_files(files, rcat_out):
    '''
    Short function for putting together an rcat operation for netcdf files.
    Assumes that the first file covers dates that come before the second file;
    there are currently no checks for contiguous dates in the two files
    '''
    #The files need to be copied to tmpdir first:
    for f in range(0, len(files)):
        files[f] = gcp(files[f])
        time.sleep(10)
    #Each of the files needs to have its time dimension recorded and added
    tdim_lengths = [get_nc_dim_lengths(f, 'time') for f in files]
    rcat_tmp = TMPDIR + "/rcat_out.nc"
    rcat_cmd = " ".join(["ncrcat -O"] + files + [rcat_tmp])
    print(rcat_cmd)
    p1 = subprocess.Popen(rcat_cmd, shell=True, stdout=PIPE, stdin=PIPE, stderr=PIPE)
    out1,err1 = p1.communicate()
    if p1.returncode != 0:
        print("Error in rcat_files: exit status of rcat was not 0!")
        print(out1)
        print(err1)
        sys.exit(1)
    print("copying rcat to new location")
    gcp(rcat_tmp, outfile=rcat_out, verbose=True)
    time.sleep(30)
    if not osp.isfile(rcat_out):
        print("Error in rcat_files: copy operation is not working!")
        sys.exit(1)
    #Return length of time dimension; that gets used for the nco commands later
    return(tdim_lengths)

def call_R_code(r_command, infiles, out_pattern, rtool,
                copy_to_tmpdir=True, verbose=False):
    '''
    Wrapper for a system call to R code.
    Set to work with an output pattern rather than a list of files.
    '''
    global TMPDIR
    TMPDIR = os.environ.get('TMPDIR')
    if TMPDIR is None:
        TMPDIR = os.environ.get('S8TMPDIR')
        if TMPDIR is None:
            print("Error in python script calculation: neither $TMPDIR nor $S8TMPDIR is set. Please set a path to the tmpdir.")
            sys.exit(1)
    tdir = TMPDIR
    if isinstance(infiles, basestring):
        infiles = [infiles]
        infiles = [re.sub("\$TMPDIR", tdir, el) for el in infiles]
    # 2019-05-17; using out_pattern instead of outfiles; easier substition
    # for permutations on out_base writing patterns
    out_pattern = re.sub("\$TMPDIR", tdir, out_pattern)
    out_pattern = osp.abspath(out_pattern)
    if not osp.isdir(osp.dirname(out_pattern)):
        os.makedirs(osp.dirname(out_pattern))
    if copy_to_tmpdir:
        infiles_tmp = [gcp(p, outfile="TMPDIR") if re.search(tdir, p) is not None else p for p in infiles]
        out_pattern_tmp = osp.join(tdir, out_pattern.lstrip("/"))
        if not osp.isdir(osp.dirname(out_pattern_tmp)):
            os.makedirs(osp.dirname(out_pattern_tmp))
        for src,dest in zip(infiles, infiles_tmp):
            r_command = re.sub(src, dest, r_command)
        for src,dest in zip([out_pattern], [out_pattern_tmp]):
            r_command = re.sub(src, dest, r_command)
    if verbose: print("Calling " + rtool); print(r_command);
    try:
        p = subprocess.Popen(r_command,shell=True,stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    except:
        print("Error in subprocess call! Please examine call in more detail.")
        print(r_command)
        sys.exit(1)
    error,output = p.communicate()
    #Check on what this returns with an R error
    print(p.returncode)
    if(p.returncode != 0):
        err_message = "error in call to " + rtool
        print("Error in R code call! There was an " + err_message)
        print(r_command); print(error); print(output); sys.exit(1)
    else:
        #diagnostic
        if verbose: print(output); print(error)
        if copy_to_tmpdir:
            tmp_outdir = osp.dirname(out_pattern_tmp)
            outdir = osp.dirname(out_pattern)
            outfiles_tmp = [osp.join(tmp_outdir, el) for el in os.listdir(tmp_outdir)]
            outfiles_tmp = [el for el in outfiles_tmp
                            if re.search(out_pattern, el) is not None]
            outfiles = [re.sub(tmp_outdir, outdir, el) for el in outfiles_tmp]
            for src,dest in zip(outfiles_tmp, outfiles):
                gcp(src, dest)
                # Examine this call - there might be a better way to do this
                #os.remove(src)
    return(outfiles)


    

