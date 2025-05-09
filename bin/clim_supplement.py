#!/bin/python
#Versions of the anomaly copying scripts for the
#clim/anom workflow
import os,sys,subprocess,shutil
from os import path as osp
from subprocess import PIPE
import argparse
import shlex
import fudgeList
import smtplib
import re
import netCDF4
from netCDF4 import Dataset
import fudgeUtils
from fudgeUtils import send_status_email
from gcp import gcp

def callCopyToAnomClim(infile, final_out_dir, exp_name, var, supp_string, mailid=None, sbase="", uinput=""):
    ##Copies output to anom+clim output directory and changes the var name
    #out_files = [f for f in os.listdir(final_out_dir) if osp.isfile(osp.join(final_out_dir, f))]
    #out_files = [f for f in out_files if re.search(var, f) is not None]
    #print("var to be searched for:")
    #print(var)
    #print("out_files:")
    #print(out_files)
    #if len(out_files) > 1:
    #    print("Error in callCopyToAnom: more than one f in " + final_out_dir + " matches " + var + "!")
    #    sys.exit(1)
    ##Get modified file path
    #fpath  = osp.join(final_out_dir, out_files[0])

    fpath = osp.join(final_out_dir, osp.basename(infile))
    climanom_var = re.sub("Anom", "", var)
    #climanom_fpath = re.sub(var, climanom_var, fpath)
    climanom_fpath = re.sub("Anom", "", fpath)
    print("Copying supplementary anomaly files to new location!")
    print(fpath)
    print(climanom_fpath)
    gcp(infile, climanom_fpath) #prev. infile=fpath
    nco_command = "ncrename -v " +var+","+climanom_var +" "+climanom_fpath
    try:
        p = subprocess.Popen(nco_command,shell=True,stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    except:
        print("Error in subprocess call of callCopyToAnomClim! Please examine call in more detail.")
        print(nco_command)
        if(mailid is not None):
            send_status_email("failure", 'postProc - copy to anom+clim', exp_name, \
                       traceback = "Error in call to ncrename - call for reference: " + nco_command,\
                       mailid=mailid, logfiles = os.path.join(sbase, "log/out.*"), \
                       outfiles = final_out_dir)
        sys.exit(1)
    error,output = p.communicate()
    print(error)
    print(output)
    if(p.returncode != 0):
        err_message = "error in call to ncrename"
        print(("Error in renaming! There was an " + err_message))
        print(nco_command)
        if(mailid is not None):
            send_status_email("failure", 'postProc - copy to anom+clim', exp_name, \
                       traceback = "Error in call to ncrename - call for reference: " + nco_command,\
                       mailid=mailid, logfiles = os.path.join(sbase, "log/out.*"), \
                       outfiles = final_out_dir)
    else:
        print("gcp and rename successful")
    return(climanom_fpath)



def callAddAnom(final_out_dir,exp_name,var,mailid=None,sbase="",uinput=""):
    #Figure out the final output in the output dir:
    out_files = [f for f in os.listdir(final_out_dir) if osp.isfile(osp.join(final_out_dir, f))]
    #If there is more that one file present, select the one with the shortest name
    anom_file = min(out_files, key=len)
    anom_file = osp.join(final_out_dir, anom_file)
    #Find the location for the clim + anom file
    clim_anom_file = re.sub('Anom','',anom_file)
    clim_anom_var = re.sub('Anom',"",var)
    with Dataset(anom_file, 'r') as nc_obj:
        try:
            clim = nc_obj.__dict__['climatology']
        except KeyError:
            clim_error = 'Climatology not found in input file ' + anom_file
            print(("Error in anomaly addition! " + clim_error + " ;exiting"))
            if( mailid is not None):
                send_status_email("failure", 'callAddAnom', exp_name,
                            traceback = clim_error,
                           mailid=mailid, logfiles = os.path.join(sbase, "log/out.*"),
                           exp_info_file = os.path.join(sbase, 'experiment_info.txt'),
                           outfiles = final_out_dir, xml = uinput)
            exit(1)

    #Remove temporary directories from beginning of pathname,
    #going as far as needed to complete
    clim_split = clim.split("/")
    s_idx = 0
    clim_exists = False
    while(s_idx < len(clim_split)):
        if(osp.exists("/".join(clim_split[s_idx:len(clim_split)]))):
            clim_exists = True
            clim_file = "/".join(clim_split[s_idx:len(clim_split)])
            break
        else:
            s_idx += 1
    if(not clim_exists):
        clim_error = "Climatology file " + clim + ' not found on system'
        print(("Error in anomaly addition!" + clim_error + " ; exiting"))
        if( mailid is not None):
            send_status_email("failure", 'postProc', exp_name,
                           traceback = "Climatology file " + clim + "not found on system",
                           mailid=mailid, logfiles = os.path.join(sbase, "log/out.*"),
                           outfiles = final_out_dir)
        exit(1)
    else:
        if( not osp.exists(osp.dirname(clim_anom_file)) ):
            print('Making output anom + clim directory')
            os.makedirs(osp.dirname(clim_anom_file))
        try:
            s8_orig = os.environ.get("S8_PATH")
        except:
            s8_orig=""
        if (s8_orig is None):
            print("Warning in anomaly addition: $S8_PATH not set.")
            print("resetting to empty string")
            s8_orig=""
        tdir = os.environ.get("TMPDIR")
        clim_anom_file_out = osp.join(tdir, clim_anom_file.lstrip("/"))
        if not osp.exists(osp.dirname(clim_anom_file_out)):
            os.makedirs(osp.dirname(clim_anom_file_out))
        clim_file_out = gcp(clim_file, 'TMPDIR')
        anom_file_out = gcp(anom_file,'TMPDIR')
        #s8_orig = os.environ["S8_PATH"]
        s8_fudge = os.environ["FUDGE_S8_LOC"]
        os.environ["S8_PATH"] = s8_fudge
        #print(os.environ)
        #r_command = "Rscript " + s8_fudge + "/AddAnom.R --clim " + str(clim_file_out) + " --anom " + str(anom_file_out) + ' --var ' + str(clim_anom_var) + " -o " + str(clim_anom_file_out) + " --clone_globals"
        #print "Calling AddAnom.R"
        anomadd_command = "tcsh $BASEDIR/utils/pp/anomadd_script.tcsh " + " ".join([str(clim_anom_var),
                                                                                    str(anom_file_out), str(clim_file_out),
                                                                                    str(clim_anom_file_out)])
        print("Calling anomaly addition")
        print(anomadd_command)
        p = subprocess.Popen(anomadd_command,shell=True,stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out,err = p.communicate()
        print("return from p:")
        print(p)
        print((str(p.returncode)))
        if p.returncode != 0:
            print("Error in subprocess call of anomaly addition! Please examine call in more detail.")
            print(anomadd_command)
            os.environ["S8_PATH"] = s8_orig
            sys.exit(1)
            if(mailid is not None):
                send_status_email("failure", 'postProc - anomAdd', exp_name,
                           traceback = "Error in call to anomaly addition - call for reference: " + anomadd_command,
                           mailid=mailid, logfiles = os.path.join(sbase, "log/out.*"),
                           outfiles = final_out_dir)
        else:
            print("Anomaly addition successful")
            print("Copying to final location")
            gcp(clim_anom_file_out, clim_anom_file)
    return clim_anom_file
