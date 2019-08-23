#!/bin/python
#makes ensemble files in a FUDGE workflow

import os, sys
from os import path as osp
import re
import subprocess
from subprocess import PIPE
import time
from get_ds_info import parse_aux_info
from fudgeUtils import send_status_email
from profile_FUDGE import make_prof_file
import tarfile
from collections import OrderedDict
from fudgeUtils import tstamp
from gcp import gcp

def submit_ens_job(raw_outdir, raw_outfile, rips, vars, exp_name, epoch, trange_string, verbose):
    '''
    Run the ensemble concatenation script for an experiment
     .... in hindishgt, any looping by variable should probably take place outside of this loop -
      it simplifies the description of what is an anomaly var, what vars have an
      associated qcmask, etcetera.
    '''
    outfiles = OrderedDict()
    faillist = []

    # infile = osp.join(scriptdir, 'experiment_info.txt')
    nrips = len(rips)
    if(nrips > 1):
        print("More than one experiment rip is present; calling makeExpEns")
        #print("MakeExpEns command-line arguments:")
        #print("makeExpEns --input " + infile)
        #outfiles = runMakeExpEns(expdict, verbose=(verbopt==1), sev=sev)

        rip_sub_string = '<RIP>'
        var_sub_string = '<VAR>'
        make_qc = False
        is_anom_var = [re.search("Anom", el) is not None for el in vars]
        is_trend_var = [re.search("Trend", el) is not None for el in vars]
        raw_outfile = re.sub(".I<ISTART>-<IEND>_J<JSTART>-<JEND>", "", raw_outfile)
        # 2019-08-22: awful hack to take into account that var subsitution
        # isn't yet common across our workflow
        if re.search(var_sub_string, raw_outfile) is None:
            raw_outfile = re.sub(vars[0]+"_", var_sub_string + "_", raw_outfile)
        status = 'success'

        for var, is_anom, is_trend in zip(vars, is_anom_var, is_trend_var):
            var_outdir = re.sub(var_sub_string, var, raw_outdir)
            raw_outpath = osp.join(var_outdir, raw_outfile)
            print("submit_ens_job outfile: " + raw_outpath)
            #sys.exit(1)
            if not osp.isfile(raw_outpath):
                try:
                    outfile = makeExpEns(raw_outpath, rips, var, rip_sub_string, var_sub_string,
                                      exp_name, epoch, trange_string,
                                      verbose=False, write_qcmask=False)
                    outfiles[var] = outfile
                except:
                    status = 'fail'
            else:
                print("File " + raw_outpath + " exists; not remaking")
            outfiles[var] = outfile
            if make_qc:
                print("making qcmask ensemble file")
                var_qc = var + "_qcmask"
                qc_outpath = osp.join(var_outdir, raw_outfile)
                print(qc_outpath)
                if not osp.isfile(raw_outpath):
                    try:
                        outfile = makeExpEns(qc_outpath, rips, var_qc, rip_sub_string, var_sub_string,
                                      exp_name, epoch, trange_string,
                                      verbose=False, write_qcmask=False)
                        outfiles[var_qc] = outfile
                    except:
                        status = 'fail'
                        faillist.append(qc_outpath)
                else:
                    print("File " + raw_outpath + " exists; not remaking")
                outfiles[var_qc] = outfile
            if is_anom:
                #var_anom = var + "Anom"
                var_anom = re.sub("Anom", "", var)
                anom_outpath = re.sub(var_sub_string, var_anom, raw_outdir)
                # 2019-08-22: awful hack for dealing with the fact that var
                # sub strings are not common in outpath file structure yet
                if anom_outpath == raw_outdir:
                    anom_outpath = re.sub(var, var_anom, anom_outpath)
                try:
                    outfile = makeExpEns(osp.join(anom_outpath, raw_outfile), rips, var_anom, rip_sub_string, var_sub_string,
                                      exp_name, epoch, trange_string,
                                      verbose=False, write_qcmask=False)
                    outfiles[var_anom] = outfile
                except:
                    status = 'fail'
                    faillist.append(anom_outpath)
                if make_qc:
                    try:
                        var_anom_qc = var_anom + "_qcmask"
                        outfile = makeExpEns(anom_outpath, rips, var_anom_qc, rip_sub_string, var_sub_string,
                                          exp_name, epoch, trange_string,
                                          verbose=False, write_qcmask=False)
                        outfiles[var_anom_qc] = outfile
                    except:
                        status = 'fail'
                        faillist.append(anom_outpath)
            if is_trend:
                #var_trend = var + "Trend"
                var_trend = re.sub("Trend", "", var)
                trend_outpath = re.sub(var_sub_string, var_trend, raw_outdir)
                # 2019-08-22: similar awful hack, but for trends
                if trend_outpath == raw_outdir:
                    trend_outpath = re.sub(var, var_trend, anom_outpath)
                try:
                    outfile = makeExpEns(trend_outpath, rips, var_trend, rip_sub_string, var_sub_string,
                                         exp_name, epoch, trange_string,
                                         verbose=False, write_qcmask=False)
                    outfiles[var_trend] = outfile
                except:
                        status = 'fail'
                        faillist.append(trend_outpath)
                if make_qc:
                    var_trend_qc = var_trend + "_qcmask"
                    try:
                        outfile = makeExpEns(trend_outpath, rips, var_trend_qc, rip_sub_string, var_sub_string,
                                          exp_name, epoch, trange_string,
                                          verbose=False, write_qcmask=False)
                        outfiles[var_trend_qc] = outfile
                    except:
                        status = 'fail'
                        faillist.append(trend_outpath)
    for el in outfiles.keys():
        print(el)
        print(outfiles[el])
    exp_info_files = []
    return(status,faillist,outfiles,exp_info_files)


def makeExpEns(raw_outpath, rips, ds_var, rip_sub_string, var_sub_string,
                  exp_name, epoch, trange_string,
                  verbose=False, write_qcmask=False):
    '''
    Calls concatenation for members of an ensemble file in an ESD-specific dir structure
    We will probably need separate calls for qcmask and anomaly, but that's the calling function's problem
    Missing setting from orig: anom_only_ens and rec_only_ens
    '''
    basedir = os.environ.get('BASEDIR')
    tdir = os.environ.get('TMPDIR')
    tdir_ens = osp.join(tdir, "proc_ens")
    if not osp.exists(tdir_ens):
        os.makedirs(tdir_ens)
    var_outpath = re.sub(var_sub_string, ds_var, raw_outpath)
    in_files = []
    for rip in rips:
        infile = re.sub(rip_sub_string, rip, var_outpath)
        if not osp.isfile(osp.abspath(infile)):
            print("Error in makeExpEns! The following file did not exist:")
            print(infile)
            sys.exit(1)
        in_files.append(infile)
    # Now you have a list of files that will need to be copied/ncecatted
    # build output file
    rip_suffix = rips[1][-4:]
    out_rip = "r1to" + str(len(rips)) + rip_suffix
    outfile = re.sub(rip_sub_string, out_rip, var_outpath)
    outfile = re.sub(".nc$", "_ensmbl.nc", outfile)
    print("Ensemble output file location: " + outfile)
    if not osp.exists(outfile):
        if not osp.exists(osp.dirname(outfile)):
            print("Making dir " + osp.dirname(outfile))
            os.makedirs(osp.dirname(outfile))
        tmp_outfile = osp.join(tdir_ens, osp.basename(outfile) + tstamp())
        orig_filestring = ",".join([osp.basename(el) for el in in_files])
        copy_filestring = ",".join([gcp(f, osp.join(tdir_ens,osp.basename(f)),
                                        verbose=True, gcp_verbose=True)
                                    for f in in_files])
        # Finally, call the ensemble generation code:
        ens_min = basedir + "utils/s7/ens_min.csh"
        ens_command = " ".join(["tcsh -f ",ens_min, tmp_outfile, ds_var,
                                ",".join(rips), exp_name, epoch, trange_string,
                                orig_filestring, copy_filestring, 'Exp'])
        # Dev edit - trying to debug the labeling process
        p = subprocess.Popen(ens_command, stdout=subprocess.PIPE, shell=True, env=None, cwd=tdir)
        out0,err0 = p.communicate()
        if (p.returncode != 0):
            print(str(out0))
            print(str(err0))
            print("Error in processing ensemble " +  outfile + "; call to c-shell failed.")
            print("Command that failed: " + ens_command)
            sys.exit(1)
            return(None)
        else:
            if verbose:
                print(str(out0))
                print(str(err0))
            gcp(tmp_outfile, outfile, False, verbose)
            return(outfile)
    else:
        print("outfile already exists; skipping ens creation")
        return(outfile)
