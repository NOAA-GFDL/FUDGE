#!/usr/local/python/2.7.3/bin/python
import os,sys,subprocess,shutil,time
from os import path as osp
from subprocess import PIPE
import argparse
import shlex
import checker
import smtplib
import re
import optparse
from optparse import OptionParser
import netCDF4
from netCDF4 import Dataset
from fudgeUtils import tstamp
from fudgeUtils import read_exp_info_file
from fudgeUtils import send_status_email
from gcp import gcp
import makeExpEns
from makeExpEns import runMakeExpEns
from makeExpEns import ens_namemap
import cgitb

def makeCVEns():
    #Input parsing and arg checking for main body of code
    usage = "usage: %prog --exp1 exp1 --exp2 exp2 --script1 script/dir/1 --script2 script/dir/2 [--verbose]"
    parser = OptionParser(usage=usage, version='makeCVEns 1.0')
    parser.add_option("--exp1", dest="exp1", help="First experiment being cross-validated. Order does not matter.")
    parser.add_option("--exp2", dest="exp2", help="Second experiment being cross-validated. Order does not matter.")
    parser.add_option("--script1", dest='script1', help="First script dir. Order does not matter.")
    parser.add_option("--script2", dest='script2', help="Second script dir. Order does not matter.")
    parser.add_option("-v", "--verbose", action="store_true", dest="verbose", default=False, help="Print extra status messages used to help debugging. Defaults to False.")
    (options, args) = parser.parse_args()
    runMakeCVEns(options.exp1, options.exp2, options.script1, options.script2, options.verbose)

def runMakeCVEns(exp1, exp2, scriptdir1, scriptdir2, verbose=False):
    #Makes the two cross-validated ensemble files for two historical
    #experiments in the current setup.

    #print(" ".join([exp1, exp2, scriptdir1, scriptdir2]))
    #sys.exit(1)
    ###Wait on adding the status files/lockfiles for later.
    #for p, exp in [scriptdir1, scriptdir2],[exp1,exp2]:
    #	fname = exp + ".lock"
    #	open(osp.join(p, "postProc/cvStatus/",fname), 'a').close()
    exp1,exp2 = sorted([exp1,exp2])
    scriptdir1,scriptdir2 = sorted([scriptdir1,scriptdir2])
    expdict1 = read_exp_info_file(osp.join(scriptdir1,"experiment_info.txt"))
    expdict2 = read_exp_info_file(osp.join(scriptdir2,"experiment_info.txt"))
    print(expdict1)
    print((expdict1['fut.train.start.year_1']))
    print((expdict1['fut.train.end.year_1']))
    trange_string = expdict1['fut.train.start.year_1'] +"-" + expdict1['fut.train.end.year_1']
    ds_exp1 = [("/").join(p.split("/")[0:len(p.split("/"))-1]) for p in expdict1['output.path']] #trim off OneD
    ds_exp2 = [("/").join(p.split("/")[0:len(p.split("/"))-1]) for p in expdict2['output.path']]
    ds_rip1 = [re.search("(r[0-9]*i[0-9]*p[0-9]*)", p).group() for p in expdict1['output.path']] #r1 group
    ds_rip2 = [re.search("(r[0-9]*i[0-9]*p[0-9]*)", p).group() for p in expdict2['output.path']] #r2 group
    print(ds_rip1)
    print(ds_rip2)
    dsvar = expdict1['target']
    if re.search("Anom", dsvar) is not None:
        isAnom=True
    else:
        isAnom=False
    if re.search("Detr", dsvar) is not None:
        isTrend=True
    else:
        isTrend=False
    print("Is this experiment an anomaly experiment?")
    print(isAnom)
    print("Is this experiment a detrended experiment experiment?")
    print(isAnom)
    #Do a brief check for anomaly vars in the input files
    #There are two distinct classes of file; create each in turn
    setdict = {'DSDEPh': {'odesc': 'dp', 'dataID':'DSDEPh', 'kfold':'K00',
                          'inDir1': ds_exp1[0], 'inDir2': ds_exp2[1],
                          'f1r' : ds_rip1[0], 'f2r' : ds_rip2[1],
                         'exp1': exp2, 'exp2': exp1,
                          'inPath1': ds_exp1[0], 'inPath2' : ds_exp2[1],
                          'ktitle': "dep"},
               'DSCVh': {'odesc': 'cv', 'dataID':'DSCVh', 'kfold':'K02',
                         'inDir1': ds_exp1[1], 'inDir2': ds_exp2[0],
                         'f1r' : ds_rip2[0], 'f2r' : ds_rip1[1],
                         'exp1': exp2, 'exp2': exp1,
                         'inPath1': ds_exp2[0], 'inPath2' : ds_exp1[1],
                         'ktitle': 'cv'}
    }
    ens_list = []
    for kty in ['DSDEPh', 'DSCVh']:
        kdict = setdict[kty]
        klist = []
        #for outpaths in [ds_exp1,ds_exp2]:
        print((kdict['inPath1']))
        outpaths = [kdict['inPath1'] , kdict['inPath2']]
        all_outfiles =  [[f for f in os.listdir(p) if osp.isfile(osp.join(p, f))] for p in outpaths]
        all_outfiles = [sorted(lst) for lst in all_outfiles]
        num_files = [len(flist) for flist in all_outfiles]
        if (num_files.count(num_files[0]) != len(num_files)):
            print(("Error in makeCVEns: number of output files in directories " + ",".join(outpaths) + "should be identical! Please re-run pp jobs."))
            sys.exit(1)
        elif (num_files[0]==0):
            print(("Error in makeCVEns: no files found in output directories "+ ",".join(outpaths) + "! Please re-run pp jobs."))
            sys.exit(1)
        all_outfiles = list(zip(*all_outfiles))
        #Each f in all_outfiles should either contain _qcmask or not contain _qcmask
        for lst in all_outfiles:
            if re.search("_qcmask", lst[0]) is not None:
                if any(re.search("_qcmask", el) is None for el in lst):
                    print("Error in makeExpEns: tried to concatenate incompatible files:")
                    print(lst)
                    sys.exit(100)
            else:
                if any(re.search("_qcmask", el) is not None for el in lst):
                    print("Error in makeExpEns: tried to concatenate incompatible files:")
                    print(lst)
                    sys.exit(100)
        #Add entries to dictionary, after sorting:
        print(all_outfiles)
        for flist in all_outfiles:
            print(flist)
            flist = [osp.join(p,f) for p,f in zip(outpaths,flist)]
            kfiles = call_cv_ens_creation(flist, dsvar, [kdict['f1r'],kdict['f2r']], \
            ",".join([kdict['f1r'],kdict['f2r']]), kty, kdict['odesc'], kdict['kfold'],\
            [kdict['exp1'], kdict['exp2']], trange_string,\
            isAnom=isAnom,isTrend=isTrend,recOnly=False,verbose=False)
            #ens_list.append(outfile)
            print(("output file(s):" + str(kfiles)))
            klist = klist + kfiles
        klist = ens_namemap(klist)
        klist = [kdict['ktitle'] + "." + named_file for named_file in klist]
        ens_list = ens_list + klist
    print("Ensemble files:")
    for el in ens_list:
        print(el)
    #title_list = ["dep.ensemble:", "cv.ensemble:"]
    #ens_title = zip(title_list, ens_list)
    #print(ens_title)
    #ens_string = "\n" + "#"*80 +"\n" + "\n".join(["".join(el) for el in ens_title])
    ens_string = "\n####" + " Downscaled cross-validated ensemble output " + "#"*31 + "\n" + "\n".join(ens_list)
    print(ens_string)
    #Okay, now append this to the bottom of all downscaled, ensembled and cross-validated experiment info files
    scriptdirs = [scriptdir1, scriptdir2]
    dsdirs = [osp.dirname(osp.dirname(el)) for el in ds_exp1] + [osp.dirname(osp.dirname(el)) for el in ds_exp2]
    #...and that should be it for now, right? Is there any need to do the experiment ensembles?
    ds_ensdirs = []
    #print(dsdirs)
    for el in dsdirs:
        dirsplit = re.split("/", el)
        rip = dirsplit[-3]
        rip = re.sub("r[0-9]*", "r1to2", rip)
        dirsplit[-3] = rip
        ds_ensdirs.append("/".join(dirsplit))
    #cross-validated script dirs
    #Note: these do not have experiment info files
    #cv_dirs = [osp.dirname(osp.dirname(osp.dirname(el))) for el in ens_list]
    alldirs = [re.sub(".*:","", el) for el in scriptdirs + dsdirs + ds_ensdirs]
    alldirs = list(set(alldirs))
    for el in alldirs:
        exp_info_file = osp.join(el, "experiment_info.txt")
        print(exp_info_file)
        if osp.isfile(exp_info_file):
            print(("Editing " + exp_info_file + " in place for xml apending."))
            with open(exp_info_file, 'a') as f:
                f.write(ens_string)
    return("\t" + "\n\t".join(ens_list))

def call_cv_ens_creation(flist,climvar,rips,rip_string,cv_type,odesc,kfold_string,
                        exp_names,trange_string,
                        isAnom=False,isTrend=False,recOnly=False,verbose=False):
    #Given a list of files and some handy metadata information,
    #calls a simple anomaly addition script to generate the
    #flist = [osp.join(p,f) for p,f in zip(outpaths,flist)]
    basedir = os.environ.get('BASEDIR')
    tdir = os.environ.get('TMPDIR')
    tdir_ens = osp.join(tdir, "proc_cv_ens")
    if not osp.exists(tdir_ens):
        os.makedirs(tdir_ens)
    for f in flist:
        if not osp.isfile(f):
            print(("Error in call_cv_ens_creation! File " + f + " does not exist!"))
    inFiles = ",".join([gcp(f, osp.join(tdir_ens,osp.basename(f)), True, verbose, verbose) for f in flist])
    orig_inFiles = ",".join([osp.basename(p) for p in flist])
    print((" ".join(["Calling", cv_type, " ensemble creation on ", inFiles])))
    with Dataset(flist[0], 'r') as nc_obj:
        varnames = list(nc_obj.variables.keys())
    vname = [varnames[v] for v in range(0, len(varnames)) if re.search(climvar,varnames[v]) is not None][0]
    supp_strings = ["qcmask","save"]
    if isAnom:
        print("Checking for presence of anomaly var")
        if supp_strings[0] not in vname and supp_strings[1] not in vname:
            if not recOnly:
                outfile_anom = call_cv_ens_creation(flist,climvar,rips,rip_string,cv_type,odesc,kfold_string,
                                                    exp_names,trange_string,isAnom=False)
            else:
                outfile_anom = []
            print(("Target var " + vname + ' is an anomaly var; searching for anom + clim files'))
            clim_files = [re.sub("Anom","",f) for f in flist]
            climvar = re.sub("Anom", "", climvar)
            try:
                outfile_clim = call_cv_ens_creation(clim_files,climvar,rips,rip_string,cv_type,odesc,kfold_string,
                                                    exp_names,trange_string,isAnom=False)
            except:
                raise
            return(outfile_clim + outfile_anom)
        else:
            #For the qcmasks and anomaly files,
            print(("Target var " + vname + ' is an anomaly var; searching for anom + clim files'))
            clim_files = [re.sub("Anom","",f) for f in flist]
            climvar = re.sub("Anom", "", climvar)
            try:
                print(("qcmask clim_files: " + str(clim_files)))
                print(("qcmask climvar: " + climvar))
                outfile_clim = call_cv_ens_creation(clim_files,climvar,rips,rip_string,cv_type,odesc,kfold_string,
                                                    exp_names,trange_string,isAnom=False)
            except:
                raise
            return(outfile_clim)
    elif isTrend:
        print("Checking for presence of detrended var")
        if supp_strings[0] not in vname and supp_strings[1] not in vname:
            if not recOnly:
                outfile_anom = call_cv_ens_creation(flist,climvar,rips,rip_string,cv_type,odesc,kfold_string,
                                                    exp_names,trange_string,isTrend=False)
            else:
                outfile_anom = []
            print(("Target var " + vname + ' is a detrended var; searching for trend + resid files'))
            trend_method = climvar.split("Detr")[-1]
            orig_var = re.sub("Detr"+trend_method, "", vname)
            orig_clim = re.sub("Detr"+trend_method, "", climvar)
            clim_files = [re.sub(climvar,orig_clim,f) for f in flist]
            try:
                outfile_clim = call_cv_ens_creation(clim_files,orig_clim,rips,rip_string,cv_type,odesc,kfold_string,
                                                    exp_names,trange_string,isTrend=False)
            except:
                raise
            return(outfile_clim + outfile_anom)
        else:
            #For the qcmasks and anomaly files,
            print(("Target var " + vname + ' is a detrended var; searching for trend + resid files'))
            trend_method = climvar.split("Detr")[-1]
            orig_var = re.sub("Detr"+trend_method, "", vname)
            orig_clim = re.sub("Detr"+trend_method, "", climvar)
            clim_files = [re.sub(climvar,orig_clim,f) for f in flist]
            try:
                print(("qcmask trend_files: " + str(clim_files)))
                print(("qcmask trendvar: " + orig_clim))
                outfile_clim = call_cv_ens_creation(clim_files,orig_clim,rips,rip_string,cv_type,odesc,kfold_string,
                                                    exp_names,trange_string,isAnom=False)
            except:
                raise
            return(outfile_clim)
    #Generate output file by doing a series of find-and-replace regexes on outdir
    current_rip = [rips[r] for r in range(0,len(rips)) if re.search(rips[r],flist[0]) is not None][0]
    outfile = make_cv_ens_name(osp.dirname(flist[0]), osp.basename(flist[0]), cv_type, odesc,kfold_string)
    print(("Output file location: " + outfile))
    if not osp.exists(osp.dirname(outfile)):
        print(("Making " + osp.dirname(outfile)))
        os.makedirs(osp.dirname(outfile))
    tmp_outfile = osp.join(tdir_ens,osp.basename(outfile)+tstamp())
    #Finally, call the ensemble generation code:
    ens_min = basedir + "utils/s7/ens_min.csh"
    ens_command = " ".join(["tcsh -f ",ens_min, tmp_outfile, vname, rip_string, exp_names[0], cv_type, trange_string, orig_inFiles, inFiles, "CV"])
    print('calling c-shell code')
    p = subprocess.Popen(ens_command, stdout=subprocess.PIPE, shell=True, env=None, cwd=tdir)
    out0,err0 = p.communicate()
    if (p.returncode != 0):
        print((str(out0)))
        print((str(err0)))
        print(("Error in processing ensemble " +  inFiles + "; call to c-shell failed."))
        print(("Command that failed: " + ens_command))
        sys.exit(1)
        return(None)
    else:
        if verbose:
            print((str(out0)))
            print((str(err0)))
        gcp(tmp_outfile, outfile, verbose, verbose)
        print(("Copied to " + outfile))
        return([outfile])

def make_cv_ens_name(orig_path, orig_fname, datatype, odesc, kfold, is_supplement=False):
    #Makes a cross-validated ensemble outpath/filename given a non-cv
    #component:
    #Note that this assumes the US48 directory is the last dir in the pathname
    #is_supplement tells you whether the output file you are looking at
    #is a qcmask or a save file - those have a slightly different
    #filename structure, and need to be parsed differently
    ds_rip = re.search("(r[0-9]*i[0-9]*p[0-9]*)", orig_path).group()
    #if is_supplement:
    orig_expname = re.split("_", orig_fname)[-5]
    #else:
    #	orig_expname = re.split("_", orig_fname)[2]
    print(("original experiment name: " + orig_expname))
    exp_design = re.split("-", orig_fname)[2]
    #Okay, you want the single-letter component before 00
    exp_re = re.compile('([A-Z]*)00')
    exp_string = re.search(exp_re, exp_design).group()
    cv_expname = re.sub("[A-Z]00r[0-9]*e2", exp_string + odesc + "e2", orig_expname)
    cv_expname = re.sub("K00", kfold, cv_expname)
    print(("New experiment name: " + cv_expname))
    cv_path = re.sub(orig_expname, cv_expname, orig_path)
    cv_fname = re.sub(orig_expname, cv_expname, orig_fname)
    #From this point on, make the changes to the new things instead
    cv_path = re.sub(ds_rip, datatype, cv_path)
    cv_fname = re.sub(ds_rip, datatype, cv_fname)
    #Finally, add _ensmbl.nc
    cv_fname = re.sub(".nc", "_ensmbl.nc", cv_fname)
    if not (osp.exists(cv_path)):
        os.makedirs(cv_path)
    return(osp.join(cv_path, cv_fname))


if __name__ == '__main__':
    makeCVEns()
    #sys.exit([0])
