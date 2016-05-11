#!/usr/local/python/2.7.3/bin/python
#import expergen 
#import fudgeList
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
from email.MIMEMultipart import MIMEMultipart
from email.MIMEBase import MIMEBase
from email.MIMEText import MIMEText
from email.Utils import COMMASPACE, formatdate
from email import Encoders
import netCDF4
from netCDF4 import Dataset
import fudgeUtils
from fudgeUtils import gcp,tstamp
from fudgeUtils import read_exp_info_file
import makeExpEns
from makeExpEns import runMakeExpEns

def runPPWatcher(infile,timeout,verbopt,nloops,sleeptime,mailid):	
	expdict = read_exp_info_file(infile)
	pp_statdir = osp.join(expdict['script.base'], "postProc/ppStatus")
	logfile_loc = osp.join(expdict['script.base'], 'log/fudge_postProc_.mfudge_*')
	nrips = len(expdict['output.path'])
	#Check for whether or not an experiment has a cross-validated companion
	exp_name = expdict['ds.experiment']
	isCrossval = False
	crossval_patt = ['00r1e2', '00r2e2']
	cv_match = [crossval_patt.index(c) for c in crossval_patt if re.search(c, exp_name) is not None]
	if(len(cv_match) == 1):
		patt2 = 1 - cv_match[0]
		exp_name2 = re.sub(crossval_patt[cv_match[0]], crossval_patt[patt2], exp_name)
		project = split(expdict['script.base'])
		project = project[len(project)-1]
		user = os.environ.get('USER')
		if (user!='esd'):
			print("Warning in runPPWatcher: checking for cross-validated experiments assumes an ESD-specific dir structure")
		complement_exp = osp.join("/home/", user, "PROJECTS/DOWNSCALING/SUBPROJECTS/", exp_name2)
		if (os.listdir(complement_exp)):
			isCrossval = True
	loop_count = 0
	complete=False
	while(loop_count < nloops and not complete):
		print("starting loop " + str(loop_count) + " of " + str(nloops))
		print("Checking " + pp_statdir + " for status files")
		status_files = [p for p in os.listdir(pp_statdir) if osp.isfile(osp.join(pp_statdir,p))]
		print(status_files)
		nfail = len([p for p in status_files if re.search('failure',p) is not None])
		if nfail != 0:
			print("Error in ppWatcher: One or more pp jobs have failed! Watcher exiting.")
			send_ppWatcher_message('success', 'what is a step', expdict['ds.experiment'], 'what is the error', \
		                       expdict['script.base'], minilog, pplog, mailid)
			sys.exit(1)
		else:
			nsucceed = len([p for p in status_files if re.search('success',p) is not None])
			print("number of postproc jobs complete: " + str(nsucceed))
			if nsucceed != nrips:
				loop_count = loop_count + 1
				time.sleep(sleeptime)
			else:
				loop_count = loop_count+1
				complete=True
	if(complete):
		print "postProc jobs are complete."
		if(nrips > 1):
			print("More than one experiment rip is present; calling makeExpEns")
			runMakeExpEns(expdict, verbose=(verbopt==1))
			#...check on the error returns from this. There shouldn't be an issue,
			#since multiple exit points aren't that much of an issue and you
			#WANT things to come to a screeching halt, but that may change later.
		if isCrossval: 
			#Check for presence of cross-validated experiment
			#Check for the presence of a cross-validated lockfile in either dir
			#before starting the process (prevents multiple writes to same file)
			cv_statfiles = len(p for p in osp.join())
			if(len(cv_statfiles) > 0):
				print("Cross-validation ensemble creation for this experiment is either in progress or has completed. Please check the experiment_info file.") 
			#...in the event that the above errors out, you probably want this to 
			#have a command-line argument as well
			else:
				print("Calling cross-validated ensemble creation script")
				print("Or we would, if it existed right now.")
				#runMakeCVEns(exp_name, exp_name2, expdict['script.base'], complement_exp, verbose=(verbopt==1))
			#Make cross-validated ensemble; add to the relevant experiment info files	
	else:
		print "postProc job watcher has timed out. Please re-submit jobs or examine logfiles located at:" + logfile_loc
		send_ppWatcher_message('failure', 'what is a step', expdict['ds.experiment'], 'what is the error', \
		                       expdict['script.base'], minilog, pplog, mailid)
	sys.exit(0)
		
def runMakeCVEns(exp1, exp2, scriptdir1, scriptdir2, verbose=False):
	#Makes the two cross-validated ensemble files for two historical
	#experiments in the current setup. 
	for p, exp in [scriptdir1, scriptdir2],[exp1,exp2]:
		fname = exp + ".lock"
		open(osp.join(p, "postProc/cvStatus/",fname), 'a').close()	
	exp1,exp2 = sort(exp1,exp2)
	scriptdir1,scriptdir2 = sort(scriptdir1,scriptdir2)
	expdict1 = read_exp_info_file(osp.join(scriptdir1,"experiment_info.txt"))
	expdict2 = read_exp_info_file(osp.join(scriptdir1,"experiment_info.txt"))	
	#Do a brief check for anomaly vars in the input files	
	#There are two distinct classes of file; create each in turn
	setdict = {'DSDEPh': {'odesc': 'dp', 'dataID':'DSDEPh', 'kfold':'K00', }}
	for kty in ['DSDEPh', 'DSCVh']:
		try:
			call_cv_ens_creation()
		except:
			for p, exp in [scriptdir1, scriptdir2],[exp1,exp2]:
				prevname = exp + ".lock"
				os.remove(osp.join(p, "postProc/cvStatus/",prevname), 'a')	
				fname = exp + "_" + kty + "_" +".failure"	
				open(osp.join(p, "postProc/cvStatus/",fname), 'a').close()	
		else:
			for p, exp in [scriptdir1, scriptdir2],[exp1,exp2]:
				prevname = exp + ".lock"
				os.remove(osp.join(p, "postProc/cvStatus/",prevname), 'a')	
				fname = exp + "_" + kty + "_" + ".success"	
				open(osp.join(p, "postProc/cvStatus/",fname), 'a').close()	
	
def call_cv_ens_creation(flist,climvar,rips,rip_string,out_rip,exp_name,epoch,
						trange_string,isAnom=False, recOnly=False,verbose=False):	
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
			print("Error in call_cv_ens_creation! File " + f + " does not exist!")
			sys.exit(1)
	inFiles = ",".join([gcp(f, osp.join(tdir_ens,osp.basename(f)), True, verbose, verbose) for f in flist])
	orig_inFiles = ",".join(flist)
	print("Calling", cv_type, "ensemble creation on " + inFiles)
	with Dataset(flist[0], 'r') as nc_obj:
		varnames = nc_obj.variables.keys()
	vname = [varnames[v] for v in range(0, len(varnames)) if re.search(climvar,varnames[v]) is not None][0]
	if isAnom:
		print("Checking for presence of anomaly var")
		supp_strings = ["qcmask","save"]
		if supp_strings[0] not in vname and supp_strings[1] not in vname:
			if not recOnly:
				outfile_anom = call_ens_creation(flist,climvar,rips,rip_string,out_rip,exp_name,epoch,trange_string,False)
			else:
				outfile_anom = []	
			print("Target var " + vname + ' is an anomaly var; searching for anom + clim files')
			anom_files = [re.sub("Anom","",f) for f in flist]
			climvar = re.sub("Anom", "", climvar)
			outfile_clim = call_ens_creation(anom_files,climvar,rips,rip_string,out_rip,exp_name,epoch,trange_string,False)
			return(outfile_clim + outfile_anom)
	#Generate output file by doing a series of find-and-replace regexes on outdir
	current_rip = [rips[r] for r in range(0,len(rips)) if re.search(rips[r],flist[0]) is not None][0]	
	outfile = re.sub(current_rip, out_rip, flist[0])
	outfile = re.sub("\.nc$", "_ensmbl.nc", outfile)
	print("Output file location: " + outfile)
	if not osp.exists(osp.dirname(outfile)):
		print("Making " + osp.dirname(outfile))
		os.makedirs(osp.dirname(outfile))
	tmp_outfile = osp.join(tdir_ens,osp.basename(outfile)+tstamp())			
	#Finally, call the ensemble generation code:	
	ens_min = basedir + "utils/s7/ens_min.csh"	
	ens_command = " ".join(["tcsh -f ",ens_min, tmp_outfile, vname, rip_string, exp_name, epoch, trange_string, orig_inFiles, inFiles])
	p = subprocess.Popen(ens_command, stdout=subprocess.PIPE, shell=True, env=None, cwd=tdir)
	out0,err0 = p.communicate()
	if (p.returncode != 0):
		print(str(out0))
		print(str(err0))
		print("Error in processing ensemble " +  inFiles + "; call to c-shell failed.")
		print("Command that failed: " + ens_command)
		sys.exit(1)
		return(None)
	else:
		if verbose:
			print(str(out0))
			print(str(err0))
		gcp(tmp_outfile, outfile, verbose, verbose)
		return([outfile])
						
