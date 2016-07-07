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
from fudgeUtils import send_status_email
import makeExpEns
from makeExpEns import runMakeExpEns
import makeCVEns
from makeCVEns import runMakeCVEns
import cgitb

def runPPWatcher(infile,timeout,verbopt,nloops,sleeptime,mailid,sev):	
	#print(" ".join([str(infile), str(timeout), str(verbopt), str(nloops), str(sleeptime), mailid, str(sev)]))
	#sys.exit("This should trigger the end of the script")
	expdict = read_exp_info_file(infile)
	pp_statdir = osp.join(expdict['script.base'], "postProc/ppStatus")
	logfile_loc = osp.join(expdict['script.base'], 'log/fudge_postProc_.mfudge_*')
	nrips = len(expdict['output.path'])
	print("Number of rips searched for:" + str(nrips))
	#Check for whether or not an experiment has a cross-validated companion
	exp_name = expdict['ds.experiment']
	isCrossval = False
	warnCrossval=False
	crossval_patt = ['00r1e2', '00r2e2']
	cv_match = [crossval_patt.index(c) for c in crossval_patt if re.search(c, exp_name) is not None]
	if(len(cv_match) == 1):
		#Need a check in here to print warnings for the r1e2 case
		print("crossval match:" + str(cv_match))
		#if re.search(crossval_patt[0], exp_name) is not None:
		#	print "This should trigger the cv_match check"
		#	warnCrossval = True
		#print cv_match	
		if cv_match[0] == 0:
			warnCrossval = True	
		patt2 = 1 - cv_match[0]
		exp_name2 = re.sub(crossval_patt[cv_match[0]], crossval_patt[patt2], exp_name)
		project = re.split("/", expdict['script.base'])
		project = project[len(project)-1]
		user = os.environ.get('USER')
		if (user!='esd'):
			print("Warning in runPPWatcher: checking for cross-validated experiments assumes an ESD-specific dir structure")
		complement_exp = osp.join("/home/", user, "PROJECTS/DOWNSCALING/SUBPROJECTS/", exp_name2)
		print(complement_exp)
		if osp.isdir(complement_exp) and not warnCrossval:
			isCrossval = True
	loop_count = 0
	complete=False
	while(loop_count < nloops and not complete):
		print("starting loop " + str(loop_count) + " of " + str(nloops))
		print("Checking " + pp_statdir + " for status files")
		status_files = [p for p in os.listdir(pp_statdir) if osp.isfile(osp.join(pp_statdir,p))]
		print(status_files)
		nfail = [p for p in status_files if re.search('failure',p) is not None]
		if len(nfail) != 0:
			print("Error in ppWatcher: One or more pp jobs have failed! Watcher exiting.")
			send_status_email("failure", 'ppWatcher', exp_name, \
		                   traceback="One or more pp jobs have failed; sample jobs include " + \
		                   (",").join([re.sub(".failure", "", p) for p in nfail]), \
		                   mailid=mailid, logfiles = logfile_loc, 
		                   exp_info_file = infile, outfiles = "")                   
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
			outfiles = runMakeExpEns(expdict, verbose=(verbopt==1), sev=sev)
			#...check on the error returns from this. There shouldn't be an issue,
			#since multiple exit points aren't that much of an issue and you
			#WANT things to come to a screeching halt, but that may change later.
		else:
			outdir = osp.dirname(expdict['output.path'][0])
			outdir_contents = os.listdir(outdir)	
			outfiles = [osp.join(outdir, p) for p in outdir_contents if re.search(".nc$", p) is not None]
			outfiles = "\n\t".join(outfiles)
		if warnCrossval: 
			print("\033[1;43mExperiment " + exp_name +" has a cross-validated complement " + exp_name2 + " ; please run complement to get cross-validated ensembles.\033[1;m ")	
		if isCrossval: 
			#Check for presence of cross-validated experiment
			#Check for the presence of a cross-validated lockfile in either dir
			#before starting the process (prevents multiple writes to same file)
			#cv_statfiles = len(p for p in osp.join())
			#if(len(cv_statfiles) > 0):
			#	print("Cross-validation ensemble creation for this experiment is either in progress or has completed. Please check the experiment_info file.") 
			#...in the event that the above errors out, you probably want this to 
			#have a command-line argument as well
			#else:
				#print("Calling cross-validated ensemble creation script")
				#print("Or we would, if it existed right now.")
				#try:
			#print("Cross-validated ensemble script not yet ready; cross-validation will not take place")	
			#print(" ".join([exp_name, exp_name2, str(expdict['script.base']), str(complement_exp)]))
			cv_outfiles = runMakeCVEns(exp_name, exp_name2, expdict['script.base'], complement_exp, verbose=(verbopt==1))
			outfiles = outfiles + "\n" + cv_outfiles
				#except:
				#	#import sys, cgitb
				#	#cgitb.enable()
				#	print(cgitb.text(sys.exc_info()))
				#	send_ppWatcher_message('failure')
				#else:
				#	send_ppWatcher_message('success')		
			#Make cross-validated ensemble; add to the relevant experiment info files
		send_status_email("success", 'complete', exp_name, \
		                  traceback="", \
		                  mailid=mailid, logfiles = osp.join(expdict['script.base'], 'log'), \
		                  exp_info_file = infile, outfiles = outfiles)
			
	else:
		print "postProc job watcher has timed out. Please re-submit jobs or examine logfiles located at:" + logfile_loc
		send_status_email("failure", 'ppWatcher', exp_name, \
		                   traceback="Job timed out", \
		                   mailid=mailid, logfiles = logfile_loc, exp_info_file = infile, 
		                   outfiles = expdict['output.path'])
		sys.exit(1)                   		                       
	sys.exit(0)		
	
if __name__ == '__main__':
	print(sys.argv)
	runPPWatcher(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], 
	             sys.argv[5], sys.argv[6], sys.argv[7])				
