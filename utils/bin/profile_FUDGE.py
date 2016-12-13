#!/bin/python
#Scrapes profiling information from the logfiles in an experiment, 
#given a path to the logfile directory.
#This information is collected on a per-minifile basis, and is written
#to a flat CSV file for later intercomparison (R would work neatly).
import os
from os import path as osp
import re

def make_prof_file(scriptdir, logdir):
	#Creates a flat file of profiling data for the experiment, located at
	# $script_dir/profiling/proffile.csv
	if not osp.isdir(osp.join(scriptdir, "profile")):
		os.makedirs(osp.join(scriptdir, "profile"))
	prof_file = osp.join(scriptdir, 'profile', 'prof_data.csv')
	with open(prof_file, 'w') as f:
		f.write(",".join(['logid', 'npts_OH', 'npts_MH', 'npts_MF',
		                  'runtime', 'maxmemuse', 'location', 
		                  'memavail', 'swapavail']))
	#Start parsing logfiles
	logfiles = os.listdir(logdir)
	logfiles = [el for el in logfiles if osp.isfile(osp.join(logdir, el)) and re.search("out.*", el) is not None]
	for log in logfiles:
		profdata = find_prof_info(osp.join(logdir,log))
		with open(prof_file, 'a') as f:
			f.write("\n"+",".join(profdata))

def find_prof_info(logfile):
	#Parses a FUDGE minifile/subset log and returns 
	#the profiling information printed by the process
	i_id = re.findall("(I[0-9]*)_|(I[0-9]*-[0-9]*)", osp.basename(logfile))
	j_id = re.findall("(J[0-9]*)_|(J[0-9]*-[0-9]*)", osp.basename(logfile))
	#Match is returned as a tuple within a list; joining is safe b/c only one match will fit
	logid = "".join(i_id[0]) + "." + "".join(j_id[0])
	#Okay, we are expecting precisely 8 quantities in the 
	#input file: npts for OH, MH and MF; runtime,
	matches = [] 
	with open(logfile, 'r') as f:
		#matches = re.findall("profile_", f.read())
		for line in f.readlines():
			if "profile_" in line:
				 matches.append(line)
	strip_matches = []			 
	for match in matches:
		match = match.partition(":")[2] #Get all after separator
		strip_matches.append(re.sub("\n", "", match))
	return([logid] + strip_matches)	
	
				                                     	                  	
