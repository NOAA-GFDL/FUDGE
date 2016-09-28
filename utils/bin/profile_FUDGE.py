#!/bin/python
#Scrapes profiling information from the logfiles in an experiment, 
#given a path to the logfile directory.
#This information is collected on a per-minifile basis, and is written
#to a flat CSV file for later intercomparison (R would work neatly).

def make_proffifle(logdir):
	#Creates a flat file of profiling data for the experiment, located at
	# $script_dir/profiling/proffile.csv
	if not exists (osp.join(scriptdir, "profile"))
		os.makedirs(scriptdir, "profile")
	prof_file = osp.join(scriptdir, 'profile', 'prof_data.csv')
	with open(prof_file, 'w') as f:
		f.write(",".join(['logid', 'npts_OH', 'npts_MH', 'npts_MF',
		                  'runtime', 'maxmemuse', 'location', 'memavail']))
	#Start parsing logfiles
	logfiles = os.listdir(logdir)
	logfiles = [el for el in logfiles if isfile(el) and 
	                                     re.search("out.*", el) is not None]
	for log in logfiles:
		profdata = find_prof_info(log)
		with open(prof_file, 'a') as f:
			f.write(",".join(profdata))

def find_prof_info(logfile):
	#Parses a FUDGE minifile/subset log and returns 
	#the profiling information printed by the process
	i_id = ""
	j_id = ""
	logid = i_id + "." + j_id
	#Okay, we are expecting precisely 8 quantities in the 
	#input file: npts for OH, MH and MF; runtime, 
	with open(logfile, 'r') as f:
		matches = re.findall("profile_", f.read())
	for match in matches:
		match = match.partition(":")[2] #Get all after separator
	return([logid] + matches)	
	
				                                     	                  	
