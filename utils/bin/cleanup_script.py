#cleanup_script.py
#performs same functions as the original csh watcher script, 
#but with easier file path parsing

import expergen
import shutil, re
import os, sys
from os import path

def get_ensemble_path(outdirs, rips):
	range_list = [int(r[1]) for r in rips]
	newrip = "r" + str(min(range_list)) + "to" + str(max(range_list)) + rips[0][2:]
	r = 0
	match = False
	ens_outdir = None
	while(match==False and r < len(rips)):
		if re.search(rips[r], outdirs[0]) is not None:
			ens_outdir = re.sub(rips[r], newrip, outdirs[0])
			match = True
		else:
			r = r + 1
	if match == False:
		print("Error in get_ensemble_path! Something has gone wrong with the ensemble path generation!")	
		sys.exit()				 
	return(ens_outdir)

def get_crossval_paths(cv_paths):
	#Okay, this is going to be more complicated - this relies on some of the code
	#that was established by the ESD team for dealing with the cross-validated paths
	orig_expname = re.split("/",cv_paths[0])[-1]
	odesc = ['dp', 'cv']
	dtype = ["DSDEPh", "DSCVh"]
	kfold = ["K00", "K02"]
	cv_outpaths = []
	for cv_path in cv_paths:
		ds_rip = re.split(path)[-2]
		for k in range(0,2):
			cv_expname = re.sub("A00r*e2", "A00"+odesc[k]+"e2", orig_expname)
			cv_expname = re.sub("K00", kfold[k], cv_expname)
			cv_expname = re.sub(ds_rip, dtype[k], cv_expname)
			#
			cv_path = re.sub(orig_expname, cv_expname, cv_path)
			cv_fname = re.sub(orig_expname, cv_expname, orig_fname)
			#From this point on, make the changes to the new things instead
			cv_path = re.sub(ds_rip, dtype[k], cv_path)
			cv_outpaths.append[cv_paths]
	return(cv_outpaths)		
			
def get_file_paths(path):
	#go up 3 levels
	return(("/").join(re.split("/", path)[:-4]))

def get_anomadd_paths(anomvar, path):
	targvar = re.sub("anom", '', anomvar)
	return(re.sub(anomvar, targvar, path))

def count_suffix(path):
	#Counts the number of directories with a similar path one 
	#level up. Needed to make sure that a messy cleanup still 
	#avoids clobbering prior output
	if not os.path.isdir(path):
		print("dir does not exit; returning 0")
		return(0)	
	outdir = re.split("/", path.rstrip("/"))[-1]
	basename = ("/").join(re.split("/", path.rstrip("/"))[:-1])
	outsuff = [p.split("~")[-1] for p in os.listdir(basename) if re.search(outdir, p) is not None]
	outnum=[]
	for el in outsuff:
		try:
			outnum.append(int(el))
		except ValueError:
			outnum.append(0)	
	if len(outnum) > 0:		
		return(max(outnum))
	else:
		return(0)	

def clean_prior_experiments(xml, basedir, opt):
#Clobbers or moves the script directories and all output directories, 
#depending on the settings of the xml passed in
	#Good news! Expergen checks for running on a workstation versus the analysis
	#nodes. This check should be safe. 
	#Check for being on an analysis node:
	output = expergen.listVars(xml, basedir)
	outdir = output[27]
	if re.search("#", outdir) is not None: #Check to see how this handles on single-rip datasets
		#Get outdirs for each rip
		out_components = re.split("#", outdir)
		#Expected parenthesis are an utter pain to make regular expressions around
		for el in ("$", ")", "("):
			out_components[1] = re.sub("\\" + el, '\\'+el, out_components[1])
		rips = out_components[2]
		rip_list = re.split(",", rips)
		outdirs = []
		for rip in rip_list:
			outdirs.append(re.sub(out_components[1], rip, out_components[0]))	
		outdirs = [get_file_paths(f) for f in outdirs]	
	#Script path
	scriptpath = output[54]
	targvar = output[25]
	#Ensemble obtain
	is_ensemble = False
	is_crossval = False
	dirpaths = [scriptpath] + outdirs
	if len(outdirs) > 1:
		is_ensemble = True
		dirpaths.append(get_ensemble_path(outdirs, rip_list))
		crossval_patt = ['00r1e2', '00r2e2']
		cv_match = [crossval_patt.index(c) for c in crossval_patt if re.search(c, outdirs[1]) is not None]
		if (len(cv_match) == 1):
			is_crossval=True
			dirpaths.append(get_crossval_paths(outdirs))
	#if(re.search('anom', targvar) is not None):
	#	anomaddpaths = get_anomadd_paths(outdirs)
	#	dirpaths.append(anomaddpaths)
	#	if is_ensemble:
	#		dirpaths.append(get_ensemble_path(anomaddpaths, rip_list))
	#	#if is_crossval:
	#	#	dirpaths.append(get_crossval_paths(anomaddpaths))	
				
	print "Starting cleanup file operations"			
	if opt=='erase':
		for el in dirpaths:
			if os.path.exists(el):
				print "removing " + str(el)
				shutil.rmtree(el) #check on this - does it work for non-empty directories?
	elif opt=='move':
		#set suffix to move new paths to: 
		print([count_suffix(el) for el in dirpaths])
		suffix = str(max([count_suffix(el) for el in dirpaths]) + 1)
		for el in dirpaths:
			if os.path.exists(el):
				print(el.rstrip("/"))
				dest = "~".join([el.rstrip("/"), suffix])
				print "moving " + str(el) + " to " + dest
				shutil.move(el, dest)
				#except: 
				#"Move failed; indexes are uneven. Trying again."
					#dest = 	"~".join([el.rstrip("/"), str(int(suffix)+1)])
					#shutil.move(el, dest)
	
