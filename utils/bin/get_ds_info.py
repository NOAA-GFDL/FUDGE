#!/bin/python
#Gets downscaling method information from the downscaling method spreadsheet.
#ATM all we need for expergen is whether or not the method is going to need
#a leapday mask copied as part of the master runscript setup

import csv
import os,sys
from os import path as osp
import json
import re
import collections
from collections import OrderedDict

def get_ds_info(ds_method, FUDGEROOT):
	#Reads the ds lookup table
	ds_table = osp.join(FUDGEROOT, "Rsuite/aux/ds_att_sheet.csv")
	with open(ds_table, 'r') as dstab:
		reader = csv.DictReader(dstab)
		ds_match = False
		downscale_oh = False
		for row in reader:
			if row['method_name'] == ds_method:
				ds_match = True
				downscale_oh = (row['downscale_oh']=='TRUE')
				multivar_target = (row['multivar_target']=='TRUE')
				multivar_possible = (row['multivar_possible']=='TRUE')
				
	if ds_match is False:
		print("Error in get_ds_info: Method " + ds_method + " not found in ds_att_sheet.csv!")
		sys.exit(1)
	else:
		return({'ds_method':ds_method, 
		        'copy_leapmasks':downscale_oh, 
		        'multivar_target':multivar_target,  
		        'multivar_possible':multivar_possible})
		
def parse_aux_info(jsonfile):
	#Reads the aux file; returns bits that we are interested in
	print("Parsing aux file")
	print(jsonfile)
	with open(jsonfile, 'r') as jsonfile:
		jsondict = json.load(jsonfile, object_pairs_hook=OrderedDict)
		#wallclock time
		if 'wallclocktime' in jsondict.keys():
			#R's json converter helpfully wraps everything in lists.
			if isinstance(jsondict['wallclocktime'], list):
				wallclocktime=jsondict['wallclocktime'][0]
			else:
				wallclocktime = jsondict['wallclocktime']	
		else:
			wallclocktime='6:00:00'
		#pp wallclocktime
		if 'pp_wallclocktime' in jsondict.keys():
			if isinstance(jsondict['pp_wallclocktime'], list):
				pp_wallclocktime=jsondict['pp_wallclocktime'][0]
			else:
				pp_wallclocktime = jsondict['pp_wallclocktime']
		else:
			pp_wallclocktime = '6:00:00'
		#njobs and ncpus parameters: 
		if 'njobs' in jsondict.keys():	
			njob_tmp = jsondict['njobs']
			if isinstance(njob_tmp, list):
				njobs = njob_tmp[0]
			else:
				njobs = njob_tmp
		else:
			njobs = 8
		if 'ncpus' in jsondict.keys():
			ncpus_tmp = jsondict['ncpus']
			if isinstance(ncpus_tmp, list):
				ncpus = ncpus_tmp[0]
			else:
				ncpus = ncpus_tmp
		else:
			ncpus = 2	
		#Subsets in i and j that we are interested in
		defaultdict = {'i_subset': 1, 'j_subset': 'all', 'tile_olap':0}	
		subsetdict = {}
		for k in defaultdict.keys():
			if k in jsondict.keys():
				tmp_k = jsondict[k]
				if isinstance(tmp_k, list):
					subsetdict[k] = tmp_k[0]
				else:
					subsetdict[k] = tmp_k	
			else:
				#default if not present in json
				subsetdict[k] = jsondict[k]
		#files that can be copied
		copy_elements = ['stencil', 'geomask']
		copy_files = []
		dataset_names = ['OH', 'MH', 'MF']
		for data_name in dataset_names:
			for filetype in copy_elements:
				if filetype in jsondict[data_name].keys():
					filetmp = jsondict[data_name][filetype]
					#R's json converter helpfully wraps everything in lists.
					if isinstance(filetmp, list):
						filetmp = filetmp[0]
					if filetmp != 'none':
						copy_files.append(filetmp)
		if len(copy_files) > 1:
			copy_files = list(set(copy_files))				
	return({'copy_files':copy_files, 'wallclocktime':wallclocktime, 
			'pp_wallclocktime':pp_wallclocktime,
	        'subsetdict':subsetdict, 'njobs':njobs, 'ncpus':ncpus}, jsondict)
	
def make_aux_input(grid, script_base, targ_var):
	#Creates an aux info file with g-or-earlier default settings: minifiles, 
	#no stencils, etc.
	#As of 2017-07-18, also checks for modified aux settings and objects 
	#if trying to implement default using them.
	print("Making default aux file")
	auxdict = OrderedDict()
	jdict = {'US48' : 114, 
	         'SCCSC0p1'  : 140, 
	         'RR'   : 140, 
	         'grid0': 1, 
	         'X1-1' : 1,
	         'NEUSA': 41, 
	         'ESTCPconus': 114}
	if grid in jdict.keys():
		jv  = str(jdict[grid])
		aux_scriptname = script_base + "/default_mini_"+grid+".json"
		print(aux_scriptname)
	else:
		print("Error in make_aux_info: grid, " + grid + " not in jdict!")
		sys.exit(1)
	
	auxdict['grid'] = grid	
	auxdict['i_subset'] = 1
	auxdict["j_subset"] = jv
	auxdict["tile_olap"] = 0
	auxdict["ds_handle_missing"] = "continue"
	auxdict["wallclocktime"] = "6:00:00"
	auxdict["pp_wallclocktime"] = "6:00:00"
	auxdict["files_in_aux"] = "FALSE"
	auxdict["file_format"] = "netcdf"
	if re.search('Anom', targ_var) is not None:
		print("Error in make_aux_input: if using an anomaly var " + targ_var + \
		      " user specification of climo_source in aux file is required!")
		sys.exit(1)
	else:    
		auxdict["climo_source"] = "MF"
	#For now, we're keeping the initialization for these here - 
	#it simplifies some of the copying workflow later.
	file_types = ["OH", "MH", "MF"]
	filedict = OrderedDict(stencil='none', geomask='none')
	for f in file_types:
		auxdict[f] = filedict

	#Most of the specs are not going to change unless you use stencils
	#...which is why it's probably safe to set the default wallclocktime
	#to 2 hours rather than 12, as we have been using forever.   
	aux_text = json.dumps(auxdict, indent=4)
	f = open(aux_scriptname, 'w')
	print >> f, aux_text
	f.close()
	return(aux_scriptname)							
	
def add_ds_params_to_auxdict(auxdict, njobs, ncpus, experiment_name, kfold,
                             ds_method, ds_arg_string, ds_region):
	#Function for parsing and adding ds_experiment_like stuff to the
	#auxdict. This is not intended to be read in from a properly-formatted 
	#files_in_aux auxfile at this time; that step comes later.
	#auxdict is an OrderedDict; keep other formatting in mind.
	auxdict["njobs"] = njobs
	auxdict["ncpus"] = ncpus
	auxdict["ds_experiment_name"] = experiment_name
	dsdict = OrderedDict()
	#if no params are used, we just get 'na'
	if ds_arg_string != 'na':
		ds_argsplit = re.split(",", ds_arg_string)
		for arg in ds_argsplit:
			argpair = re.split("=", arg)
			dsdict[argpair[0]] = argpair[1]
	auxdict["ds_method"] = ds_method
	auxdict["ds_args"] = dsdict
	auxdict["kfold"] = kfold
	#...I'm sticking this in here because I'm not sure where else to put it;
	#I'm actually not sure whether it gets used. Mark as a to-do item?
	auxdict["ds_region"] = ds_region
	return(auxdict)			
