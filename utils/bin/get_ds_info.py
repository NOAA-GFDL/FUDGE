#!/bin/python
#Gets downscaling method information from the downscaling method spreadsheet.
#ATM all we need for expergen is whether or not the method is going to need
#a leapday mask copied as part of the master runscript setup

import csv
import os,sys
from os import path as osp
import json
import re

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
		jsondict = json.load(jsonfile)
		#wallclock time
		if 'wallclocktime' in jsondict.keys():
			#R's json converter helpfully wraps everything in lists.
			if isinstance(jsondict['wallclocktime'], list):
				wallclocktime=jsondict['wallclocktime'][0]
			else:
				wallclocktime = jsondict['wallclocktime']	
		else:
			wallclocktime='5:00:00'
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
	        'subsetdict':subsetdict, 'njobs':njobs, 'ncpus':ncpus})
	
def make_aux_input(grid, script_base, targ_var):
	#Creates an aux info file with g-or-earlier default settings: minifiles, 
	#no stencils, etc.
	#As of 2017-07-18, also checks for modified aux settings and objects 
	#if trying to implement default using them.
	print("Making default aux file")
	jdict = {'US48' : 114, 
	         'SCCSC0p1'  : 140, 
	         'RR'   : 140, 
	         'grid0': 1, 
	         'NEUSA': 41, 
	         'ESTCPconus': 114}
	if grid in jdict.keys():
		jv  = str(jdict[grid])
		aux_scriptname = script_base + "/default_mini_"+grid+".json"
		print(aux_scriptname)
	else:
		print("Error in make_aux_info: grid, " + grid + " not in jdict!")
		sys.exit(1)	         
	i_subset = '"i_subset": [1]'          + ","
	j_subset = '"j_subset": [' + jv + ']' + ","
	tile_olap = '"tile_olap": [0]'        + ","
	#Most of the specs are not going to change unless you use stencils
	#...which is why it's probably safe to set the default wallclocktime
	#to 2 hours rather than 12, as we have been using forever.
	main_text = "\n".join(['"geomask_names": [""],',
                           '"ds_handle_missing": ["continue"],',
                           '"wallclocktime": ["5:00:00"],',
                           '"OH": {',
                           '    "stencil": ["none"],',
                           '    "geomask": ["none"]',
                           '},',
                           '"MH": {',
                           '    "stencil": ["none"],',
                           '    "geomask": ["none"]',
                           '},',
                           '"MF": {',
                           '    "stencil": ["none"],',
                           '    "geomask": ["none"]',
                           '}'])
	print(targ_var)
	if re.search('Anom', targ_var) is not None:
		print("Error in make_aux_input: if using an anomaly var " + targ_var + \
		      " user specification of climo_source in aux file is required!")
		sys.exit(1)
	else:    
		meta_text = '"climo_source":["MF"],'                           
	aux_text = "\n".join(["{", i_subset, j_subset, tile_olap, meta_text, main_text, "}"])
	with open(aux_scriptname, 'w') as f: 
		f.write(aux_text)
	return(aux_scriptname)							
			
