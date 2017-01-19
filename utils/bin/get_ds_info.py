#!/bin/python
#Gets downscaling method information from the downscaling method spreadsheet.
#ATM all we need for expergen is whether or not the method is going to need
#a leapday mask copied as part of the master runscript setup

import csv
import os
from os import path as osp
import json

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
	if ds_match is False:
		print("Error in get_ds_info: Method " + ds_method + " not found in ds_att_sheet.csv!")
		sys.exit(1)
	else:
		return({'copy_leapmasks':downscale_oh, 'multivar_target':multivar_target})
		
def parse_aux_info(jsonfile):
	#Reads the aux file; returns bits that we are interested in
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
			wallclocktime='12:00:00'
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
	return({'copy_files':copy_files, 'wallclocktime':wallclocktime})					
			
