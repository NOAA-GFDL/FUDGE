#!/bin/python
#Gets downscaling method information from the downscaling method spreadsheet.
#ATM all we need for expergen is whether or not the method is going to need
#a leapday mask copied as part of the master runscript setup

import csv
import os
from os import path as osp

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
			
