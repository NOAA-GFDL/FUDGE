#!/bin/python
#Gets the multi_npts specifications for the input XML.
#Which is done by looking for the string npts_by=${var}
import os
from os import path as osp
import re

def get_npts_by(xml):
	#Opens a file and searches each line for the string we need, 
	#throwing error if there are too few cases
	if not osp.isfile(xml):
		print "Error in get_npts_by! File " + xml + " does not exist"
	npts_specs = []	
	with open(xml, 'r') as f:	
		for line in f.readlines():
			linematch = re.search("multi_npts", line)
			if linematch is not None:
				nmatch = line.partition("=")[2]
				nmatch = re.findall('[0-9]+', nmatch)[0]
				if nmatch is None:
					print "Error in get_npts_by! line " + line + " failed the regular expression!"
				else:	
					npts_specs.append(nmatch)
	if len(npts_specs) == 0:
		npts_specs = [1,1,1]
	elif len(npts_specs) == 3:
		print(npts_specs)
		npts_specs = [int(el) for el in npts_specs]
		if any( [ (el % 2) == 0 for el in npts_specs] ):
			print "Error in get_npts_by: all by specifications should be odd!"
	else:
		print "Error in get_npts_by: there should either be 3 matches or 0 matches!"
	return(npts_specs)								
