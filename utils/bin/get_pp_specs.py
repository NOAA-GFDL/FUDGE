#!/bin/python
#Gets the pp specs (njobs and ncpus) from the input xml
#Which is done by looking for the string {njobs,ncpus}=${var}
import os
from os import path as osp
import re

def get_pp_specs(xml):
	#Opens a file and searches each line for the string we need, 
	#throwing error if there are too few cases
	if not osp.isfile(xml):
		print "Error in get_npts_by! File " + xml + " does not exist"
	#set default values of njobs,ncpus	
	pp_spec_dict = {'njobs' : 8, 'ncpus' : 2}	
	pp_strings = ['njobs', 'ncpus']
	with open(xml, 'r') as f:	
		for line in f.readlines():
			for patt in pp_strings:
				linematch = re.search(patt, line)
				if linematch is not None:
					nmatch = line.partition("=")[2]
					nmatch = re.findall('[0-9]+', nmatch)[0]
					if nmatch is None:
						print "Error in get_pp_specs! line " + line + " failed the regular expression!"
					else:	
						pp_spec_dict[patt] = nmatch
	if len(pp_spec_dict) != 2:
		print "Error in get_pp_specs: there should either be 2 matches or 0 matches!"
	return(pp_spec_dict)								
