"""runWatcher.py: Given the experiment_info file, this tool watches the runs as it runs to completion with the desired output files, and then launches the ppscript(postProc) job,notifies user and quits."""
__author__ = "A.Radhakrishnan"

import os,sys,subprocess
from subprocess import PIPE
import optparse
from optparse import OptionParser
import shlex
import checker

def parserWatcher():
     force = False #default
     usage = "\n############################################################################\n"
     parser = OptionParser(usage)
     usage = usage + "Eg. runWatcher -i /home/a1r/cew/experiment_info.txt -t 5 \n"
     parser = OptionParser(usage)

     try:
        userinput
        uinput = userinput
     except:
         userinput = "none"
     parser.add_option("-i", "--file", dest="uinput",help="pass location of fudgeList out/experiment_info.txt" , metavar="FILE")
     parser.add_option("-t", "--timeout_after_ppsubmission", dest="timeout",help="pass timeout in minutes (waits until t mins after ppscript is submitted and before quitting the tool)", metavar="FILE")
     (options, args) = parser.parse_args()
     for opts,vals in options.__dict__.items():
                if(opts == 'uinput'):
                         uinput = vals
                         print "experiment_info.txt: ",uinput
                if(opts == 'timeout'):
                         timeout = vals
     runWatcher(uinput,timeout)

def asciiParser(uinput):
	op = False #not yet spotted output_path
	spatmask = False
	f = open(uinput, 'r')
	for line in f:
	    line = line.strip()
	    if(line.startswith('minifile.I.start:')):	
		print line
	        minifile_i_start = line.split('minifile.I.start:')[1]
            if(line.startswith('minifile.I.end:')):     
		print line
                minifile_i_end = line.split('minifile.I.end:')[1]
	    if(line.startswith('output.path:')):
		print line
		output_path = line
		op  = True
	    if(op == True) & (spatmask == False):
		if not ":" in line: 
		  output_path = output_path+","+line
            if(line.startswith('spat.mask.path_1:')):
                spatmask  = True
	f.close()
	return minifile_i_start,minifile_i_end,output_path

def runWatcher(uinput,timeout):
	minifile_i_start,minifile_i_end,output_path = asciiParser(uinput)
	print minifile_i_start,minifile_i_end,output_path
	exp_cnt = int(minifile_i_end) - int(minifile_i_start) 
	dirs_output = output_path.split(':')[1]
	print "----",dirs_output 
	list_output = dirs_output.split(',')	
	print list_output
	#will be nice to have target also in experiment_info.txt
	ctr = 1
	#todo : include return status for checker 
	#based on status while loop through the following with sleep to check minifile existence
	#when checker is  a success msub the ppscript (even if one outpath is only complete at that time and wait for the rest, notify via email)
	for out in list_output:
		ctr = ctr+1
		checker.checker(out,exp_cnt,"warning")

if __name__ == '__main__':
   parserWatcher()
