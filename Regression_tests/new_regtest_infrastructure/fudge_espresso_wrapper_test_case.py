#!/usr/local/python/2.7.1/bin/python

import time
from time import localtime, strftime
import pprint,getopt, os, shutil, re
from os import path
#from os import join #isfile?
import sys, subprocess, traceback
from subprocess import PIPE, STDOUT
import optparse
from optparse import OptionParser
import csv, string

#And don't forget to get the relevant python scripts from ongoing work
#sys.path.append('/home/cew/Code/espresso_wrapper/')
sys.path.append('/home/cew/Code/fudge2014/bin/')
#sys.path.append('/home/cew/Code/fudge2014/Regression_tests/new_regtest_infrastructure')
import expergen
import fudge_espresso_wrapper_helper
from fudge_espresso_wrapper_helper import submit_expergen_job, check_minfile_status, submit_msub_postproc, check_postproc_status, set_complete_status, submit_s8_job
import parse_espresso_xml
from parse_espresso_xml import parseEspressoXML

#####Start the main method of the function here
def main():	
	parser = OptionParser()
	parser.add_option("-i", "--input", dest="input",
		help="input file of commands to be run", metavar="FILE")
#	parser.add_option("-p", "--pause", dest='pause', help='pause execution of a set of files')
	parser.add_option("--regtest", dest='regtest', action = 'store_true', default= False, help='whether to run as a regression test, comparing results to the contents of /archive')
	parser.add_option("--debug", dest="debug", default = False, help="print extra debugging options for extra error testing")
	parser.add_option("--summary_file", dest='summary_file', default="/home/cew/Code/testing/test_wrapper_out.txt", help="Specify location of summary of script to date")
	parser.add_option("--copy_opt", action='store_true', dest='copy_opt', default=False, help="Whether to copy the output location to the current location in /archive")
#	parser.add_option("--dry_run", dest='dry_run', help='do a dry run to make sure that the xml are parseable and write to correct dirs; no scripts are executed.')
#	parser.add_option("-v", "--verbose", dest='verbose', default=False, help='Whether or not to print extra status messages. Defaults to FALSE')
#	parser.add_option("-w", "--wait_time", dest='wait_time', help="How long to wait for minifile completion. Defaults to 60 min.")
#	parser.add_option("--summary_file", dest="summ_file", help="File for summarizing the log output and writing to a file")
	
#	parser.add_option("-s", "--store_results", action='store_true', dest='save_results', 
#                          help="save results for later analysis", default=False)
#	parser.add_option("-o", "--output_directory", dest="outdir", default="/archive/esd/REGRESSION_TESTS/results/", 
#                          help="set a directory in which to write the results")
#	parser.add_option("-r", "--recordfile", dest="recordfile", default="", 
#			  help="set a summary file to append the results of the test to. If not set, summfile is written to a sep. file")
#if options.save_results
	(options, args) = parser.parse_args()
	
	
	#Regtest and the setting the input status should be mutually exclusive options
	#If setting input status is going to be a thing, should probably also set the
#Parse file (or command-line input) containing the xmls of the downscaling
#experiments that you want to run
#	options.input = "/home/cew/Code/testing/test_script.txt"
	complete = 0
	loops = 0
	#Set some variables that SHOULD be passed in via command line
	verbose = True
	dry_run = True
	waittime = 60
	#You should really make a check for existance at this level - otherwise it's nasty elsewhere
	#summary_file = "/home/cew/Code/testing/test_wrapper_out.txt"
	summary_file = options.summary_file
	timestamp = time.strftime("%y-%m-%d:%X")
	#regtest = True #Will eventually be set by the XML
	copy_opt = options.copy_opt
	print "Value of the copy option: " + str(copy_opt)
	regtest = options.regtest
	regtest_dir = timestamp.replace("-", "_").replace(":","_")
	if regtest:
		regtest_basedir = "/work/cew/downscaled/"
		regtest_dir = regtest_basedir + regtest_dir
		os.makedirs(regtest_dir)
	teststatus = 0
	experiments = []
	#Implement this later, once you've looked up more about error throwing
#	(read_status, run_dict, xml_errors) = read_xml_file(options.input)
#	if read_status!= 0:
#		print "The following XML were not correctly parsed:"
#		for err in xml_errors:
#			print err[0] + ": " + err[1]
#		sys.exit()
	with open(options.input, 'rb') as instructions:
		instructreader = csv.reader(instructions, delimiter=' ', quotechar='"')
		status = 0
		run_dict = {}
		for row in instructreader:
	   		if ("#" not in row[0]):
	    		#run_dict[row] = makeXMLRundict(row)
	   			print row[0]
	   			try:
	   				run_dict[row[0]] = parseEspressoXML(row[0], regtest=regtest, regtest_dir=regtest_dir, copy_opt=copy_opt)
	   			except:
	   				print "Error in parsing line " + row[0]
	   				status = 1
	   				sys.exc_info()
	     			#raise #You want this to stop. If you can't read one of the lines, no point in going on.
	#Loop over each XML
	xml_total = len(run_dict)
	#Fast edit to summary file:
	with open(summary_file, "a") as f:
		f.write("================================================================================== \n")
		f.write("Operation started at:" + timestamp + "\n")
	while complete < xml_total:
		print "There have been " + str(loops) + " loops so far."
		for xml in run_dict:
			if verbose:
				print "Starting XML " + run_dict[xml]['xml']
			instructions = run_dict[xml]
	#		if verbose:
	#			print instructions
			print "current status: " + str(instructions['status'])
			if instructions['status'] <= 100:
				try:
					callNextStep(instructions, verbose, copy_opt)
				except:
					print "Unexpected error:" + str(sys.exc_info()) #Previously separated with comma
					print "the current summary string:"
					print instructions['summary_string']
					#for el in sys.exc_info():
					instructions['summary_string'] += str(sys.exc_info()[1]) + '\n'
					instructions['summary_string'] += traceback.format_exc()
				#instructions['summary_string'] =+ ("Unexpected error: ", "\n".join(sys.exc_info()))
				#writeSummaryToFile(instructions, summary_file)
					instructions['status'] = 200
				#raise#I'm not sure whether this shoulld be raised or not. There's not anything higher, and errors shouldn't propagate
				finally:
					if instructions['status'] >= 100: #If completed (or errored out) during run
						complete += 1
						writeSummaryToFile(instructions, summary_file)
						if options.debug:
							with open(summary_file, "a") as f:
								f.write(str(instructions))
					run_dict[xml] = instructions
			print "current status: " + str(instructions['status'])
		if verbose:
			print str(complete) + " of " + str(xml_total) + " complete"
		if complete != xml_total:
			if loops==0:
				if verbose:
					print "Sleeping for six minutes"
				time.sleep(360)
			#time.sleep(100)
			else:
				if verbose:
					print "Sleeping for two minutes"
				time.sleep(120)
		loops=loops + 1
		#Check on status of timed jobs; if one or more jobs are timed (and haven't started yet)
		#compute time to wait to job that will go soonest and wait until then, printing status
		#message as you go	
	#End while loop
	#Now, print status messages and write to any summary files you want to include
	print "Summary file output located at: " + summary_file
	
def callNextStep(instructions, verbose=False, dry_run=False, copy_opt=False):				
	if instructions['status'] < 100:
		#submit msub jobs step (0)
		if instructions['status']==0:
			print "starting job submission"
			instructions = submit_expergen_job(instructions, verbose=verbose)
		#check on completeness of msub jobs step; throw errors if not ready (1-9)
		elif instructions['status'] <= 9:
			print "checking on minifile status"
			instructions = check_minfile_status(instructions, verbose=verbose)
		#submit postproc job step (10-19)
		elif instructions['status'] <= 19:
			if instructions['call_postproc']:
				print "preparing to submit the postproc command"
				instructions = submit_msub_postproc(instructions)
			else:
				instructions['status'] = 30
		#check on status of postproc job step; throw error if error caught or
		#   postproc obviously not complete (20-29)
		elif instructions['status'] <= 29:
			print "checking on status of postproc command"
			instructions = check_postproc_status(instructions)
		elif instructions['status'] <= 39:
			#Submit section 8 job(s) - ensemble files, comparing aginst prev. output
			if instructions['s8_command']['s8_method'] is None:
				print "No section 8 job submitted; moving to completion"
				instructions['status'] += 100
			else:
				print "submitting a section 8 job (probably a regest comparison)"
				instructions = submit_s8_job(instructions)
		elif instructions['status'] <= 49: #(40-49)
			#Check status of section 8 job(s); keep in mind, this might need to be 
			#looped over in the context of the 
			print "checking on status of section 8 job"
			instructions = check_s8_status(instructions, copy_opt)
					
				#raises an interesting point - how many of these will be running in background 
				#vs how many will be writing .okay files to some process?
				#In the case of the minfiles/postproc output, the .okay files are the 
				#presence/absence of the minifiles themselves; in the case of the postproc
				#script, the .okay file could be considered to be the post-processed output.
				#What about the idea of sequential s8 steps (i.e analysis THEN ens, or 
				#ensemble THEN compare? That implies another step dedicated to the 
				#changing of statuses that might not be appropriate otherwise
				#...and logging should prob. take place within the code itself. 
				#Or write as a buffer to pass back into fxn, and add to the logfile
				#at the end of this code. 
			#else:
				#do nothing, go to next non-complete element
		#Wait 1-5 minutes for ALL processes, not one at a time
def writeSummaryToFile(xml_dict, summary_file):
	with open(summary_file, "a") as f:
		f.write("---------------------------------------------------------------------------------- \n")
		f.write("Summary of xml " + xml_dict['xml'] + ": \n")
		f.write(xml_dict['summary_string'])
		f.write("End status: " + str(xml_dict['status']) + "\n") 
		f.write("Sub-operation completed at: " + time.strftime("%y-%m-%d:%X") + "\n")
		
def read_xml_file(input_file):
	with open(input_file, 'rb') as instructions:
		instructreader = csv.reader(instructions, delimiter=' ', quotechar='"')
		status = 0
		run_dict = {}
	   	for row in instructreader:
	   		if ( ("#" not in row[0]) ):
	    				#run_dict[row] = makeXMLRundict(row)
	   			print row[0]
	   			try:
	   				run_dict[row[0]] = parseEspressoXML(row[0], regtest=regtest, regtest_dir=regtest_dir)
	   			except:
	   				print "Error in parsing line " + row[0]
	   				status = 1
	   				sys.exc_info()
	   					#raise #You want this to stop. If you can't read one of the lines, no point in going on.

	
#Set those XMLs as the elements of a dictionary, each containing:
#- the current status(pre_submission, submission, complete, postproc, complete postproc, s8)
#- the location of the output dir
#- the location of the script dir
#- the expected location of the sourcable msub command
#- the number of files that will need to be present before the script is complete
#(i.e 194, 114, or similar). This can be derived by parsing the XML, but you should prob. write your own fxn for this. It will totes be a learning experience, and there are going to be
#no negative consequences to this whatsoever.

#While there are uncompleted XMLs (look up how to properly query elements of a python
#dictionary; this is something you wrote a fxn to do for you in R
#...would it make sense to have a separate list of the dimensions for which
#you could reasonable expect postProc to return a concatenated output, and which you couldn't do that for? 
#you could do regression tests as a section 8 script - feed in the name of the file
#against which you are comparing, do a nccmp -d, and a ncdump with select input parsing
#to check on the presence/absence of the metadata that you are interested in. 
#...if you can implement the section 8 process scripts as part of the process that you are interested in, then you can include the arguments to those scripts as whatever section 8 tag you are including as well. This could include a call to the section 

#Main method invocation
if __name__ == '__main__':
    main()
