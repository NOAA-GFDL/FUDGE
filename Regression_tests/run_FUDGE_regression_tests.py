#!/usr/local/python/2.7.1/bin/python
#
# Sample script calling a set of unit tests to run
# against FUDGE code before it is pushed to a remote repo
# Note: this code runs on all branches
# Carolyn Whitlock, November 2014

#source /etc/profile.d/modules.csh #on GFDL system, source explicitly runs shells as batch at the moment. It is strange.
#source /usr/share/Modules/init/sh
#module load nco
#module load nccmp
#module load R
#source /home/esd/local/.cshrc_esd
#module load fudge/cinnamon

#Import commands
import time
import pprint,getopt, os, shutil, re
import sys, subprocess
from subprocess import PIPE 
import optparse
from optparse import OptionParser
import csv

def main():
	parser = OptionParser()
	#parser.add_option("-r", "--runcode", action='store_true', dest='run_runcode',
#	                  help="run only regression tests using runcode", default=False)
	#parser.add_option("-x", "--xml", action='store_false', dest="run_xml",
#	                  help="run only the regression tests using full XML", default=False)
	#parser.add_option("-a", "--all", action='store_true', dest="run_all",
#	                  help="run both runcode and XML regression tests", default=False)
	parser.add_option("-i", "--input", dest="input",
                  help="input file of commands to be run", metavar="FILE")
	parser.add_option("-s", "--store_results", action='store_true', dest='save_results', 
                          help="save results for later analysis", default=False)
	parser.add_option("-o", "--output_directory", dest="outdir", default="/archive/esd/REGRESSION_TESTS/results/", 
                          help="set a directory in which to write the results")
	parser.add_option("-r", "--recordfile", dest="recordfile", default="", 
			  help="set a summary file to append the results of the test to. If not set, summfile is written to a sep. file")
#if options.save_results
	(options, args) = parser.parse_args()

	#Assume that you are running this to test the version of FUDGE that is ready to use (i.e. with env variable $BASEDIR
	#regdir = "/archive/esd/REGRESSION_TESTS/"
	regdir = '/home/cew/Code/testing/' #Keep for testing and creating new regression tests
	#regdir = '/work/cew/testing/'
	olddir = regdir + "/old_output/"

	datestring = time.strftime("%y-%m-%d:%X") #Need string representation of the date to separate dirs
	#Need to replace ":" with "_" for anything that uses templates and sed terminators
	datestring_dir = re.sub(":", "_", datestring)
	newdir = options.outdir +datestring_dir+"/"	
	#Given the options in the R code, this needs to be turned on REGARDLESS of whether or not you want to save results
	tmpdir = os.environ.get('TMPDIR')
	if (tmpdir is None):
		print "Error: TMPDIR not set for the system. This is about to cause major issues."
		sys.exit(1)
	#newtmpdir = tmpdir + "/" + newdir
	newtmpdir = tmpdir + "/" + datestring_dir
	print "Making tmp output dir " + newtmpdir
	os.makedirs(newtmpdir)
	if(options.save_results==True): 
		print "Making output dir " + newdir
		os.makedirs(newdir)
		newdir_xml = newdir
	else:
		print "Making tmp work output dir (for any XML tests)"
		user = os.environ.get('USER')
		newdir_xml  = "/work/" + user + "/fudge_regtets/" + datestring_dir + "/"
		os.makedirs(newdir_xml)
		print "Establishing tmpdir as newdir"
		newdir = newtmpdir	

	###CEW edit: this needs to be changed to something else. Maybe check for creation in c-shell script?
	


	basedir = os.environ.get('BASEDIR')
	if(basedir is None):
		print "Error: BASEDIR not set. This is going to cause major problems in a minute or two."
		sys.exit(1)
	#basedir = "/home/cew/Code/fudge2014/"
	rundir = basedir +"/Regression_tests/runcodes/"
	xmldir = basedir + "/Regression_tests/xmls/"
	#Note: The new VERY STRONG convention is to pass the olddir and newdirs under the comparison script, 
	#...no, that's not going to work. At present, you really do need to link the output directory/filename
	#and the input directory/filename to get the results that you are looking for....
	#Honestly, you can view the creation of the input and output dirs as a further form of testing on the
	#consistency of your input...
	if(os.path.exists(options.input)):
		
		#Create your logfile and summfile and start appending the results to it
		log_file = newtmpdir + "/stdout.log"
		if(options.recordfile!=""):
			summary_file = options.recordfile
		else:
			
			summary_file = newtmpdir + "/test_status.summary"
		with open(summary_file, 'a') as summfile:
			summfile.write("Regression test started on " + datestring + '\n')
		#Do all the things related to the test
		teststatus = 0
		with open(options.input, 'rb') as instructions:
			instructreader = csv.reader(instructions, delimiter=' ', quotechar='"')
		     	for row in instructreader:
			#Expected input is a list of strings of this format: 
			#0: the runcode or XML being run through the testing suite
			#1: the name of the output file 
			#2: one of 'xml' or 'runcode'
			#3: ...I am kind of tempted to start adding section 8 stuff in here as well, but that's probably
			#   not such a good idea at this time. 
			#Lines with "#" at the beginning are comments
				print row
				if ( ("#" not in row[0]) ): #TODO: fix this
					newstatus = compareRuncode(row, olddir, newtmpdir, basedir, newdir_xml, log_file, summary_file)
					#Honestly, this is probably better suited to running as a python subfxn
	        			teststatus = teststatus + int(newstatus)
		if(options.save_results==True):
			print "Tests complete, copying runcode results to output..."
			commandstr = "gcp --sync -cd -r "+ newtmpdir + " " + os.path.dirname(newdir)
			print commandstr
			proc_out = subprocess.Popen(commandstr,shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
       			output0, errors0 = proc_out.communicate()
			print output0, errors0
			print "...and done!"
		#If, at the end of everything, all tests were passed: 
		if (teststatus==0):
			print "All regression tests in file "+ options.input + " passed. Congratulations."
		else:
			print "Regression tests not passed. Please check these tests:" 
			newtest=1
			for line in open(summary_file):
				if newtest==1:
					if datestring in line:
						newtest=0
				else:
	 				if "FAILED" in line:
	  					print line
			print "Regression test summary output located at:" + newdir + "/test_status.summary"
			print "Regression test logfile output located at:" + newdir + "/stdout.log"
			sys.exit(1)
		print "Tests ran from " + datestring + " to " + time.strftime("%y-%m-%d:%X")
		print "All tests have passed. Summary output is located at:" + summary_file 
		sys.exit(0)
	else:
		print options.input + " does not exist. Please check your input"
		sys.exit(1)

def compareRuncode(row, olddir, newtmpdir, basedir, newdir_xml, logfile, summfile):
	#Creates the args for calling a c-shell script that 
	#the status of a nccmp and writes the results (and stdout)
	#to file
	oldfile = olddir + row[1]
	#summfile = newtmpdir + "/test_status.summary"
	#logfile = newtmpdir + "/stdout.log"
	#description = "'" + row[2] + "'"
	mode=row[2]
	if (mode=='xml'):
		script = basedir + "/Regression_tests/xmls/" + row[0]
		newtmpdir = newdir_xml
	elif (mode=='runcode'):
		script = basedir + "/Regression_tests/runcodes/" + row[0]
	else:
		print "Error in compareRuncode: Invalid option for mode."
	#location of script to be called:
	c_shell_script = basedir + "Regression_tests/run_reg_tests.csh" #old
	command_tup  = (c_shell_script, script, oldfile, newtmpdir, summfile, logfile, mode)
	commandstr = command_tup[0]
	for i in range(1, len(command_tup)):
		commandstr = commandstr + " " + command_tup[i]
	print commandstr
	proc_out = subprocess.Popen(commandstr,shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
	#check return code
        output0, errors0 = proc_out.communicate()
	print output0, errors0
	#Add temporary code to run the correlations after the new data is added
	#Rscript /home/cew/Code/calculate_CF_correlation.R
	return proc_out.returncode

#Main method invocation
if __name__ == '__main__':
    main()

