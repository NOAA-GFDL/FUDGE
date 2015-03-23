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
import pprint,getopt, os, shutil
import sys, subprocess
from subprocess import PIPE 
import optparse
from optparse import OptionParser
import csv

def main():
	parser = OptionParser()
	parser.add_option("-r", "--runcode", action='store_true', dest='run_runcode',
	                  help="run only regression tests using runcode", default=False)
	parser.add_option("-x", "--xml", action='store_false', dest="run_xml",
	                  help="run only the regression tests using full XML", default=False)
	parser.add_option("-a", "--all", action='store_true', dest="run_all",
	                  help="run both runcode and XML regression tests", default=False)
	parser.add_option("-s", "--store_results", action='store_true', dest='save_results', 
                          help="save results for later analysis", default=False)
#if options.save_results
	(options, args) = parser.parse_args()

	#Assume that you are running this to test the version of FUDGE that is ready to use (i.e. with env variable $BASEDIR
	#regdir = "/archive/esd/REGRESSION_TESTS/"
	regdir = '/home/cew/Code/testing/'
	#regdir = '/work/cew/testing/'

	olddir = regdir + "/old_output/"
	datestring = time.strftime("%y-%m-%d:%X") #Need string representation of the date to separate dirs	
	newdir = regdir + "/new_output/"+datestring+"/"
	if(options.save_results)
		tmpdir = os.environ.get('TMPDIR')
		if (tmpdir is None)
			print "Error: TMPDIR not set for the system. This is about to cause major issues."
			sys.exit(1)
		newdir = tmpdir + "/" + newdir
	os.makedirs(newdir)

	###CEW edit: this needs to be changed to something else. Maybe check for creation in c-shell script?
	summary_file = newdir + "/test_status.summary"

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
	
	if(options.run_runcode | options.run_all):
		commandfile = rundir +"/runcode_commands"
		#commandfile = "/home/cew/Code/testing/cmndfile.txt"
		teststatus = 0
		mode='runcode'
		with open(commandfile, 'rb') as instructions:
			instructreader = csv.reader(instructions, delimiter=' ', quotechar='"')
		     	for row in instructreader:
			#Expected return is a list of strings of this format: 
			#0: the runcode or XML being run through the testing suite
			#1: a brief description of the input 
			#2: the name of the output file
			#Lines with "#" at the beginning are comments
				print row
				if ( ("#" not in row[0]) ): #TODO: fix this
					newstatus = compareRuncode(row, olddir, newdir, basedir, mode)
					#Honestly, this is probably better suited to running as a python subfxn
	        			teststatus = teststatus + int(newstatus)
		#If, at the end of everything, all tests were passed: 
		if (teststatus==0):
			print "All runcode regression tests passed. Congratulations."
		else:
			print "Runcode regression tests not passed. Please check these tests:" 
			for line in open(summary_file):
	 			if "FAILED" in line:
	  				print line
			print "Regression test summary output located at:" + summary_file
			print "Regression test logfile output located at:" + newdir + "/stdout.log"
			sys.exit(1)
	
	if( options.run_xml | options.run_all): 
		commandfile = xmldir +"/xml_commands"
		#commandfile = "/home/cew/Code/testing/cmndfile.txt"
		teststatus = 0
		mode = 'xml'
		with open(commandfile, 'rb') as instructions:
			instructreader = csv.reader(instructions, delimiter=' ', quotechar='"')
		     	for row in instructreader:
				print row
				newstatus = compareRuncode(row, mode, olddir, newdir, basedir, mode)
	        		teststatus = teststatus + int(newstatus)
		if (teststatus==0): 
			print "All xml/entire workflow tests passed. Congratulations."
		else:
			print "XML/entire workflow tests not passed. Please check these tests:"
			for line in open(summary_file):
	 			if "FAILED" in line:
	  				print line
			sys.exit(1)
	print "All tests specified have passed. Summary output is located at:" + summary_file 
	sys.exit(0)

def compareRuncode(row, olddir, newdir, basedir, mode):
	#Creates the args for calling a c-shell script that 
	#the status of a nccmp and writes the results (and stdout)
	#to file
	if (mode=='xml'):
		script = basedir + "/Regression_tests/xmls/" + row[0]
	elif (mode=='runcode'):
		script = basedir + "/Regression_tests/runcodes/" + row[0]
	else:
		print "Error in compareRuncode: Invalid option for mode."
	oldfile = olddir + row[1]
	summfile = newdir + "/test_status.summary"
	logfile = newdir + "/stdout.log"
	description = "'" + row[2] + "'"
	#location of script to be called:
	c_shell_script = basedir + "Regression_tests/run_reg_tests_from_file_old.csh" #old
	command_tup  = (c_shell_script, script, oldfile, newdir, summfile, logfile, mode, description)
	commandstr = command_tup[0]
	for i in range(1, len(command_tup)):
		commandstr = commandstr + " " + command_tup[i]
	print commandstr
	proc_out = subprocess.Popen(commandstr,shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
	#check return code
        output0, errors0 = proc_out.communicate()
	print output0, errors0
	return proc_out.returncode

#Main method invocation
if __name__ == '__main__':
    main()

