#More complete summary of some of the helper functions getting used in the
#wrapper script over there. 
import os, sys, subprocess, time
from subprocess import PIPE, STDOUT

def submit_expergen_job(instructions, verbose=False, dry_run=False):
	#Setting this environment might be the only one that you need - subsequent processes source the environment
	if instructions['nloops'] == 0: 
		instructions['start_time']  = time.time()
		instructions['start_time_string'] = time.strftime("%y-%m-%d:%X") 
	fudge_env = dict(os.environ)
	print "perparing to call expergen"
	BASEDIR = fudge_env['BASEDIR']
	if BASEDIR is None:
		print "Warning! BASEDIR not set, and it will cause major problems in a minute..."
	exp_cmnd = "python " + BASEDIR + "/bin/expergen.py -i "+ instructions['xml']
	#if(instructions['start_time']==0): #If starting IMMEDIATELY #For now, assuming no delay and going on forward always
	exp_cmnd = exp_cmnd + " --msub"
	if(verbose):
		print exp_cmnd
	instructions['summary_string'] += exp_cmnd + " \n"
	exp_out = subprocess.Popen(exp_cmnd,shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE, env = fudge_env)
	output0,errors0 = exp_out.communicate()
	if(verbose):
		print output0
		print errors0
	if(exp_out.returncode != 0):
		print "Error in calling expergen! Please check your input and logfiles."
		instructions['status']=101
		instructions['summary_string'] += "Ended in expergen error. Status = " + str(110) + "\n" 
	else:
		instructions['status']=1
		instructions['summary_string'] += "Status = " + str(instructions['status']) + "\n"
	if verbose: 
		instructions['summary_string'] += set_verbose_info(instructions['start_time_string'], instructions['start_time'])
	#Need to add a sus.exit in here? sys.exit(110)
	if instructions['call_postproc']:
		#Wait five minutes to give the analysis nodes more time
		time.sleep(300)
	return(instructions)
	
def check_minfile_status(instructions, verbose=False, dry_run=False):
	#Keep in mind, instructions is a dict of the following form:
	if instructions['nloops'] == 0: 
		instructions['start_time']  = time.time()
		instructions['start_time_string'] = time.strftime("%y-%m-%d:%X") 
	print "checking number of minfiles present"
	outdirs = [instructions['outdir']] #Temporarly cast as list until ensemble code is working
	print outdirs
	num_minfiles = instructions['files_expected']
	outdirs_status = 0
	for outdir in outdirs:
		print outdir
		#file_count = len([name for name in os.listdir(outdir) if os.path.isfile(name)])
		file_count = len([name for name in os.listdir(outdir) if os.path.isfile(outdir + "/" + name)])
		if verbose:
			print "Number of output minfiles: " + str(file_count)
		instructions['summary_string'] += ("At " +  time.strftime("%y-%m-%d:%X") + " there are " + str(file_count) + " minifiles in " + outdir + "\n") 
		if(file_count != num_minfiles):
			outdirs_status = outdirs_status + 1
	if(outdirs_status == 0):
		#If all are done, mark as complete
		instructions['status'] = 10
		instructions['summary_string'] += "Status = " + str(instructions['status']) + "\n"
		instructions['summary_string'] += "minifiles written after " + str(instructions['nloops']) + " loops \n"
		instructions['nloops'] = 0
		if verbose: 
			instructions['summary_string'] += set_verbose_info(instructions['start_time_string'], instructions['start_time'])
	elif instructions['status'] < 9:
		#Otherwise, wait and keep going
		instructions['summary_string'] += "minifiles written after " + str(instructions['nloops']) + " loops \n"
		instructions['status'] = instructions['status'] + 1
		instructions['summary_string'] += "Status = " + str(instructions['status']) + "\n"
		instructions['nloops'] += 1
	else:
		#10 checks performed on status of minfile operations; 
		#this shold be alterable by other params as well
		instructions['status'] = instructions['status'] + 100
		instructions['summary_string'] += "Status = " + str(instructions['status']) + "\n"
		if verbose: 
			instructions['summary_string'] += set_verbose_info(instructions['start_time_string'], instructions['start_time'])
		#Not sure why I was returning outdirs_status, but I was for a while
	return(instructions)			
	
def submit_msub_postproc(instructions, verbose=False, dry_run = False):
	if instructions['nloops'] == 0: 
		instructions['start_time']  = time.time()
		instructions['start_time_string'] = time.strftime("%y-%m-%d:%X") 
	#Keep in mind, instructions is a dict of the following form: 
	#msub_loc = instructions['scriptdir'] + "/postProc/msubwrapper"
	msub_cmnd = "source " + instructions['msub_command']
	instructions['summary_string'] += (msub_cmnd + "\n")
	msub_out = subprocess.Popen(msub_cmnd,shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
	output0,errors0 = msub_out.communicate()
	out_list = output0.split()
	process_count = len(out_list) #The names of the subprocesses being sent by msub
	if msub_out.returncode == 0:
		instructions['status'] = 20
		instructions['summary_string'] += "Status = " + str(instructions['status']) + "\n"
		instructions['summary_string'] += "Job(s) "+ ", ".join(out_list) + " submitted" + "\n" 
	else:
		instructions['status'] += 100
		instructions['summary_string'] += "Status = " + str(instructions['status']) + "\n"
		instructions['summary_string'] += "Job(s) "+ ", ".join(out_list) + " NOT submitted; status is "+ str(msub_out.returncode) + "\n" 
		print "Something went wrong submitting the msub jobs " + ", ".join(out_list) + ". Please check logfiles."	
	return(instructions)
	
def check_postproc_status(instructions, verbose=False, dry_run = False):
	#Checks for the presence/absence of the post-processed output file in the
	#directory one up from the minifiles
	#Note: in contrast to some of the earlier code, this does not seem entirely ensemble-friendly.
	#Revise and discuss in more detail?
	if instructions['nloops'] == 0: 
		instructions['start_time']  = time.time()
		instructions['start_time_string'] = time.strftime("%y-%m-%d:%X") 
	print('searching for completed postproc run')
	pp_outdir = os.path.dirname(os.path.dirname(instructions['outdir']))
	print "Checking for output in: " + pp_outdir
	num_files = 1
	if instructions['qc_file_exists']:
		num_files += 1
	#print name for name in os.listdir(pp_outdir) if os.path.isfile(name)
	file_count = len([name for name in os.listdir(pp_outdir) if os.path.isfile(pp_outdir + "/" + name)])
	if verbose:
		instructions['summary_string'] += ("At " +  time.strftime("%y-%m-%d:%X") + " there are " + str(file_count) + " files in " + pp_outdir + "\n") 
	if instructions['nloops'] < 9:
		if file_count == num_files:
			instructions['status'] = 30
			instructions['summary_string'] += "Status = " + str(instructions['status']) + "\n"
			instructions['summary_string'] += "postproc complete after " + str(instructions['nloops']) + " loops \n"
			instructions['nloops']  = 0
			if verbose: 
				instructions['summary_string'] += set_verbose_info(instructions['start_time_string'], instructions['start_time'])
		else:	
			instructions['status'] += 1
			instructions['summary_string'] += "Status = " + str(instructions['status']) + "\n"
			instructions['nloops'] += 1
	else:
		instructions['summary_string'] += "postproc NOT complete after " + str(instructions['nloops']) + " loops \n" 
		instructions['status'] += 100
		instructions['summary_string'] += "Status = " + str(instructions['status']) + "\n"
		if verbose: 
			instructions['summary_string'] += set_verbose_info(instructions['start_time_string'], instructions['start_time'])
	return(instructions)
	
def submit_s8_job(instructions, verbose=False, dry_run = False, loop=0, copy_opt=False):
	#Working on this; adding min
	#nccmp_file = [f for f in os.listdir(instructions['outdir'])]
	#[f for f in os.listdir(out) if "300" in f and os.isfile(f)]
	#s8_instructions = instructions['s8_command'][loop]
	print "Starting s8 job"
	if instructions['nloops'] == 0: 
		instructions['start_time']  = time.time()
		instructions['start_time_string'] = time.strftime("%y-%m-%d:%X") 
	s8_instruct = instructions['s8_command']
	if instructions['call_postproc']:
		pp_outdir = os.path.dirname(os.path.dirname(instructions['outdir']))
		old_outdir = os.path.dirname(os.path.dirname(s8_instruct['old_outdir']))
	else:
		#Assume that you are only doing a single output file with minfile XML
		pp_outdir = instructions['outdir']
		old_outdir = s8_instruct['old_outdir']
	new_files = [f for f in os.listdir(pp_outdir) if os.path.isfile(pp_outdir + "/" + f)]
	old_files = [(old_outdir + "/" + f) for f in new_files] #old_outdir.join(new_files)
	new_files = [(pp_outdir + "/" + f) for f in new_files]
	print "New files:" + ", ".join(new_files)
	print "old files:" + ", ".join(old_files)
	print "Number of new files: " + str(len(new_files))
	for f in range(len(new_files)):
		#instructions['summary_string'] += ("nccmp "+new_files[f] + " " + old_files[f] + "\n")
		if copy_opt:
			instructions['summary_string'] += "Copying " + new_files[f] + " " + old_files[f] + "\n"
			if os.path.isfile(new_files[f]):
				shutil.copy2(new_files[f], old_files[f])
			else:
				instructions['summary_string'] += "Error in copy_output option: file "+new_files[f] + " does not exist\n"
		else:
			fudge_env = dict(os.environ)
			print "Printing current dict for later reference"
			print instructions
			s8_str = "tcsh /home/cew/Code/nccmp_script.csh " + new_files[f] + " " + old_files[f]
			#s8_str = "nccmp -d " + new_files[f] + " " + old_files[f]
			instructions['summary_string'] += s8_str + "\n" 
			s8_out = subprocess.Popen(s8_str,shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE, env = fudge_env)
			output0,errors0 = s8_out.communicate()
			instructions['summary_string'] += (output0 + "\n")
			print output0
			print errors0
	instructions['status'] += 100
	if verbose: 
		instructions['summary_string'] += set_verbose_info(instructions['start_time_string'], instructions['start_time'])
	return(instructions)	
	
def run_espresso_regtest(instructions, verbose=False, dry_run = False):
	nccmp_script = ""
	 		
def set_complete_status(instructions):
	instructions['summary_string'] += "End Status = " + str(instructions['status']) + "\n"
	if instructions['status'] < 100:
		instructions['status'] = 100
		
	return(instructions)	 
	
def set_verbose_info(start_time_string, start_time):
	import time
	end_time = time.time()
	total_time = end_time - start_time
	total_hrs, remainder = divmod(total_time, 3600)
	total_min, remainder = divmod(total_time, 60)
	total_sec = int(remainder)
	total_str = "{} hours {} min {} seconds".format(total_hrs, total_min, total_sec)
	#time.strftime("%H:%M:%S", time_1)
	return("Function started at " + start_time_string + " and ran for " + total_str + " total.\n")
	
def set_debug_info(instructions):
	instructions['summary_string'] += str(instructions)
