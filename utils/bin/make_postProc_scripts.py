#!/bin/python
#make_postProc_scripts
import expergen 
import fudgeList
import fudgeUtils
from fudgeUtils import read_exp_info_file
import os,stat,shutil
from os import path as osp
import shlex
import smtplib
import re
	

def make_postProc_scripts(ensembles, script_dir, fudgetag, mailid, runtime='4:00:00'):
	#Okay, this is going to go in a few steps:
	runmode = ( stat.S_IROTH | stat.S_IRGRP |stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR )	
	pp_dir = osp.join(script_dir, 'postProc') 
	pp_source_dir = osp.join(pp_dir, 'aux')	
	pp_log_dir = osp.join(script_dir, "log")
	pp_stat_dir = osp.join(pp_dir, "ppStatus")
	
	exp_info_file = osp.join(script_dir, "experiment_info.txt")
	expdict = read_exp_info_file(exp_info_file)
	
	#Wallclock time is hard-coded here. Setting it lower as of 2017-05, since
	#we've been staying in the queue for far longer than we normally do.
	code_list = []							   
	#for each ensemble present:
	for i,ds_dir in zip(range(0, len(ensembles)), expdict['output.path']):
		print("Working with ensemble " + ensembles[i])
		suffix_list = ['none', 'qcmask', 'save']
		pp_sourcelist = []
		for j,suffix in zip(range(0, len(suffix_list)), suffix_list):
			pp_runcode_header = "\n".join(["#!/bin/tcsh", 
								   "#PBS -S /bin/tcsh", 
								   "#PBS -l nodes=1:ppn=2,walltime="+str(runtime),
								   "#PBS -r y",
								   "#PBS -j oe", 
								   "#PBS -N fudge_postProc_" + str(i+1) + "_" + str(j+1), 
								   "#PBS -E", 
								   "#PBS -o " + pp_log_dir + "/fudge_postProc_.m${MOAB_JOBNAME}.${MOAB_JOBID}", 
								   "#PBS -q batch"])
			#make the sourcable code (command-line arg)
			pp_source = osp.join(pp_source_dir, "postProc_source_" + str(i+1) + "_" + str(j+1))
			out_dir = osp.dirname(ds_dir.rstrip("/"))
			pp_source_text = "python $BASEDIR/bin/postProc " + "-e " + exp_info_file + " -d " + ds_dir + " -o " + out_dir + " -s " + suffix + "\n"
			#Check on verbosity settings set earlier for message commands
			if osp.isfile( osp.join(pp_dir, "verb") + "/verbose.true" ):
				pp_source_text = pp_source_text + " -m " + mailid
			with open(pp_source, 'w') as f:
				f.write('set echo\n')
				f.write(pp_source_text)
			#pp_sourcelist.append(pp_source)
			#trying to get permissions right
			#os.chmod(pp_source, runmode)	
			##append to list of sourcable files
			#source_list.append(pp_source)	
			#make the complicated postProc runcode
		 	pp_runcode = osp.join(pp_dir, "postProc_command_" + str(i+1) + "_" + str(j+1))
			#would also work by using a -w for ensembles[i] - that would only match on ensembles[i]
			#as a word, not as a pattern in other words (i.e. the scRIPts problem)
			pp_statfile_remove = "find " + pp_stat_dir + "/* | grep " + ensembles[i] + " | grep 'status' | xargs rm -rf "
			pp_checkout_commands = "\n".join(["module load git/2.4.6", 
											  "eval `/home/esd/bin/checkout-fudge-s8 --fudgetag " + fudgetag + "`"])
			pp_status_commands = "\n".join(["if ( $status == 0 ) then", 
											"\t touch " + pp_stat_dir + "/" + ensembles[i] + "." + suffix + ".success", 
											"else", 
											"\t touch " + pp_stat_dir + "/" + ensembles[i] + "." + suffix + ".failure", 
											"endif"])																  
			with open(pp_runcode, 'w') as f:
				f.write(pp_runcode_header + "\n")
				f.write("\n" + "echo 'Setting FUDGE environment ...'" + "\n")
				f.write(pp_statfile_remove + "\n")
				f.write(pp_checkout_commands + "\n")
				f.write("source " + pp_source + "\n")
				#for el in pp_sourcelist:
				#	f.write("eval `cat " + el + "`\n")	
				f.write(pp_status_commands)
			#append to list of sourcable files
			code_list.append(pp_runcode)
	#print(code_list)	
	#change execute permissions	
	for el in code_list:
		#print(el)
		os.chmod(el, runmode)				
	#write the msubwrapper
	msub_file = osp.join(pp_dir, 'msubwrapper')
	msub_text = "#!/bin/tcsh -f \n"
	msub_text = msub_text + "rm -f " + pp_stat_dir + "/*\n"
	msub_commands = "\n".join(["msub -d $HOME " + el for el in code_list]) + "\n"
	msub_text = msub_text + msub_commands + "\n"
	#print('writing to msub_file ' + msub_file)
	#print("code_list "+ str(code_list))
	#print(ensembles)
	#print(expdict['output.path'])
	#sys.exit('examine this output')
	with open(msub_file, 'w') as f: 
		f.write(msub_text)
	os.chmod(msub_file, runmode)		
