def parseEspressoXML(xml, start_status=0, end_status=100, regtest=False, regtest_dir=None, copy_opt=False):
	import expergen
	import os, shutil, fileinput, time, sys
	#Check for XML existing
	if not os.path.isfile(xml):
		#Stop and raise error in Python
		print "XML " + xml + " does not exist. Please try again." 
		sys.exit()

	output_grid,kfold,lone,region,fut_train_start_time,fut_train_end_time,file_j_range,hist_file_start_time,hist_file_end_time,hist_train_start_time,hist_train_end_time,lats,late,lons,late, basedir,method,target_file_start_time,target_file_end_time,target_train_start_time,target_train_end_time,spat_mask,fut_file_start_time,fut_file_end_time,predictor,target,params,outdir,dversion,dexper,target_scenario,target_model,target_freq,hist_scenario,hist_model,hist_freq,fut_scenario,fut_model,fut_freq,hist_pred_dir,fut_pred_dir,target_dir,expconfig,target_time_window,hist_time_window,fut_time_window,tstamp,ds_region,target_ver,auxcustom,qc_switch,qc_varname,qc_type,adjust_out,sbase,pr_opts,masklist,idirsuffix = expergen.listVars(xml,basedir='/home/cew/Code/fudge2014/',pp=True) 
	#Start_status defaults to 1
	#End status defaults to 
	#Check on the number of expected minifiles
	expected_minfiles = int(lone) - int(lons) + 1
	#Add another check for thether postproc is appropriate based off of above
	if output_grid == 'US48' and expected_minfiles == 194:
		call_postproc = True	
	elif output_grid == "SCCSC0p1" and expected_minfiles == 190:
		call_postproc = True
	else:
		call_postproc = False
	#Check on whether a qc file exists	
	if qc_switch=='on':
		qc_out = True
	else:
		qc_out = False
	#Check to make sure that the script dir exists (leads to errors later)
	orig_sbase = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(sbase))))
	if not os.path.isdir(orig_sbase):
		print "Script dir " + orig_sbase + " does not exist. Please try again." 
		sys.exit()
	#Remove any trailing #nas from the outdir
	outdir = outdir.replace("#na", "")
	#Default s8 script:
	s8_command = {"s8_method": None}
	orig_xml = ""
	orig_sbase = ""
	new_sbase = ""
	#If calling as part of a regression test, copy to regtest dir and set the s8 script:
	if regtest:
		regtest_compare_dir = "/archive/esd/REGRESSION_TESTS/old_output/"
		#refer to previously created timestamped regtest_dir; 
		#copy XML there and prepare to store script and regtest output dir
		#Look, don't worry about the s8 stuff for the moment; 
		print "Regression test dir: " + regtest_dir 
		orig_sbase = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(sbase))))
		new_outdir = regtest_dir + "/" + outdir
		#new_scriptdir = regtest_dir + "/" + sbase
		#The two following dirs are NOT THE SAME THING; scriptidr in xml is different from 
		#scriptdir returned from parsing via FudgeList
		new_sbase = regtest_dir + "/" + orig_sbase
		new_scriptdir = regtest_dir + "/" + sbase
		#os.makedirs(new_scriptdir) #Errors later on if scriptdir does not exist
		new_xml = regtest_dir + "/" +  os.path.basename(xml)
		print "New XML location: " + new_xml
		print "old XML location: " + xml
		shutil.copy2(xml, new_xml)
		time.sleep(30) #This is a step that needs to happen; it takes a while for file transfers to take effect
		for line in fileinput.input(new_xml, inplace = True):
			line = line.replace(orig_sbase, new_sbase)
			line = line.replace(outdir, new_outdir)
			#print(line, end="")
			sys.stdout.write(line)
		#And finally, determine the args for the s8 script (if regtest)
		time.sleep(30)
		compare_out = regtest_compare_dir + outdir
		if call_postproc: #If calling postproc, check final postproc output location
			compare_out = os.path.dirname(compare_out)
		s8_command = {
					'old_outdir': compare_out, 
					's8_method': "regression_test",
					}
		#Finally, check to make sure that you have access to the appropriate 
		print "Attempting dir check now"
		if os.access(compare_out, os.R_OK):
			if copy_opt and os.access(compare_out, os.W_OK):
				print "Write access enabled appropriately"
			else:
				print "Write access NOT enabled properly. Something is wrong, please try again."
		else:
			print "Do NOT have read access to the top-level directory. Something is wrong, please try again."
		#And finally, fix outdir and scriptdir for script reference
		outdir = new_outdir
		sbase = new_scriptdir
		orig_xml = xml
		xml = new_xml	
	rundict = {
				'xml':            xml,
				'status':         start_status,       #Have just started the process; on first try	
				'end_status':     end_status,
				'outdir':         outdir,      #Not yet set; letFudgelist get that for you - keep in mind, can be more than one of these; store as dict
				'ens_outdir':     "",  #Not used yet, but will be if you get some of the other stuff in place such as s8 scripting
				'scriptdir':      sbase,
				'msub_command':   sbase + "/postProc/msubwrapper",
				'files_expected': expected_minfiles,
				'call_postproc':  call_postproc,
				'target_var':     target,  
				'qc_file_exists': qc_out,
				'nloops':         0,
				#assume that you are never going to stagger the start
				#time of the jobs that you are submitting - all start
				#from the same wait command....
				#This means that you could specify it as a wait command
				#instead - via the command-line arguments
				'start_time': 0,
				'start_time_string': "",
				#start_time is the time at which you start; refers to the 
				'summary_string': "",
				's8_command': s8_command,
				'orig_xml': orig_xml,
				'orig_sbase': orig_sbase,
				'new_sbase': new_sbase,
				}
	return rundict
#Current status posting makes it complex to write regression tests in pieces rather than all at once
#Arguably, 
