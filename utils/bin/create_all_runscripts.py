import re
import os, stat, shutil
from os import path as osp
import itertools
import json
from collections import OrderedDict

def create_all_runscripts(i_start, i_end, i_by, i_offset,
						 j_start, j_end, j_by, j_offset,
						 njobs, ncpu, fudgetag, scriptdir,
						 OH_dir, MH_dir, MF_dir,
						 oh_timerange, mh_timerange, mf_timerange, predlist,
						 spatmask, OH_tmask, MH_tmask, MF_tmask, trim_tmask,
						 outdir_string, target_var, ds_method, j_range, idirsuffix, multivar_target,
						 auxfile, aux_file_list, wallclocktime,
						 auxdict, ds_minifile):
	#Creates master and single-slice miniifles for a FUDGE job
	#if --msub is included, also submits jobs at the end of everything

	##print(outdir_string)
	##print(idirsuffix)
	##sys.exit('examine this output')
	#which i_indices are we going to be looping over?
	i_elements = range(int(i_start), int(i_end)+1, i_by)
	i_elements.append(int(i_end)+1)
	i_elements = list(set(i_elements))
	i_elements = sorted(i_elements)
	#i_offset = int(i_start)  - 1
	i_max = int(i_end) - i_offset
	#which j indices are we going to be looping over?
	j_elements = range(int(j_start), int(j_end),j_by)
	j_elements.append(int(j_end)+1)
	j_elements = list(set(j_elements))
	j_elements = sorted(j_elements)
	#j_offset = int(j_start)  - 1
	j_max = int(j_end) - j_offset
	#sys.exit("Examine this out")

	#Okay - as of 207-05-09, adding a one-off special case for the synthetic data
	#cases - as distinguished by the presence of grid0.
	is_syn = False
	if idirsuffix=="/grid0/":
		print("Adding synthetic data workaround")
		idirsuffix = idirsuffix + "ZeroD/"
		j_elements  = [1, 2]
		is_syn = True
	if idirsuffix=='/X1-1/':
		print("Adding other synthetic data workaround")
		idirsuffix = idirsuffix
		j_elements  = [1, 2]
		is_syn = True

	moab_header = "\n".join(["#!/bin/tcsh ",
							"#PBS -S /bin/tcsh ",
							"#PBS -l nodes=1:ppn="+str(ncpu)+",walltime="+wallclocktime,
							"#PBS -r y ",
							'#PBS -j oe ',
							'#PBS -N fudge_master ',
							'#PBS -E ',
							#Swapping out 2017-05-31. Apparently, the curly brackets are
							#an unsupported feature? See Tim emails.
							#'#PBS -o ' + scriptdir+'/log/'+'fudge.${MOAB_JOBNAME}.${MOAB_JOBID}',
							'#PBS -o ' + scriptdir+'/log/'+'fudge.$(JOBNAME).$(JOBID)',
							"#PBS -q batch "]) + "\n"
	#The path to the checkout script is hard-coded, and probably going to
	#stay that way for now; the load of git is DEFINETLY needed if the
	#checkout script is going to be used.
	module_loads = "\n".join(['module load R',
							  'module load gcp',
							  'module load netcdf',
							  'module load nco',
							  'module load git/2.4.6',
							  'eval `/home/esd/bin/checkout-fudge-s8 --fudgetag ' + fudgetag + '`']) + "\n"
	#Okay, now define the set of files that you will work with
	#(no more minifiles=all files are the same for each job slice)
	#first, checking for the presence of the optional files
	spatmask_present = (spatmask != 'na')
	tmask_present = (OH_tmask != 'na')
	trim_time_mask_present = (trim_tmask != 'na')
	#do all of your dmgets (requests for files on tape) in the script at once
	#it is easier on the infrastructure if they are all done at once, since
	#odds are some of the files we want are on the same tapes
	dmget_string = ""
	if spatmask_present:
		#Need the file located at one level up from the spatmask dir -
		#spatmask itself points only to the OneD dir
		spatmask_dir = osp.dirname(osp.abspath(spatmask))
		spatmask_file = [osp.join(spatmask_dir, el) for el in os.listdir(spatmask_dir) if re.search('masks', el) is not None][0]
		dmget_string = dmget_string + "# Obtain spatial mask" + "\n"
		spatmask_files = [spatmask_file]
		dmget_string = dmget_string + "dmget " + spatmask_file + " \n"
		spatmask_path = spatmask_file
	else:
		spatmask_files = []
		spatmask_path = 'none'

	#MH and MF files have an assumption that you can have multiple
	#predictor vars present. At some point, var classification checks
	#(such is is_spaceless for ENSO data) should go in here	.
	#MH and MF paths are currently passed in several dirlevels earlier than OH
	#Switch on this later, when things get added to the aux
	present_in_aux = auxdict['files_in_aux']
	if not present_in_aux:
		#OH_dir = os.path.dirname(osp.abspath(OH_dir))
		prefix_loc = len(MH_dir.split("/"))
		oh_prefix = "/".join(OH_dir.split("/")[0:prefix_loc])
		oh_suffix = "/".join(OH_dir.split("/")[(prefix_loc+1):-1])
		if not multivar_target:
			oh_vars = [target_var]
			oh_files,oh_dmget = build_xml_input_paths(oh_vars, oh_prefix, oh_timerange, "/" + oh_suffix, False)
		else:
			#Only triggered by MBC; may change later
			oh_vars = predlist
			oh_files, oh_dmget = build_xml_input_paths(predlist, oh_prefix, oh_timerange, idirsuffix, True)
		mh_files,mh_dmget = build_xml_input_paths(in_vars=predlist, in_dir=MH_dir, trange=mh_timerange, suffix=idirsuffix, is_multivar=True)
		mf_files,mf_dmget = build_xml_input_paths(in_vars=predlist, in_dir=MF_dir, trange=mf_timerange, suffix=idirsuffix, is_multivar=True)
		#Add modified aux copy & structure addition
		oh_auxdict = make_aux_entry(oh_vars,  oh_files, stencil=auxdict['OH']['stencil'], geomask=spatmask_path)
		mh_auxdict = make_aux_entry(predlist, mh_files, stencil=auxdict['MH']['stencil'], geomask=spatmask_path)
		mf_auxdict = make_aux_entry(predlist, mf_files, stencil=auxdict['MF']['stencil'], geomask=spatmask_path)
		ds_auxdict = make_ds_aux_entry(ds_minifile, outdir_string, oh_vars[0])
		auxdict['OH'] = oh_auxdict
		auxdict['MH'] = mh_auxdict
		auxdict['MF'] = mf_auxdict
		auxdict['DS'] = ds_auxdict
		auxfile_mod = scriptdir + "/config_mod/" + osp.basename(auxfile)
		os.makedirs(osp.dirname(auxfile_mod))
		auxstr = json.dumps(auxdict, indent=4)
		f = open(auxfile_mod, 'w')
		print >> f, auxstr
		f.close()
	else:
		oh_files, oh_dmget = build_aux_input_paths(auxdict['OH'])
		mh_files, mh_dmget = build_aux_input_paths(auxdict['MH'])
		mf_files, mf_dmget = build_aux_input_paths(auxdict['MF'])
		auxfile_mod = scriptdir + "/config_mod/" + osp.basename(auxfile)
		os.makedirs(osp.dirname(auxfile_mod))
		shutil.copyfile(auxfile, auxfile_mod)
	#For either case, concatenate all the dmget strings together
	dmget_string = dmget_string + "# Get OH data " + oh_dmget + "# Get MH data\n" + mh_dmget + "# Get MF data\n" + mf_dmget
	tmask_files = []
	leapmask_files = []
	if tmask_present:
		tmask_files = [OH_tmask, MH_tmask, MF_tmask]
		dmget_string = dmget_string + "# Get all tmasks (OH MH MF trimming) " + "\n"
		if trim_time_mask_present:
			tmask_files.append(trim_tmask)
			#dmget_string = dmget_string + "# Get MF overlap tmask " + "\n"
			#dmget_string = dmget_string + "dmget " + trim_tmask + "\n"
		tmask_dmget = "dmget " + " ".join(tmask_files)	 + "\n"
		dmget_string = dmget_string + tmask_dmget
	#And finally, add the files that the aux file specifies
	if len(aux_file_list) > 0:
		aux_strlist = ["#Copy files from aux specification"]
		for el in aux_file_list:
			aux_strlist.append("dmget " + el)
		dmget_string = "\n".join(aux_strlist)
	#do all of your gcps next
	gcp_string = "# Copy all input files to $TMPDIR (-cd option creates input directories) " + "\n"
	file_list = leapmask_files + spatmask_files + tmask_files + oh_files + mh_files + mf_files + aux_file_list
	print("file_list:")
	print(file_list)
	for f in file_list:
		gcp_string = gcp_string + "mkdir -p $TMPDIR/" + os.path.dirname(f) + " \n"
		gcp_string = gcp_string + make_gcp_cmd(f, "$TMPDIR/"+f)
	gcp_string = gcp_string + "echo 'Copy operations complete.' \n"
	gcp_string = gcp_string + "sleep 10 \n"

	#Add code for creating common output directories
	#(copy opts for each minifile/slice take place within runscript
	#as well as writing of script status files)
	'''
	#Current string in form of blank_outdir#(blank_rip)#rip1,rip2,rip3
	outdir_cmd = create_outdir_string(outdirs, scriptdir, target_var)
	'''

	outdirs = [re.sub('<RIP>', el, auxdict['DS']['out_dir']) + "/" + auxdict['DS']['out_subdir'] + "/" for el in auxdict['DS']['rips']]
	outdir_cmd = create_outdir_string(outdirs, scriptdir, target_var)
	outdir_string = create_outdir_string(outdirs, scriptdir, target_var)

	#Finally, start opts that need to loop by index available
	runmode = ( stat.S_IROTH | stat.S_IRGRP |stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR )
	#print(j_elements)
	n_tiles = 0
	for j in range(0, len(j_elements)-1):
		jval = j_elements[j]
		j_string = "J"+str(j_elements[j]) + "-" + str(j_elements[j+1]-1)
		#j_max and j_by are passed in already; they are numeric
		for i in range(0, len(i_elements)-1):
			ival = i_elements[i]
			n_tiles = n_tiles + 1
			i_string = "I" + str(i_elements[i]) + "-" + str(i_elements[i+1]-1)
			if i % njobs == 0 :
				#Create the master scriptname
				end_idx = i+njobs-1
				if end_idx > len(i_elements) - 1:
					end_idx  = len(i_elements) - 1
				i_master_string = "I" + str(i_elements[i]) + "-" + str(i_elements[end_idx])
				master_scriptname = osp.join(scriptdir, 'master', 'master_script_'+i_master_string+'_'+j_string)
				runscript_files = []
			#Since master is created on first (always), can proceed with rest of runscript calls
			runscript_name = osp.join(scriptdir, 'runscript', "script."+target_var+"."+ds_method+"."+i_string+"_"+j_string)
			i_idx = i_offset - (i+1 + i*(i_by-1))
			j_idx = j_offset - (j+1 + j*(j_by-1))
			if is_syn:
				i_idx = 1
				imax = 1
				i_by = 1
				j_idx = 1
				jmax = 1
				j_by = 1
			else:
				i_idx = ival - i_offset + 1
				imax = i_max + 1
				j_idx = jval - j_offset + 1
				jmax = j_max + 1
			#2018-04-09: Need to copy modified auxfile all the time, since that
			#is where the file paths are.
			runscript_code = create_slice_runscript(i_string, j_string, scriptdir, \
													i_idx, imax, i_by, i_offset, \
													j_idx, jmax, j_by, j_offset, auxfile_mod, target_var,outdirs)
			with open(runscript_name, 'w') as f:
				f.write(runscript_code)
			os.chmod(runscript_name, runmode)	#f now executable
			runscript_files.append(runscript_name)
			if i %njobs == (njobs-1) or i == (len(i_elements) - 2):
				#Prepare to write all accululated data to file
				executable_code = make_executable_stuff(runscript_files, scriptdir, auxfile_mod,
														i_idx, imax, i_by, i_offset, j_idx, jmax, j_by, j_offset)
				with open(master_scriptname, 'w') as f:
					f.write(moab_header)
					f.write(module_loads)
					f.write(dmget_string)
					f.write(gcp_string)
					f.write(outdir_cmd)
					f.write("echo 'Beginning calls to executable'\n")
					f.write(executable_code)
					#f.write("wait")
	print('master runscript and mini creation completed')
	#return(n_tiles)

def build_xml_input_paths(in_vars, in_dir, trange, suffix, is_multivar=True):
	'''
	Builds input paths from the components that are present in the XML.
	'''
	in_files = []
	dmget_string = ""
	for v in in_vars:
		f_dir = osp.join(in_dir, v) + suffix
		f_dir_files = [osp.join(f_dir, el) for el in os.listdir(f_dir) if re.search(trange, el) is not None]
		#the above should match one, and exactly one file. Include a check in here?
		in_file = [el for el in f_dir_files if re.search(".nc$", el) is not None][0] #should only match ONE
		in_files.append(in_file)
		dmget_string = dmget_string + "dmget "+ in_file + "\n"
	return(in_files, dmget_string)

def build_aux_input_paths(aux_subset):
	'''
	Makes the dmget strings and does file copy opts for a subset of the aux
	dictionary - one of the OH,MH,MF subsets
	'''
	in_files = []
	dmget_string = ""
	for v in aux_subset.keys():
		in_file = aux_subset[v]['file']
		in_files.append(in_file)
		dmget_string += 'dmget ' + in_file + "\n"
	return(in_files, dmget_string)

def make_gcp_cmd(sourcefile, destfile):
	'''
	Wraps a gcp command in a status checker; exits with an error if that command fails
	'''
	gcp_cmd  = "gcp --sync --verbose " + sourcefile + " " + destfile
	gcp_echo = "echo '" + gcp_cmd + "'"
	stat_cmd = "\n".join(["if ($status != 0) then",
						 "\t echo 'Error in runscript: gcp from " + sourcefile + " to " + destfile + " failed!'",
						 "\t exit 99",
						 "endif"])
	return(gcp_cmd + "\n" + gcp_echo + "\n" + stat_cmd + "\n")

def create_outdir_string(outdirs, scriptdir, target_var):
	#Wrapper for making the outdir creation commands
	outdir_string = "# Creating output directories \n"
	qcmaskdir = "/" + target_var + "_qcmask/"
	for out in outdirs:
		outdir_string = outdir_string + "mkdir -p " + out + "\n"
		outdir_string = outdir_string + "if ($status != 0) then"+ "\n"
		outdir_string = outdir_string + "\t echo 'Error in creation of output dir! You may not have write permissions'"+ "\n"
		outdir_string = outdir_string + "\t touch " + scriptdir + "/status/.mini.fail"+ "\n"
		outdir_string = outdir_string + "endif"	+ "\n"
		outdir_string = outdir_string + "mkdir -p $TMPDIR/" + out + "\n"
		outdir_string = outdir_string + "mkdir -p " + out + qcmaskdir + "\n"
		outdir_string = outdir_string + "mkdir -p $TMPDIR/" + out + qcmaskdir + "\n"
	return(outdir_string)

def create_slice_runscript(i_string, j_string, scriptdir,
							i_idx, i_max, i_by, i_offset,
							j_idx, j_max, j_by, j_offset, auxfile_mod, target_var, outdirs):
	#Creates the runscript code specific to a slice of the grid downscaled
	#by the R code
	#Technically only the shebang is needed, but extra info in the event that we ever split
	#these differently is not going to hurt.
	# i: 1:194
	# j_range: 'J454_576'
	runscript_log = osp.join(scriptdir, 'log') + "/ind.${MOAB_JOBNAME}.${MOAB_JOBID}"
	runscript_header = "\n".join(['#!/bin/tcsh',
								  '#PBS -S /bin/csh',
								  '#PBS -l nodes=1:ppn=1,walltime=6:00:00',
								  '#PBS -j oe',
								  '#PBS -r y ',
								  '#PBS -o ' + runscript_log,
								  '#PBS -N fudge_run',
								  '#PBS -E',
								  '#PBS -q batch']) + "\n"
	exp_name = scriptdir.split("/")[-1]
	if(exp_name==''):
		exp_name = scriptdir.split("/")[-2]
	runcode = osp.join(scriptdir, 'runcode') + "/code." + target_var + "." + exp_name + "." + i_string + "_" +j_string
	logfile = osp.join(scriptdir, 'log') + "/out."+exp_name+"."+ i_string + "_" +j_string
	#print(runcode)
	#print(logfile)
	#Okay - this defines the list of runcodes that are going to be run.
	#i_idx is in the same space as i_max; i is in minifile space.
	#r_command = "Rscript " + runcode + " " + " ".join([str(el) for el in [i_idx, i_max, auxfile, 'TRUE']]) + " >>& " + logfile #+ " &"
	r_command = "Rscript " + runcode + " " + " ".join(str(el) for el in [i_idx, i_max, i_by, i_offset, j_idx, j_max, j_by, j_offset, auxfile_mod, 'TRUE'])+ " >>& " + logfile + " &"
	runscript = runscript_header + r_command + "\n" + "wait \n"
	for outdir in outdirs:
		runscript = runscript + create_runscript_copy_ops(outdir, i_string, j_string, target_var)
	return(runscript)

def create_runscript_copy_ops(outdir, i_string, j_string, target_var):
	#Makes the copy operations for the output of the R code
	#Currently assumes a single file of _qcmask output;
	#when we get to writing coordinates, this needs to be updated
	#Note: right now, for a given script job, all J are going to be identical.
	outstr = []
	outstr.append('echo `ls "$TMPDIR/' + outdir + '"`')
	outstr.append('echo gcp "$TMPDIR/' + outdir + "/*."+i_string+"_"+j_string + ".nc " + outdir + '"')
	outstr.append("gcp $TMPDIR/" + outdir + "/*."+i_string+"_"+j_string + ".nc " + outdir)
	outstr.append("set ostat = $?")
	outstr.append("if ( $ostat == 0 ) then")
	outstr.append("\t touch " + outdir + "/.mini.success")
	outstr.append("else")
	outstr.append("\t touch " + outdir + "/.mini.fail")
	outstr.append('endif')
	#Now do the same for the qc mask file
	out_mask_dir = outdir + "/" + target_var + "_qcmask/"
	outstr.append('echo "gcp $TMPDIR/' + out_mask_dir + "/*."+i_string+"_"+j_string + ".nc " + out_mask_dir + '"')
	outstr.append("gcp $TMPDIR/" + out_mask_dir + "/*."+i_string+"_"+j_string + ".nc " + out_mask_dir)
	outstr.append("set ostat = $?")
	outstr.append("if ( $ostat == 0 ) then")
	outstr.append("\t touch " + out_mask_dir + "/.mini.success")
	outstr.append("else")
	outstr.append("\t touch " + out_mask_dir + "/.mini.fail")
	outstr.append('endif')
	return("\n".join(outstr) + "\n")

def awful_runcode_hack(runcode_dir, runscript_dir):
	#I don't want to have to tinker with something that we're planning on
	#replacing during this flavor or the next. For now, it's not much of
	#a stretch to copy some of the runcodes to a location that fits the
	#expected pattern in the runscripts.
	runscript_files = [el for el in os.listdir(runscript_dir) if re.search("script", el) is not None]
	runcode_files = [el for el in os.listdir(runcode_dir) if re.search("code", el) is not None]
	runscript_patterns = [re.split("\.", el)[-1] for el in runscript_files if (re.split("\.", el)[-1]).count('-') == 2]
	#print(runscript_patterns)
	runcode_split = re.split("\.", runcode_files[0])
	#print(runcode_split)
	for patt in runscript_patterns:
		tmp_file = runcode_split
		tmp_file[-1] = patt
		tmp_dest = osp.join(runcode_dir, ".".join(tmp_file))
		tmp_src = osp.join(runcode_dir, runcode_files[0])
		shutil.copy2(tmp_src, tmp_dest)

def make_executable_stuff(runscript_files, scriptdir, auxfile_mod,
						  ival, imax, i_by, i_offset, jval, jmax, j_by, j_offset):
	'''
	Makes the things that call R code (eventually) in the master.
	The batch branch sources runscripts with pre-defined x,y locations
	  Workflow: master -> runscript -> runcode -> Master_Driver
	The interactive branch is activated when master is called with command-line
	  arguments, and pops an rstudio terminal with a config file that allows
	  access to the command-line params that set the x,y locations in the
	  runscripts.
	  Workflow: master -> config file -> runcode -> Master_Driver
	'''
	runscript_calls = ["\t" + el + " & \n" for el in runscript_files]
	batch_cmd = "\t sleep 10 \n".join(runscript_calls) + "\twait"
	#Then, do the code that gets used for the alt running
	echo_cmd = "<newline> ".join([
						  "# clean environment before calls",
						  "rm(list=ls())",
						  "# set twindow parameters:",
						  "## if not set to 'all', you can set to a number from",
						  "## 1:nmasks to run a specific mask",
						  "twindow_run <<- 'all'",
						  "# set i and j location:",
						  "## idx is the i/j index from which you start,",
						  "## stride is the number of indices from that point you step (1 being a single i or j val)",
						  "## max is the length of the i/j dimension (DONT EDIT) ",
						  "## offset is the i,j offset (DONT EDIT YET - USED FOR FILENAME CONSTRUCTION)",
						  "i_idx    <<- " + str(ival),
						  "i_max    <<- " + str(imax),
						  "i_stride <<- " + str(i_by),
						  "i_offset <<- " + str(i_offset),
						  "j_idx    <<- " + str(jval),
						  "j_max    <<- " + str(jmax),
						  "j_stride <<- " + str(j_by),
						  "j_offset <<- " + str(j_offset),
						  "# point to the modified auxfile (has full file paths)",
						  "auxfile <<- '" + auxfile_mod + "'",
						  "# source the runcode which sources Master_Driver",
						  "source('$generic_runcode')"])
	echo_cmd = '"' + echo_cmd + '"'

	int_cmd = "\n\t".join(["\tmodule load rstudio/0.98.1103",
						  "module unload R/2.15.2",
						  "module load R/3.2.1",
						  "set auxfile_mod = " + auxfile_mod,
						  "set runcode_dir = " + scriptdir + "/" + 'runcode',
						  "set generic_runcode_base = `ls $runcode_dir | head -n 1`",
						  "set generic_runcode = $runcode_dir/$generic_runcode_base",
						  #Below line shouldn't give an error: removes if there,
						  #moves on if not.
						  "rm -rf $TMPDIR/command_line_params.R",
						  "set r_cmd = " + echo_cmd,
						  "set r_header = '<shebang>/bin/Rscript'",
						  "echo $r_header | sed -e 's:<shebang>:#\!:g' >> $TMPDIR/command_line_params.R",
						  "echo $r_cmd | sed -e 's:<newline>:" + r'\n' + ":g' >> $TMPDIR/command_line_params.R",
						  "echo 'Command to run R minifile code from the command line:'",
						  'echo "rstudio $TMPDIR/command_line_params.R &"',
						  "rstudio $TMPDIR/command_line_params.R &"]
						  )
	#And then, join both commands together with ifelse
	executable_stuff = "\nset arrlen = $#argv\n\nif ( $arrlen == 0 ) then\n" + batch_cmd + "\nelse\n" + int_cmd + "\nendif"
	return(executable_stuff)


def make_aux_entry(varnames=['fakevar'], files=['fake_file.nc'], stencil='none', tmask='none', geomask='none'):
	'''
	Makes an aux file entry of input data files for one of OH, MH or MF.
	'''
	aux_entry = {}
	for var,f in zip(varnames, files):
		aux_entry[var] = {'file': f,
						  'is_fixed':'FALSE',
						  'stencil': stencil,
						  'geomask': geomask,
						  'tmask':   tmask}
	#2017-04-06: R code currently looks for stencil under top-level check,
	#not under the var names.
	#aux_entry['stencil'] = stencil
	return(aux_entry)

#And make the output dict for the json file
def make_ds_aux_entry(out_file, outdir, var):
	'''
	Makes an output dict for the aux file.
	'''
	outdir_opts = outdir.split("#")
	outdir_path = "/".join(re.split('/', outdir_opts[0].rstrip('/'))[:-1])
	riplist = outdir_opts[2].split(",")
	if (riplist[0] == 'na'):
		riplist = [outdir_path.split("/")[-5]]
	#The parenthesis made sense in c-shell, but pythons' sub functions are PICKY
	outrip_patt = "\$\(RIP\)"
	outdir_path = re.sub(outrip_patt, '<RIP>', outdir_path)
	#
	out_subdir = re.split('/', outdir_opts[0].rstrip('/'))[-1]
	out_vars = [var]
	#Edit this when we start adding more in the way of supplementary output writing
	out_supp = [var + '_qcmask']
	out_rips = riplist
	dsdict = {'out_dir' : outdir_path,
			  'out_file' : out_file,
			  'out_subdir' : out_subdir,
			  'out_supp' : out_supp,
			  'vars' : out_vars,
			  'rips' : out_rips}
	return(dsdict)
