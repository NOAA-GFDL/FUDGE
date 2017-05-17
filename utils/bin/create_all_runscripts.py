import re
import os, stat, shutil
from os import path as osp
import itertools

def create_all_runscripts(i_start, i_end, i_by, j_start, j_end, j_by, njobs, ncpu, fudgetag, scriptdir, OH_dir, MH_dir, MF_dir, oh_timerange, mh_timerange, mf_timerange, predlist, spatmask, OH_tmask, MH_tmask, MF_tmask, trim_tmask, outdir_string, target_var, ds_method, j_range, idirsuffix, copy_leapmasks, multivar_target, auxfile, aux_file_list, wallclocktime):
	#Creates master and single-slice miniifles for a FUDGE job
	#if --msub is included, also submits jobs at the end of everything
	
	#which i_indices are we going to be looping over?
	i_elements = range(int(i_start), int(i_end)+1, i_by)
	i_elements.append(int(i_end)+1)
	i_elements = list(set(i_elements))
	i_elements = sorted(i_elements)	
	i_offset = int(i_start)  - 1
	i_max = int(i_end) - i_offset	
	#which j indices are we going to be looping over?
	j_elements = range(int(j_start), int(j_end),j_by)
	j_elements.append(int(j_end)+1)
	j_elements = list(set(j_elements))
	j_elements = sorted(j_elements)	
	j_offset = int(j_start)  - 1
	j_max = int(j_end) - j_offset
	#print(i_elements)
	#print(j_elements)
	#sys.exit("Examine this out")
	
	#Okay - as of 207-05-09, adding a one-off special case for the synthetic data
	#cases - as distinguished by the presence of grid0. 
	if idirsuffix=="/grid0/":
		print("Adding synthetic data workaround")
		idirsuffix = idirsuffix + "ZeroD/"
	
	### As of 3-15-17, working on script subset changes needed for 
	### handling entire-grid downscaling and the arbitrary x,y subsets.
	####don't worry about the njobs parameter for now; it's handled in the subset looping.
	###mini_start = range(int(i_start), int(i_end), njobs)
	###mini_end = [el + njobs for el in mini_start]
	###mini_end = [el if el <= (int(i_end)+1) else (int(i_end)+1) for el in mini_end]
	###i_offset = int(i_start) - 1
	###i_max = int(i_end) - i_offset
	####correct j_range
	###j_range = re.sub('"', "", j_range)
	#One of the goals for this was to get away from using gcp-specific operations
	#As of 1-19-17, wallclocktime defaults to 12 hrs; we can set it higher or lower
	#in the aux file
	moab_header = "\n".join(["#!/bin/tcsh ", 
	                        "#PBS -S /bin/tcsh ", 
	                        "#PBS -l nodes=1:ppn="+str(ncpu)+",walltime="+wallclocktime, 
	                        "#PBS -r y ", 
	                        '#PBS -j oe ', 
	                        '#PBS -N fudge_master ', 
	                        '#PBS -E ', 
	                        '#PBS -o ' + scriptdir+'/log/'+'fudge.${MOAB_JOBNAME}.${MOAB_JOBID}',  
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
		#print(spatmask)
		#print(spatmask_dir)
		#sys.exit()
		spatmask_file = [osp.join(spatmask_dir, el) for el in os.listdir(spatmask_dir) if re.search('masks', el) is not None][0]
		dmget_string = dmget_string + "# Obtain spatial mask" + "\n"
		spatmask_files = [spatmask_file]
		dmget_string = dmget_string + "dmget " + spatmask_file + " \n" 
	else:
		spatmask_files = []	
	dmget_string = dmget_string + "# Get OH data" + "\n"
	#OH directory currently described with /OneD/ appended
	OH_dir = os.path.dirname(osp.abspath(OH_dir))
	oh_files = [osp.join(OH_dir,el) for el in os.listdir(OH_dir) if re.search(oh_timerange, el) is not None]
	#multivar_target is a hack for getting around alternate  var specifications. 
	#currently, it's only triggered by MBC; depending on what methods we add, 
	#that may change.
	if not multivar_target: 
		for f in oh_files:
			dmget_string = dmget_string + "dmget " + f + "\n"
	else:
		prefix_loc = len(MH_dir.split("/"))
		oh_prefix = "/".join(OH_dir.split("/")[0:prefix_loc])
		oh_suffix = idirsuffix
		for pred_var in predlist:
			#Do a series of replaces on the predictor var dir
			oh_dir = osp.join(oh_prefix, pred_var) + oh_suffix
			#print([osp.join(oh_dir, el) for el in os.listdir(oh_dir) if re.search(mh_timerange, el) is not None])
			oh_file = [osp.join(oh_dir, el) for el in os.listdir(oh_dir) if re.search(mh_timerange, el) is not None][0] #should only match ONE
			oh_files.append(oh_file)
			dmget_string = dmget_string + "dmget " + oh_file + " \n"		
	#MH and MF files have an assumption that you can have multiple 
	#predictor vars present. At some point, var classification checks
	#(such is is_spaceless for ENSO data) should go in here	.
	#MH and MF paths are currently passed in several dirlevels earlier than OH 
	dmget_string = dmget_string + "# Get MH data" + "\n"
	mh_files = []
	for pred_var in predlist:
		mh_dir = osp.join(MH_dir, pred_var) + idirsuffix
		print(idirsuffix)
		print(mh_dir + " , " + mh_timerange)
		mh_file = [osp.join(mh_dir, el) for el in os.listdir(mh_dir) if re.search(mh_timerange, el) is not None][0] #should only match ONE
		mh_files.append(mh_file)
		dmget_string = dmget_string + "dmget " + mh_file + " \n"
	dmget_string = dmget_string + "# Get MF data" + "\n"
	mf_files = []
	for pred_var in predlist:
		mf_dir = osp.join(MF_dir, pred_var) + idirsuffix
		mf_file = [osp.join(mf_dir,el) for el in os.listdir(mf_dir) if re.search(mf_timerange, el) is not None][0]
		mf_files.append(mf_file)
		dmget_string = dmget_string + "dmget " + mf_file + " \n"
	tmask_files = []
	leapmask_files = []
	if tmask_present:
		tmask_files = [OH_tmask, MH_tmask, MF_tmask]
		dmget_string = dmget_string + "# Get OH tmask " + "\n"
		dmget_string = dmget_string + "dmget " + OH_tmask + "\n" 
		dmget_string = dmget_string + "# Get MH tmask " + "\n"
		dmget_string = dmget_string + "dmget " + MH_tmask + "\n" 
		dmget_string = dmget_string + "# Get MF tmask " + "\n"
		dmget_string = dmget_string + "dmget " + MF_tmask + "\n" 
		if trim_time_mask_present:
			tmask_files.append(trim_tmask)
			dmget_string = dmget_string + "# Get MF overlap tmask " + "\n"	
			dmget_string = dmget_string + "dmget " + trim_tmask + "\n" 
		if copy_leapmasks:	
			#Need a historic and a future leapday mask file
			leapmask_base = "/archive/esd//PROJECTS/DOWNSCALING/MASKS/timemasks/maskdays_missingleapdays_"
			#Unfortunately, oh_timerange and mf_timerange can have '_ensmbl' tacked onto the end.
			#That is never going to be present in the leapday masks.
			#...I don't have to worry about _NOLEAP speficications at this point, 
			#but that might be something to keep in mind for the future.
			oh_leap_trange = re.sub("_ensmbl", "", oh_timerange)
			mf_leap_trange = re.sub("_ensmbl", "", mf_timerange)
			hist_leapmask = leapmask_base + oh_leap_trange + ".nc"
			fut_leapmask  = leapmask_base + mf_leap_trange + ".nc"
			leapmask_files = [hist_leapmask, fut_leapmask]
			dmget_string = dmget_string + "# Get leapday masks" + "\n"
			dmget_string = dmget_string + "dmget " + hist_leapmask + "\n"
			dmget_string = dmget_string + "dmget " + fut_leapmask + "\n"
	#And finally, add the files that the aux file specifies
	print(aux_file_list)
	if len(aux_file_list) > 0:
		aux_strlist = ["#Copy files from aux specification"]
		for el in aux_file_list:
			aux_strlist.append("dmget " + el)
		dmget_string = "\n".join(aux_strlist)			
	#do all of your gcps next
	gcp_string = "# Copy all input files to $TMPDIR (-cd option creates input directories) " + "\n"
	file_list = leapmask_files + spatmask_files + tmask_files + oh_files + mh_files + mf_files + aux_file_list
	for f in file_list:
		gcp_string = gcp_string + "mkdir -p $TMPDIR/" + os.path.dirname(f) + " \n"
		gcp_string = gcp_string + "gcp --sync --verbose " + f + " $TMPDIR/" + f + " \n"
	gcp_string = gcp_string + "echo 'Copy operations complete.' \n"	
	gcp_string = gcp_string + "sleep 10 \n"
		
	#Add code for creating common output directories 
	#(copy opts for each minifile/slice take place within runscript
	#as well as writing of script status files)
	#Current string in form of blank_outdir#(blank_rip)#rip1,rip2,rip3	
	outdir_opts = outdir_string.split("#")
	#The parenthesies made sense in c-shell, but pythons' sub functions are PICKY
	outrip_patt = "\$\(RIP\)"
	out_rips = outdir_opts[2].split(",")
	outdirs = []
	for rip in out_rips:
		outdirs.append(re.sub(outrip_patt, rip, outdir_opts[0]))
	outdir_string = create_outdir_string(outdirs, scriptdir, target_var)
	
	#Finally, start opts that need to loop by index available
	runmode = ( stat.S_IROTH | stat.S_IRGRP |stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR )	
	#print(j_elements)
	n_tiles = 0
	for j in range(0, len(j_elements)-1):
		j_string = "J"+str(j_elements[j]) + "-" + str(j_elements[j+1]-1)
		#j_max and j_by are passed in already; they are numeric	
		for i in range(0, len(i_elements)-1):
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
			###THIS NEEDS TRANLSATION WORK
			#print("components for R code:")
			#print([(i + 1 + i*(i_by-1)), i_max, i_by, i_offset, (j+1 + j*(j_by-1)), j_max, j_by, j_offset])
			runscript_code = create_slice_runscript(i_string, j_string, scriptdir, \
													(i+1 + i*(i_by-1)), i_max, i_by, i_offset, \
													(j+1 + j*(j_by-1)), j_max, j_by, j_offset, auxfile, target_var,outdirs)
			with open(runscript_name, 'w') as f:
				f.write(runscript_code)
			os.chmod(runscript_name, runmode)	#f now executable	
			runscript_files.append(runscript_name)
			if i %njobs == (njobs-1) or i == (len(i_elements) - 2):
				#Prepare to write all accululated data to file
				#print("writing master to file")
				runscript_calls = [el + " & \n" for el in runscript_files]
				runscript_calls = "sleep 10 \n".join(runscript_calls)
				with open(master_scriptname, 'w') as f: 
					f.write(moab_header)
					f.write(module_loads)
					f.write(dmget_string)
					f.write(gcp_string)
					f.write(outdir_string)
					f.write("echo 'Beginning calls to runscripts'\n")
					f.write(runscript_calls) 
					f.write("wait")			
	print('master runscript and mini creation completed')
	#return(n_tiles)	
			
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
							j_idx, j_max, j_by, j_offset, auxfile, target_var, outdirs):
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
	r_command = "Rscript " + runcode + " " + " ".join(str(el) for el in [i_idx, i_max, i_by, i_offset, j_idx, j_max, j_by, j_offset, auxfile, 'TRUE'])+ " >>& " + logfile + " &"
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
	outstr.append('echo gcp "$TMPDIR/' + outdir + "*."+i_string+"_"+j_string + ".nc " + outdir + '"')	
	outstr.append("gcp $TMPDIR/" + outdir + "*."+i_string+"_"+j_string + ".nc " + outdir)
	outstr.append("set ostat = $?")
	outstr.append("if ( $ostat == 0 ) then")
	outstr.append("\t touch " + outdir + "/.mini.success")
	outstr.append("else")
	outstr.append("\t touch " + outdir + "/.mini.fail")
	outstr.append('endif')
	#Now do the same for the qc mask file
	out_mask_dir = outdir + "/" + target_var + "_qcmask/"
	outstr.append('echo "gcp $TMPDIR/' + out_mask_dir + "*."+i_string+"_"+j_string + ".nc " + out_mask_dir + '"')
	outstr.append("gcp $TMPDIR/" + out_mask_dir + "*."+i_string+"_"+j_string + ".nc " + out_mask_dir)
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
	runscript_patterns = [re.split("\.", el)[-1] for el in runscript_files]
	print(runscript_patterns)
	runcode_split = re.split("\.", runcode_files[0])
	print(runcode_split)
	for patt in runscript_patterns:
		tmp_file = runcode_split
		tmp_file[-1] = patt
		tmp_dest = osp.join(runcode_dir, ".".join(tmp_file))
		tmp_src = osp.join(runcode_dir, runcode_files[0])
		shutil.copy2(tmp_src, tmp_dest)	
	
                       
