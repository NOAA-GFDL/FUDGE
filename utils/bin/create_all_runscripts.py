import re
import os, stat
from os import path as osp

def create_all_runscripts(i_start, i_end, njobs, ncpu, fudgetag, scriptdir, OH_dir, MH_dir, MF_dir, oh_timerange, mh_timerange, mf_timerange, predlist, spatmask, OH_tmask, MH_tmask, MF_tmask, trim_tmask, outdir_string, target_var, ds_method, j_range, idirsuffix, 
OH_by, MH_by, MF_by, copy_leapmasks):
	#Creates master and single-slice miniifles for a FUDGE job
	#if --msub is included, also submits jobs at the end of everything
	mini_start = range(int(i_start), int(i_end), njobs)
	mini_end = [el + njobs for el in mini_start]
	mini_end = [el if el <= (int(i_end)+1) else (int(i_end)+1) for el in mini_end]
	i_offset = int(i_start) - 1
	i_max = int(i_end) - i_offset
	#correct j_range
	j_range = re.sub('"', "", j_range)
	#One of the goals for this was to get away from using gcp-specific operations
	moab_header = "\n".join(["#!/bin/tcsh ", 
	                        "#PBS -S /bin/tcsh ", 
	                        "#PBS -l nodes=1:ppn="+str(ncpu)+",walltime=12:00:00", 
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
	for f in oh_files:
		dmget_string = dmget_string + "dmget " + f + "\n"
	#MH and MF files have an assumption that you can have multiple 
	#predictor vars present. At some point, var classification checks
	#(such is is_spaceless for ENSO data) should go in here	.
	#MH and MF paths are currently passed in several dirlevels earlier than OH 
	dmget_string = dmget_string + "# Get MH data" + "\n"
	mh_files = []
	for pred_var in predlist:
		mh_dir = osp.join(MH_dir, pred_var) + idirsuffix
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
		else:
			leapmask_files = []
	#do all of your gcps next
	gcp_string = "# Copy all input files to $TMPDIR (-cd option creates input directories) " + "\n"
	file_list = leapmask_files + spatmask_files + tmask_files + oh_files + mh_files + mf_files
	for f in file_list:
		gcp_string = gcp_string + "mkdir -p $TMPDIR/" + os.path.dirname(f) + " \n"
		gcp_string = gcp_string + "gcp --sync " + f + " $TMPDIR/" + f + " \n"
		
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
	for s,e in zip(mini_start,mini_end):
		master_scriptname = osp.join(scriptdir, 'master', 'master_script_'+str(s)+'_'+str(e))
		#then, for each slice, first create your runscript
		#and then add the runscript call to the
		runscript_files = []
		runmode = ( stat.S_IROTH | stat.S_IRGRP |stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR )
		for i in range(s,e):
			runscript_name = osp.join(scriptdir, 'runscript', "script."+target_var+"."+ds_method+".I"+str(i)+"_"+j_range)
			runscript_code = create_slice_runscript(i, j_range, scriptdir, OH_by, MH_by, MF_by, i-i_offset, i_max, target_var,outdirs)
			with open(runscript_name, 'w') as f:
				f.write(runscript_code)
			os.chmod(runscript_name, runmode)	#f now executable
			runscript_files.append(runscript_name)
		#Calls are $scriptname & \n sleep -s 30 \n for each $scriptname	
		runscript_calls = [el + " & \n" for el in runscript_files]
		#tinkering wiht sleep parameter to see what weems to work best (tried: 30,)
		runscript_calls = "sleep 20 \n".join(runscript_calls)		
		with open(master_scriptname, 'w') as f: 
			f.write(moab_header)
			f.write(module_loads)
			f.write(dmget_string)
			f.write(gcp_string)
			f.write(outdir_string)
			f.write(runscript_calls) 
			f.write("wait")
	print('master runscript and mini creation completed')		
			
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
	
def create_slice_runscript(i, j_range, scriptdir, OH_by, MH_by, MF_by, i_idx, i_max, target_var, outdirs):
	#Creates the runscript code specific to a slice of the grid downscaled 
	#by the R code
	#Technically only the shebang is needed, but extra info in the event that we ever split
	#these differently is not going to hurt.
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
	runcode = osp.join(scriptdir, 'runcode') + "/code." + target_var + "." + exp_name + ".I"+ str(i) + "_" +j_range
	logfile = osp.join(scriptdir, 'log') + "/out."+exp_name+".I" + str(i) + "_" +j_range
	r_command = "Rscript " + runcode + " " + " ".join([str(el) for el in [OH_by, MH_by, MF_by, i_idx, i_max]]) + " >>& " + logfile #+ " &"
	runscript = runscript_header + r_command + "\n" + "wait \n"
	for outdir in outdirs:
		runscript = runscript + create_runscript_copy_ops(outdir, str(i), j_range, target_var)
	return(runscript)	
		
def create_runscript_copy_ops(outdir, ival, j_range, target_var):
	#Makes the copy operations for the output of the R code
	#Currently assumes a single file of _qcmask output; 
	#when we get to writing coordinates, this needs to be updated
	outstr = []
	outstr.append("gcp $TMPDIR/" + outdir + "*.I"+ival+"_"+j_range + ".nc " + outdir)
	outstr.append("set ostat = $?")
	outstr.append("if ( $ostat == 0 ) then")
	outstr.append("\t touch " + outdir + "/.mini.success")
	outstr.append("else")
	outstr.append("\t touch " + outdir + "/.mini.fail")
	outstr.append('endif')
	#Now do the same for the qc mask file
	out_mask_dir = outdir + "/" + target_var + "_qcmask"
	outstr.append("gcp $TMPDIR/" + out_mask_dir + "*.I"+ival+"_"+j_range + ".nc " + out_mask_dir)
	outstr.append("set ostat = $?")
	outstr.append("if ( $ostat == 0 ) then")
	outstr.append("\t touch " + out_mask_dir + "/.mini.success")
	outstr.append("else")
	outstr.append("\t touch " + out_mask_dir + "/.mini.fail")
	outstr.append('endif')
	return("\n".join(outstr) + "\n")
	
                       
