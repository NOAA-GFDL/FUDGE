def runMakeCVEns(exp1, exp2, scriptdir1, scriptdir2, verbose=False):
	#Makes the two cross-validated ensemble files for two historical
	#experiments in the current setup. 
	for p, exp in [scriptdir1, scriptdir2],[exp1,exp2]:
		fname = exp + ".lock"
		open(osp.join(p, "postProc/cvStatus/",fname), 'a').close()	
	exp1,exp2 = sort(exp1,exp2)
	scriptdir1,scriptdir2 = sort(scriptdir1,scriptdir2)
	expdict1 = read_exp_info_file(osp.join(scriptdir1,"experiment_info.txt"))
	expdict2 = read_exp_info_file(osp.join(scriptdir1,"experiment_info.txt"))	
	ds_exp1 = [("/").join(p.split("/")[0:len(p.split("/"))-1]) for p in expdict1['output.path']] #trim off OneD
	ds_exp2 = [("/").join(p.split("/")[0:len(p.split("/"))-1]) for p in expdict2['output.path']]
	dsvar = expdict1['target']
	#Do a brief check for anomaly vars in the input files	
	#There are two distinct classes of file; create each in turn
	setdict = {'DSDEPh': {'odesc': 'dp', 'dataID':'DSDEPh', 'kfold':'K00', 
	                      'inDir1': ds_exp1[0], 'inDir2': ds_exp2[1], 
	                      'inPath1': osp.join(), 'inPath2' : osp.join()}, 
	           'DSCVh': {'odesc': 'cv', 'dataID':'DSCVh', 'kfold':'K02', 
	                     'inDir1': ds_exp1[1], 'inDir2': ds_exp2[0], 
	                     'inPath1': osp.join(), 'inPath2' : osp.join()}
	}
	for kty in ['DSDEPh', 'DSCVh']:
		kdict = setdict[kty]
			##
		for outpaths in [ds_exp1,ds_exp2]:
			all_outfiles =  [[f for f in os.listdir(p) if osp.isfile(osp.join(p, f))] for p in outpaths]
			num_files = [len(flist) for flist in all_outfiles]
			if (num_files.count(num_files[0]) != len(num_files)):
				print("Error in makeExpEns: number of output files in directories " + ",".join(outpaths) + "should be identical! Please re-run pp jobs.")
				sys.exit(1)
			elif (num_files[0]==0):
				print("Error in makeExpEns: no files found in output directories "+ ",".join(outpaths) + "! Please re-run pp jobs.")		
			all_outfiles = zip(*all_outfiles)
			#Add entries to dictionary, after sorting:
			
			ens_list = []
			for flist in all_outfiles:
				flist = [osp.join(p,f) for p,f in zip(outpaths,flist)]
				outfile = call_ens_creation(flist,climvar,rips,rip_string,out_rip,\
				exp_name,epoch,trange_string,isAnom,rec_only_ens,verbose)
			ens_list = ens_list + outfile
	##
		try:
			call_cv_ens_creation(flist, sdvar, [f1r,f2r], ",".join([f1r,f2r]), kty, \
				exp_name, epoch, trange,string,isAnom=False,recOnly=False,verbose=False)
		except:
			for p, exp in [scriptdir1, scriptdir2],[exp1,exp2]:
				prevname = exp + ".lock"
				os.remove(osp.join(p, "postProc/cvStatus/",prevname), 'a')	
				fname = exp + "_" + kty + "_" +".failure"	
				open(osp.join(p, "postProc/cvStatus/",fname), 'a').close()
				raise	
				return(1)
		else:
			for p, exp in [scriptdir1, scriptdir2],[exp1,exp2]:
				prevname = exp + ".lock"
				os.remove(osp.join(p, "postProc/cvStatus/",prevname), 'a')	
				fname = exp + "_" + kty + "_" + ".success"	
				open(osp.join(p, "postProc/cvStatus/",fname), 'a').close()	
	
def call_cv_ens_creation(flist,climvar,rips,rip_string,out_rip,exp_name,epoch,
						trange_string,isAnom=False, recOnly=False,verbose=False):	
	#Given a list of files and some handy metadata information, 
	#calls a simple anomaly addition script to generate the 
	#flist = [osp.join(p,f) for p,f in zip(outpaths,flist)]
	basedir = os.environ.get('BASEDIR')
	tdir = os.environ.get('TMPDIR')
	tdir_ens = osp.join(tdir, "proc_cv_ens")
	if not osp.exists(tdir_ens):
		os.makedirs(tdir_ens)
	for f in flist:
		if not osp.isfile(f):
			print("Error in call_cv_ens_creation! File " + f + " does not exist!")
	inFiles = ",".join([gcp(f, osp.join(tdir_ens,osp.basename(f)), True, verbose, verbose) for f in flist])
	orig_inFiles = ",".join(flist)
	print("Calling", cv_type, "ensemble creation on " + inFiles)
	with Dataset(flist[0], 'r') as nc_obj:
		varnames = nc_obj.variables.keys()
	vname = [varnames[v] for v in range(0, len(varnames)) if re.search(climvar,varnames[v]) is not None][0]
	if isAnom:
		print("Checking for presence of anomaly var")
		supp_strings = ["qcmask","save"]
		if supp_strings[0] not in vname and supp_strings[1] not in vname:
			if not recOnly:
				outfile_anom = call_ens_creation(flist,climvar,rips,rip_string,out_rip,exp_name,epoch,trange_string,False)
			else:
				outfile_anom = []	
			print("Target var " + vname + ' is an anomaly var; searching for anom + clim files')
			anom_files = [re.sub("Anom","",f) for f in flist]
			climvar = re.sub("Anom", "", climvar)
			try:
				outfile_clim = call_cv_ens_creation(anom_files,climvar,rips,rip_string,out_rip,exp_name,epoch,trange_string,False)
			except:
				raise	
			return(outfile_clim + outfile_anom)
	#Generate output file by doing a series of find-and-replace regexes on outdir
	current_rip = [rips[r] for r in range(0,len(rips)) if re.search(rips[r],flist[0]) is not None][0]	
	outfile = re.sub(current_rip, out_rip, flist[0])
	outfile = re.sub("\.nc$", "_ensmbl.nc", outfile)
	print("Output file location: " + outfile)
	if not osp.exists(osp.dirname(outfile)):
		print("Making " + osp.dirname(outfile))
		os.makedirs(osp.dirname(outfile))
	tmp_outfile = osp.join(tdir_ens,osp.basename(outfile)+tstamp())			
	#Finally, call the ensemble generation code:	
	ens_min = basedir + "utils/s7/ens_min.csh"	
	ens_command = " ".join(["tcsh -f ",ens_min, tmp_outfile, vname, rip_string, exp_name, epoch, trange_string, orig_inFiles, inFiles])
	try:
		p = subprocess.Popen(ens_command, stdout=subprocess.PIPE, shell=True, env=None, cwd=tdir)
		out0,err0 = p.communicate()
	except:
	else:	
		if (p.returncode != 0):
			print(str(out0))
			print(str(err0))
			print("Error in processing ensemble " +  inFiles + "; call to c-shell failed.")
			print("Command that failed: " + ens_command)
			sys.exit(1)
			return(None)
		else:
			if verbose:
				print(str(out0))
				print(str(err0))
			gcp(tmp_outfile, outfile, verbose, verbose)
			return([outfile])
			
def make_cv_ens_name(orig_path, orig_fname, datatype, odesc, kfold):
	#Makes a cross-validated ensemble outpath/filename given a non-cv
	#component:
	#Note that this assumes the US48 directory is the last dir in the pathname
	ds_rip = ""
	orig_expname = ""
	cv_expname = re.sub("A00r*e2", "A00"+odesc+"e2", orig_expname)
	cv_expname = re.sub("K00", kfold, cv_expname)
	cv_expname = re.sub(ds_rip, datatype, cv_expname)
	#
	cv_path = re.sub(orig_expname, cv_expname, orig_path)
	cv_fname = re.sub(orig_expname, cv_expname, orig_fname)
	#From this point on, make the changes to the new things instead
	cv_path = re.sub(ds_rip, datatype, cv_path)
	#Finally, add _ensmbl.nc
	cv_fname = re.sub(".nc", "_ensmbl.nc", cv_fname)
	if not (osp.exists(cv_path)):
		os.makedirs(cv_path)
	return(osp.join(cv_path, cv_fname))	
	
				
		
def main():
	sys.exit([0])		
