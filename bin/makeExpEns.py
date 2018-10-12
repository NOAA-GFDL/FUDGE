#import expergen 
import fudgeList
import fudgeUtils
from fudgeUtils import gcp,tstamp
from fudgeUtils import read_exp_info_file
import os,sys,subprocess,shutil,time,copy
from os import path as osp
from subprocess import PIPE
import argparse
import shlex
import smtplib
import re
import optparse
from optparse import OptionParser
from email.MIMEMultipart import MIMEMultipart
from email.MIMEBase import MIMEBase
from email.MIMEText import MIMEText
from email.Utils import COMMASPACE, formatdate
from email import Encoders
import netCDF4
from netCDF4 import Dataset

def makeExpEns():
	#Input parsing and arg checking for main body of code
	usage = "usage: %prog [-p project -e exp_name] [-i input] [--anom_only_ens] [--verbose]"
	parser = OptionParser(usage=usage, version='makeExpEns 1.0')
	parser.add_option("-p", "--project", dest="project", default="NA", help="Project used for the experiment (i.e. PM, RR2). Will be used to reconstruct an ESD-standard path to the scripts directory.")
	parser.add_option("-e", "--exp_name", dest="exp_name", default='NA', help="Experiment name used for the experiment (PMeCRtnp1-CDFtmod-A04r1e3X01K00). Will be used to reconstruct an ESD-standard path to the scripts directory.")
	parser.add_option("-i", "--input", dest="input", default="NA", help="Path to an experiment_info file for the experment. If no experiment_info.txt file is provided, the project (-p) and exp_name (-e) arguments will be used to derive the scripts directory.")
	parser.add_option("--anom_only_ens", action="store_true", dest="anom_only_ens", default=False, help="If present and ensembles of downscaled anomaly data are being made, will not make ensembles of downscaled anom+clim data - only the downscaled anomaly data. Cannot be specified with --reconstruct_only_ens.")
	parser.add_option("--reconstruct_only_ens", action="store_true", dest='rec_only_ens', default=False, help="If present and ensembles of downscaled anomaly are being made, will only make ensembles of downscaled anom + clim data - not the downcsaled anomaly data. Cannot be specified with --anom_only_ens.")
	parser.add_option("-v", "--verbose", action="store_true", dest="verbose", default=False, help="Print extra status messages used to help debugging. Defaults to False.")
	parser.add_option("--sev", dest="sev", default=1, help="Level of status messages provided. Defaults to 1 and goes to 3; higher numbers mean more messages.")
	(options, args) = parser.parse_args()
	if([options.input,options.project,options.exp_name].count("NA") == 3):
		print("Error in makeExpEns: one of the options -i -p -e should be non-missing!")
		sys.exit(1)
	if(options.anom_only_ens and options.rec_only_ens):
		print("Error in makeExpEns: only one of --anom_only_ens and --reconstruct_only_ens should be specficied!")
	if (options.input!="NA"):
		if not osp.exists(options.input):
			print("Error in makeExpEns: file " + options.input + " not found!")
			sys.exit(1)
		expdict = read_exp_info_file(options.input)
	elif (options.project != "NA" and options.exp_name != "NA"):
		user = os.environ.get('USER')
		if (user!='esd'):
			print("Warning in makeExpEns: -p and -e args assume an ESD-standard directory structure.")
		infile = osp.join("/home/", user, "PROJECTS/DOWNSCALING/SUBPROJECTS/", options.project, options.exp_name, "experiment_info.txt")
		print(infile)
		if not osp.exists(infile):
			print("Error in makeExpEns: file " + infile + " not found! Please re-examine directory structure or use -i to point to a file directly.")
			sys.exit(1)
		expdict = read_exp_info_file(infile)
	else:
		print("Error in makeExpEns: either --input or --project and --exp_name must be provided!")
		sys.exit(1)
	verbose = options.verbose
	sev = options.sev
	runMakeExpEns(expdict, options.anom_only_ens, options.rec_only_ens, verbose, sev)

def runMakeExpEns(expdict, anom_only_ens=False, rec_only_ens=False, verbose=False, sev=1):
	###main script
	#Performs steps over all ensembles that count as part of the
	#standardization process (such as collecting into ensemble files for the experiment)
	#obtain environment variables
	basedir = os.environ.get('BASEDIR')
	tdir = os.environ.get('TMPDIR')
	gfdl_platform = os.environ.get('gfdl_platform')
	if(gfdl_platform=='desktop'):
		print("Error in makeExpEns: this script should not be run from a desktop! Please run on PP/AN.")
		sys.exit(1)
	#Determine output rip based on number of output directories
	rips = [p.split("/")[len(p.split("/"))-6] for p in expdict['output.path']]
	epochs = [p.split("/")[len(p.split("/"))-10] for p in expdict['output.path']]
	trange_string = expdict['fut.train.start.year_1'] +"-" + expdict['fut.train.end.year_1']
	rip_string = ",".join(rips)
	if(len(rips) == 1):
		print("Error in makeExpEns: More than one rip needed to make an ensemble! Please choose an ensemble experiment and try again.")
		sys.exit(1)
	elif( len(list(set(rips))) != len(rips)):
		print("Error in makeExpEns: One or more of the rips in " + rip_string +" are identical!")
		sys.exit(1)
	if(epochs.count(epochs[0]) != len(epochs)):
		print("Error in makeExpEns: epochs" +  ",".join(epochs) +  "in output dirs should be identical!")
		sys.exit(1)
	else:
		epoch = epochs[0]
	climvar = expdict['target']
	exp_name = expdict["ds.experiment"]
	isAnom=False
	isTrend=False
	if ("Anom" in climvar):
		print("This var may be an anomaly var")
		if(anom_only_ens):
			print("however, since anom_only_ens is set, the anom + clim data will not be searched for")
		else:
			isAnom=True
	if ("Detr" in climvar):
		print("This var may be a detrended var")
		if(anom_only_ens):
			print("however, since anom_only_ens is set, the trend and detrended data will not be searched for")
		else:
			isTrend=True
	rip_suffix = rips[1][-4:]
	out_rip = "r1to" + str(len(expdict['output.path'])) + rip_suffix
	#Create ensembles of the data from this experiment:
	outpaths = [osp.dirname(p) for p in expdict['output.path']]
	print outpaths
	all_outfiles =  [[f for f in os.listdir(p) if osp.isfile(osp.join(p, f))] for p in outpaths]
	all_outfiles = [sorted(lst) for lst in all_outfiles]
	num_files = [len(flist) for flist in all_outfiles]
	if (num_files.count(num_files[0]) != len(num_files)):
		print("Error in makeExpEns: number of output files in directories " + ",".join(outpaths) + "should be identical! Please re-run pp jobs.")
		sys.exit(1)
	elif (num_files[0]==0):
		print("Error in makeExpEns: no files found in output directories "+ ",".join(outpaths) + "! Please re-run pp jobs.")
	all_outfiles = zip(*all_outfiles)
	#Each f in all_outfiles should either contain _qcmask or not contain _qcmask
	for lst in all_outfiles:
		if re.search("_qcmask", lst[0]) is not None:
			if any(re.search("_qcmask", el) is None for el in lst):
				print("Error in makeExpEns: tried to concatenate incompatible files:")
				print(lst)
				sys.exit(100)
		else:
			if any(re.search("_qcmask", el) is not None for el in lst):
				print("Error in makeExpEns: tried to concatenate incompatible files:")
				print(lst)
				sys.exit(100)
	print("all_outfiles:")
	print(all_outfiles)
	ens_list = []
	for flist in all_outfiles:
		flist = [osp.join(p,f) for p,f in zip(outpaths,flist)]
		outfile = call_ens_creation(flist,climvar,rips,rip_string,out_rip,\
				exp_name,epoch,trange_string,isAnom,isTrend,rec_only_ens,verbose, sev)
		ens_list = ens_list + outfile
	#When everything is over, add the list of output ensemble files to the experiment_info.txt files
	#in all locations.
	if(len([e for e in ens_list if e is not None]) == 0):
		print("No ensemble output produced; exiting")
		sys.exit(0)
	print("Ensemble files " + ",".join(ens_list) + " written to file")
	sbase_loc = [osp.join(expdict['script.base'], "experiment_info.txt")]
	outpath_loc = [osp.join(("/").join(p.split("/")[0:len(p.split("/"))-3]),"experiment_info.txt") for p in expdict['output.path']]
	outens_loc = [osp.join(("/").join(p.split("/")[0:len(p.split("/"))-3]),"experiment_info.txt") for p in ens_list]
	outens_loc = list(set(outens_loc))
	outpaths_all = sbase_loc + outpath_loc + outens_loc
	tmpfile = osp.join(tdir, "experiment_info.txt")
	ens_list = ens_namemap(ens_list)
	#ens_list_ii = [n,ens for e in range(0, len(en_list)) if not None]
	#append_str = "\n" +"#"*80 + '\n' + 'ensemble.files:' + "\n".join(ens_list)
	append_str = "\n####" + " Downscaled ensemble output " +"#"*47 +"\n"+"\n".join(ens_list)+"\n"
	#fcmd = "python "+basedir+"/bin/fudgeList.py -f -i "+expdict['xml.path']+" -o "+tmpfile
	#f = subprocess.Popen(fcmd, stdout=subprocess.PIPE, shell=True)
	#out, err = f.communicate()
	shutil.copy2(sbase_loc[0], tmpfile)
	with open(tmpfile, 'a') as f:
		f.write(append_str)
	for out in outpaths_all:
		print("Copying " + tmpfile + " " + out)
		gcp(tmpfile, out, False, False, sev > 1) #verbose, verbose
	print("Modified experiment.info files copied to all locations:")
	print("\n".join(outpaths_all))
	return("\n\t".join(ens_list))

def call_ens_creation(flist,climvar,rips,rip_string,out_rip,exp_name,epoch,
						trange_string,isAnom=False, isTrend=False,recOnly=False,verbose=False, sev=1):
	#Given a list of files and some handy metadata information,
	#calls a simple anomaly addition script to generate the
	#flist = [osp.join(p,f) for p,f in zip(outpaths,flist)]
	basedir = os.environ.get('BASEDIR')
	tdir = os.environ.get('TMPDIR')
	tdir_ens = osp.join(tdir, "proc_ens")
	if not osp.exists(tdir_ens):
		os.makedirs(tdir_ens)
	for f in flist:
		if not osp.isfile(f):
			print("Error in call_ens_creation! File " + f + " does not exist!")
			sys.exit(1)
	#inFiles = ",".join([gcp(f, "TMPDIR", verbose, verbose) for f in flist])
	inFiles = ",".join([gcp(f, osp.join(tdir_ens,osp.basename(f)), True, sev > 1, sev > 1) for f in flist])
	#orig_inFiles = ",".join(flist)
	orig_inFiles = ",".join([osp.basename(p) for p in flist])
	print("Calling ensemble creation on " + inFiles)
	with Dataset(flist[0], 'r') as nc_obj:
		varnames = nc_obj.variables.keys()
	print("varnames in file:")
	print(varnames)
	print("climvar:")
	print(climvar)
	vname = [varnames[v] for v in range(0, len(varnames)) if re.search(climvar,varnames[v]) is not None][0]
	if isAnom:
		print("Checking for presence of anomaly var")
		#supp_strings = ["qcmask","save"]
		#if supp_strings[0] not in vname and supp_strings[1] not in vname:
		if not recOnly:
			outfile_anom = call_ens_creation(flist,climvar,rips,rip_string,out_rip,exp_name,epoch,trange_string,False)
		else:
			outfile_anom = []
		print("Target var " + vname + ' is an anomaly var; searching for anom + clim files')
		anom_files = [re.sub("Anom","",f) for f in flist]
		climvar = re.sub("Anom", "", climvar)
		outfile_clim = call_ens_creation(anom_files,climvar,rips,rip_string,out_rip,exp_name,epoch,trange_string,False)
		return(outfile_clim + outfile_anom)
	if isTrend:
		print("Checking for presence of detrended var")
		if not recOnly:
			outfile_anom = call_ens_creation(flist,climvar,rips,rip_string,out_rip,exp_name,epoch,trange_string,False)
		else:
			outfile_anom = []
		print("Target var " + vname + ' is a detrended var; searching for trend + resid files')
		print(climvar)
		print(vname)
		#check on climvar/vname distinction
		trend_method = climvar.split("Detr")[-1]
		orig_var = re.sub("Detr"+trend_method, "", vname)
		orig_clim = re.sub("Detr"+trend_method, "", climvar)
		anom_files = [re.sub(climvar,orig_clim,f) for f in flist]
		print(orig_var)
		print(orig_clim)
		print(anom_files)
		outfile_clim = call_ens_creation(anom_files,orig_var,rips,rip_string,out_rip,exp_name,epoch,trange_string,False)
		return(outfile_clim + outfile_anom)
	#Generate output file by doing a series of find-and-replace regexes on outdir
	current_rip = [rips[r] for r in range(0,len(rips)) if re.search(rips[r],flist[0]) is not None][0]
	outfile = re.sub(current_rip, out_rip, flist[0])
	if (re.search('_ensmbl.nc', outfile) is None): 
		outfile = re.sub("\.nc$", "_ensmbl.nc", outfile)
	print("Output file location: " + outfile)
	if not osp.exists(osp.dirname(outfile)):
		print("Making " + osp.dirname(outfile))
		os.makedirs(osp.dirname(outfile))
	#tmp_outfile = osp.join(tdir, outfile.lstrip("/"))
	tmp_outfile = osp.join(tdir_ens,osp.basename(outfile)+tstamp())
	#Finally, call the ensemble generation code:
	ens_min = basedir + "utils/s7/ens_min.csh"
	ens_command = " ".join(["tcsh -f ",ens_min, tmp_outfile, vname, rip_string, exp_name, epoch, trange_string, orig_inFiles, inFiles, 'Exp'])
	#Dev edit - trying to debug the labeling process
	p = subprocess.Popen(ens_command, stdout=subprocess.PIPE, shell=True, env=None, cwd=tdir)
	out0,err0 = p.communicate()
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
		gcp(tmp_outfile, outfile, False, verbose, sev > 1)
		return([outfile])

def ens_namemap(ens_list):
	#Sorts the esemble files into the order in which they should be written
	#to the experiment_info.txt file, since dicts are unordered.
	ens_loc = copy.copy(ens_list)
	ens_names = ['qc.ensemble','save.ensemble', 'ds.ensemble']
	anom_ens_names = ['qc.anom.ensemble', 'save.anom.ensemble','ds.anom.ensemble']
	trend_ens_names = ['qc.detr.ensemble', 'save.detr.ensemble', 'ds.detr.ensemble']
	ens_search = ['Anom_qcmask','Anom_save','Anom','Detr.*_qcmask','Detr.*_save','Detr','qcmask', 'save', '']
	all_ens_names = anom_ens_names + trend_ens_names + ens_names
	ens_total_order = [all_ens_names[x] for x in [8, 6, 7, 5, 3, 4, 2, 0, 1]]
	#ens_order = [ens_names[x] for x in [3, 2, 0, 1]]
	ens_indexed = []
	for e in range(0,len(ens_search)):
		print(str(e))
		match_indices = [x for x in range(0,len(ens_loc)) if re.search(ens_search[e], osp.basename(ens_loc[x])) is not None]
		matchlen = len(match_indices)
		#print([ens_loc[el] for el in match_indices])
		#print(str(ens_loc[match_indices[0]]))
		if matchlen == 1:
			if re.search("Anom", ens_loc[match_indices[0]]) is not None:
				appendstr = all_ens_names[e] + ":" + ens_loc[match_indices[0]]
				ens_indexed.append(appendstr)
			elif re.search("Detr", ens_loc[match_indices[0]]) is not None:
				appendstr = all_ens_names[e] + ":" + ens_loc[match_indices[0]]
				ens_indexed.append(appendstr)
			else:
				appendstr = all_ens_names[e] + ":" + ens_loc[match_indices[0]]
				ens_indexed.append(appendstr)
			del ens_loc[match_indices[0]]
			print(ens_indexed)
		elif matchlen != 0:
			print("Error in ens_namemap: match in " + str(matchlen)+" locations in " + ",".join(ens_list))
			sys.exit(1)
	out = sorted(ens_indexed, key=lambda x: [e for e in range(0,len(ens_total_order)) if re.search(ens_total_order[e],x) is not None][0])
	return(out)


if __name__ == '__main__':
	print(sys.argv)
	makeExpEns()
