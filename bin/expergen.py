#!/usr/local/python/2.7.1/bin/python
"""expergen.py: Experiment scripts generator driver for FUDGE."""
__author__ = "A.Radhakrishnan"

import xmlhandler
import naming
import pprint,datetime,getopt, os, shutil
import sys, subprocess, glob
import fudgeList
from subprocess import PIPE
from os import path as osp
import re
import optparse
from optparse import OptionParser
import cleanup_script
from cleanup_script import clean_prior_experiments
import get_npts_by
from get_npts_by import get_npts_by
import create_all_runscripts
from create_all_runscripts import create_all_runscripts
from create_all_runscripts import awful_runcode_hack
from get_comment_specs import get_comment_specs, get_exp_check_tag
import get_ds_info
from get_ds_info import get_ds_info, parse_aux_info, make_aux_input
import make_postProc_scripts
from make_postProc_scripts import make_postProc_scripts
from collections import OrderedDict
import watcher
from watcher import runWatcher #,_v_print
import fudgeUtils
from fudgeUtils import read_exp_info_file

list_title = []
dictDS_hi = {}
listh = []
listlos =[]
listl = []
dictDS_lo = {}
list  = []
dictDIR_hi = {}
dictDIR_lo = {}
dictProperty = {}
copyprop = {}
rootdir = ''
projectRoot = ""
project = ""
ppn = 2 #TODO get from XML custom?
overwrite = False #default don't overwrite existing output or scripts
preexist_glob = "erase" #if overwriting old, default is erasing, not archiving
idirsuffix = ""
global mutli
def checkTags(dictParams,key):
		if(dictParams.has_key(key)):
	       	 	val = dictParams[key]
		else:
			if(key == 'oroot'):
				print "output root tag is being deprecated from darkchocolate."
				sys.exit("error")

			elif(key == 'sroot'):
				print "Script root default value is being deprecated from darkchocolate. elease specify <script_root> in XML and try again "
				sys.exit("error")
			elif(key == 'outdir'):
				val = 'na'
			elif(key == 'params'):
				val = 'na'
			elif(key == 'pr_opts'):
				val = 'na'
			elif(key == 'experiment'):
				val = 'na'
			elif(key == 'fut_time_trim_mask'):
				val = 'na'
			elif(key == 'qc_mask'):
				val = 'off'
			elif(key == 'qc_varname'):
				val = 'na'
			elif(key == 'qc_type'):
				val = 'na'
			elif(key == 'qcparams'):
				val = 'na'
			elif(key == 'adjust_out'):
				val = 'na'
			elif(key == 'masklists'):
				val = 'na'
			elif(key == 'target_time_window'):
				val = 'na'
			elif(key == 'hist_time_window'):
				val = 'na'
			elif(key == 'fut_time_window'):
				val = 'na'
			elif(key == 'spat_mask'):
				val = 'na'
			elif(key == 'maskvar'):
				val = 'na'
			elif(key == 'future_predictor_suffix'):
				val = ''
			elif(key == 'historical_predictor_suffix'):
				val = ''
			elif(key == 'target_suffix'):
				val = ''
			elif(key == 'property'):
				val = {}
			else:
				print "Error: Missing value for ",key
				sys.exit(1)
		return val

def parse_outdir(out_dir,property):
	#print "Parsing out_dir to check if it references property tags"
	list_subs = []
	listout=[]
	outdirsplit = out_dir.split('/')
	replace = ""
	expect_property = 0
	pname = 'na'
	for outdirs in outdirsplit:
		if '$(' in outdirs:
			#print "outdirs as defined expects a property tag.."
			expect_prop = 1
			for pname,pval in property.iteritems():
				pname = '$('+pname+')'
				if pname == outdirs:
					#print "we may have located the property tag"
					replace = pval
					break;
	if(replace != ""):
	 if (out_dir  != ""):
	     list_new_out_dir = replace.split(",")
	     for lnew in list_new_out_dir:
		listout.append(out_dir.replace(pname,lnew))
	#print "test",pname,new_out_dir,listout
	if(replace == "") & (expect_property == 1):
		replace = "na"
		print "\033[1;41mWARNING Your outdir specification references a variable. But reference property tag is UNDEFINED \033[1;41m"
	if (replace == "") & (expect_property == 0):
		replace ="na"
 	if not bool(property):
	 list_subs = out_dir+"#na#na"
	else:
	 list_subs = out_dir+"#"+pname+"#"+replace # for use in shell arr
	return list_subs,listout


def getFacets(listDatasetID,var,dim,label="label",output_grid="na"):
	ctr = 0
	dictDS = {}
	listDS = []
	if(output_grid == "na"):
		print "ERROR, output_grid not accepted as na"
		sys.exit(1)
	for itemj in listDatasetID:
		#print "counter .......",ctr
		ds_title = label+str(ctr)
		list_title.append(ds_title)
		dshi = itemj
		hi = dshi.split('.')
		hi_proj =hi[0]
		hi_product = hi[1]
		if(hi[2] != 'NOAA-GFDL'):
		 ind = 2
		 hi_instit = ''
		else:
		 hi_instit = hi[2]
		 ind = 3
		hi_model = hi[ind]
		hi_exp = hi[ind+1]
		hi_freq = hi[ind+2]
		hi_realm = hi[ind+3]
		hi_mip = hi[ind+4]
		hi_rip = hi[ind+5]
		hi_ver = hi[ind+6]
		hi_expname = hi_exp+"_"+hi_rip
		hi_expname = hi_expname
#		dictDS[ds_title] = [hi_proj, hi_product,hi_instit,hi_model,hi_exp,hi_mip,hi_realm,hi_rip,hi_ver,hi_expname]
		hiresdir = "/"+hi_proj+"/"+hi_product+"/"+hi_instit+"/"+hi_model+"/"+hi_exp+"/"+hi_freq+"/"+hi_realm+"/"+hi_mip+"/"+hi_rip+"/"+hi_ver+"/"+var+"/"+output_grid+"/"+dim+"/"
		dictDS['proj']=hi_proj
		dictDS['product']=hi_product
		dictDS['instit']=hi_instit
		dictDS['model']=hi_model
		dictDS['exp']=hi_exp
		dictDS['freq']=hi_freq
		dictDS['mip']=hi_mip
		dictDS['realm']=hi_realm
		dictDS['rip']=hi_rip
		dictDS['ver']=hi_ver
		dictDS['expname']=hi_expname
		listDS.append(hiresdir)
		ctr = ctr + 1
	#########################################
	return dictDS,listDS

def getDir(listDir,label):
#GCM_DATA.CMIP5.BOGUS.amip.day.atmos.anomSbx7C360LR.r1i1p1.v1
	i=0
	dictDIR = {}
	for getdir in listDir:
		#cprint getdir
		dir_lo = label+str(i)
		dictDIR[dir_lo] = rootdir+"/"+getdir
		i = i + 1
		dire = dictDIR[dir_lo].strip()
	return dire


def listVars(uinput,basedir=None,msub=False,pp=False):
	print "PP option is set to ",pp
	if os.path.exists(uinput):
		handler = xmlhandler.XMLHandler(  )
		dictParams = handler.getParams(uinput)
		try:
			dversion = dictParams['dversion']
		except:
			dversion='' #dversion deprecated
	#print "----Downscaling XML template-", uinput
      #  print "----Force Override existing output Flag-", force
	###################################
	print "Input from XML"
	print "########################"
	for name,val in dictParams.iteritems():
		print name,"=",val
	###### get dictParams #################################
	predictor =  checkTags(dictParams,'predictor_list')
	target = checkTags(dictParams,'target')
	target_ID = checkTags(dictParams,'target_ID')
	target_id = checkTags(dictParams,'target_id')
	target_train_start_time = checkTags(dictParams,'target_train_start_time')
	target_train_end_time = checkTags(dictParams,'target_train_end_time')
	target_file_start_time = checkTags(dictParams,'target_file_start_time')
	target_file_end_time = checkTags(dictParams,'target_file_end_time')
	target_time_window = checkTags(dictParams,'target_time_window')
	hist_id = checkTags(dictParams,'hist_id')
	hist_train_start_time = checkTags(dictParams,'hist_train_start_time')
	hist_train_end_time = checkTags(dictParams,'hist_train_end_time')
	hist_file_start_time = checkTags(dictParams,'hist_file_start_time')
	hist_file_end_time = checkTags(dictParams,'hist_file_end_time')
	hist_time_window = checkTags(dictParams,'hist_time_window')
	fut_id = checkTags(dictParams,'fut_id')
	fut_train_start_time = checkTags(dictParams,'fut_train_start_time')
	fut_train_end_time = checkTags(dictParams,'fut_train_end_time')
	fut_file_start_time = checkTags(dictParams,'fut_file_start_time')
	fut_file_end_time = checkTags(dictParams,'fut_file_end_time')
	fut_time_window = checkTags(dictParams,'fut_time_window')
 	fut_time_trim_mask = checkTags(dictParams,'fut_time_trim_mask')
	######## use auxcustom ############
	auxcustom = fut_time_trim_mask
	###########################################
	dim = checkTags(dictParams,'dim')
	output_grid = checkTags(dictParams,'output_grid')
	###CEW edit
	#print "Initiate checkTags"
	region = checkTags(dictParams,'maskvar')
	spat_mask = checkTags(dictParams,'spat_mask')
	spat_mask_ID = checkTags(dictParams,'spat_mask_ID')
	ds_region = spat_mask_ID
	file_j_range = checkTags(dictParams,'file_j_range')
	lats = checkTags(dictParams,'lats')
	late = checkTags(dictParams,'late')
	lons = checkTags(dictParams,'lons')
	lone = checkTags(dictParams,'lone')
	kfold = checkTags(dictParams,'kfold')
	method = checkTags(dictParams,'method')
	if(basedir is None):#env variable has precedence over XML basedir
		basedir = checkTags(dictParams,'basedir')
	experiment = checkTags(dictParams,'experiment') #downscaling experiment configuration
	######## construct experiment config name ###########
	if(experiment == 'na'): #if not present in XML
		proj = checkTags(dictParams,'project')
		project_ID = checkTags(dictParams,'project_ID')
		series = checkTags(dictParams,'series') #experiment series
 		#experiment,dsold_region = naming.constructExpname(proj,target,series,method,kfold,basedir)
		experiment,dsold_region = naming.constructExpname(project_ID,target_ID,series,method,kfold,basedir,dumb="yes")
	#####################################################
	params = checkTags(dictParams,'params')
	#### pr_opts #######
	pr_opts = checkTags(dictParams,'pr_opts')
	#### get file name suffix #####
	hist_suffix = checkTags(dictParams,'historical_predictor_suffix')
	target_suffix = checkTags(dictParams,'target_suffix')
	fut_suffix = checkTags(dictParams,'future_predictor_suffix')
	##### end filename suffix ######
	# removing the following precipitation check. Checks in XMLGen should be good enough. And this condition may not apply any more. Espresso release
	# if(target != 'pr'):
	#	if(pr_opts != 'na'):
	#		print "ERROR: You specified pr_opts for a target field that's not precipitation_flux. Please check and try again"
	#		sys.exit()
	print "pr_opts:---------------- ",pr_opts
	#### pr_opts #########
	auxcustom1 = "na"  #$auxcustom1 now not used
	if (params != 'na'):
		splitparams = params.split(';')
		#TODO support multiple auxfiles in custom
		for par in splitparams:
	    		getpar = par.split('=')
	    		val = getpar[1]
	    		if (val.startswith( "'/archive" )):
	    			#print "file to be gcpied to vftmp: ",val
				auxcustom1 = val
			#print "auxcustom1:",auxcustom1
	#projectRoot = checkTags(dictParams,'oroot')
	global rootdir
	rootdir = checkTags(dictParams,'in_root')
	outdir = checkTags(dictParams,'outdir')
	out_dir = checkTags(dictParams,'out_dir')
	out_dir = out_dir.strip()

	property = checkTags(dictParams,'property')
#c	if not bool(property):
#c	    out_dir = out_dir+"#na#na"
#c	    listoutdir = out_dir
#c	else:
#c	   print "or not"
	out_dir,listoutdir =  parse_outdir(out_dir,property)
	#print "new out_dir", out_dir
	###CEW edits - debugging single-ens I35 case
	global copyprop
	if len(property) == 0:
		#print("splitting filename to determine rip")
		property = out_dir.split("/")[-7]
		copyprop = property
	else:
		copyprop = property['RIP']
		#copyprop = property.iteritems()
#	print(property)
#	print(out_dir, listoutdir)
#	print(copyprop)
#	print(property)
#	sys.exit()
	if (len(listoutdir) > 1):
	   print "Multi-output ON"
	   global multi
	   multi = "on"
	else:
	   print "single output on"
	   global multi
	   multi = "off"
	##script -and- log prefix section ##
	sroot = checkTags(dictParams,'sroot')
	sbase = sroot+"/scripts/"+project_ID+"/"+experiment+"/"
	## pp section ##
	qc_mask = checkTags(dictParams,'qc_mask')
	#print "qc_mask....... ",qc_mask
	adjust_out = checkTags(dictParams,'adjust_out')
	#adjust_out = "na"
	qc_varname = checkTags(dictParams,'qc_varname')
	if(qc_varname is None):
		qc_varname = target+"_qcmask"
	qc_type = checkTags(dictParams,'qc_type')
	### get qc options ##
	qc_params = checkTags(dictParams,'qcparams')
	#new way of getting masklist
	masklists = checkTags(dictParams,'masklists')
	preexist = checkTags(dictParams,'preexist')
	#TODO add preexist as another return variable
	#print "preexist"+preexist
	if(preexist == 'exit'):
		force = False
	elif(preexist == 'erase'):
		force = True
	elif(preexist == 'move'):
		force = True #new move feature in clobber - support
	else:
		print "\033[1;41mERROR code -2: Please provide a valid value {'exit','erase','move'} for ifpreexist tag in XML \033[1;m",preexist
		sys.exit(-2)
	if(output_grid == 'station'):
		if(latjstart == latjend):
			dsuffix = "J"+str(latjstart)
			region = output_grid
		else:
			print "Please specify a single station. "
			sys.exit()
	else:
	   if(file_j_range != ''):
	   	dsuffix=file_j_range
		dim1 = dim
	#comment out RR a1r	dim = output_grid+"/"+dim1
	   else:
	   	sys.exit( "Please specify region information and file_j_range and try again. Quitting now \n")
	#print(dim1)
	############ target get dir info ########################
	dict_target,listt = getFacets(target_id,target,dim,'target_id',output_grid)
	#print dict_target
	############ historical predictor ######################
	dict_hist,listh = getFacets(hist_id,"","",'hist_id',"") #make it base
	#print dict_hist
	########### future predictor information ##############
	dict_fut,listf = getFacets(fut_id,"","",'fut_id',"") #make it basedir for support 2x vars
	#print dict_fut
	###########  esdgen information ####################################

	####################################################################
	target_dir = getDir(listt,'hidir')
	target_dir = os.path.abspath(target_dir)
	#print "target dir: ",target_dir
	hist_pred_dir = getDir(listh,'lodir')
	hist_pred_dir = os.path.abspath(hist_pred_dir)
	#print "historical predictor dir",hist_pred_dir
	fut_pred_dir = getDir(listf,'futdir')
	fut_pred_dir = os.path.abspath(fut_pred_dir)
	#print "future predictor dir",fut_pred_dir
	global idirsuffix
	idirsuffix = "/"+output_grid+"/" #+dim+"/" CEW edit 10-2016: removing OneD in path specs
	#print "idirsuffix",idirsuffix
	#### get outdir #####
	if(out_dir != 'na'):
	    category = 'downscaled'
	    instit="NOAA-GFDL"
	    predModel = dict_fut['model']
	    freq = dict_fut['freq']
	    realm = dict_fut['realm']
	    mip = dict_fut['mip']
	    pversion = dict_fut['ver']
	    dmodel = method
	    dexper = dict_fut['expname']
	    ens = dict_fut['rip']
	    predictand = target
	    #dversion from command line args. default value is v20120422
	    if (out_dir == 'na'):
#Commenting the following since xmlGen now constructs outpath and is already set in xml
	    	outdir = getOutputPath(projectRoot,category,instit,predModel,dexper,freq,realm,mip,ens,pversion,experiment,predictand,ds_region,dim1,"")
	    else:
		outdir = out_dir
	    #experiment in the above is expconfig that's constructed
	    #print "Output directory is :",outdir
	    #print "Script directory is:",sbase
#new
	    if (pp == False):
		    	for lon in range(int(lons),int(lone)):
				exists = checkExisting(outdir,lon,dsuffix,predictand,freq,dmodel,experiment,dexper,ens,fut_train_start_time,fut_train_end_time,'mini',ds_region)
			 	if (exists == True) & (force == False):
					print "\033[1;41mERROR code -7: Output already exists. Use <ifpreexist>erase</ifpreexist>  to override existing output. Quitting now..\033[1;m",'\n',outdir
					sys.exit(-7)
			 	if (exists == True) & (force == True):
					print '\033[1;41mCAUTION Output already exists. -f is turned on. Any existing output will be overwritten.\033[1;m'
			       # 	print "CAUTION Output already exists. -f is turned on. Any existing output will be overwritten."
					global overwrite
					overwrite = True
					break
	    if(force == True):
		overwrite = True
		if(preexist == 'move'):
			global preexist_glob
			preexist_glob = 'move'
			#print "preexist turned ON................"
#	    return targetdir1,hist_pred_dir1,fut_pred_dir1
	    target_scenario = dict_target['exp']+"_"+dict_target['rip']
	    target_model = dict_target['model']
	    #CEW edit for testing purposes mip to freq
	    target_freq = dict_target['freq']
	    target_ver = dict_target['ver']
	    hist_scenario = dict_hist['exp']+"_"+dict_hist['rip']
	    hist_freq = dict_hist['freq'] #CEW edit to test
	    hist_model = dict_hist['model']
	    fut_scenario = dict_fut['exp']+"_"+dict_fut['rip']
	    fut_model = dict_fut['model']
	    fut_freq = dict_fut['freq'] #CEW edit mip to freq
	    tstamp = str(datetime.datetime.now().date())+"."+str(datetime.datetime.now().time())
	    ### add those suffix in

	    hist_file_end_time = hist_file_end_time+hist_suffix
	    fut_file_end_time = fut_file_end_time+fut_suffix
	    target_file_end_time = target_file_end_time+target_suffix
	    return output_grid,kfold,lone,region,fut_train_start_time,fut_train_end_time,file_j_range,hist_file_start_time,hist_file_end_time,hist_train_start_time,hist_train_end_time,lats,late,lons,late, basedir,method,target_file_start_time,target_file_end_time,target_train_start_time,target_train_end_time,spat_mask,fut_file_start_time,fut_file_end_time,predictor,target,params,outdir,dversion,dexper,target_scenario,target_model,target_freq,hist_scenario,hist_model,hist_freq,fut_scenario,fut_model,fut_freq,hist_pred_dir,fut_pred_dir,target_dir,experiment,target_time_window,hist_time_window,fut_time_window,tstamp,ds_region,target_ver,auxcustom,qc_mask,qc_varname,qc_type,adjust_out,sbase,pr_opts,masklists,idirsuffix
############end of listVars######################
def main():
    #################### args parsing ############
	help = "#################Usage:(run from AN nodes)##################\n dsTemplater -i <input XML> \n "
	help = help + "Example 1: expergen -i dstemplate.xml \n "
	help = help + "Example 2: expergen -i examples/dstemplate.60lo0FUTURE.xml \n"
	#print usage
	basedir = os.environ.get('BASEDIR')
	if(basedir is None):
	   print "Warning: BASEDIR environment variable not set"
	##### get version BRANCH info ######################
	branch = os.environ.get('BRANCH')
	fudgetag = os.environ.get('FUDGETAG')
	env_file = os.environ.get('FUDGE_ENV')
	if branch is None and fudgetag is None:
		print("Error in expergen: One of BRANCH or FUDGETAG must be set! Please load a module or check out a tag.")
		exit(1)
	else:
		if branch is None:
			branch = "NA"
		if fudgetag is None:
			fudgetag = "NA"
	   #print "Warning: BRANCH env variable not set. BRANCH will be set to undefined"
	   #branch = "undefined"
	####################################################
	parser = OptionParser(usage=help)
	parser.add_option("-i", "--file", dest="uinput",
	help="pass location of XML template", metavar="FILE")
	parser.add_option("-a", "--aux", default="none",dest="auxinput", help="pass location of auxilary JSON input for stencil, mask, subset and other specificaions. If not set in XML or by command line, a default aux will be created.")
	parser.add_option("--msub", "--msub",action="store_true",default=False, help="Automatically submit the master runscripts using msub.Default is set to FALSE")
	parser.add_option("-m", "--mail",action="store", dest="mail",default="none", help="Pass user email for notifications from tools like postProc (this is optional)")
	parser.add_option("--msubmail", action='store_true', dest='msubmail', default=False, help="Should email notifications be sent from the master scripts sent to the PP system?")
	parser.add_option("--statmail", action='store_true', dest='statmail', default=False, help="Should email notifications upon step completion be sent from the FUDGE workflow?")
	parser.add_option('--run-watcher', action='store_true', dest='run_watcher', default=False,
	                   help="Whether to run the watcher script directly after expergen completes. Not expected for use in production, but useful for development and running synthetic data experiments.")

	(options, args) = parser.parse_args()
	#CEW edits 3-3 for clarity (and only haing to search env vars once)
	if(options.mail=="none"):
		try:
			NEMSemail1 = os.environ["NEMSemail"]
		except KeyError:
   				print "-m option unused and NEMSemail environmental var not set. If you want email notifications, enter them manually."
   				NEMSemail1 = 'none'
	else:
		NEMSemail1 = options.mail
	print "Status emails will be sent to: " + NEMSemail1
	msub = options.msub
	print "Will jobs be automatically submitted? " + str(msub)
	if (options.run_watcher and not options.msub):
		print("Error in expergen: option run-watcher should only be used with msub!")
		sys.exit(1)
	if(os.path.exists(options.uinput)):
		uinput = options.uinput
		print "XML input: " + uinput
	else:
		print "XML input " + options.uinput + "not found!"
		print "Please pass a valid input XML filename with the -i argument and try again. See -h for syntax. Quitting now.."
		sys.exit()
	#1-19-17: aux input
	#5-1-17: default aux input
	if(os.path.exists(options.auxinput)):
		auxinput = os.path.abspath(options.auxinput)
		search_xml_aux = False
		print "JSON input: " + auxinput
	else:
		print "JSON input " + options.auxinput + "not found!"
		if (options.auxinput == "none"):
			print("Aux file will be searched for in XML (or a default will be created)")
			search_xml_aux = True
		else:
			print "Please pass a valid input JSON filename with the -i argument and try again. See -h for syntax. Quitting now.."
			sys.exit()

	#Similar edits for the

	#CEW edits 6-30 for msub mail verbosity settings
	msubmail = 0
	if options.msubmail:
		msubmail = 1
	statmail = options.statmail


#########if platform is not PAN, disable expergen at this time 11/14/2014 ###############
	system,node,release,version,machine = os.uname()
	if(node.startswith('pp')):
		print "Running on PP(PAN) node", node
	if(node.startswith('an')):
		print "Running on AN(PAN) node", node
	else:
		 print "\033[1;41mERROR code -5: Running on a workstation not tested for end-to-end runs yet. Please login to analysis nodes to run expergen. \033[1;m",node
		 sys.exit(-5)

#########################################################################################
	#########  call listVars() #############################################################
	output_grid,kfold,lone,region,fut_train_start_time,fut_train_end_time,file_j_range,hist_file_start_time,hist_file_end_time,hist_train_start_time,hist_train_end_time,lats,late,lons,late, basedir,method,target_file_start_time,target_file_end_time,target_train_start_time,target_train_end_time,spat_mask,fut_file_start_time,fut_file_end_time,predictor,target,params,outdir,dversion,dexper,target_scenario,target_model,target_freq,hist_scenario,hist_model,hist_freq,fut_scenario,fut_model,fut_freq,hist_pred_dir,fut_pred_dir,target_dir,expconfig,target_time_window,hist_time_window,fut_time_window,tstamp,ds_region,target_ver,auxcustom,qc_mask,qc_varname,qc_type,adjust_out,sbase,pr_opts,masklists,idirsuffix= listVars(uinput,basedir,msub)

		######### call script creators..  #######################################################
	############################### 1 ###############################################################
	#  make.code.tmax.sh 1 748 756 /vftmp/Aparna.Radhakrishnan/pid15769 outdir 1979 2008 tasmax

	# If the runscript directory already exists, please quit
	#/home/a1r/gitlab/cew/fudge2014///scripts/tasmax/35txa-CDFt-A00X01K02
	#scriptdir = [sbase+"/master",sbase+"/runcode",sbase+"/runscript"]
	#print(scriptdir)
	sd = sbase
	#for sd in scriptdir:
	if(os.path.exists(sd)):
		if os.listdir(sd):
			if (overwrite == False):
				print '\033[1;41mERROR: Scripts Directory already exists. Clean up using cleanup_script and try again please -or- (Use <ifpreexist>erase</ifpreexist> if output/scripts already exist and you want to override this and let expergen run the cleanup_script for you; Use <ifpreexist>move</ifpreexist> to move existing output)\033[1;m',sd
				print "\033[1;41mERROR code -6: script directory already exists.Check --\033[1;m",scriptdir
				sys.exit(-6)
			if (overwrite == True):
				print "\033[1;43mWarning: Scripts Directory already exists. But, since <ifpreexist> has different settings, cleanup utility will handle this\033[1;m "
				print "Now invoking cleanup utility..........."
				clean_prior_experiments(uinput, basedir, preexist_glob)
	else:
		print "scriptdir "+sd+" does not exist. Looks like a clean slate "

	##Create the scripts subdirectories
	subdirs = ['master', 'runscript', 'runcode', 'log','postProc', 'config', 'status', 'profile']
	for el in subdirs:
		subdir = osp.join(sbase,el)
		if not osp.exists(subdir):
			os.makedirs(subdir)

	##CEW edit 10-2016 for the multi-pt ds params
		#CEW edit 10-2016: check for the presence of the 3 'by' tags in the XML; if
	#they are not present, set to 1. These get used in the runcode creation
	#for setting command-line parameters.
	#by_params = get_npts_by(uinput)
	#mh_by = str(by_params[0]) #OH
	#oh_by = str(by_params[1]) #MH
	#mf_by = str(by_params[2])#MF
		### As of 10-13, get the ncpus and njobs specifications from the XML
	specdict = get_comment_specs(uinput)
	njobs = int(specdict['njobs'])
	ncpus = int(specdict['ncpus'])
	#And as of 1-19-17, get the aux file specifications from the options
	#You don't need to parse any of these, but you do need to get a list of the
	#copy-able files (and possibly a wallclock time parameter)
	#If no aux was specified as --aux
	if search_xml_aux:
		if specdict['aux'] != "none":
			auxinput = specdict['aux']
			if(os.path.exists(auxinput)):
				auxinput = os.path.abspath(auxinput)
			else:
				print("Error in expergen: aux input from xml, "+ auxinput + " does not exist on this filesystem!")
				print("Please use --aux input for other aux or remove from xml.")
				sys.exit(1)
		else:
			print('making default aux file')
			auxinput = make_aux_input(output_grid, sbase, target)
	#auxinfo is selective reads, auxdict is entire, messy thing
	auxinfo,auxdict = parse_aux_info(auxinput)
	#If aux file is not default, aux file overrides XML parameters
	if auxinfo['njobs'] != 8:
		njobs = auxinfo['njobs']
	if auxinfo['ncpus'] != 2:
		ncpus = auxinfo['ncpus']



	#############################################################
	print "Step 0: Create the unified runcscripts and master runscripts"
	#First check to see whether the leapday masks need to be copied...
	ds_info = get_ds_info(method, basedir)
	#copy_leapmasks = ds_info['copy_leapmasks']

	#As of 3-15-17, this needs a check for the 'all' string; turn into coords.
	i_subset = auxinfo['subsetdict']['i_subset']
	j_subset = auxinfo['subsetdict']['j_subset']
	if i_subset == 'all':
		i_subset = (int(lone)-int(lons)) + 1
	else:
		i_subset = int(i_subset)
	if j_subset == 'all':
		j_subset = (int(late)-int(lats)) + 1
	else:
		j_subset = int(j_subset)
	#print(copy_leapmasks)
	#print('examine runscript output')
	#sys.exit()
	#Then, make the runscripts
	oh_timerange = str(target_file_start_time) + "-" + str(target_file_end_time)
	mh_timerange = str(hist_file_start_time) + "-" + str(hist_file_end_time)
	mf_timerange = str(fut_file_start_time) + "-" + str(fut_file_end_time)
	predlist = predictor.split(",")
	print("Calling create_all_runscripts")
	idict = {'US48' : 748,
	         'SCCSC0p1'  : 181,
	         'RR'   : 181,
	         'grid0': 0,
	         'NEUSA': 1, 
	         'NEUSA0p1': 425}
	jdict = {'US48' : 454,
	         'SCCSC0p1'  : 31,
	         'RR'   : 31,
	         'grid0': 0,
	         'NEUSA': 1, 
	         'NEUSA0p1': 127}	         
	try:
		i_offset = idict[output_grid]
	except KeyError:
		i_offset = 1
	try:
		j_offset = jdict[output_grid]
	except KeyError:
		j_offset = 1
	#2018-06-06: Adding check between the experiment check and expconfig
	#for purposes of checking kfold cross-valdiation
	exp_check = get_exp_check_tag(uinput)
	if exp_check is not None:
		if (exp_check != expconfig):
			print("Error in expergen: constructed exp_config " + expconfig + " and xml exp_check tag " + exp_check + " did not match!")
			sys.exit(1)
	#Add the commands to create the components for the DS entry in the aux file
	ds_minifile = get_minifile_name(target, fut_freq, expconfig, fut_scenario,
	                                "<RIP>",
	                                ds_region, fut_file_start_time, fut_file_end_time)
	create_all_runscripts(lons, lone, i_subset, i_offset, \
						  lats, late, j_subset, j_offset, \
						  njobs, ncpus, fudgetag, sbase, \
	                      target_dir, hist_pred_dir, fut_pred_dir,\
	                      oh_timerange, mh_timerange, mf_timerange, predlist, \
	                      spat_mask, target_time_window, hist_time_window, \
	                      fut_time_window, auxcustom, outdir, target, method, \
	                      file_j_range, idirsuffix, \
	                      ds_info['multivar_target'],
	                      auxinput, auxinfo['copy_files'], auxinfo['wallclocktime'],
	                      auxdict, ds_minifile)
	#print('examine runscript output')
	#sys.exit()

	#############################################################
	#sys.exit("examine outcome of script")
	#print  "hist_freq"+hist_freq
	##Adding copy of environment source script to expergen 6-8
	#for f in glob.glob(os.path.dirname(env_file)+"\environment_info.*"):
	#	shutil.copy2(f, sd)
	script1Loc = basedir+"/utils/bin/create_runcode"
	make_code_cmd = script1Loc+" "+str(predictor)+" "+str(target)+" "+str(output_grid)+" "+str(spat_mask)+" "+str(region)
	make_code_cmd = make_code_cmd+" "+str(file_j_range)+" "+str(lons)+" "+str(lats)+" "+str(late)
	make_code_cmd = make_code_cmd+" "+str(hist_file_start_time)+" "+str(hist_file_end_time)+" "+str(hist_train_start_time)+" "+str(hist_train_end_time)+" "+str(hist_scenario)+" "+str(hist_model)+" "+hist_freq+" "+str(hist_pred_dir)+" "+str(hist_time_window)
	make_code_cmd = make_code_cmd+" "+str(fut_file_start_time)+" "+str(fut_file_end_time)+" "+str(fut_train_start_time)+" "+str(fut_train_end_time)+" "+str(fut_scenario)+" "+str(fut_model)+" "+fut_freq+" "+str(fut_pred_dir)+" "+str(fut_time_window)
	make_code_cmd = make_code_cmd+" "+str(target_file_start_time)+" "+str(target_file_end_time)+" "+str(target_train_start_time)+" "+str(target_train_end_time)+" "+str(target_scenario)+" "+str(target_model)+" "+target_freq+" "+str(target_dir)+" "+str(target_time_window)
	make_code_cmd = make_code_cmd+" "+str(method)+" "+str(expconfig)+" "+str(kfold)+" '"+str(outdir)+"' "+str(tstamp)+" "+'na'+" "+basedir+" "+str(lone)

	print "Step 1: R code starters generation ..in progress"
	#p = subprocess.Popen('tcsh -c "'+make_code_cmd+'"',shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)

	params_new =  '"'+str(params)+'\"'
	params_pr_opts = '"'+str(pr_opts)+'\"'
	make_code_cmd = make_code_cmd +" "+params_new+" "+"'"+str(ds_region)+"'"
	make_code_cmd = make_code_cmd+" "+str(auxcustom)+" "+str(qc_mask)+" "+str(qc_varname)+" "+str(qc_type)+" "+str(adjust_out)+" "+str(sbase)+" "+str(params_pr_opts)+" "+str(branch)+" "+'"'+str(masklists)+'"'+" "+str(idirsuffix)
	#print(make_code_cmd)
	#p = subprocess.Popen(make_code_cmd +" "+params_new,shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
	#cprint make_code_cmd
	p = subprocess.Popen(make_code_cmd,shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
	output, errors = p.communicate()
	if(p.returncode != 0):
	 print "Step1:!!!! FAILED !!!!, please contact developer."
	 print output, errors
	 sys.exit(0)
	#print output, errors
	#cprint "----Log-----"
	#print output,errors
	###############################################################################################
	print "1- completed\n"
	print("Copying runcodes to modified locations from general runscript mods")
	awful_runcode_hack(osp.join(sbase, "runcode"), osp.join(sbase, "runscript"))
	#print "debug............msub turned ",msub
	############################### 2 ################################################################
	##################################### 4 ############################################
	# copy xml to configs dir
	cdir = sbase+"/config/"
#	try:
#	  os.mkdir(cdir)
#	except:
#	  print "Unable to create dir. Dir may exist already", cdir
	shutil.copy2(uinput, cdir)
	shutil.copy2(auxinput, cdir)
	#print "Config XML and aux file saved in ",cdir
	#print "RunScripts will be saved under:",sbase
	##Copy environment config file
	#print("Copying environment info to "+sd+"environment_info.csh")
	shutil.copy2(env_file, sd+ "/environment_info.csh")
	################ step 6 fudgeList invocation ##############################################
	slogloc = sbase+"/"+"experiment_info.txt"
	fcmd = "python "+basedir+"/bin/fudgeList.py -f -i "+uinput+" -o "+slogloc
	f = subprocess.Popen(fcmd, stdout=subprocess.PIPE, shell=True)
	out, err = f.communicate()
	#print "Summary Log File: ", slogloc

	auxloc = sbase + "/config/" + osp.basename(auxinput)
	shutil.copy2(auxinput, auxloc)
	#print("Aux file location: " + auxloc)

	with open(slogloc, 'a') as f:
		f.write("\naux:" + os.path.abspath(auxinput))
		f.write("\nNEMSemail:" + NEMSemail1)
	#sys.exit(1)

	# Create emergency backstop directory for capturing status files when
	# permissions are an issue
#	statdir = sbase + "/status"
#	try:
#		os.mkdir(statdir)
#	except:
#		print("Directory " + statdir + " may already exist")

############### crte ppscript #################
	print "Step 4: --------------PP postProc SCRIPT GENERATION-----------------------"
	#print " ========================="
	dev = "off"
	msubwrapper = []
	#CEW edit 6-30 for better status file monitoring when queue is full
	msubwrapper.append("rm -f " + sbase + "postProc/ppStatus/*")
	print "multi output (Writing to multiple output directories) ....",multi
	#if cmd line -m is passsed use that, otherwise use following
	#CEW edit 3-3

	'''
	if (NEMSemail1 == "none"):
		  print "debug bde................", NEMSemail1
		  try:
			NEMSemail1 = os.environ["NEMSemail"]
			print "Notification email set to .......", NEMSemail1
		  except KeyError:
			NEMSemail1 = "none"
	 '''
	mail_command = ""
	msub_mail_command = ""
	if (NEMSemail1!='none'):
	 if msubmail==1:
	 	mail_command = "-m " + NEMSemail1 + " "
	 	msub_mail_command = "-m ae -M "+ NEMSemail1 + " "

	#print(sbase+"/postProc/aux/")
	if (multi == "on"):
	 suffix = ['suff1','suff2','suff3']
	 listrips = []
	else:
	 suffix = ['']
	 listrips = ['na']
	 listrips = [copyprop]

	if not os.path.exists(sbase+"/postProc/aux/"):
		os.makedirs(sbase+"/postProc/aux/")
	#CEW edit 5-2-16 for writing postproc status files
	if not os.path.exists(sbase+"/postProc/ppStatus/"):
		os.makedirs(sbase+"/postProc/ppStatus/")
	#CEW edit 5-4-16 for writing cross-validation status files
	if not os.path.exists(sbase+"/postProc/cvStatus/"):
		os.makedirs(sbase+"/postProc/cvStatus/")
	#CEW edit 6-30-16 for the statmail settings
	if not os.path.exists(sbase+"/postProc/verb/"):
		os.makedirs(sbase+"/postProc/verb/")
	if statmail:
		open(sbase+"/postProc/verb/verbose.true", 'a').close()
	else:
		open(sbase+"/postProc/verb/verbose.false", 'a').close()
	#CEW edits 3-21-2017:
	#Making new tool for generating postProc scripts
	print("Making postproc scripts")
	listrips = copyprop.split(",")
	#print("script-making command:")
	make_postProc_scripts(listrips, sbase, fudgetag, NEMSemail1, auxinfo['pp_wallclocktime'])
		#Note: may need to include commands on the nemsemail as well
		#os.chmod(msubwrap.name, 0744)
		#msub -m ae -M <email> script
	print "----------------------------------------"
	print "\033[1;42m You can also use this msubwrapper script that launches all the post-processing(postProc) jobs for you in case of multi-ensemble output. If it's a single output file, you may still use this wrapper script \033[1;m", sbase + "/postProc/msubwrapper"
	print "NOTE: postProc will succeed only if you're running the model for the full downscaled region. (it will fail if you're running downscaling for a single slice for example)"
################ step 5 job submission (if applicable) ##############################################
	print "Starting step 5: job submission"
	if msubmail == 1 and NEMSemail1 != 'none':
			msub_string = "msub -d $HOME -m ae -M " + NEMSemail1 + " "
	else:
		msub_string = "msub -d $HOME "
	master_scripts = [osp.join(sbase,'master/',el) for el in os.listdir(osp.join(sbase, 'master')) if re.search('master_script_', el) is not None]
	msub_cmd = "module load moab; "
	for el in master_scripts:
		#msub_cmd can be saved for later
		msub_cmd = msub_cmd + msub_string + el + "; "
	if msub:
		p5 = subprocess.Popen('tcsh -c "'+msub_cmd+'"',shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
		output5, errors5 = p5.communicate()
		#######
		if(p5.returncode != 0):
			print "Step5:!!!! FAILED !!!!, please contact developer."
			print output5, errors5
			sys.exit(0)
		print output5, errors5
		print "5 - completed"
	print "-----------"
	if not msub:
		print "--msub not set on command line. To submit jobs at a later point, please use the following:"
		print "\033[1;95m"+msub_cmd+"\033[1;m"
	print "Upon submission of downscaling batch jobs, please use the following watcher script command to watch for DS output from this and submit the postProc PP jobs automatically \n -- Use watcher interactively from AN nodes and load fudge module while doing so to set the environment. \n -- Use watcher -h for a complete list of command-line options including verbosity   "

	if(NEMSemail1 != "none"):
		print "\033[1;42mwatcher -i "+(str)(os.path.abspath(slogloc))+" -m "+NEMSemail1+"\033[1;m"
	else:
		print "\033[1;42mwatcher -i "+(str)(os.path.abspath(slogloc))+"\033[1;m"
	
	#Script exit options - needs to exit with 0
	if options.run_watcher:
		print("Invoking watcher script immediately")
		expinfo = sbase + "/experiment_info.txt"
		expdict = read_exp_info_file(expinfo)
		#Note: check on this when mailid is not set. Is the scrip smart enough to parse it?
		runWatcher(expinfo,0,1,120,180,NEMSemail1,1, expdict)
	else:
		sys.exit(0)


####################################################################################
def getOutputPath(projectRoot,category,instit,predModel,dexper,freq,realm,mip,ens,pversion,dmodel,predictand,ds_region,dim,dversion):
    ##Sample:
    #${PROJECTROOT}/downscaled/NOAA-GFDL/GFDL-HIRAM-C360-COARSENED/amip/day/atmos/day/r1i1p1/v20110601/GFDL-ARRMv1A13X01/tasmax/OneD/v20130626/tasmax_day_GFDL-ARRMv1A13X01_amip_r1i1p1_US48_GFDL-HIRAM-C360-COARSENED_19790101-20081231.XXXX.nc

    print "No outdir specified. Deducing the standardized output directory is deprecated. out_dir is required from darkchocolate."
    if(projectRoot == 'na'):
	print "Output root directory not specified. Default project root specifiction is deprecated from darkchocolate. Use out_dir to specify absolute path "
    dexperonly = dexper.split("_")[0]
    if(freq == ""):
	freq = mip
    stdoutdir = projectRoot+"/"+category+"/"+instit+"/"+predModel+"/"+dexperonly+"/"+freq+"/"+realm+"/"+mip+"/"+ens+"/"+pversion+"/"+dmodel+"/"+predictand+"/"+ds_region+"/"+dim+"/"+dversion+"/"
    return stdoutdir

#def checkExisting(dire,lon,Jsuffix,variable,freq,method,scenario,ens,start_year_s1,end_year_s1,fileid):
def checkExisting(dire,lon,Jsuffix,variable,freq,model,exper,scenario,ens,start_year_s1,end_year_s1,fileid,region):

	## to check if the given directory structure and ffilenams already exist in the file system.  Even if there is a single output file that already exists, the program quits. Use ifpreexist erase to force overwriting.-tasmin_day_RRtnp1-CDFt-B38atL01K00_rcp85_r1i1p1_r1i1p1_RR_20060101-20991231.I369_"J31-170".nc

	#filename  out.file1_a <- paste(variable,'_',freq,'_',method,'_',scenario1,'_',ens,'_',start_year_s1,'-',end_year_s1,fileid,sep='')
#checkExisting(outdir,lon,dsuffix,target,freq,dmodel,dexper,ens,fut_train_start_time,fut_train_end_time,'mini')

	exists = False
	if(os.path.exists(dire)):
		#print "Warning Warning---------Output directory",dire,"already exists"
		filesuff = ".nc"
		Jsuffix = Jsuffix.replace('"', '').strip()
		fileid= ".I"+str(lon)+"_"+str(Jsuffix)+filesuff
		filename = variable+"_"+"day"+"_"+exper+"_"+str(scenario)+"_"+str(region)+"_"+str(start_year_s1)+"0101-"+str(end_year_s1)+"1231"+fileid
		if(os.path.exists(dire+"/"+filename)):
		#	print "OOPS!!!!!!!!!Output file already exists:",dire,"/",filename
			exists = True
	return exists

#Looking at minifile name generation
def get_minifile_name(var, freq, exp_name, scenario, rip, region, tstart, tend):
	'''
	Makes minifile names as they would normally have appeared in the R code.
	Trying to move away from some xml dependencies in R.
	'''
	#scenario is weird - rough time period / emissions + '_$rip'
	scen_split = re.split("_", scenario)
	tstring = str(tstart) + "-" + str(tend)
	root_filename = "_".join([var, freq, exp_name, scen_split[0], rip, region, tstring])
	suffix = ".I<ISTART>-<IEND>_J<JSTART>-<JEND>.nc"
	mini_name = root_filename + suffix
	return(mini_name)

if __name__ == '__main__':
    main()
