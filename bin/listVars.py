#New function for listVars

import xmlhandler
import naming
import pprint,datetime,getopt, os, shutil
import sys, subprocess, glob
import fudgeList
from subprocess import PIPE
from os import path as osp
import re
import optparse

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
