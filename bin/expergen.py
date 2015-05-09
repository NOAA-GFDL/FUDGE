#!/usr/local/python/2.7.1/bin/python
import xmlhandler
import naming
import pprint,datetime,getopt, os, shutil
import sys, subprocess
import fudgeList
from subprocess import PIPE 
import optparse
from optparse import OptionParser

list_title = []
dictDS_hi = {}
listh = []
listlos =[]
listl = []
dictDS_lo = {}
list  = []
dictDIR_hi = {}
dictDIR_lo = {}
rootdir = ''
projectRoot = "" 
project = "" 
ppn = 2 #TODO get from XML custom?
overwrite = False #default don't overwrite existing output or scripts
preexist_glob = "erase" #if overwriting old, default is erasing, not archiving
def checkTags(dictParams,key):
	        if(dictParams.has_key(key)):
               	 	val = dictParams[key]
		else:
			if(key == 'oroot'):
				print "output root tag is being deprecated from darkchocolate."
				sys.exit("error")
				
                        elif(key == 'sroot'):
                                print "Script root default value is being deprecated from darkchocolate. Please specify <script_root> in XML and try again " 
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
		        else:		
				print "Error: Missing value for ",key
				sys.exit(1)  
		return val 

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
#                dictDS[ds_title] = [hi_proj, hi_product,hi_instit,hi_model,hi_exp,hi_mip,hi_realm,hi_rip,hi_ver,hi_expname]
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
        print "----Downscaling XML template-", uinput
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
	print "Initiate checkTags"
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
	if(target != 'pr'):
		if(pr_opts != 'na'):
			print "ERROR: You specified pr_opts for a target field that's not precipitation_flux. Please check and try again"
			sys.exit()	 
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
	    			print "file to be gcpied to vftmp: ",val
				auxcustom1 = val
			print "auxcustom1:",auxcustom1
        #projectRoot = checkTags(dictParams,'oroot')
	global rootdir 
        rootdir = checkTags(dictParams,'in_root') 
	outdir = checkTags(dictParams,'outdir')
	out_dir = checkTags(dictParams,'out_dir')
        out_dir = out_dir.strip()
	##script -and- log prefix section ##
        sroot = checkTags(dictParams,'sroot')
	sbase = sroot+"/scripts/"+project_ID+"/"+experiment+"/"
	## pp section ##
	qc_mask = checkTags(dictParams,'qc_mask')
        print "qc_mask....... ",qc_mask
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
	print "preexist"+preexist
	if(preexist == 'exit'):
		force = False
        elif(preexist == 'erase'):
		force = True
	elif(preexist == 'move'):
		force = True #new move feature in clobber - support
	else:
	        print "\033[1;41mERROR code -2: Please provide a valid value {'exit','erase','move'} for ifpreexist tag in XML \033[1;m",preexist
		sys.exit(-2)
        #print "----Force Override existing output Flag-", force
        ####### end get  dictParams ###########################
        ## OneD or ZeroD that's the question and starting feb 10 2015 this will be received from xml##  
        #if(region != "station"):
        # dim = "OneD"
        #else:
        # dim = "ZeroD"
	#print(dim)
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
        dict_hist,listh = getFacets(hist_id,predictor,dim,'hist_id',output_grid)
	#print dict_hist
	########### future predictor information ############## 
        dict_fut,listf = getFacets(fut_id,predictor,dim,'fut_id',output_grid)
	#print dict_fut
	###########  esdgen information ####################################

	####################################################################
	target_dir = getDir(listt,'hidir')
	print "target dir: ",target_dir 
        hist_pred_dir = getDir(listh,'lodir')
	print "historical predictor dir",hist_pred_dir
	fut_pred_dir = getDir(listf,'futdir')
	print "future predictor dir",fut_pred_dir

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
	    print "Output directory is :",outdir
	    print "Script directory is:",sbase  
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
			print "preexist turned ON................"
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

	    return output_grid,kfold,lone,region,fut_train_start_time,fut_train_end_time,file_j_range,hist_file_start_time,hist_file_end_time,hist_train_start_time,hist_train_end_time,lats,late,lons,late, basedir,method,target_file_start_time,target_file_end_time,target_train_start_time,target_train_end_time,spat_mask,fut_file_start_time,fut_file_end_time,predictor,target,params,outdir,dversion,dexper,target_scenario,target_model,target_freq,hist_scenario,hist_model,hist_freq,fut_scenario,fut_model,fut_freq,hist_pred_dir,fut_pred_dir,target_dir,experiment,target_time_window,hist_time_window,fut_time_window,tstamp,ds_region,target_ver,auxcustom,qc_mask,qc_varname,qc_type,adjust_out,sbase,pr_opts,masklists
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
        if(branch is None):
           print "Warning: BRANCH env variable not set. BRANCH will be set to undefined"
           branch = "undefined"
	####################################################
        parser = OptionParser(usage=help)
        parser.add_option("-i", "--file", dest="uinput",
        help="pass location of XML template", metavar="FILE")
      #since we now have an XML tag  parser.add_option("-f", "--force",action="store_true",default=False, help="Force override existing output. Default is set to FALSE")
        parser.add_option("--msub", "--msub",action="store_true",default=False, help="Automatically submit the master runscripts using msub.Default is set to FALSE")
        #parser.add_option("-v", "--version",action="store_true", dest="version",default="v20120422", help="Assign version for downscaled data. Default is set to v20120422")        
        
        (options, args) = parser.parse_args()
        verOpt = True #default
	msub = False 
        forOpt = True #default
        inter = 'on' #for debugging
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
        if (inter == 'off'):
		uinput = 'dstemplate.xml'
	        dversion = "v20120422"
                force = False
                if os.path.exists(uinput):
                                        parser = xml.sax.make_parser(  )
                                        handler = temphandler.TempHandler(  )
                                        parser.setContentHandler(handler)
                                        parser.parse(uinput)
        if (inter == 'on'):  
        	for opts,vals in options.__dict__.items():
           	# print opts,vals
            		if(opts == 'uinput'):
                		uinput = vals
                		if os.path.exists(uinput):
				        print "XML input:",uinput
                    			#parser = xml.sax.make_parser(  )
                    			#handler = temphandler.TempHandler(  )
                    			#parser.setContentHandler(handler)
                    			#parser.parse(uinput)
                		else:
                    			print "Please pass a valid input XML filename with the -i argument and try again. See -h for syntax. Quitting now.."
                    			sys.exit()
            		if(opts == 'msub'):
                		if (vals == True):
                    			msub = True 
				else:
					msub = False
                		msub = vals 
        #########  call listVars() #############################################################
	output_grid,kfold,lone,region,fut_train_start_time,fut_train_end_time,file_j_range,hist_file_start_time,hist_file_end_time,hist_train_start_time,hist_train_end_time,lats,late,lons,late, basedir,method,target_file_start_time,target_file_end_time,target_train_start_time,target_train_end_time,spat_mask,fut_file_start_time,fut_file_end_time,predictor,target,params,outdir,dversion,dexper,target_scenario,target_model,target_freq,hist_scenario,hist_model,hist_freq,fut_scenario,fut_model,fut_freq,hist_pred_dir,fut_pred_dir,target_dir,expconfig,target_time_window,hist_time_window,fut_time_window,tstamp,ds_region,target_ver,auxcustom,qc_mask,qc_varname,qc_type,adjust_out,sbase,pr_opts,masklists= listVars(uinput,basedir,msub)
        ######### call script creators..  #######################################################
        ############################### 1 ###############################################################
        #  make.code.tmax.sh 1 748 756 /vftmp/Aparna.Radhakrishnan/pid15769 outdir 1979 2008 tasmax

	# If the runscript directory already exists, please quit
	#/home/a1r/gitlab/cew/fudge2014///scripts/tasmax/35txa-CDFt-A00X01K02
	scriptdir = [sbase+"/master",sbase+"/runcode",sbase+"/runscript"]
        for sd in scriptdir:
	    if(os.path.exists(sd)):
       		 if os.listdir(sd):
		    if (overwrite == False):    
			print '\033[1;41mERROR: Scripts Directory already exists. Clean up using cleanup_script and try again please -or- (Use <ifpreexist>erase</ifpreexist> if output/scripts already exist and you want to override this and let expergen run the cleanup_script for you; Use <ifpreexist>move</ifpreexist> to move existing output)\033[1;m',sd
                        print "\033[1;41mERROR code -6: script directory already exists.Check --\033[1;m",scriptdir
                	sys.exit(-6)
		    if (overwrite == True):
			print "\033[1;43mWarning: Scripts Directory already exists. But, since <ifpreexist> has different settings, cleanup utility will handle this\033[1;m "
			print "Now invoking cleanup utility..........."
		        cleaner_script = basedir+"/utils/bin/"+"cleanup_script.csh"
			if(preexist_glob == 'erase'):
				print "Lets erase it ........... ifpreexist"
		        	cleaner_cmd = cleaner_script+" d "+uinput 
			else: 
				if(preexist_glob != 'move') & (preexist_glob != 'exit'):
					print "CHECK ifpreexist settings, quitting now"
					sys.exit(1)
				print "Lets move it............... if preexist" 
                               	cleaner_cmd = cleaner_script+" m "+uinput
	                print "cleaner_cmd"
			print cleaner_cmd		
	                pclean = subprocess.Popen(cleaner_cmd,shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
			#check return code
        		output0, errors0 = pclean.communicate()
        		if(pclean.returncode != 0):
         			print "033[1;41mCleaner Step:!!!! FAILED !!!!, please contact developer.033[1;m"
         			print output0, errors0
        			sys.exit(1)
			else:
				print output0,errors0
			print "continue expergen run...................." 
			break 
	    else:
		   print "scriptdir "+sd+" does not exist. Looks like a clean slate "
        #print  "hist_freq"+hist_freq
        script1Loc = basedir+"/utils/bin/create_runcode"
        make_code_cmd = script1Loc+" "+str(predictor)+" "+str(target)+" "+str(output_grid)+" "+str(spat_mask)+" "+str(region)
        make_code_cmd = make_code_cmd+" "+str(file_j_range)+" "+str(lons)+" "+str(lats)+" "+str(late)
	make_code_cmd = make_code_cmd+" "+str(hist_file_start_time)+" "+str(hist_file_end_time)+" "+str(hist_train_start_time)+" "+str(hist_train_end_time)+" "+str(hist_scenario)+" "+str(hist_model)+" "+hist_freq+" "+str(hist_pred_dir)+" "+str(hist_time_window)
        make_code_cmd = make_code_cmd+" "+str(fut_file_start_time)+" "+str(fut_file_end_time)+" "+str(fut_train_start_time)+" "+str(fut_train_end_time)+" "+str(fut_scenario)+" "+str(fut_model)+" "+fut_freq+" "+str(fut_pred_dir)+" "+str(fut_time_window)
        make_code_cmd = make_code_cmd+" "+str(target_file_start_time)+" "+str(target_file_end_time)+" "+str(target_train_start_time)+" "+str(target_train_end_time)+" "+str(target_scenario)+" "+str(target_model)+" "+target_freq+" "+str(target_dir)+" "+str(target_time_window)
        make_code_cmd = make_code_cmd+" "+str(method)+" "+str(expconfig)+" "+str(kfold)+" "+str(outdir)+" "+str(tstamp)+" "+'na'+" "+basedir+" "+str(lone)

        print "Step 1: R code starters generation ..in progress"
        #p = subprocess.Popen('tcsh -c "'+make_code_cmd+'"',shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
        
        params_new =  '"'+str(params)+'\"'
	params_pr_opts = '"'+str(pr_opts)+'\"'
        make_code_cmd = make_code_cmd +" "+params_new+" "+"'"+str(ds_region)+"'"
        make_code_cmd = make_code_cmd+" "+str(auxcustom)+" "+str(qc_mask)+" "+str(qc_varname)+" "+str(qc_type)+" "+str(adjust_out)+" "+str(sbase)+" "+str(params_pr_opts)+" "+str(branch)+" "+'"'+str(masklists)+'"' 
	#cprint make_code_cmd
        #p = subprocess.Popen(make_code_cmd +" "+params_new,shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
        p = subprocess.Popen(make_code_cmd,shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
        output, errors = p.communicate() 
        if(p.returncode != 0):
         print "Step1:!!!! FAILED !!!!, please contact developer."
         print output, errors
         sys.exit(0)
        #cprint output, errors
	#cprint "----Log-----"
        #cprint output,errors
        ###############################################################################################
        print "1- completed\n"
        #print "debug............msub turned ",msub
        ############################### 2 ################################################################
	#target_time_window,hist_time_window,fut_time_window
        script2Loc = basedir+"/utils/bin/"+"create_runscript"
        create_runscript_cmd = script2Loc+" "+str(lons)+" "+str(lone)+" "+str(expconfig)+" "+str(basedir)+" "+target+" "+method+" "+target_dir+" "+hist_pred_dir+" "+fut_pred_dir+" "+outdir+" "+str(file_j_range)+" "+tstamp+" "+str(target_file_start_time)+" "+str(target_file_end_time)+" "+str(hist_file_start_time)+" "+str(hist_file_end_time)+" "+str(fut_file_start_time)+" "+str(fut_file_end_time)+" "+str(spat_mask)+" "+str(region)+" "+auxcustom+" "+target_time_window+" "+hist_time_window+" "+fut_time_window+" "+sbase
        print "Step 2: Individual Runscript generation: \n"+create_runscript_cmd
        p1 = subprocess.Popen('tcsh -c "'+create_runscript_cmd+'"',shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
        print "Step 2: Individual runscript  creation.. in progress"
        output1, errors1 = p1.communicate()
	#cprint output1,errors1
        print "2- completed\n"
        if(p1.returncode != 0):
         print "Step2:!!!! FAILED !!!!, please contact developer."
         print output1, errors1
         sys.exit(0)
        #print output1, errors1
        #c print errors1
        
        ###################################### 3 ################################################################
        script3Loc = basedir+"/utils/bin/"+"create_master_runscript"
        create_master_cmd= script3Loc+" "+str(lons)+" "+str(lone)+" "+str(predictor)+" "+method+" "+sbase+" "+expconfig+" "+file_j_range+" "+tstamp+" "+str(ppn)+" "+str(msub)
        print "Step 3: --------------MASTER SCRIPT GENERATION-----------------------"#+create_master_cmd
        p2 = subprocess.Popen('tcsh -c "'+create_master_cmd+'"',shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
        #cprint create_master_cmd
        print "Create master script .. in progress"
        output2, errors2 = p2.communicate()
        #######
        if(p2.returncode != 0):
         print "Step3:!!!! FAILED !!!!, please contact developer."
         print output2, errors2
         sys.exit(0)
        print output2, errors2
        print "3- completed"
	##################################### 4 ############################################
        # copy xml to configs dir 
        cdir = sbase+"/config/"
        try:
	  os.mkdir(cdir)
	except:
	  print "Unable to create dir. Dir may exist already", cdir 
	shutil.copy2(uinput, cdir)
        print "Config XML saved in ",cdir 
        print "RunScripts will be saved under:",sbase
	print "----See readMe in fudge2014 directory for the next steps----"
	print "Use submit_job to submit scripts"

############### crte ppscript #################
	dev = "off" 
        print(sbase+"/postProc/aux/")
        if not os.path.exists(sbase+"/postProc/aux/"):
        	os.makedirs(sbase+"/postProc/aux/")
        if (dev == "off"):
		tsuffix = ""
        	ppbase = sbase+"/postProc/aux/"+"/postProc_source"
	else:
		tsuffix = "_"+tstamp
                ppbase = sbase+"/postProc/aux/"+"/postProc_source"+tstamp
	try:
  		ppfile = open(ppbase, 'w')
#check if qc_mask is relevant  
		if(qc_mask != 'off'):
  			pp_cmnd = "python $BASEDIR/bin/postProc -i "+os.path.abspath(uinput)+" -v "+target+","+target+"_qcmask\n"
	 	else:
                        pp_cmnd = "python $BASEDIR/bin/postProc -i "+os.path.abspath(uinput)+" -v "+target+"\n"
  		ppfile.write(pp_cmnd)
  		ppfile.close()
	except:
  		print "Unable to create postProc command file. You may want to check your settings."
	#c	print create_pp_cmd
        if(os.path.exists(ppbase)):
######################### write postProc_job to be used ############################
                ppLoc = basedir+"/utils/bin/"+"create_postProc"
                create_pp_cmd= ppLoc+" "+ppbase+" "+sbase+"  "+basedir+" "+tstamp+" "+branch
                print "Step 4: --------------PP postProc SCRIPT GENERATION-----------------------"
                p4 = subprocess.Popen('tcsh -c "'+create_pp_cmd+'"',shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
                output4, error4 = p4.communicate()
                if(p4.returncode != 0):
                        print "Step4:!!!! FAILED !!!!, please contact developer."
                        print output4, error4
                        sys.exit(-4)
                print output4, error4
                print "4- completed"
                print "NOTE: postProc will succeed only if you're running the model for the full downscaled region. (it will fail if you're running downscaling for a single slice for example)"
                print "----------------------------------------"
		print "\033[1;42mPlease use this script to run post post-processing (or msub this script), postProc when downscaling jobs are complete \033[1;m",sbase+"postProc/postProc_command"+tsuffix
                try:  
   			NEMSemail = os.environ["NEMSemail"]
                        print "msub -d $HOME -m ae -M "+os.environ.get('NEMSemail')+" "+sbase+"postProc/postProc_command"+tsuffix
		except KeyError: 
   			print "NEMSemail not set. Please use your email for notification in the following msub command i.e msub -m ae -M <email> script " 
			print "msub -d $HOME "+sbase+"postProc/postProc_command"+tsuffix
	else:
		print "postProc_command cannot be created. postProc_source does not exist"
################ step 5 fudgeList invocation ##############################################
        slogloc = sbase+"/"+"experiment_info.txt"
        fcmd = "python "+basedir+"/bin/fudgeList.py -f -i "+uinput+" -o "+slogloc
        f = subprocess.Popen(fcmd, stdout=subprocess.PIPE, shell=True)
        out, err = f.communicate()
        #print "fudgeList out", out
        #if err is not None:
        #       print "fudgeList err", err
        print "Summary Log File: ", slogloc
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
if __name__ == '__main__':
    main()
