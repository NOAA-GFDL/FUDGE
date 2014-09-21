#!/usr/local/python/2.7.3/bin/python
import xmlhandler
import naming
import pprint,datetime,getopt, os, shutil
import sys, subprocess
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
rootdir = "/archive/esd/PROJECTS/DOWNSCALING/"  #default
projectRoot = "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th" 
project = "35" #default TODO get from XML
ppn = 2 #TODO get from XML custom?

def checkTags(dictParams,key):
	        if(dictParams.has_key(key)):
               	 	val = dictParams[key]
		else:
			if(key == 'maskvar'):
				print "use output_grid/region name in XML instead -- applicable for US48/PM"
				key='output_grid'
				val = checkTags(dictParams.key)
			elif(key == 'oroot'):
				#projectRoot = "/work/a1r/PROJECTS/DOWNSCALING/RedRiver/"
				print "use output root default value", projectRoot
				val = projectRoot
			elif(key == 'outdir'):
				val = 'na'
                        elif(key == 'params'):
                                val = 'na'
			elif(key == 'experiment'):
				val = 'na'
		        else:		
				print "Error: Missing value for ",key
				sys.exit(1)  
		return val 

def getFacets(listDatasetID,var,dim,label="label"):
        ctr = 0
	dictDS = {}
	listDS = []
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
                if(hi[ind+2] != 'day'):
                 ind = ind-1
                 hi_freq = ''
                else:
                 hi_freq = hi[ind+2]
                hi_realm = hi[ind+3]
                hi_mip = hi[ind+4]
                hi_rip = hi[ind+5]
                hi_ver = hi[ind+6]
                hi_expname = hi_exp+"_"+hi_rip
                hi_expname = hi_expname
#                dictDS[ds_title] = [hi_proj, hi_product,hi_instit,hi_model,hi_exp,hi_mip,hi_realm,hi_rip,hi_ver,hi_expname]
                hiresdir = "/"+hi_proj+"/"+hi_product+"/"+hi_instit+"/"+hi_model+"/"+hi_exp+"/"+hi_freq+"/"+hi_realm+"/"+hi_mip+"/"+hi_rip+"/"+hi_ver+"/"+var+"/"+dim+"/"
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
	i=0
	dictDIR = {}
        for getdir in listDir:
                dir_lo = label+str(i)
                dictDIR[dir_lo] = rootdir+"/"+getdir
                i = i + 1
                dire = dictDIR[dir_lo].strip()
	return dire	
def listVars(uinput,basedir=None,force=False,pp=False):
	print "PP option is set to ",pp
        if os.path.exists(uinput):
		handler = xmlhandler.XMLHandler(  )
		dictParams = handler.getParams(uinput)
		try:
			dversion = dictParams['dversion']
	        except:
			print "Using default version v20140108"
			dversion='v20140108'
        print "----Downscaling XML template-", uinput
        print "----Downscaled data version-", dversion
        print "----Force Override existing output Flag-", force
        ###################################
        print "Input from XML"
        print "########################"
	for name,val in dictParams.iteritems():
		print name,"=",val 
        ###### get dictParams #################################
        predictor =  checkTags(dictParams,'predictor_list')
        target = checkTags(dictParams,'target') 
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
        output_grid = checkTags(dictParams,'output_grid')
        region = checkTags(dictParams,'maskvar')
	spat_mask = checkTags(dictParams,'spat_mask') 
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
                series = checkTags(dictParams,'series') #experiment series
 		experiment,ds_region = naming.constructExpname(proj,target,series,method,kfold,basedir)
        ##################################################### 
	params = checkTags(dictParams,'params')
        projectRoot = checkTags(dictParams,'oroot')
	outdir = checkTags(dictParams,'outdir')
        ####### end get  dictParams ###########################
        ## OneD or ZeroD that's the question ##  
        if(region != "station"):
         dim = "OneD"
        else:
         dim = "ZeroD"
        if(output_grid == 'station'):
                if(latjstart == latjend):
                        dsuffix = "J"+str(latjstart)
			region = output_grid
                else:
                        print "Please specify a single station. "
                        sys.exit()
        elif((output_grid == 'US48') | (output_grid == 'us48')):
		output_grid = "US48"
                dsuffix = "J454-567"
		region = output_grid
        elif(region == 'global'):
                dsuffix = "J1-720"
		region = output_grid
        else:
	   if(file_j_range != ''):
	   	dsuffix=file_j_range
		dim1 = dim
		dim = output_grid+"/"+dim1
	   else:
           	sys.exit( "Please specify region information and file_j_range and try again. Quitting now \n")
        ############ target get dir info ########################
	dict_target,listt = getFacets(target_id,target,dim,'target_id')
	#print dict_target
	############ historical predictor ######################
        dict_hist,listh = getFacets(hist_id,predictor,dim,'hist_id')
	#print dict_hist
	########### future predictor information ############## 
        dict_fut,listf = getFacets(fut_id,predictor,dim,'fut_id')
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
        if(outdir == 'na'):
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
            outdir = getOutputPath(projectRoot,category,instit,predModel,dexper,freq,realm,mip,ens,pversion,experiment,predictand,ds_region,dim1,dversion)
            #experiment in the above is expconfig that's constructed 
	    print "Output directory is :",outdir
#new
	    if (pp == False):	
                    	for lon in (lons,lone):
	                	exists = checkExisting(outdir,lon,dsuffix,target,freq,dmodel,dexper,ens,fut_train_start_time,fut_train_end_time,'mini')
			 	#print "Exists? ",exists
                         	if (exists == True) & (force == False):
                                	sys.exit("ERROR: Output already exists. Use -f to override existing output. Quitting now..")
                         	if (exists == True) & (force == True):
                                	print "CAUTION Output already exists. -f is turned on. Any existing output will be overwritten."

#	    return targetdir1,hist_pred_dir1,fut_pred_dir1
	    target_scenario = dict_target['exp']+"_"+dict_target['rip']
	    target_model = dict_target['model']
	    target_freq = dict_target['mip']
	    target_ver = dict_target['ver']
	    hist_scenario = dict_hist['exp']+"_"+dict_hist['rip']
	    hist_freq = dict_hist['mip']
            hist_model = dict_hist['model']
	    fut_scenario = dict_fut['exp']+"_"+dict_fut['rip']	
            fut_model = dict_fut['model']
            fut_freq = dict_fut['mip']
	    tstamp = str(datetime.datetime.now().date())+"."+str(datetime.datetime.now().time())

	    return output_grid,kfold,lone,region,fut_train_start_time,fut_train_end_time,file_j_range,hist_file_start_time,hist_file_end_time,hist_train_start_time,hist_train_end_time,lats,late,lons,late, basedir,method,target_file_start_time,target_file_end_time,target_train_start_time,target_train_end_time,spat_mask,fut_file_start_time,fut_file_end_time,predictor,target,params,outdir,dversion,dexper,target_scenario,target_model,target_freq,hist_scenario,hist_model,hist_freq,fut_scenario,fut_model,fut_freq,hist_pred_dir,fut_pred_dir,target_dir,experiment,target_time_window,hist_time_window,fut_time_window,tstamp,ds_region,target_ver
############end of listVars###################### 
def main():
    #################### args parsing ############
        help = "#################Usage:##################\n dsTemplater -i <input XML> \n "
        help = help + "Example 1: dsTemplater -i dstemplate.xml \n "
        help = help + "Example 2: dsTemplater -i examples/dstemplate.60lo0FUTURE.xml \n"
        #print usage
	basedir = os.environ.get('BASEDIR')
        if(basedir is None):
	   print "Warning: BASEDIR environment variable not set"	
        parser = OptionParser(usage=help)
        parser.add_option("-i", "--file", dest="uinput",
        help="pass location of XML template", metavar="FILE")
        parser.add_option("-f", "--force",action="store_true",default=False, help="Force override existing output. Default is set to FALSE")
        #parser.add_option("-v", "--version",action="store_true", dest="version",default="v20120422", help="Assign version for downscaled data. Default is set to v20120422")        
        
        (options, args) = parser.parse_args()
        verOpt = True #default 
        forOpt = True #default
        inter = 'on' #for debugging
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
            		if(opts == 'force'):
                		if (vals == True):
                    			forOpt = False
                		force = vals 
        #########  call listVars() #############################################################
	output_grid,kfold,lone,region,fut_train_start_time,fut_train_end_time,file_j_range,hist_file_start_time,hist_file_end_time,hist_train_start_time,hist_train_end_time,lats,late,lons,late, basedir,method,target_file_start_time,target_file_end_time,target_train_start_time,target_train_end_time,spat_mask,fut_file_start_time,fut_file_end_time,predictor,target,params,outdir,dversion,dexper,target_scenario,target_model,target_freq,hist_scenario,hist_model,hist_freq,fut_scenario,fut_model,fut_freq,hist_pred_dir,fut_pred_dir,target_dir,expconfig,target_time_window,hist_time_window,fut_time_window,tstamp,ds_region= listVars(uinput,basedir,force)
        ######### call script creators..  #######################################################
        ############################### 1 ###############################################################
        #  make.code.tmax.sh 1 748 756 /vftmp/Aparna.Radhakrishnan/pid15769 outdir 1979 2008 tasmax

	# If the runscript directory already exists, please quit
	#/home/a1r/gitlab/cew/fudge2014///scripts/tasmax/35txa-CDFt-A00X01K02
	scriptdir = basedir+"/scripts/"+target+"/"+expconfig

	if(os.path.exists(scriptdir)):
		print "Warning: script directory already exists.",scriptdir
		#print "ERROR: Directory already exists. Clean up and try again please:",scriptdir 
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
        make_code_cmd = make_code_cmd +" "+params_new+" "+"'"+str(ds_region)+"'"
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
        ############################### 2 ################################################################
        script2Loc = basedir+"/utils/bin/"+"create_runscript"
        create_runscript_cmd = script2Loc+" "+str(lons)+" "+str(lone)+" "+str(expconfig)+" "+str(basedir)+" "+target+" "+method+" "+target_dir+" "+hist_pred_dir+" "+fut_pred_dir+" "+outdir+" "+str(file_j_range)+" "+tstamp+" "+str(target_file_start_time)+" "+str(target_file_end_time)+" "+str(hist_file_start_time)+" "+str(hist_file_end_time)+" "+str(fut_file_start_time)+" "+str(fut_file_end_time)+" "+str(spat_mask)+" "+str(region)
        #print "Step 2: Individual Runscript generation: \n"+create_runscript_cmd
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
        create_master_cmd= script3Loc+" "+str(lons)+" "+str(lone)+" "+str(predictor)+" "+method+" "+basedir+" "+expconfig+" "+file_j_range+" "+tstamp+" "+str(ppn)
        print "Step 3: --------------MASTER SCRIPT GENERATION-----------------------"#+create_master_cmd
        p2 = subprocess.Popen('tcsh -c "'+create_master_cmd+'"',shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
        #cprint create_master_cmd
        #print "Create master script .. in progress"
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
        cdir = basedir+"/"+"/scripts/"+predictor+"/"+expconfig+"/config/"
        try:
	  os.mkdir(cdir)
	except:
	  print "Unable to create dir. Dir may exist already", cdir 
	shutil.copy2(uinput, cdir)
        print "Config XML saved in ",cdir 
	print "----See readMe in fudge2014 directory for the next steps----"
def getOutputPath(projectRoot,category,instit,predModel,dexper,freq,realm,mip,ens,pversion,dmodel,predictand,ds_region,dim,dversion):
    ##Sample:
    #${PROJECTROOT}/downscaled/NOAA-GFDL/GFDL-HIRAM-C360-COARSENED/amip/day/atmos/day/r1i1p1/v20110601/GFDL-ARRMv1A13X01/tasmax/OneD/v20130626/tasmax_day_GFDL-ARRMv1A13X01_amip_r1i1p1_US48_GFDL-HIRAM-C360-COARSENED_19790101-20081231.XXXX.nc
     
    print "No outdir specified. Let's deduce the standardized output directory here.."        
    if(projectRoot == 'na'):
        print "Output root directory not specified. Default project root will be used. (Default project root is /archive/esd/)"
    dexperonly = dexper.split("_")[0]	
    if(freq == ""):
	freq = mip
    stdoutdir = projectRoot+"/"+category+"/"+instit+"/"+predModel+"/"+dexperonly+"/"+freq+"/"+realm+"/"+mip+"/"+ens+"/"+pversion+"/"+dmodel+"/"+predictand+"/"+ds_region+"/"+dim+"/"+dversion+"/"  
    return stdoutdir  

def checkExisting(dire,lon,Jsuffix,variable,freq,method,scenario,ens,start_year_s1,end_year_s1,fileid):
	## to check if the given directory structure and ffilenams already exist in the file system.  Even if there is a single output file that already exists, the program quits. Use -f to force overwriting.
	#filename  out.file1_a <- paste(variable,'_',freq,'_',method,'_',scenario1,'_',ens,'_',start_year_s1,'-',end_year_s1,fileid,sep='')
	exists = False 
        if(os.path.exists(dire)):
		print "---------Output directory",dire,"already exists"
		filesuff = ".nc"
	        fileid= ".I"+str(lon)+"_"+str(Jsuffix)+filesuff
                filename = variable+"_"+freq+"_"+method+"_"+scenario+"_"+str(ens)+"_"+str(start_year_s1)+"0101-"+str(end_year_s1)+"1231"+fileid
		if(os.path.exists(dire+"/"+filename)):
			print "OOPS!!!!!!!!!Output file already exists:",dire,"/",filename
			exists = True 
        return exists			
if __name__ == '__main__':
    main()
