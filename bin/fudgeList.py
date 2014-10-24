#!/usr/local/x64/python/2.7.3/bin/python
import expergen 
import os,sys,subprocess
from subprocess import PIPE
import optparse
from optparse import OptionParser
import shlex
cnt = 0
def fudgeVer():
        try:
		fudgeversion = 'fudge/'+os.environ['BRANCH'] 
		print "version",fudgeversion        
	except:
		fudgeversion = "<undefined>"
		
	return fudgeversion
def fudgeList():
     #userinput = '/home/a1r/gitlab/fudge/autogen/finalTests/GFDL-ARRMv2-A01P01X01.CM3.xml'
     force = False #default
     usage = "\n############################################################################\n"
     parser = OptionParser(usage)
     usage = usage + "Eg. fudgeList -i ../utils/xml/tests/vanilla.tasmax.A38.xml -o /tmp/testsummary122 \n"
     usage =  usage + "Eg. (with force option)fudgeList -f -i ../utils/xml/tests/vanilla.tasmax.A38.xml -o /tmp/testsummary122"
     parser = OptionParser(usage)

     try:
	userinput 
	uinput = userinput
     except:
#	 print "Looks like uinput is not defined with this piece of code. If you passed it as an option, I'll try to use the command-line option with the XML location instead.." 	
	 userinput = "none"	
     parser.add_option("-i", "--file", dest="uinput",help="pass location of XML template", metavar="FILE")
     parser.add_option("-f", "--force",action="store_true",default=False, help="Force override existing output. Default is set to FALSE/no-override")

     parser.add_option("-o", "--outlog", dest="sumlogloc",help="pass location of summary log file", metavar="FILE")
     parser.add_option("-v", "--fversion",action="store_true",default=False,help="Print FUDGE CODE VERSION. Default is set to FALSE/no-printing")

     (options, args) = parser.parse_args()
     #if userinput == "none":
       # print "Looking for XML location taken from command line option.."
       # print "Looks like uinput is not defined with this piece of code. If you passed it as an option, I'll try to use the command-line option with the XML location instead.."
     for opts,vals in options.__dict__.items():
         	if(opts == 'uinput'):
                	 uinput = vals
                 	 print "XML: ",uinput
                if(opts == 'sumlogloc'):
                         sumlogloc = vals
		if(opts == 'force'):
			force = vals 
                if(opts == 'fversion'):
			print "fudge version retriever.."	
                        fudgeversion = fudgeVer() 
			print fudgeversion
     if uinput is None:	
        uinput = userinput
     if sumlogloc is None:
	sys.exit("Please provide -o location_of_summarylog_output and try again.Quitting now..") 		
     if not os.path.exists(uinput):
                        print "ERROR Invalid XML path.Quitting. Please use -h for help ",uinput
                        sys.exit()
     print "Force override existing output is set to ",force
     #start_time1, start_time2, end_time1, end_time2, varname, hires1,lowres1,hires2,lowres2,strlist,futstart,futend,amip,calendar,esdMethod,expname1,expname2,freq,truth, predictor,rootdir1,rootdir2,dsuffix,region,lats,late,tstamp,lons,lone,yrtot,leaveit,basedire,outdire,futprefix,params,listlos,listfuts,ver = expergen.listVars(uinput,pp=True)
################ get BASEDIR #################
     basedir = os.environ.get('BASEDIR')
     if(basedir is None):
           print "ERROR: BASEDIR environment variable not set"
	   sys.exit(1)
     print "==============================runtime log from parser==================================================="
     output_grid,kfold,lone,region,fut_train_start_time,fut_train_end_time,file_j_range,hist_file_start_time,hist_file_end_time,hist_train_start_time,hist_train_end_time,lats,late,lons,late, basedir,method,target_file_start_time,target_file_end_time,target_train_start_time,target_train_end_time,spat_mask,fut_file_start_time,fut_file_end_time,predictor,target,params,outdir,dversion,dexper,target_scenario,target_model,target_freq,hist_scenario,hist_model,hist_freq,fut_scenario,fut_model,fut_freq,hist_pred_dir,fut_pred_dir,target_dir,expconfig,target_time_window,hist_time_window,fut_time_window,tstamp,ds_region,target_ver,auxcustom,qc_switch,qc_varname,qc_type,adjust_out = expergen.listVars(uinput,basedir=basedir,pp=True) 
     print "=================================================================================="		
     basedire = basedir	
     esdMethod = method
     varname = target 
     grid = spat_mask+"../"+region+".nc"
     region = ds_region
     outrec = outdir.split('_')
     #amip = outrec[0] 
     #fut = outrec[1]
     #print "amip outdir", amip
     #print "fut outdir", fut
     suff = ".nc"
     cond = 1 
     indir = fut_pred_dir 
     freq = fut_freq
     exper_rip = fut_scenario
     scenario = exper_rip.split('_')[0]
     ens = exper_rip.split('_')[1]	
     ver = dversion
     predModel = fut_model 	
     indir = outdir #Our mini op from dscaling is the input to PP  
     obsid = "'"+target_model+"."+target_scenario+"."+target_freq+"."+target_ver+"("+target_train_start_time+"-"+target_train_end_time+")"+"'"


     if (cond == 1):
				#print "enter condi...."
           			if (os.path.exists(indir)):
                                        exists = checkExists(varname,indir,region,ver,freq,expconfig,exper_rip,predModel,str(fut_train_start_time),str(fut_train_end_time),suff,force)
             				if (exists == False):

           					print "PP Output does not exist already",indir
						#cnt = call_ppFudge(basedire,indir,esdMethod,varname,str(expconfig),exper_rip,region,ver,lons,lone,lats,late,grid,freq,str(fut_train_start_time),str(fut_train_end_time),obsid)
					else:
						print "PP Output exists"
				        ###### start writing to summary log file ###############
				ready = "no"
            			if not os.path.exists(sumlogloc):
						ready = "yes" #ready to write log
				else:
						if (force == False):
							print "ERROR: The passed location pointing to the output summary log [-o] already exists. Please use -f to force overwrite this file"
							sys.exit("Quitting now")
						else:
							ready = "yes" #since force is True
				if(ready is "yes"):
                                                	logop = open(sumlogloc, 'w')
						######### what do we want to write in summary log ########
 
							ascii = "ds.experiment:"+expconfig
							ascii = ascii+"\n"+"xml.path:"+os.path.abspath(uinput)+"\n"
							ascii = ascii+"FUDGE.version:"+fudgeversion+"\n"
							delim="_"
						        target_parts = (target,target_freq,target_model,target_scenario,output_grid,target_file_start_time+"0101-"+target_file_end_time+"1231"+".I*",file_j_range.replace('"','').strip()+".nc")	
							target_file = delim.join(target_parts)	
							target_inpath_1=os.path.normpath(target_dir)+"/"+target_file
							ascii = ascii+"target.inpath_1:"+target_inpath_1+"\n"
                                                        hist_parts = (predictor,hist_freq,hist_model,hist_scenario,output_grid,hist_file_start_time+"0101-"+hist_file_end_time+"1231"+".I*",file_j_range.replace('"','').strip()+".nc")
                                                        hist_file = delim.join(hist_parts)

							hist_inpath_1=os.path.normpath(hist_pred_dir)+"/"+hist_file
							ascii = ascii+"hist.inpath_1:"+hist_inpath_1+"\n"
                                                        fut_parts = (predictor,fut_freq,fut_model,fut_scenario,output_grid,fut_file_start_time+"0101-"+fut_file_end_time+"1231"+".I*",file_j_range.replace('"','').strip()+".nc")
                                                        fut_file = delim.join(fut_parts)
                                                        fut_inpath_1=os.path.normpath(fut_pred_dir)+"/"+fut_file
                                                        ascii = ascii+"fut.inpath_1:"+fut_inpath_1+"\n"
						        output_path = os.path.normpath(outdir)	
							ascii = ascii+"output.path:"+output_path+"\n"
							spat_mask_path_1 = os.path.normpath(grid)
                                                        ascii = ascii+"spat.mask.path_1:"+spat_mask_path_1+"\n"
							ascii = ascii+"target.time.window:"+target_time_window+"\n"
							ascii = ascii+"hist.time.window:"+hist_time_window+"\n"
							ascii = ascii+"fut.time.window:"+fut_time_window+"\n"  
							ascii = ascii+"target.train.start.year_1:"+target_train_start_time+"\n"
							ascii = ascii+"target.train.end.year_1:"+target_train_end_time+"\n"
                                                        ascii = ascii+"hist.train.start.year_1:"+hist_train_start_time+"\n"
							ascii = ascii+"hist.train.end.year_1:"+hist_train_end_time+"\n" 
                                                        ascii = ascii+"fut.train.start.year_1:"+fut_train_start_time+"\n"
                                                        ascii = ascii+"fut.train.end.year_1:"+fut_train_end_time+"\n"

                                                	logop.write(ascii)
                                                	logop.close()

					###### end writing to summary log file #########
							print "----------Summary Log File: ", sumlogloc 	
	




def checkExists(var,indir,region,ver,freq,dexper,exper_rip,predictor,start,end,suff,force):
                                        filedire = indir+"/../../../"+region+"/"+ver+"/"
                                        filename =var+"_"+freq+"_"+dexper+"_"+exper_rip+"_"+region+"_"+start+"0101"+"-"+end+"1231"+".nc"
#                                        filename =var+"_"+freq+"_"+esdMethod+"_"+dexper+"_"+drip+"_"+region+"_"+predictor+"_"+start+"0101"+"-"+end+"1231"+".nc"

					print "test test",filedire,filename	
                                        if not os.path.exists(filedire+"/"+filename):                
                                           exists = False  
                                        else:
					   if (force == True):	
					   	print "CAUTION : PP output  already exists. But -f is turned on, so output will be overwritten"	
						exists = False
					   else:
						exists = True
						print "ERROR: PP output already exists. ", filedire,"/",filename,". Please use -f option to override this. Quitting now.."
					print filename
				        return exists		

if __name__ == '__main__':
    fudgeList()
