#!/usr/local/x64/python/2.7.3/bin/python

"""fudgeList.py: Prints out a summary log experiment_info file for the corresponding experiment XML."""
__author__ = "A.Radhakrishnan"

#import expergen
import listVars
import os,sys,subprocess
from os import path as osp
from subprocess import PIPE
import optparse
from optparse import OptionParser
import shlex,re
cnt = 0
def fudgeVer():
    try:
     #fudgeversion = 'fudge/'+os.environ['BRANCH']
     fudgeversion = 'fudge/'+os.environ['FUDGETAG']
     print "version",fudgeversion
    except:
        fudgeversion = "<undefined>"
    return fudgeversion

def fudgeList(verbose=False):
    #userinput = '/home/a1r/gitlab/fudge/autogen/finalTests/GFDL-ARRMv2-A01P01X01.CM3.xml'
    force = False #default
    usage = "\n############################################################################\n"
    parser = OptionParser(usage)
    usage = usage + "Eg. fudgeList -i ../utils/xml/tests/vanilla.tasmax.A38.xml -o /tmp/testsummary122 \n"
    usage =    usage + "Eg. (with force option)fudgeList -f -i ../utils/xml/tests/vanilla.tasmax.A38.xml -o /tmp/testsummary122"
    parser = OptionParser(usage)

    try:
        userinput
        uinput = userinput
    except:
        userinput = "none"

    parser.add_option("-i", "--file", dest="uinput",help="pass location of XML template", metavar="FILE")
    parser.add_option("-f", "--force",action="store_true",default=False, help="Force override existing output. Default is set to FALSE/no-override")
    parser.add_option("-o", "--outlog", dest="sumlogloc",help="pass location of summary log file", metavar="FILE")
    parser.add_option("--parse-j-style", dest='parse_j', action='store_true', default=False, help="Whether to parse input filenames according to j conventions")
    parser.add_option("-v", "--fversion",action="store_true",default=False,help="Print FUDGE CODE VERSION. Default is set to FALSE/no-printing")
    (options, args) = parser.parse_args()

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
    if not osp.exists(uinput):
        print "ERROR Invalid XML path.Quitting. Please use -h for help ",uinput
        sys.exit()
    print "Force override existing output is set to ",force

    basedir = os.environ.get('BASEDIR')
    if(basedir is None):
        print "ERROR: BASEDIR environment variable not set"
        sys.exit(1)
    print "==============================runtime log from parser==================================================="
    output_grid,kfold,lone,region,fut_train_start_time,fut_train_end_time,file_j_range,hist_file_start_time,hist_file_end_time,hist_train_start_time,hist_train_end_time,lats,late,lons,late, basedir,method,target_file_start_time,target_file_end_time,target_train_start_time,target_train_end_time,spat_mask,fut_file_start_time,fut_file_end_time,predictor,target,params,outdir,dversion,dexper,target_scenario,target_model,target_freq,hist_scenario,hist_model,hist_freq,fut_scenario,fut_model,fut_freq,hist_pred_dir,fut_pred_dir,target_dir,expconfig,target_time_window,hist_time_window,fut_time_window,tstamp,ds_region,target_ver,auxcustom,qc_switch,qc_varname,qc_type,adjust_out,sbase,pr_opts,masklist,idirsuffix = listVars.listVars(uinput,basedir=basedir,pp=True)
    print "=================================================================================="

    basedire = basedir
    esdMethod = method
    varname = target
    #### split and put in list ####
    list_pred = predictor.split(',')

    if(spat_mask == "na"):
        grid = "na"
    else:
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
        if (osp.exists(indir)):
            exists = checkExists(varname,indir,region,ver,freq,expconfig,exper_rip,predModel,str(fut_train_start_time),str(fut_train_end_time),suff,force)
            if (exists == False):
                print "PP Output does not exist already",indir
            else:
                print "PP Output exists"
    ###### start writing to summary log file ###############
    ready = "no"
    if not osp.exists(sumlogloc):
        ready = "yes" #ready to write log
    else:
        if (force == False):
            print "ERROR: The passed location pointing to the output summary log [-o] already exists. Please use -f to force overwrite this file"
            sys.exit("Quitting now")
        else:
            ready = "yes" #since force is True
    ready = 'yes'
    if(ready is "yes"):
        logop = open(sumlogloc, 'w')
    ######### what do we want to write in summary log ########

    ascii = "ds.experiment:"+expconfig
    ascii = ascii+"\n"+"xml.path:"+osp.abspath(uinput)+"\n"
    ascii = ascii+"FUDGE.version:"+fudgeversion+"\n"
    ascii = ascii+"region:"+region+"\n"
    ascii = ascii+"minifile.I.start:"+lons+"\n"
    ascii = ascii+"minifile.I.end:"+lone+"\n"
    ascii = ascii+"qc.mask:"+qc_switch+"\n"
    ascii = ascii+"target:"+target+"\n"
    j_parse = options.parse_j
    if (j_parse):
        OH_vars = [target]
        MH_vars = list_pred
        MF_vars = list_pred
        #so! Thanks to earlier processing, target_dir needs to have a /OneD/ dir stripped off of it
        OH_dir = target_dir
        #Parsing to get the proper amount of OH file
        prefix_loc = len(hist_pred_dir.split("/"))
        oh_prefix = "/".join(OH_dir.split("/")[0:prefix_loc])
        oh_suffix = "/".join(OH_dir.split("/")[(prefix_loc+1):-1])
        OH_trange = target_file_start_time+"-"+target_file_end_time
        OH_files = build_xml_input_paths(OH_vars, oh_prefix, OH_trange, suffix=idirsuffix)
        for n in range(len(OH_vars)):
            ascii += "target.inpath_" + str(n+1) + ":" + OH_files[n] + '\n'
        #Similar for MH, MF
        MH_dir = hist_pred_dir
        MH_trange = hist_file_start_time+"-"+hist_file_end_time
        MH_files = build_xml_input_paths(MH_vars, MH_dir, MH_trange, suffix=idirsuffix)
        for n in range(len(MH_vars)):
            ascii += "hist.inpath_" + str(n+1) + ":" + MH_files[n] + '\n'
        MF_dir = fut_pred_dir
        MF_trange = fut_file_start_time+"-"+fut_file_end_time
        MF_files = build_xml_input_paths(MF_vars, MF_dir, MF_trange, suffix=idirsuffix)
        for n in range(len(MF_vars)):
            ascii += "fut.inpath_" + str(n+1) + ":" + MF_files[n] + '\n'
    else:
        delim="_"
        target_parts = (target,target_freq,target_model,target_scenario,output_grid,target_file_start_time+"-"+target_file_end_time+".I*",file_j_range.replace('"','').strip()+".nc")
        target_file = delim.join(target_parts)
        target_inpath_1=osp.normpath(target_dir)+"/"+target_file
        ascii = ascii+"target.inpath_1:"+target_inpath_1+"\n"
        n_plist = 0
        for plist in list_pred:
            n_plist = n_plist + 1
            plist = plist.strip()
            hist_parts = (plist,hist_freq,hist_model,hist_scenario,output_grid,hist_file_start_time+"-"+hist_file_end_time+".I*",file_j_range.replace('"','').strip()+".nc")
            hist_file = delim.join(hist_parts)

            #####dict test
            histdict = {}
            histdict["hist.inpath_"+str(n_plist)]=osp.normpath(hist_pred_dir+"/"+plist)+idirsuffix+hist_file
            for hkey,hval in histdict.iteritems():
                ascii = ascii+str(hkey)+":"+hval+"\n"
            ##########
            fut_parts = (plist,fut_freq,fut_model,fut_scenario,output_grid,fut_file_start_time+"-"+fut_file_end_time+".I*",file_j_range.replace('"','').strip()+".nc")
            fut_file = delim.join(fut_parts)
            futdict = {}
            futdict["fut.inpath_"+str(n_plist)]=osp.normpath(fut_pred_dir+"/"+plist)+idirsuffix+fut_file
            for fkey,fval in futdict.iteritems():
                ascii = ascii+str(fkey)+":"+fval+"\n"
    listout = []
    property = {}
    listfacet = outdir.split('#')
    if (listfacet[1] == "na"):
        listout.append(listfacet[0])
    else:
        #### start property resolution
        property[listfacet[1]] = listfacet[2]
        unres_outdir = listfacet[0]
        if '$(' in unres_outdir:
            for pname,pval in property.iteritems():
                replace = pval
                break;
            list_new_out_dir = replace.split(",")
            for lnew in list_new_out_dir:
                listout.append(unres_outdir.replace(pname,lnew))
    #### end property resolution
    output_path = ""
    if (j_parse):
        tmp = listout[0].rstrip('/$') #strips trailing /
        mini = tmp.rsplit('/', 1)[-1] #name of dir minifiles write to
        output_path = "\n".join([osp.normpath(re.sub(mini, '', el)) + "/" for el in listout])
    else:
        for outdir in listout:
            if (output_path != ""):
                output_path = output_path +"\n"+ osp.normpath(outdir)
            else:
                output_path = output_path + osp.normpath(outdir)
    ascii = ascii+"output.path:"+output_path+"\n"
    spat_mask_path_1 = osp.normpath(grid)
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
    ascii = ascii+"script.base:"+osp.normpath(sbase)
    #print(ascii)
    logop.write(ascii)
    logop.close()
    ###### end writing to summary log file #########
    if (verbose):
        print "----------Summary Log File: ", sumlogloc

def checkExists(var,indir,region,ver,freq,dexper,exper_rip,predictor,start,end,suff,force):
    filedire = indir+"/../../../"+region+"/"+ver+"/"
    filename =var+"_"+freq+"_"+dexper+"_"+exper_rip+"_"+region+"_"+start+"-"+end+".nc"
    if not osp.exists(filedire+"/"+filename):
        exists = False
    else:
        if (force == True):
            print "CAUTION : PP output already exists. But -f is turned on, so output will be overwritten"
            exists = False
        else:
            exists = True
    print "ERROR: PP output already exists. ", filedire,"/",filename,". Please use -f option to override this. Quitting now.."
    print filename
    return exists

#Helper functions
def build_xml_input_paths(in_vars, in_dir, trange, suffix, is_multivar=True):
    '''
    Builds input paths from the components that are present in the XML.
    '''
    in_files = []
    for v in in_vars:
        f_dir = osp.join(in_dir, v) + suffix
        print(f_dir)
        f_dir_files = [osp.join(f_dir, el) for el in os.listdir(f_dir) if re.search(trange, el) is not None]
        in_file = [el for el in f_dir_files if re.search(".nc$", el) is not None]
        #at this point, there should be one and only one element in in_file
        if len(in_file) > 1:
            print("Error in build_xml_input_paths: more than one file matching regex in output dir!")
            print(f_dir)
            sys.exit(1)
        elif len(in_file) == 0:
            print("Error in build_xml_input_paths: no files match regex in output dir!")
            print(f_dir)
            sys.exit(1)
        in_files.append(in_file[0])
    return(in_files)

if __name__ == '__main__':
    fudgeList()
