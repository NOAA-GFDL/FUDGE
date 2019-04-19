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
import json
import cleanup_script
from cleanup_script import clean_prior_experiments, parse_outdirs_from_xml
import get_npts_by
from create_all_runscripts import create_auxfile_runscripts
from create_all_runscripts import create_xml_runscripts
from get_comment_specs import get_comment_specs, get_exp_check_tag
from get_ds_info import get_ds_info, parse_aux_info, make_aux_input
from get_ds_info import add_ds_params_to_auxdict, add_s3_s5_params_to_auxdict
from make_postProc_scripts import make_postProc_scripts
from collections import OrderedDict
from watcher import runWatcher #,_v_print
from fudgeUtils import read_exp_info_file
from get_grid_params import get_grid_params
from check_aux_consistency import check_aux_consistency

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
    force = True
    if(preexist == 'exit'):
        force = True
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
                    global overwrite
                    overwrite = True
                    break
                if(force == True):
                    overwrite = True
        global preexist_glob
        preexist_glob = preexist
        ## CEW edit 2018-09-14
        #if(preexist == 'move'):
        #	global preexist_glob
        #	preexist_glob = 'move'
        #	#print "preexist turned ON................"
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
    '''
    Main method for program. Basic consistency checks on input,
    controls experiment preexist settings.
    '''
    # First, check platform
    system, node, release, version, machine = os.uname()
    if(node.startswith('pp')):
        print "Running on PP(PAN) node", node
    if(node.startswith('an')):
        print "Running on AN(PAN) node", node
    else:
        print "\033[1;41mERROR code -5: Running on a workstation not tested for end-to-end runs yet. Please log in to analysis nodes to run expergen. \033[1;m",node
        sys.exit(-5)

    # Then, check environment variables
    basedir = os.environ.get('BASEDIR')
    if(basedir is None):
        print("Error in expergen: no BASEDIR set! Is a fudge version checked out?")
        sys.exit(1)
    fudgetag = os.environ.get('FUDGETAG')
    env_file = os.environ.get('FUDGE_ENV')
    if fudgetag is None:
        print("Error in expergen: FUDGETAG must be set! Please load a module or check out a tag.")
        sys.exit(1)

    use = '''
          Expergen usage (two cases:)
          expergen -i path_to_xml.xml [--sbatch --run-watcher --mail --sbatchmail]
          expergen -a path_to_aux.json [-a path_to_aux.yml --sbatch --run-watcher --mail --sbatchmail]
          '''
    parser = OptionParser(usage=use)
    parser.add_option("-i", "--file", dest="uinput", default='none',
                      help="Path to XML file used as input. Should not be set if --aux is used.", metavar="FILE")
    parser.add_option("-a", "--aux", default="none",dest="auxinput",
                      help="pass location of auxilary JSON/YAML alternative input specifications. Not expected for production use yet; if set, no xml should be used.")
    parser.add_option("--sbatch",action="store_true",default=False,
                      help="Automatically submit the master runscripts using SLURM's sbatch command. Default is set to FALSE")
    parser.add_option("-m", "--mail", action="store",
                     dest="mail", default="none",
                     help="Pass user email for notifications from tools like postProc (this is optional)")
    parser.add_option("--sbatchmail", action='store_true', dest='msubmail',
                      default=False, help="Should email notifications be sent from the master scripts sent to the PP system?")
    parser.add_option("--statmail", action='store_true', dest='statmail',
                      default=False, help="Should email notifications upon step completion be sent from the FUDGE workflow?")
    parser.add_option('--run-watcher', action='store_true', dest='run_watcher',
                       default=False,
                       help="Whether to run the watcher script directly after expergen completes. Not expected for use in production, but useful for development and running synthetic data experiments.")
    parser.add_option('--not-strict-metadata', action='store_true', default=False,
                      dest='not_strict', help="Whether to continue with a FUDGE experiment if the metadata parser could not get the attributes needed to classify input from the netcdf files. Defaults to false, which exits with an error if classification fails. For csv files this option is effectively always set, since csv files don't have the metadata we use.")
    (options, args) = parser.parse_args()

    if(options.mail=="none"):
        try:
            NEMSemail1 = os.environ["NEMSemail"]
        except KeyError:
            print "-m option unused and NEMSemail environmental var not set. If you want email notifications, enter them manually."
            NEMSemail1 = 'none'
    else:
        NEMSemail1 = options.mail
    print "Status emails will be sent to: " + NEMSemail1
    msub = options.sbatch
    print "Will jobs be automatically submitted? " + str(msub)
    if (options.run_watcher and not options.sbatch):
        print("Error in expergen: option run-watcher should only be used with msub!")
        sys.exit(1)
    # xml and aux input specifications
    inputs = [options.uinput, options.auxinput]
    if all([el is "none" for el in inputs]):
        print("Error in expergen: one of -i or --aux must be specified!")
        sys.exit(1)
    elif all([el is not "none" for el in inputs]):
        print("Error in expergen: only one of -i or --aux should be set!")
        sys.exit(1)

    parse_xml = False
    create_default_aux = False
    is_xml = re.search('xml$', options.uinput) is not None
    parse_xml = is_xml
    is_json = re.search('json$', options.auxinput) is not None
    #is_yaml = any([re.search(el, options.auxinput) for el in ['yaml$', 'yml$'] is not None])
    if not is_xml and not is_json:
        print("Error in expergen: either --input was not xml or --aux was not json!")
        sys.exit(1)

    if is_xml and options.uinput != "none":
        if osp.exists(options.uinput):
            print("XML input: " + options.uinput)
            # TODO: parse this for preexist settings
            tmp = listVars(options.uinput, basedir, msub)
            outdir = tmp[27]
            outdirs,rip_list = parse_outdirs_from_xml(outdir)
            #Obtain rip information from xml
            scriptdir = tmp[54]
            # targvar = tmp[25] #This is never used right now
            is_csv = False
            # I don't remember why this is passed as a global.
            # It's bad, but it goes away soon.
            preexist = preexist_glob
            # Check XML for aux specs
            specdict = get_comment_specs(options.uinput)
            auxfile = specdict['aux']
            if auxfile != "none":
                if(os.path.exists(auxfile)):
                    create_default_aux = False
                    auxfile = os.path.abspath(auxfile)
                    '''
                    #auxinfo is selective reads, auxdict is entire, messy thing
                    auxinfo, auxdict = parse_aux_info(auxinput)
                    if 'file_format' in auxdict.keys():
                        if auxdict['file_format'] == 'csv':
                            is_csv = True
                    '''
                else:
                    print("Error in expergen: aux input from xml, " + auxfile +
                          " does not exist on this filesystem!")
                    print("Please use --aux input for other aux or remove from xml.")
                    sys.exit(1)
            else:
                create_default_aux = True
                is_csv = False
        else:
            print("Error in expergen: xml input " + options.uinput + " was " +
                  " badly formed or not found. Please check input!")
            sys.exit(1)

    if osp.exists(options.auxinput):
        # Note: this can be in json or yaml
        print("Aux input: " + options.auxinput)
        print("Aux will be used for all inputs; DO NOT CALL POSTPROC")
        auxfile = options.auxinput
        auxinfo, auxdict = parse_aux_info(options.auxinput)
        outdirs = auxdict['DS']['out_dir']
        rip_list = auxdict['DS']['rips']
        if re.search("<RIP>", outdirs) is not None:
            outdirs = [re.sub("<RIP>", el, outdirs) for el in rip_list]
        else:
            outdirs = [outdirs]
        scriptdir = auxdict['script_settings']['scriptdir']
        preexist = auxdict['cleanup_settings']['if_preexist']
        is_csv = True if auxdict['file_format'] == 'csv' else False
    elif options.auxinput is not "none":
        print("Error in expergen: aux input " + options.auxinput + " was " +
              " not found. Please check input!")
        sys.exit(1)

    #CEW edits 6-30 for msub mail verbosity settings
    msubmail = 0
    if options.msubmail:
        msubmail = 1
    statmail = options.statmail

    #Finally, clean experiments if they already exist
    ##clean_prior_experiments(uinput, sd, basedir, preexist_glob, is_csv)
    clean_prior_experiments(outdirs, scriptdir, rip_list, basedir, preexist, is_csv)
    # Ask MJN: are these settings still used?
    #njobs = int(specdict['njobs'])
    #ncpus = int(specdict['ncpus'])
    generate_experiment(parse_xml, create_default_aux,
                        options.uinput, auxfile, NEMSemail1,
                        basedir, fudgetag, env_file, msub, scriptdir,
                        msubmail, statmail, options.run_watcher, options.not_strict)


def generate_experiment(parse_xml, create_default_aux,
                        uinput, auxinput, NEMSemail1,
                        basedir, fudgetag, env_file, msub,
                        sbase, msubmail, statmail, run_watcher,
                        not_strict_metadata):
    '''
    After the consistency checks, parses config (xml or aux) to get params.
    Then, generates master and runscripts, submitting if needed.
    '''
    #First, create experiment directories. Note that this assumes
    #you've cleaned up your experiment beforehand:
    subdirs = ['master', 'runscript', 'runcode', 'log','postProc', 'config', 'status', 'profile']
    for el in subdirs:
        subdir = osp.join(sbase,el)
        if not osp.exists(subdir):
            os.makedirs(subdir)

    if parse_xml:
        output_grid,kfold,lone,region,fut_train_start_time,fut_train_end_time,file_j_range,hist_file_start_time,hist_file_end_time,hist_train_start_time,hist_train_end_time,lats,late,lons,late, basedir,method,target_file_start_time,target_file_end_time,target_train_start_time,target_train_end_time,spat_mask,fut_file_start_time,fut_file_end_time,predictor,target,params,outdir,dversion,dexper,target_scenario,target_model,target_freq,hist_scenario,hist_model,hist_freq,fut_scenario,fut_model,fut_freq,hist_pred_dir,fut_pred_dir,target_dir,expconfig,target_time_window,hist_time_window,fut_time_window,tstamp,ds_region,target_ver,auxcustom,qc_mask,qc_varname,qc_type,adjust_out,sbase,pr_opts,masklists,idirsuffix= listVars(uinput,basedir,msub)

        #This is a bit of a hack, but I think I need this here to account for multi being non-global in the other branch
        outdirs,rip_list = parse_outdirs_from_xml(outdir)
        multi = 'on' if len(rip_list) > 1 else 'off'

        #Need to do this after dirs are created - interferes with the process
        #of cleaning up experiments otherwise.
        njobs = 8
        ncpus = 2
        if create_default_aux:
            print('making default aux file')
            auxinput = make_aux_input(output_grid, sbase, target)
            #auxinfo is selective reads, auxdict is entire, messy thing
        auxinfo,auxdict = parse_aux_info(auxinput)
        #If aux file is not default, aux file overrides XML parameters
        if auxinfo['njobs'] != 8:
            njobs = auxinfo['njobs']
        if auxinfo['ncpus'] != 2:
            ncpus = auxinfo['ncpus']
        auxdict = add_ds_params_to_auxdict(auxdict, njobs, ncpus,
                                           expconfig, kfold, method, params,
                                           output_grid, ds_region)
        ds_info = get_ds_info(method, basedir)
        auxdict = add_s3_s5_params_to_auxdict(auxdict, masklists, pr_opts, method, ds_info, target)
        # Set up vars needed for runscript creation
        oh_timerange = str(target_file_start_time) + "-" + str(target_file_end_time)
        mh_timerange = str(hist_file_start_time) + "-" + str(hist_file_end_time)
        mf_timerange = str(fut_file_start_time) + "-" + str(fut_file_end_time)
        predlist = predictor.split(",")
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
        print("Calling create_xml_runscripts")
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
        print("Step 0: Create the unified runcscripts and master runscripts")
        create_xml_runscripts(lons, lone, i_subset, i_offset,
                              lats, late, j_subset, j_offset,
                              njobs, ncpus, fudgetag, sbase,
                              target_dir, hist_pred_dir, fut_pred_dir,
                              oh_timerange, mh_timerange, mf_timerange, predlist,
                              spat_mask, target_time_window, hist_time_window,
                              fut_time_window, auxcustom, outdir, target, method,
                              file_j_range, idirsuffix,
                              ds_info['multivar_target'],
                              auxinput, auxinfo['copy_files'], auxinfo['wallclocktime'],
                              auxdict, ds_minifile, expconfig, not_strict_metadata)
    else:
        # read in auxdict, which can be in json or yaml
        if re.search('json$', auxinput) is not None:
            with open(osp.abspath(auxinput), 'r') as f:
                auxdict = json.load(f)
        else:
            print("Error in expergen: non-json input not supported until the helpdesk ticket is done")
            sys.exit(1)
            '''
            with open(auxinput, 'r') as f:
                auxdict = yaml.load(f)
            '''
        #TODO: add an auxdict consistency-checking function
        check_aux_consistency(auxdict)
        ds_info = get_ds_info(auxdict['ds_method'], basedir)
        i_subset = auxdict['i_subset']
        j_subset = auxdict['j_subset']
        i_s,i_e,i_off,j_s,j_e,j_off = get_grid_params(auxdict['output_grid'], basedir)
        #Set expconfig for use in the final print
        expconfig = auxdict['ds_experiment_name']
        # Setting 'multi' for use later in the workflow; this is meant to be a temporary hack
        multi = 'on' if len(auxdict['DS']['rips']) > 1 else 'off'
        # same for the pp_wallclocktime - this should get added to the aux dict
        auxinfo = {'pp_wallclocktime': '5:00:00'}
        print("Step 0: Create the unified runcscripts and master runscripts")
        create_auxfile_runscripts(i_s, i_e, i_subset, i_off,
                                  j_s, j_e, j_subset, j_off,
                                  auxdict, auxinput, sbase, fudgetag,
                                  not_strict_metadata)

    # copy xml to configs dir
    cdir = sbase+"/config/"
    if uinput is not "none":
        shutil.copy2(uinput, cdir)
    shutil.copy2(auxinput, cdir)
    shutil.copy2(env_file, sbase + "/environment_info.csh")
    ################ step 6 fudgeList invocation ##############################################
    slogloc = sbase+"/"+"experiment_info.txt"
    fcmd = "python "+basedir+"/bin/fudgeList.py -f -i "+uinput+" -o "+slogloc
    if uinput is not "none":
        indirs = [target_dir, hist_pred_dir, fut_pred_dir]
    else:
        indirs = []
        dnames = ['OH', 'MH', 'MF']
        for d in dnames:
            indirs += [osp.dirname(auxdict[d][el]['file']) for el in auxdict[d].keys()]
    if any([re.search('Pwl', el) is not None for el in indirs]):
        fcmd += ' --parse-j-style'
    f = subprocess.Popen(fcmd, stdout=subprocess.PIPE, shell=True)
    out, err = f.communicate()

    auxloc = sbase + "/config/" + osp.basename(auxinput)
    shutil.copy2(auxinput, auxloc)

    with open(slogloc, 'a') as f:
        f.write("\naux:" + os.path.abspath(auxinput))
        f.write("\nNEMSemail:" + NEMSemail1)

############### crte ppscript #################
    print "Step 4: --------------PP postProc SCRIPT GENERATION-----------------------"
    #print " ========================="
    dev = "off"
    msubwrapper = []
    #CEW edit 6-30 for better status file monitoring when queue is full
    msubwrapper.append("rm -f " + sbase + "postProc/ppStatus/*")
    print("multi output (Writing to multiple output directories) ...." + multi)
    mail_command = ""
    msub_mail_command = ""
    if (NEMSemail1!='none'):
     if msubmail==1:
         mail_command = "-m " + NEMSemail1 + " "
         msub_mail_command = "-m ae -M "+ NEMSemail1 + " "

    #print(sbase+"/postProc/aux/")
    '''
    if (multi == "on"):
     suffix = ['suff1','suff2','suff3']
     listrips = []
    else:
     suffix = ['']
     listrips = ['na']
     '''
    if parse_xml:
         listrips = [copyprop]
         listrips = copyprop.split(",")
    else:
         listrips = auxdict['DS']['rips']

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

    if parse_xml:
        print("Making postproc scripts")
        make_postProc_scripts(listrips, sbase, fudgetag, NEMSemail1, auxinfo['pp_wallclocktime'])
        print "----------------------------------------"
        print "\033[1;42m You can also use this sbatchwrapper script that launches all the post-processing(postProc) jobs for you in case of multi-ensemble output. If it's a single output file, you may still use this wrapper script \033[1;m", sbase + "/postProc/msubwrapper"
        print "NOTE: postProc will succeed only if you're running the model for the full downscaled region. (it will fail if you're running downscaling for a single slice for example)"
    else:
        print("NOT making postProc scripts!!")
        print("This experiment was run from an aux file; the postProc workflow needs the experiment_info.txt file")
        print("and there is currently no good substitution in the auxfile workflow!")
        print("However, for the csv workflow (which is the primary reason for using this right now)")
        print("you wre not going to use postProc anyway.")
################ step 5 job submission (if applicable) ##############################################
    print "Starting step 5: job submission"
    if msubmail == 1 and NEMSemail1 != 'none':
            msub_string = "sbatch --mail-type END --mail-user " + NEMSemail1 + " "
    else:
        msub_string = "sbatch "
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
        print "--sbatch not set on command line. To submit jobs at a later point, please use the following:"
        print "\033[1;95m"+msub_cmd+"\033[1;m"
    if parse_xml:
        print "Upon submission of downscaling batch jobs, please use the following watcher script command to watch for DS output from this and submit the postProc PP jobs automatically \n -- Use watcher interactively from AN nodes and load fudge module while doing so to set the environment. \n -- Use watcher -h for a complete list of command-line options including verbosity   "
        if(NEMSemail1 != "none"):
            print "\033[1;42mwatcher -i "+(str)(os.path.abspath(slogloc))+" -m "+NEMSemail1+"\033[1;m"
        else:
            print "\033[1;42mwatcher -i "+(str)(os.path.abspath(slogloc))+"\033[1;m"
    else:
        print("Path to master script for experiment:")
        mdir = osp.join(sbase, 'master')
        mpath = osp.join(mdir, os.listdir(mdir)[0])
        print("\033[1;42m" + mpath + "\033[1;m")

    print("It is possible to cancel all jobs belonging to this experiment with the following command:")
    print("\033[1;42mscancel -u $USER -n " + expconfig + "\033[1;m")

    #Script exit options - needs to exit with 0
    if run_watcher:
        if parse_xml:
            print("Invoking watcher script immediately")
            expinfo = sbase + "/experiment_info.txt"
            expdict = read_exp_info_file(expinfo)
            #Note: check on this when mailid is not set. Is the scrip smart enough to parse it?
            runWatcher(expinfo,0,1,120,180,NEMSemail1,1, expdict)
        else:
            print("run_watcher option is not compatible with the csv workflow yet!")
            print('There is not a good substitute for the experiment_info file in the FUDGE workflow.')
            print('However, you CAN run this experiment in debug mode if you want to.')
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
    #_ensbl is stuck to the end of tend, when it takes place
    tend_split = re.split("_", tend)
    tend = tend_split[0]
    tstring = str(tstart) + "-" + str(tend)
    root_filename = "_".join([var, freq, exp_name, scen_split[0], rip, region, tstring])
    suffix = ".I<ISTART>-<IEND>_J<JSTART>-<JEND>.nc"
    mini_name = root_filename + suffix
    return(mini_name)

if __name__ == '__main__':
    main()
