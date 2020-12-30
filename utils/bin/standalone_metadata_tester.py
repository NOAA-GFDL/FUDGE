
#!/usr/local/bin/python
#Stand-alone FUDGE metadata tester

import os,sys,stat,shutil,subprocess,time
from os import path as osp
import numpy as np
from subprocess import PIPE
import re, json, csv, netCDF4, collections
from netCDF4 import Dataset
import collections
from collections import OrderedDict
import argparse
import shlex
import smtplib
import optparse
from optparse import OptionParser
import configparser
import traceback
import netCDF4
from netCDF4 import Dataset
global BASEDIR
BASEDIR = os.environ.get('BASEDIR')
if BASEDIR is None:
    print("Error in metadata setup: no version of fudge is checked out! Please check out a jamoca-or-later version of fudge.")
    sys.exit(1)
sys.path.append(osp.join(BASEDIR, 'utils/aux'))
from gcp import gcp
sys.path.append(osp.join(BASEDIR, 'utils/bin'))
from get_ds_info import make_aux_input, get_ds_info
from get_ds_info import add_ds_params_to_auxdict,add_s3_s5_params_to_auxdict
from metadata_classify_fxns import get_experiment_metadata
sys.path.append(osp.join(BASEDIR, 'bin'))
import expergen
from expergen import listVars

def parseScriptArgs():
    #Input parsing and arg checking for main body of code
    usage = "usage: %prog -i xml_input -a aux_input -o output [--verbose]"
    parser = OptionParser(usage=usage, version='test_metadata_1.0')
    parser.add_option("-i", "--input", dest="input", default='none', help="Fudge-compatible XML file which will be used in a FUDGE experiment. If there is an aux in that file, it's ignored.")
    parser.add_option("-a", "--aux_input", dest="aux_input", default='none', help="Fudge-compatible aux file, as might be written to config_mod directory. Meant to run without expergen parsing.")
    parser.add_option("-o", "--output", dest='output', help="Output file to which to dump json.")
    parser.add_option("--is-standalone", action='store_true', default=False, dest="is_standalone", help="Whether the tool is running as the standalone metadata tester. If set, netcdf will be copied to $TMPDIR before opening.")
    parser.add_option("--is-strict", dest='is_strict', action='store_true', default=False, help="Whether the strictest standard of metadata parsing should be used for the tool. If set, errors in metadata classification will lead to a failure of the tool.")
    parser.add_option("--metadata-table", dest='meta_table', action='store',
                      default='none',
                      help="What path to use for the metadata lookup table. Defaults to the path within the checked-out repo.")
    parser.add_option("--verbose", action="store_true", dest="verbose", default=False, help="Print extra status messages used to help debugging. Defaults to False.")
    (options, args) = parser.parse_args()
    if not options.output:
        parser.error("--output specification is required!")
    if options.aux_input == 'none':
        #Check that input file is xml
        #parse XML
        #make rudimentary aux file
        #parse rudimentary aux file, getting auxdict
        #add ds params to auxdict
        auxdict = {}
        auxdict = add_ds_params_to_auxdict(auxdict, njobs, ncpus,
                                           expconfig, kfold, method, params,
                                           output_grid, ds_region)
        #add s3/s5 params to auxdict
        auxdict = add_s3_s5_params_to_auxdict(auxdict, masklists, pr_opts, method, ds_info, target)

        #And then add the file path parameters.
        auxdict = add_input_file_params(auxdict, other_long_param_list)
    else:
        with open(options.aux_input, 'r') as f:
            auxdict = json.load(f)
        ds_info = get_ds_info(auxdict['ds_method'], BASEDIR)
    #make_fudge_metadata takes as its args a modified auxdict - i.e. an auxfile
    #with all the stuff that the R code would need to run in it.
    make_fudge_metadata(auxdict, ds_info, options.output,
                        is_running_standalone = options.is_standalone,
                        is_strict = options.is_strict, metadata_table = options.meta_table,
                        verbose=options.verbose)

def make_fudge_metadata(auxdict, ds_info, meta_file, is_running_standalone = False, is_strict = False, metadata_table = "none", verbose=False):
    '''
    Parses and builds fudge metadata attributes, given a list of config parameters.
    General structure looks like this:
    dict:
      global_att1: att1
      global_att2: att2
      by_vars:
        tasmax:
          comment: "comment for tasmax"
        tasmax_qcmask:
          comment: "comment for tasmax_qcmask"
    '''
    if metadata_table == 'none' or re.search('BASEDIR', metadata_table) is not None:
        BASEDIR = os.environ.get('BASEDIR')
        if BASEDIR is None:
            print("Error in metadata setup: no --metadata-table is specified and no version of fudge is checked out! Please check out fudge or point to a metadata table!")
            sys.exit(1)
        else:
            metadata_table = osp.join(BASEDIR, "utils/aux/metadata_table.csv")
    #Initial check on whether "strict" parsing is appropriate
    if auxdict['file_format'] == "csv":
        if is_strict:
            print("Error in make_fudge_metadata: strict parsing is meaningless with csv files! Please turn off is_strict option")
            sys.exit(1)
    pred_dict = {}
    #This needs a provision for a qcmask of some kind
    #qc gets several special atts:
    #comment, longname
    #for v in auxdict$MH.keys():
    #    pred_dict[v] = {'none': None}
    gd = OrderedDict(); pd = OrderedDict(); vd = OrderedDict();
    title = "" #initializing title here, because title gets added sooner
    gd['title'] = title
    gd['institution'] = "NOAA/GFDL (201 Forrestal Rd, Princeton, NJ, 08540)"
    gd['institute_id'] = "NOAA/GFDL"
    gd['comment'] = "" #same deal for comment
    ##I think this should include this, but its not there yet
    ##gd['references'] = ds_info['reference']
    gd['realizations'] = ",".join(auxdict['DS']['rips'])
    #This is not included in the struct - should it be?
    fudgetag = os.environ.get('FUDGETAG')
    gd['fudgetag'] = fudgetag
    gd['ds_downscaling_method'] = auxdict['ds_method']
    ## Similar false positive issue compared to the one below
    # gd['ds_experiment_name'] = auxdict['ds_experiment_name']
    p0 = subprocess.Popen("R --version", shell=True, stdout=PIPE, stderr=PIPE)
    out0,err0 = p0.communicate()
    # I'm turning off R and python version for the moment, since they
    # create too many false positives when doing comaprisons.
    gd['r_version'] = re.split(" --", out0)[0]
    # gd['python_version'] = re.split("\n", sys.version)[0]
    #Filenames of data
    gd['filenames_OH'] = get_filename_atts(auxdict['OH'])
    gd['filenames_MH'] = get_filename_atts(auxdict['MH'])
    gd['filenames_MF'] = get_filename_atts(auxdict['MF'])
    #add the atts that are a dataset-dependent parse on metadata atts
    '''
    prime_var = [k for k in auxdict['MF'].keys() if auxdict['MF'][k]['is_prime']]
    #All of this needs to be checked for working with a non-stated prime var
    if len(prime_var) != 1:
        print(prime_var)
        print("Error in prime_var search: should only match one!")
        sys.exit(1)
    '''
    prime_var = list(auxdict['OH'].keys())[0]
    OH_file = auxdict['OH'][prime_var]['file']
    MH_file = auxdict['MH'][prime_var]['file']
    MF_file = auxdict['MF'][prime_var]['file']
    gd, pd = get_experiment_metadata(gd, pd, OH_file, MH_file, MF_file,
                                     auxdict['ds_experiment_name'],
                                     running_standalone=is_running_standalone,
                                     is_strict=is_strict,
                                     config=metadata_table, verbose=verbose)
    #Get the info options
    gd['ds_downscaling_method'] = ds_info['ds_method']
    gd['info_ds'] = generate_fxn_arglist({'d1': {'type': ds_info['ds_method'],
                                         'args': auxdict['ds_args']}}, True)
    gd['info_pre_ds'] = generate_fxn_arglist(merge_dicts(auxdict['s3']['outloop'], auxdict['s3']['inloop']))
    gd['info_post_ds'] = generate_fxn_arglist(merge_dicts(auxdict['s5']['inloop'], auxdict['s5']['outloop']))
    gd['info_tmask'] = get_tmask_atts(auxdict)
    gd['info_missval'] = "After removing missing values with the " +\
                         ds_info['missval_method'] + " method, inputs had at least " +\
                         str(auxdict['minsamplesize']) + " non-missing value(s);"
    gd['info_stencils'] = get_stencil_atts(auxdict)
    gd['info_ds_xy_incomplete'] = auxdict['ds_handle_missing']
    predictor_vars = ",".join(list(auxdict['MF'].keys()))
    train_string = ds_info['train_datasets']
    fut_rips = ",".join(auxdict['DS']['rips'])
    gd['comment'] = make_global_comment(auxdict['ds_method'], prime_var,
                                        auxdict['ds_region'],
                                        auxdict['output_grid'],
                                        auxdict['kfold'],
                                        auxdict['ds_experiment_name'],
                                        train_string, predictor_vars, fut_rips)
    #add timestamp of script creation
    exp_info = auxdict['script_settings']['scriptdir'].rstrip("/") + "/experiment_info.txt"
    gd['scriptgen_timestamp'] = time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime(osp.getmtime(exp_info)))
    gd['experiment_info'] = exp_info
    #Climatology and trend are more complicated - depending on the workflow,
    #they may or may not be present. For now, let's go off of the presence/status
    #of the MF dataset.
    gd = merge_dicts(gd, get_climatology_atts(auxdict, prime_var, is_running_standalone))
    #attributes by variable
    var_longname = ""
    vd[prime_var] = {"longname": var_longname}
    if auxdict['make_qcmask']:
        qc_var = prime_var + "_qcmask"
        qc_comment = make_qc_comment(auxdict['qc_method'], auxdict['ds_method'], BASEDIR)
        qc_longname = make_qc_longname(auxdict['qc_method'], auxdict['ds_method'], prime_var)
        global_comment = make_qc_global_comment(auxdict['ds_method'], auxdict['qc_method'], prime_var, auxdict['ds_region'], auxdict['output_grid'], auxdict['kfold'], auxdict['ds_experiment_name'], train_string, predictor_vars, fut_rips, str(auxdict['minsamplesize']))
        vd[qc_var] = {"longname": qc_longname,
                      "comment": qc_comment,
                      "globals": {"comment": global_comment}}
    #so now we've got 3 dicts: globals, provenance (of globals) and by var
    ad = OrderedDict()
    ad['globals'] = gd; ad['vars'] = vd; ad['provenance'] = pd;
    #And finally, write the whole thing to file
    print(("Wrting to: " + meta_file))
    meta_text = json.dumps(ad, indent=2)
    f = open(meta_file, 'w')
    print(meta_text, file=f)
    f.close()

def merge_dicts(x, y):
    #Helper function taken from stackoverflow
    z = x.copy()   # start with x's keys and values
    z.update(y)    # modifies z with y's keys and values & returns None
    return z

def generate_fxn_arglist(arglist, is_ds=False):
    #creates formatted list for keeping track of args to functions in sequence
    #we assume that arglist is an OrderedDict
    if len(arglist) > 0:
        ret = ";".join([make_arglist(arglist[k], is_ds) for k in list(arglist.keys())])
        return(ret)
    else:
        return("none specified")

def make_arglist(argd, is_ds):
    #Dict describing a s3/s5 adjustment or a ds method. Contains an args subdict
    #that describes the args the fxn takes.
    head = "" if is_ds else "fxn "
    argn = 'args' if is_ds else "qc_args"
    fxargs = argd[argn]
    argstring = ""
    type = "PR" if argd['type'] in ["pr_in", "pr_out"] else argd['type']
    if len(fxargs) > 0:
        argstring = head + argd['type'] + " : (" + ",".join([k + "=" + arg_tostring(fxargs[k]) for k in sorted(fxargs.keys())]) + ")"
    else:
        argstring = head + argd['type'] + " : (default settings)"
    return(argstring)


def arg_tostring(arg):
    # Converts a list or single el into a string:
    if type(arg) is list:
        ret = "(" + ",".join([str(k) for k in arg]) + ")"
    else:
        ret = str(arg)
    return(ret)


def get_climatology_atts(auxdict, var, is_running_standalone):
    #Gets the attributes that you'd need for writing trend/climatology metadata
    clim_source = auxdict['climo_source']
    if type(clim_source) is list:
        clim_source = clim_source[0]
    if clim_source == "MF":
        clim_file = auxdict['MF'][var]['file']
    elif clim_source == "OH":
        clim_file = auxdict['OH'][var]['file']
    else:
        print("Error in standalone_metadata_tester.py: climo_source was not set in the auxdict!")
        sys.exit(1)
    #this is probably present in tmpdir already, if calling from workflow;
    #otherwise, do a copy.
    if is_running_standalone:
        clim_source = gcp(clim_file)
    attdict = {}
    with Dataset(clim_file, 'r') as nc_obj:
        try:
            attdict['climatology'] = nc_obj.__dict__['climatology']
        except:
            print("No climatology found")
        try:
            attdict['trend'] = nc_obj.__dict__['trend']
        except:
            print("No trend found")
    return(attdict)

def get_filename_atts(data_struct):
    #Assumes that you have a dict with unknown keys, where each dict
    #entry has a key 'filename'
    #Takes an optional arg for setting the first element in the returned list
    #otherwise, sorts alphabetically
    f_out = []
    for k in list(data_struct.keys()):
        fname = data_struct[k]['file']
        if data_struct[k]['is_prime']:
            f_out = [osp.basename(fname)] + f_out
        else:
            f_out += [osp.basename(fname)]
    f_out.sort()
    return(",".join(f_out))

def get_tmask_atts(ds):
    #Works off of a subset dictionary - assumes that most of the
    #elements are dicts with a tmask att.
    keys = ['OH', 'MH', 'MF']
    tmasks_all = []
    miss_count = 0
    for k in keys:
        subset = ds[k]
        vars = list(subset.keys())
        tmasks = [subset[v]['tmask'] for v in vars]
        tmask_set = list(set(tmasks))
        if tmask_set[0] == 'na':
            miss_count += 1
        tmasks_all.append(k + ":" + ",".join([osp.basename(el) for el in tmask_set]))
    # Conditional statement for the indexing tmask
    if ds['DS']['indexing_tmask'] != "na":
        tmasks_all.append("trim : " + ds['DS']['indexing_tmask'])
    if miss_count == 3:
        return("none_used")
    else:
        return("; ".join(tmasks_all))

def get_stencil_atts(auxdict):
    #Works off of a subset dictionary for OH, MH, MF;
    #assumes we only use one stencil per dataset (which isn't always going to be true)
    keys = ['OH', 'MH', "MF"]
    stencil_list = []
    for k in keys:
        stencils = [auxdict[k][v]['stencil'] for v in auxdict[k]]
        for n in range(len(stencils)):
            if type(stencils[n]) is list:
                stencils[n] = stencils[n][0]
        stencils = list(set(stencils))
        stencil_list.append(k + ":" + ",".join(stencils))
    return(",".join(stencil_list))

def make_qc_longname(qc_method, ds_method, ds_var):
    #Makes a longname for the qc data (or more generally, s3/s5)
    if qc_method == 'qc.train':
        if ds_method == 'DeltaSD':
            qc_longname = "Delta of " + ds_var
        else:
            qc_longname = "Training qcmask for " + ds_var
    else:
        qc_longname = "QC mask of " + ds_var
    return(qc_longname)

def make_qc_comment(qc_method, ds_method, BASEDIR):
    #Makes the variable comment att for a qcmask (derived from lookup, mostly)
    print(BASEDIR)
    qc_metatable = osp.join(BASEDIR, 'Rsuite/aux/qcmask_metadata.csv')
    qc_comment = "1 = unadjusted; NA = adjusted"
    with open(qc_metatable, 'r') as f:
        freader = csv.reader(f, delimiter=",")
        for row in freader:
            if row[1] == qc_method:
                qc_comment = row[2]
    #qc.train (and only qc.train) is different for Delta
    if qc_method == 'qc.train':
        if ds_method == 'DeltaSD':
            qc_comment = "Delta used to generate DS output at a given x,y,t"
    return(qc_comment)

def make_global_comment(ds_method, var, region, grid, kfold, exp_name, train_data, predictor_vars, fut_rips):
    #Makes a global comment attribute.
    start = "Output produced by downscaling by the " + ds_method + " method "
    desc = "This is a downscaled estimate of " + var
    main = make_main_comment(region, grid, kfold, exp_name, desc, train_data, predictor_vars, fut_rips)
    return(start + main)

def make_qc_global_comment(ds_method, qc_method, var, region, grid, kfold, exp_name, train_data, predictor_vars, fut_rips, minsamplesize):
    #Makes global comment for qc vars; some switches based on qc method
    make_start = True
    if qc_method == 'qc.train':
        if ds_method == 'DeltaSD':
            make_start = False
            start = "Output to document the delta ratio applied at individual x,y,twindow locations "
        else:
            qc_str = "where 1=minsamplesize requirements (n>" + minsamplesize + ") met for " + train_data + " and missing indicates retry option invoked"
    elif qc_method == "tail.adjust":
        qc_str = "where 1=tail adjustment took place and missing indicates it did not"
    else:
        qc_str = qc_method
    if make_start:
        start = "Output produced from a QC mask " + qc_str + " performed upon " + ds_method + ' downscaled output '
    desc = "This is a QC check related to a downscaled estimate of " + var
    main = make_main_comment(region, grid, kfold, exp_name, desc, train_data, predictor_vars, fut_rips)
    return(start + main)

def make_main_comment(region, grid, kfold, exp_name, desc, train_data, predictor_vars, fut_rips):
    #Makes the main body of the comment string
    main = "on the subset " + region + " of the " + grid + " grid, "
    kfold = "and based on " + str(kfold) + "-fold cross-validation, "
    config = "with experiment configuration " + exp_name + '. '
    train = " trained on " + train_data + " using predictor variable(s) " + predictor_vars + " and future predictor rips " + fut_rips
    return(main + kfold + config + desc + train)

def add_input_file_params(auxdict, lots_of_other_params):
    '''
    Tweaked code from create_all_runscripts; just returns the input file paths.
    '''
    is_syn = False
    if idirsuffix=="/grid0/":
        print("Adding synthetic data workaround for grid0")
        idirsuffix = idirsuffix + "/ZeroD/"
        j_elements  = [1, 2]
        is_syn = True
    if idirsuffix=='/X1-1/':
        print("Adding other synthetic data workaround for X1-1")
        idirsuffix = idirsuffix
        j_elements  = [1, 2]
        is_syn = True
    #first, checking for the presence of the optional files
    spatmask_present = (spatmask != 'na')
    tmask_present = (OH_tmask != 'na')
    trim_time_mask_present = (trim_tmask != 'na')
    #do all of your dmgets (requests for files on tape) in the script at once
    #it is easier on the infrastructure if they are all done at once, since
    #odds are some of the files we want are on the same tapes
    if spatmask_present:
        #Need the file located at one level up from the spatmask dir -
        #spatmask itself points only to the OneD dir
        spatmask_dir = osp.dirname(osp.abspath(spatmask))
        spatmask_file = [osp.join(spatmask_dir, el) for el in os.listdir(spatmask_dir) if re.search('masks', el) is not None][0]
        spatmask_files = [spatmask_file]
        spatmask_path = spatmask_file
    else:
        spatmask_files = []
        spatmask_path = 'none'
    #Get presence of OH,MH,MF files
    prefix_loc = len(MH_dir.split("/"))
    oh_prefix = "/".join(OH_dir.split("/")[0:prefix_loc])
    oh_suffix = "/".join(OH_dir.split("/")[(prefix_loc+1):-1])
    if not multivar_target:
        oh_vars = [target_var]
        if not is_syn:
            oh_files,_ = build_xml_input_paths(oh_vars, oh_prefix, oh_timerange, "/" + oh_suffix, False)
        else:
            oh_files,_ = build_xml_input_paths(oh_vars, oh_prefix, oh_timerange, idirsuffix, False)
    else:
        #Only triggered by MBC; may change later
        oh_vars = predlist
        oh_files,_ = build_xml_input_paths(predlist, oh_prefix, oh_timerange, idirsuffix, True)
    mh_files,_ = build_xml_input_paths(in_vars=predlist, in_dir=MH_dir, trange=mh_timerange, suffix=idirsuffix, is_multivar=True)
    mf_files,_ = build_xml_input_paths(in_vars=predlist, in_dir=MF_dir, trange=mf_timerange, suffix=idirsuffix, is_multivar=True)
    #Set placeholder hooks for the is_prime and is_fixed qualities for the vars
    oh_prime = oh_fixed = [False for el in oh_vars]
    mh_prime = mh_fixed = [False for el in predlist]
    mf_prime = mf_fixed = [False for el in predlist]
    #Add modified aux copy & structure addition
    oh_auxdict = make_aux_entry(oh_vars, oh_files, oh_prime, oh_fixed,
                                stencil=auxdict['OH']['stencil'],
                                geomask=spatmask_path, tmask=OH_tmask)
    mh_auxdict = make_aux_entry(predlist, mh_files, mh_prime, mh_fixed,
                                stencil=auxdict['MH']['stencil'],
                                geomask=spatmask_path, tmask=MH_tmask)
    mf_auxdict = make_aux_entry(predlist, mf_files, mf_prime, mf_fixed,
                                stencil=auxdict['MF']['stencil'],
                                geomask=spatmask_path, tmask=MF_tmask)
    ds_auxdict = make_ds_aux_entry(ds_minifile, outdir_string, oh_vars[0], tmask=trim_tmask)
    auxdict['OH'] = oh_auxdict
    auxdict['MH'] = mh_auxdict
    auxdict['MF'] = mf_auxdict
    auxdict['DS'] = ds_auxdict
    return(auxdict)

if __name__ == '__main__':
    parseScriptArgs()
