#!/bin/python
#Gets downscaling method information from the downscaling method spreadsheet.
#ATM all we need for expergen is whether or not the method is going to need
#a leapday mask copied as part of the master runscript setup

import csv
import os,sys
from os import path as osp
import json
import re
import collections
from collections import OrderedDict

def get_ds_info(ds_method, FUDGEROOT):
    #Reads the ds lookup table
    ds_table = osp.join(FUDGEROOT, "Rsuite/aux/ds_att_sheet.csv")
    with open(ds_table, 'r') as dstab:
        reader = csv.DictReader(dstab)
        ds_match = False
        downscale_oh = False
        for row in reader:
            if row['method_name'] == ds_method:
                ds_match = True
                downscale_oh = (row['downscale_oh']=='TRUE')
                multivar_target = (row['multivar_target']=='TRUE')
                multivar_possible = (row['multivar_possible']=='TRUE')
                adjust_pr_in_window = (row['adjust_pr_in_window']=='TRUE')

    if ds_match is False:
        print("Error in get_ds_info: Method " + ds_method + " not found in ds_att_sheet.csv!")
        sys.exit(1)
    else:
        return({'ds_method':ds_method,
                'copy_leapmasks':downscale_oh,
                'multivar_target':multivar_target,
                'multivar_possible':multivar_possible,
                'adjust_pr_in_window': adjust_pr_in_window})

def parse_aux_info(jsonfile):
    #Reads the aux file; returns bits that we are interested in
    print("Parsing aux file")
    print(jsonfile)
    with open(jsonfile, 'r') as jsonfile:
        jsondict = json.load(jsonfile, object_pairs_hook=OrderedDict)
        #wallclock time
        if 'wallclocktime' in jsondict.keys():
            #R's json converter helpfully wraps everything in lists.
            if isinstance(jsondict['wallclocktime'], list):
                wallclocktime=jsondict['wallclocktime'][0]
            else:
                wallclocktime = jsondict['wallclocktime']
        else:
            wallclocktime='6:00:00'
        #pp wallclocktime
        if 'pp_wallclocktime' in jsondict.keys():
            if isinstance(jsondict['pp_wallclocktime'], list):
                pp_wallclocktime=jsondict['pp_wallclocktime'][0]
            else:
                pp_wallclocktime = jsondict['pp_wallclocktime']
        else:
            pp_wallclocktime = '6:00:00'
        #njobs and ncpus parameters:
        if 'njobs' in jsondict.keys():
            njob_tmp = jsondict['njobs']
            if isinstance(njob_tmp, list):
                njobs = njob_tmp[0]
            else:
                njobs = njob_tmp
        else:
            njobs = 8
        if 'ncpus' in jsondict.keys():
            ncpus_tmp = jsondict['ncpus']
            if isinstance(ncpus_tmp, list):
                ncpus = ncpus_tmp[0]
            else:
                ncpus = ncpus_tmp
        else:
            ncpus = 2
        #Subsets in i and j that we are interested in
        defaultdict = {'i_subset': 1, 'j_subset': 'all', 'tile_olap':0}
        subsetdict = {}
        for k in defaultdict.keys():
            if k in jsondict.keys():
                tmp_k = jsondict[k]
                if isinstance(tmp_k, list):
                    subsetdict[k] = tmp_k[0]
                else:
                    subsetdict[k] = tmp_k
            else:
                #default if not present in json
                subsetdict[k] = jsondict[k]
        #files that can be copied
        copy_elements = ['stencil', 'geomask']
        copy_files = []
        dataset_names = ['OH', 'MH', 'MF']
        for data_name in dataset_names:
            for filetype in copy_elements:
                if filetype in jsondict[data_name].keys():
                    filetmp = jsondict[data_name][filetype]
                    #R's json converter helpfully wraps everything in lists.
                    if isinstance(filetmp, list):
                        filetmp = filetmp[0]
                    if filetmp != 'none':
                        copy_files.append(filetmp)
        if len(copy_files) > 1:
            copy_files = list(set(copy_files))
    return({'copy_files':copy_files, 'wallclocktime':wallclocktime,
            'pp_wallclocktime':pp_wallclocktime,
            'subsetdict':subsetdict, 'njobs':njobs, 'ncpus':ncpus}, jsondict)

def make_aux_input(grid, script_base, targ_var):
    #Creates an aux info file with g-or-earlier default settings: minifiles,
    #no stencils, etc.
    #As of 2017-07-18, also checks for modified aux settings and objects
    #if trying to implement default using them.
    print("Making default aux file")
    auxdict = OrderedDict()
    jdict = {'US48' : 114,
             'SCCSC0p1'  : 140,
             'RR'   : 140,
             'grid0': 1,
             'X1-1' : 1,
             'NEUSA': 41,
             'ESTCPconus': 114}
    if grid in jdict.keys():
        jv  = str(jdict[grid])
        aux_scriptname = script_base + "/default_mini_"+grid+".json"
        print(aux_scriptname)
    else:
        print("Error in make_aux_info: grid, " + grid + " not in jdict!")
        sys.exit(1)

    auxdict['grid'] = grid
    auxdict['i_subset'] = 1
    auxdict["j_subset"] = jv
    auxdict["tile_olap"] = 0
    auxdict["ds_handle_missing"] = "continue"
    auxdict["wallclocktime"] = "6:00:00"
    auxdict["pp_wallclocktime"] = "6:00:00"
    auxdict["files_in_aux"] = "FALSE"
    auxdict["file_format"] = "netcdf"
    if re.search('Anom', targ_var) is not None:
        print("Error in make_aux_input: if using an anomaly var " + targ_var + \
              " user specification of climo_source in aux file is required!")
        sys.exit(1)
    else:
        auxdict["climo_source"] = "MF"
    #For now, we're keeping the initialization for these here -
    #it simplifies some of the copying workflow later.
    file_types = ["OH", "MH", "MF"]
    filedict = OrderedDict(stencil='none', geomask='none')
    for f in file_types:
        auxdict[f] = filedict

    #Most of the specs are not going to change unless you use stencils
    #...which is why it's probably safe to set the default wallclocktime
    #to 2 hours rather than 12, as we have been using forever.
    aux_text = json.dumps(auxdict, indent=4)
    f = open(aux_scriptname, 'w')
    print >> f, aux_text
    f.close()
    return(aux_scriptname)

def add_ds_params_to_auxdict(auxdict, njobs, ncpus, experiment_name, kfold,
                             ds_method, ds_arg_string,
                             output_grid, ds_region):
    #Function for parsing and adding ds_experiment_like stuff to the
    #auxdict. This is not intended to be read in from a properly-formatted
    #files_in_aux auxfile at this time; that step comes later.
    #auxdict is an OrderedDict; keep other formatting in mind.
    auxdict["njobs"] = njobs
    auxdict["ncpus"] = ncpus
    auxdict["ds_experiment_name"] = experiment_name
    dsdict = OrderedDict()
    #if no params are used, we just get 'na'
    if ds_arg_string != 'na':
        ds_argsplit = re.split(",", ds_arg_string)
        for arg in ds_argsplit:
            argpair = re.split("=", arg)
            #filter out quotes
            arg_strip = re.sub("'", "", argpair[1])
            dsdict[argpair[0]] = as_num(arg_strip)
    auxdict["ds_method"] = ds_method
    auxdict["ds_args"] = dsdict
    auxdict["kfold"] = kfold
    #...I'm sticking this in here because I'm not sure where else to put it;
    #I'm actually not sure whether it gets used. Mark as a to-do item?
    #Both versions get used in creation of downscaled output paths
    #(...which xmlgen does for us, frankly)
    #and creation of comment attribute metadata.
    #ds_region also gets used to make minifile names.
    #Theoretically, ds_region is the smaller area (like a spatial mask),
    #and output_grid the larger; in practice, we've let the distinction slip.
    auxdict["ds_region"] = ds_region
    auxdict["output_grid"] = output_grid
    return(auxdict)

def add_s3_s5_params_to_auxdict(auxdict, masklists, pr_opts, ds_method, ds_method_properties, target_var):
    '''
    Add section 3 and section 5 parameters (in a manner similar to the s3/s5
    code in the R code) to the auxdict. This is moving functionality
    from the R code to expergen, with the intention that it might get reused
    by something else.

    ...or that the effort of moving it is going to turn up interesting and worthwhile
    reminders of how we should restructure things. I think this is going to get
    rewritten in a few months.

    masklists: list(mask1=list(type='simpletailadj',adjust.out='on',qc.mask='on',qc_options=list(adjmethod='mult',lastNpts=1)))
    pr_opts: pr_adjmethod='approx',pr_threshold_in='us_trace',pr_freqadj_in='on',pr_conserve_in='off',
             pr_threshold_out='us_trace',pr_conserve_out='off',ignore0='on',transform='power',arg=1/3,vars='pr',flag_neg='on'
    '''
    adjust_dict = {}
    s3_inloop = []; s3_outloop = []; s5_inloop = []; s5_outloop = [];
    make_qcmask = False
    qc_method = 'none'; qc_loc = 'none'; flag_neg_in_propts = False

    if pr_opts != 'na':
        prdict = OrderedDict()
        pr_optsplit = re.split(",", pr_opts)
        for arg in pr_optsplit:
            argpair = re.split("=", arg)
            #filter out quotes
            arg_strip = re.sub("'", "", argpair[1])
            prdict[argpair[0]] = as_num(arg_strip) #no longer casting as numeric
        #Now separate out args into pre_ds and post_ds components
        pre_ds = ['pr_adjmethod', 'pr_threshold_in', 'pr_freqadj_in', 'pr_conserve_in', 'ignore0']
        pre_ds_default = ['exact', None, None, False]
        post_ds = ['pr_threshold_out', 'pr_conserve_out', 'ignore0', 'pr_adjmethod']
        post_ds_default = [None, None, False, 'exact']
        for k,d in zip(post_ds, post_ds_default):
            if k not in prdict.keys():
                if d is not None:
                    print("Warning for transform in add_s3_s5_params_to_auxdict: value " + k +
                      "not found in args!")
                    print("Using legacy code default of " + d)
                    prdict[k] = d
                else:
                    print("Error found in add_s3_s5_params_to_auxdict: param " +
                           k + " is required!")
                    sys.exit(1)
        transform = ['transform', 'arg', 'vars']
        transform_default = ['none', 'none', 'pr']
        for k,d in zip(transform, transform_default):
            if k not in prdict.keys():
                print("Warning for transform in add_s3_s5_params_to_auxdict: value " + k +
                      "not found in args!")
                print("Using legacy code default of " + d)
                prdict[k] = d
        #Here's the point where you start specifying 'defaults' -
        #settings we've kept from legacy XML that we need to keep an eye out for.
        pairwise_ds = ['PARM', 'lm', 'lmstep']
        #And check for an alt specification of flag.neg that we've kept
        if 'flag.neg' in prdict.keys():
            flag_neg_in_propts = True
            s5_outloop = s5_outloop + ['flag.neg']
            adjust_dict['flag.neg'] = make_adjust('flag.neg', 'pr', 'outloop',
                                                   {'none':'none'}, False)
        #You place the preciptation options as the first thing that you do when
        #you start downscaling, and the last thing you complete when the process
        #ends
        if ds_method_properties['adjust_pr_in_window']:
            pr_loc = 'inloop'
            allow_low_mh = True
            s3_inloop = ['pr_in'] + s3_inloop
            s5_inloop = s5_inloop + ['pr_out']
        else:
            pr_loc = 'outloop'
            allow_low_mh = False
            s3_outloop = ['pr_in'] + s3_inloop
            s5_outloop = s5_outloop + ['pr_out']
        argnames = ['pr_adjmethod', 'thold', 'freqadj', 'conserve', 'ignore0']
        argdict = OrderedDict([(n, prdict[k]) for n,k in zip(argnames,pre_ds)])
        argdict['allow_low_mh'] = allow_low_mh
        adjust_dict['pr_in'] = make_adjust('pr_in', 'pr', pr_loc,
                                           argdict, False)
        argnames = ['thold', 'conserve', 'ignore0','pr_adjmethod']
        argdict = OrderedDict([(n, prdict[k]) for n,k in zip(argnames,post_ds)])
        adjust_dict['pr_out'] = make_adjust('pr_out', 'pr', pr_loc,
                                            argdict, False)
        #Transforms, on the other hand, are the last things that you do before
        #you start downscaling and the first things that you undo when
        #you move on to section 5.
        if prdict['transform'] != 'none':
            tform_vars = re.split(',', prdict['vars'])
            s3_inloop = s3_inloop + ['transform']
            #TODO later: we do the s5 bit in the code right now; can we
            #also handle the other transforms this normally includes?
            #s5_inloop = ['transform'] + s5_inloop
            adjust_dict['transform'] = make_adjust('transform', tform_vars, 'inloop',
                                                   {'transform': prdict['transform'],
                                                   'arg':as_num(prdict['arg']), 'vars': tform_vars},
                                                   False)
        #Precip creates section 3 and section 5 adjustments
        #The s3 opt is the first thing that happens, but the s5 opt comes before
        #any tail adjustment you make
    #The mask options are, fortunately, more simple; however, they have two
    #special cases that need to be accounted for.
    return_qcmask_from_ds = False
    if masklists != 'na':
        maskdict = OrderedDict()
        mask_optsplit = re.split(",", masklists)
        masktype = re.sub("'", "", re.split('=', mask_optsplit[0])[-1])
        if masktype == 'qc.train':
            #This is technically not a actionable adjust item;
            #it's reference saying that we need to prepare to write a qc mask
            return_qcmask_from_ds = method_qcmask = True
            qc_method = 'qc.train'
        elif masktype == 'flag.neg':
            #flag.neg can effectively be either a pr opt OR a s3/s5 option.
            if flag_neg_in_propts:
                print("Error in parsing s3/s5 opts: flag.neg was specified in propts and the qcmask!")
                sys.exit(1)
            s5_outloop = s5_outloop + ['flag.neg']
            method_qcmask = ("qc.mask='on'" in mask_optsplit)
            qc_method = 'flag.neg' if method_qcmask else 'none'
            qc_loc = 'outloop'
            adjust_dict['flag.neg'] = make_adjust('flag.neg',target_var,'outloop',
                                                  {'none':'none'}, method_qcmask)
        else:
            s5_inloop = s5_inloop + [masktype]
            method_qcmask = ("qc.mask='on'" in mask_optsplit)
            qc_method = masktype if method_qcmask else 'none'
            qc_loc = 'inloop'
            #...and now to extract the arglist for the method
            optstring = ",".join(mask_optsplit[3:])
            optsplit = re.split("\(", optstring)[1:]
            optsplit[-1] = re.sub("\)", "", optsplit[-1])
            optsplit = re.split(",", optsplit[-1])
            optdict = {}
            for el in optsplit:
                elsplit = re.split("=", el)
                optdict[elsplit[0]] = as_num(re.sub("'", "", elsplit[1]))
            adjust_dict[masktype] = make_adjust(masktype, target_var,
                                                'inloop', optdict, method_qcmask)
        #TODO: this should include a check to make sure there isn't more than one
        #qcmask present
        #also needs to include a check on whehter
        make_qcmask = method_qcmask
    #Intent here is to separate out options that operate within looping by
    #time window and outside of looping by time window
    s3 = OrderedDict()
    s5 = OrderedDict()
    s3['inloop'] = OrderedDict([(k, adjust_dict[k]) for k in s3_inloop])
    s3['outloop'] = OrderedDict([(k, adjust_dict[k]) for k in s3_outloop])
    s5['inloop'] = OrderedDict([(k, adjust_dict[k]) for k in s5_inloop])
    s5['outloop'] = OrderedDict([(k, adjust_dict[k]) for k in s5_outloop])
    auxdict['s3'] = s3
    auxdict['s5'] = s5
    auxdict['return_qcmask_from_ds'] = return_qcmask_from_ds
    auxdict['make_qcmask'] = make_qcmask
    auxdict['qc_method'] = qc_method
    auxdict['qc_loc'] = qc_loc
    return(auxdict)

def make_adjust(type, adjust_vars, loc, args, make_qc_mask=False):
    '''
    Simple constructor function for building the dicts that describe an adjustment.
    '''
    adjust = OrderedDict({'type' : type, 'apply_var': adjust_vars, 'loc': loc,
                          'qc_args': args, 'make_qc_mask': make_qc_mask})
    return(adjust)

def as_num(val):
    #attempts to convert a string to a numeric; stops if it can't
    #Note: if you try to feed this floats, it turns them into ints.
    #That's not exactly expected behavior, but I'm using this to sanitize strings.
    try:
        return(int(val))
    except ValueError:
        try:
            return(float(val))
        except:
            return(val)
