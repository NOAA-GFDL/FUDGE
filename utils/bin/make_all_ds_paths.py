#!/bin/python
#make_all_ds_paths.py
#makes all paths for a downscaled FUDGE experiment
# takes a confg_mod file as an argument
# and either returns an object or dumps the results to file

import sys
import re
import os
from os import path as osp
from get_ds_info import parse_aux_info
from collections import OrderedDict
#import yaml
import json
import optparse
from optparse import OptionParser

def make_all_ds_paths(auxfile, out_file = "/home/cew/Code/testing/test_out.txt"):
    '''
    Makes all paths for the experimental output from FUDGE,
    and either writes to file or passes in info as a struct.
    '''
    auxinfo, auxdict = parse_aux_info(auxfile)
    ds_out = OrderedDict()
    for el in ['ds', 'ens', 'anom', 'trend', 'cv']:
        ds_out[el] = OrderedDict()
    ds_vars = auxdict['DS']['vars']
    ds_rips = auxdict['DS']['rips']
    if len(ds_rips) > 1:
        rip_suffix = ds_rips[1][-4:]
        all_rip = "r1to" + str(len(ds_rips)) + rip_suffix
        ds_rips = ds_rips + ["ens"]
    write_qc = auxdict['make_qcmask']
    rip_sub_string = '<RIP>'
    var_sub_string = '<VAR>'
    end_suffix = "I<ISTART>-<IEND>_J<JSTART>-<JEND>."
    ens_suffix = "_ensmbl.nc"
    ds_path = auxdict["DS"]['out_dir']
    ds_file = auxdict['DS']['out_file']
    if re.search(var_sub_string, ds_file) is None:
        var_sub_string = auxdict["DS"]['vars'][0]
    exp_name = auxdict['ds_experiment_name']
    ds_file = re.sub(end_suffix, "", ds_file)
    #Step 1: single-ens output from FUDGE
    for var in ds_vars:
        ds_out['ds'][var] = {} #OrderedDict()
        if write_qc:
            var_qc = var + '_qcmask'
            ds_out['ds'][var_qc] = {}
    for rip in ds_rips:
        if rip != 'ens':
            loop_path = re.sub(rip_sub_string, rip, ds_path)
            loop_file = re.sub(rip_sub_string, rip, ds_file)
        else:
            loop_path = re.sub(rip_sub_string, all_rip, ds_path)
            loop_file = re.sub(rip_sub_string, all_rip, ds_file)
        for var in ds_vars:
            if rip != 'ens':
                #Single-ens file path creation
                loop_var_path = re.sub(var_sub_string, var, loop_path)
                loop_var_file = re.sub(var_sub_string, var, loop_file)
                ds_out['ds'][var][rip] = osp.join(loop_var_path, loop_var_file)
                if write_qc:
                    loop_qc_file = re.sub("_" + var_sub_string, "_" + var_qc, loop_file)
                    ds_out['ds'][var_qc][rip] = osp.join(loop_var_path, loop_qc_file)
            else:
                #Multi-ens file path creation
                loop_var_path = re.sub(var_sub_string, var, loop_path)
                loop_var_file = re.sub(var_sub_string, var, loop_file)
                loop_var_file = re.sub("\.nc", ens_suffix, loop_var_file)
                ds_out['ds'][var]['ens'] = osp.join(loop_var_path, loop_var_file)
                if write_qc:
                    loop_qc_file = re.sub("_" + var_sub_string, "_" + var_qc, loop_file)
                    loop_qc_file = re.sub(".nc^", ens_suffix, loop_qc_file)
                    ds_out['ds'][var_qc]['ens'] = osp.join(loop_var_path, loop_qc_file)
    # Now that we've got the qcmask calculation, recycle that
    ds_vars = list(ds_out['ds'].keys())
    ds_vars.sort()
    print("ds_vars:")
    print(ds_vars)
    #Step 3: anomaly output, single and multi-ens
    ds_out['anom'] = {}
    for var in ds_vars:
        is_anom = re.search("Anom", var) is not None
        if is_anom:
            anom_var = re.sub('Anom', '', var)
            ds_out['anom'][anom_var] = {} #OrderedDict()
            #ds_out['anom'][anom_var]['is_anom'] = is_anom
            # So now that we have this file, we want all dir path instances of
            # var in the ds and ens vars
            anom_dict = make_AnomTrend_filepaths(var, anom_var, ds_out, ds_rips, write_qc)
            for k in list(anom_dict.keys()):
                ds_out['anom'][k] = anom_dict[k]
    #Step 4: trend output, single and multi-ens
    for var in ds_vars:
        ds_out['trend'][var] = OrderedDict()
        is_trend = re.search("Detr", var) is not None
        #ds_out['trend']['is_trend'] = is_trend
        if is_trend:
            trend_var = re.sub('Detr', '', var)
            #ds_out['trend'][var]['is_trend'] = True
            trend_dict = make_AnomTrend_filepaths(var, trend_var, ds_out, ds_rips, write_qc)
            #ds_out['trend'][trend_var] = trend_dict
            for k in list(trend_dict.keys()):
                ds_out['trend'][k] = trend_dict[k]
    #Step 5: cross-validated output
    crossval_patt = ['00r1e2', '00r2e2']
    cv_match = [crossval_patt.index(c) for c in crossval_patt if re.search(c, exp_name) is not None]
    is_cv = (len(cv_match) > 0) and (len(ds_rips) > 1)
    ds_out['cv']['is_cv'] = is_cv
    if not is_cv:
            ds_out['cv']['make_cv_ens'] = False
    else:
        ds_out['cv']['make_cv_ens'] = (cv_match[0] == 1)
    if is_cv:
        patt2 = 1 - cv_match[0]
        exp_name2 = re.sub(crossval_patt[cv_match[0]], crossval_patt[patt2], exp_name)
        # Note to self: these are only created if we're in the CVh experiment,
        # but that is not determined at this time.
        ds_out['cv']['complement'] = exp_name2
        if cv_match == 0:
            ds_out['cv']['r1e2'] = exp_name
            ds_out['cv']['r2e2'] = exp_name2
        else:
            ds_out['cv']['r1e2'] = exp_name2
            ds_out['cv']['r2e2'] = exp_name
        ds_out['cv']['DSDEPh_exp'] = exp_name
        ds_out['cv']['DSCVh_exp'] = exp_name2
    if ds_out['cv']['make_cv_ens']:
        out_dict = OrderedDict()
        for var in ds_vars:
            #out_dict[var] = OrderedDict({'dsdeph':{},'dscvh':{}})
            out_dict[var] = make_cv_output_files(ds_out['ds'][var]['ens'], all_rip, exp_name)
            dummy_file = ds_out['ds'][var]['r1i1p1']
            dummy_rip = 'r1i1p1'
            complement_file = make_cv_ens_filename(exp_name, ds_out['cv']['r2e2'], dummy_rip, 'r2i1p1', dummy_file)
            out_dict[var]['dsdeph']['input'] = [ds_out['ds'][var]['r1i1p1'], complement_file]
            complement_file = make_cv_ens_filename(exp_name, ds_out['cv']['r1e2'], dummy_rip, 'r1i1p1', dummy_file)
            out_dict[var]['dscvh']['input'] = [ds_out['ds'][var]['r2i1p1'], complement_file]
            ds_out['cv'][var] = out_dict[var]
    if out_file is not None:
        print(("Writing downscaling experiment output paths to " + out_file))
        ds_all_vars = list(set(list(ds_out['ds'].keys()) + list(ds_out['anom'].keys()) + list(ds_out['trend'].keys())))
        print_file_dict(ds_out, ds_all_vars, ds_rips, out_file)
    else:
        return(ds_out)

def make_AnomTrend_filepaths(var, anom_var, dict, rips, write_qc=False):
    '''
    Helper function for swapping out anomaly/trend vars for orig vars
    in the filepaths/filenames. Assumes ESD-standard naming conventions.
    '''
    out = OrderedDict()
    #anom_var = var + vstring
    out[anom_var] = OrderedDict()
    is_qc = False
    if re.search("_qcmask", var) is not None:
        is_qc = True
        qc_var = anom_var
        var = re.sub("_qcmask", "", var)
        anom_var = re.sub("_qcmask", "", anom_var)
        out[qc_var] = {}
    for rip in rips:
        if rip != 'ens':
            anom_dir,anom_file = swap_anom_var(var, anom_var, dict['ds'][var][rip])
            if not is_qc:
                out[anom_var][rip] = osp.join(anom_dir, anom_file)
            else:
                anom_file = re.sub(anom_var + "_", qc_var + "_", anom_file)
                out[qc_var][rip] = osp.join(anom_dir, anom_file)
        else:
            anom_dir,anom_file = swap_anom_var(var, anom_var, dict['ds'][var]['ens'])
            if not is_qc:
                out[anom_var]['ens'] = osp.join(anom_dir, anom_file)
            else:
                anom_file = re.sub(anom_var + "_", qc_var + "_", anom_file)
                out[qc_var]['ens'] = osp.join(anom_dir, anom_file)
    return(out)

def swap_anom_var(var, anom_var, filepath):
    '''
    Swaps out instances of var in filepath for anom_var such that:
    a) any dir before the filename that matches var in its entirety changes
    b) any split in the filename that matches var in its entirety changes
    This assumes an ESD-standard dir and filename structure, and will break
    terribly otherwise.
    '''
    anom_dir = swap_delim_segment(var, anom_var, '/', osp.dirname(filepath))
    anom_file = swap_delim_segment(var, anom_var, '_', osp.basename(filepath))
    return(anom_dir,anom_file)

def swap_delim_segment(patt, rep, delim, string):
    '''
    Swaps a segment of a delimiter for a new string
    '''
    strsplit = re.split(delim, string)
    strsplit = [rep if re.search("^"+patt+"$", el) is not None else el for el in strsplit]
    return(delim.join(strsplit))

def make_cv_output_files(ens_file, ens_rip, ens_experiment_name):
    '''
    Makes a set of cv files from a given input file
    '''
    dsdict = {'dscvh':{'short_rip': 'cve2', 'rip_equiv': "DSCVh"},
              "dsdeph": {'short_rip': 'dpe2', 'rip_equiv': "DSDEPh"}}
    short_rip_string = ens_experiment_name[ens_experiment_name.find("00"):ens_experiment_name.find("X01")]
    out = OrderedDict()
    for k in list(dsdict.keys()):
      out[k] = {}
      cv_exp_name = re.sub(short_rip_string, "00"+dsdict[k]['short_rip'], ens_experiment_name)
      cv_rip = dsdict[k]['rip_equiv']
      out[k]['ens'] = make_cv_ens_filename(ens_experiment_name, cv_exp_name, ens_rip, cv_rip, ens_file)
    return(out)

def make_cv_ens_filename(ens_experiment,cv_experiment, ens_rip,cv_rip, orig_file):
    '''
    Makes a file for the cross-validated output based off of the input file.
    '''
    cv_file = re.sub(ens_experiment, cv_experiment, orig_file)
    cv_file = re.sub(ens_rip, cv_rip, cv_file)
    return(cv_file)

def print_file_dict(dict, var_list, ds_rips, outfile):
    '''
    Writes the dictionary of output files from the
    '''
    dictstring_1 = file_dict_to_str(dict['ds'], var_list, ds_rips, "")
    dict.pop('ds')
    for el in ['anom', 'trend', 'cv']:
        dictstring_1 += file_dict_to_str(dict[el], var_list, ds_rips, "")
    with open(outfile, 'w') as f:
        f.write(dictstring_1)
        f#.write(dictstring_2)

def file_dict_to_str(ds, var_list, ds_rips, prefix, return_string=""):
    '''
    Prints a flattened version of the dictionary of output files
    '''
    #var_list = sorted(ds_vars + qc_vars)
    cv_rips = ['dscvh', 'dsdeph']
    rip_list = ds_rips + cv_rips
    #if (prefix == 'anom'):
    #    print(ds['tasmax'].keys())
    #    print(ds['tasmax']['r1i1p2'])
    if not isinstance(ds, (dict, OrderedDict)):
        return("")
    key_list = list(ds.keys())
    for k in key_list:
        if k in var_list:
            for rip in rip_list:
                #if isinstance(ds[k], (dict, OrderedDict)):
                if rip in list(ds[k].keys()):
                    if prefix == "":
                        k_pref = ".".join([k, rip])
                    else:
                        k_pref = ".".join([prefix, k, rip])
                    if rip in cv_rips:
                        return_string += k_pref + ": " + str(ds[k][rip]['ens']) + "\n"
                    else:
                        return_string += k_pref + ": " + str(ds[k][rip]) + "\n"
        else:
            if prefix == "":
                k_pref = k
            else:
                k_pref = prefix + "." + k
            return_string += file_dict_to_str(ds[k], var_list, ds_rips, prefix=k_pref, return_string=return_string)
    return(return_string)


def parse_makepath_args():
    '''
    Parses command-line args to the path-making script and either returns a dict
    or passes args to file
    '''
    usage = "usage: %prog --auxfile auxfile.json --outfile outfile.txt"
    parser = OptionParser(usage=usage, version='make_ds_paths 1.0')
    parser.add_option("--auxfile", dest="auxfile", help="Path to an auxfile for an experiment.")
    parser.add_option("--outfile", dest="outfile", default=None, help="Path to the output file to which you will write. Defaults to no file, which returns a dict.")
    #parser.add_option("-v", "--verbose", action="store_true", dest="verbose", default=False, help="Print extra status messages used to help debugging. Defaults to False.")
    (options, args) = parser.parse_args()
    make_all_ds_paths(options.auxfile, options.outfile)

if __name__ == '__main__':
    print((sys.argv))
    parse_makepath_args()
    #make_all_ds_paths(sys.argv[1])
