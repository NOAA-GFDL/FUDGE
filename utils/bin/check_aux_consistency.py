#!/usr/bin/python
import sys
from os import path as osp
import json

def check_aux_consistency(aux):
    '''
    Checks the aux dictionary to make sure that its's got the relevant pieces
    that FUDGE depents upon for its parsing.
    At present, that's the only check - no making sure files exist, no making
    sure that args don't disagree with each other.
    '''
    # Do we still use geomask_names? Anywhere?
    sol_components = ["i_subset", "j_subset", "tile_olap",
"geomask_names","njobs","ncpus","wallclocktime","pp_wallclocktime","files_in_aux","file_format","ds_region","output_grid","climo_source","return_qcmask_from_ds","make_qcmask","qc_method","qc_loc"]
    for el in sol_components:
        if el not in aux.keys():
            print("Error in check_aux_consistency: compoenent " + el + " is not defined! Please add component to the aux input file.")
            sys.exit(1)
    data_names = ['OH', 'MH', 'MF']
    for d in data_names:
        check_data_struct(aux[d])
    check_ds_struct(aux['DS'])
    ds_components = ["ds_experiment_name", "ds_method","ds_args","kfold","minsamplesize" 'ds_handle_missing']
    #cleanup_settings:
    preexist = aux['cleanup_settings']['if_preexist']
    preexist_opts = ['move', 'erase', 'exit']
    if preexist not in preexist_opts:
        print("Error in check_aux_consistency: aux['cleanup_settings']['if_preexist'] should be one of " + ",".join(preexist_opts))
        sys.exit(1)
    #script_settings:
    if not osp.isdir(aux['script_settings']['scriptdir']):
        print("Error in check_aux_consistency: aux['script_settings']['scriptdir'] does not exist on this filesystem!")
        sys.exit(1)
    # I don't think this returns anything

def check_data_struct(data_struct):
    '''
    Makes sure that one of the data components of the aux has the expected structure.
    '''
    if len(data_struct) == 0:
        sys.exit(1)
    for el in data_struct.keys():
        d_el = data_struct[el]
        d_keys = sorted(d_el.keys())
        fix_keys = sorted(['tmask', 'stencil','geomask', 'file', 'is_prime', 'is_fixed'])
        d_comp = zip(d_keys, fix_keys)
        if not all(el[0] == el[1] for el in d_comp):
            print("Error in check_data_struct: expected keys and found keys did not match for el " + el + "!")
            print("Expected keys:\n\t" + "\n\t".join(fix_keys))
            print("Found keys:\n\t" + "\n\t".join(d_keys))
            sys.exit(1)

def check_ds_struct(ds_struct):
    '''
    Makes sure that the DS component of the aux has the expected structure
    '''
    if len(ds_struct) == 0:
        sys.exit(1)
    fixed_keys = sorted(['out_file', 'vars', 'out_dir', 'out_subdir', 'indexing_tmask', 'rips', 'out_supp'])
    ds_keys = sorted(ds_struct.keys())
    ds_comp = zip(ds_keys, fixed_keys)
    if not all(el[0] == el[1] for el in ds_comp):
        print("Error in check_ds_struct: expected keys and found keys did not match!")
        print("Expected keys:\n\t" + "\n\t".join(fixed_keys))
        print("Found keys:\n\t" + "\n\t".join(ds_keys))
        sys.exit(1)

def check_s3_s5_struct(step_dict):
    '''
    Checks the s3/s5 components to make sure they have the expected structure
    '''
    # Two components, inloop and outloop; they can be blank
    locs = ['inloop', 'outloop']
    for l in locs:
        ldict = step_dict[l]
        l_keys = ldict.keys()
        for k in l_keys:
            step = ldict[k]
            fix_keys = sorted(['make_qc_mask', 'loc', 'qc_args', 'type', 'apply_var'])
            step_keys = sorted(step.keys())
            s_comp = zip(fix_keys, step_keys)
            if not all(el[0] == el[1] for el in s_comp):
                print("Error in check_s3_s5: expected keys and found keys did not match for loc " + l + " step " + k + "!")
                print("Expected keys:\n\t" + "\n\t".join(fix_keys))
                print("Found keys:\n\t" + "\n\t".join(step_keys))
                sys.exit(1)

if __name__ == '__main__':
    '''
    Parse input json aux
    '''
    with open(sys.argv[1], 'r') as f:
        jdict = json.load(f)
    check_aux_consistency(jdict)
