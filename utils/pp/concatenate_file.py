#!/bin/python
#concatenate_files.py
#Takes a directory of netcdf files;
#works in a temporary directory and
#outputs a concatenated file to the dir
#specified

#import expergen
import fudgeList
import fudgeUtils
from fudgeUtils import tstamp
# from fudgeUtils import read_exp_info_file
from get_ds_info import parse_aux_info
import os,sys,subprocess,shutil,time,copy
from os import path as osp
from subprocess import PIPE
import argparse
import shlex
import smtplib
import re
import optparse
from optparse import OptionParser
from email.MIMEMultipart import MIMEMultipart
from email.MIMEBase import MIMEBase
from email.MIMEText import MIMEText
from email.Utils import COMMASPACE, formatdate
from email import Encoders
import netCDF4
from netCDF4 import Dataset

def concatenate_files(input_dir, patt, output_dir, aux='none'):
    #Get all files in the input dir that match a given pattern
    #(note: assumed to start at the beginning of the filename)

    global basedir
    global tdir
    basedir = os.environ.get('BASEDIR')
    tdir = os.environ.get('TMPDIR')

    print("getting list of files")
    file_subset = [f for f in os.listdir(input_dir) if re.search(patt, f) is not None]
    #print("file_subset:")
    #print(file_subset)
    #Start dividing the file subset into subsets based on the separators.
    #Each one is going to be in the general form of $capital_letter$start_index-$end_index
    #find_list  = [re.findall("\.(I[0-9]*)_|\.(I[0-9]*-[0-9]*)_", f) for f in file_subset]
    #unwind_list = [el for f in find_list for el in f ]
    #i_matches = sorted([el for f in unwind_list for el in f if el != ""]) #remove duplicates
    index_subset = [re.split("\.", f)[1] for f in file_subset]
    i_patterns = find_match_string('I', index_subset, file_subset)
    j_patterns = find_match_string('J', index_subset, file_subset)
    file_split = re.split("\.", file_subset[0])
    print(file_split)
    out_filename = ".".join([file_split[0], file_split[-1]])
    outfile = osp.join(output_dir, out_filename)
    ###Get input dimensions and dim mapping
    dimlist = determine_dims_present(osp.join(input_dir, file_subset[0]))
    #reversing dimlist to try getting permutations right
    #NOTE TO SELF: ncks uses dims in different order than python utils
    dimlist = dimlist[::-1]
    dimmap = get_dim_mapping(dimlist)
    print(dimmap)
    ############################################################################
    tdir_i = osp.join(tdir, "i_cat")
    if not osp.exists(tdir_i):
        os.makedirs(tdir_i)
    tdir_j = osp.join(tdir, "j_cat")
    if not osp.exists(tdir_j):
        os.makedirs(tdir_j)
    outdir_j = osp.join(tdir, 'j_out')
    if not osp.exists(outdir_j):
        os.makedirs(outdir_j)
    tmp_outfile = osp.join(tdir, out_filename)
    ## Since there is always going to be at least one element of these arrays,
    ## no worries about a 0-length array - and length-one is fine.
    #i_outfiles = [i for i in range(len(i_patterns))]
    i_outfiles = []
    i_out_paths = []
    print("looping over i_patterns and concatenating in j")
    print(i_patterns)
    for i_patt,i in zip( i_patterns, range( len(i_patterns) ) ):
        i_subset = [f for f in file_subset if re.search(i_patt, f) is not None]
        natural_sort(i_subset)
        #i_out_file = out_filename = ".".join(i_idx) #[file_split[0], , file_split[-1]
        i_out_file =  "i_pattern_"+str(i) +".nc"
        i_outfiles.append(i_out_file)
        i_out_paths.append(osp.join(outdir_j, i_out_file)) #osp.join(input_dir, i_out_file)
        y_idx = dimmap.index('y')
        y_perm_in =  dimlist[y_idx] + "," + ",".join([y for i,y in enumerate(dimlist) if i!=y_idx])
        y_perm_out = ",".join(dimlist)
        if len(j_patterns) > 1:
            #If there's more than one J, find the j matches for this i
            loop_file = call_cat_min(y_perm_in, y_perm_out, input_dir, i_subset,
                                        tdir_j, osp.join(outdir_j, i_out_file), i_patt)
        else:
            #Copy to directory that i concatenation expects to find results in
            shutil.copy2(osp.join(input_dir, i_subset[0]), osp.join(outdir_j, i_out_file))
        #End i loop
    #Final concatenation operation
    print("final concatenation operation in i")
    x_idx = dimmap.index('x')
    x_perm_in =  dimlist[x_idx] + "," +",".join([x for i,x in enumerate(dimlist) if i!=x_idx])
    x_perm_out = ",".join(dimlist)
    print("outer call to cat_min")
    xy_out = call_cat_min(x_perm_in, x_perm_out, outdir_j, i_outfiles,
                  tdir_i, tmp_outfile, "all values")
    print("concatenated file: " + xy_out)
    #Final step needs to be finalizing some metadata atts
    #experiment_info = "/archive/cew/PROJECTS/DOWNSCALING/PMhCR/downscaled/GFDL-HIRAM-C360-COARSENED/sst2090/day/atmos/day/r1i1p1/v20110601/PMhCRtxp1-EDQM-A01r1e3X01K00/experiment_info.txt"
    if aux != 'none':
        print("finalizing file metadata")
        xy_out = finalize_metadata_out(xy_out, aux)
    else:
        print("no experiment_info file; metadata unchanged")
    print("copying file to final destination")
    print("cp " + tmp_outfile + " " + outfile)
    shutil.copy2(tmp_outfile, outfile)
    return(outfile)


def finalize_metadata_out(xy_file, aux):
    #Sets up final metadata attributes on the output. Fortunately, there's going to
    #be a decent header from the concatenation operations
    #NOTE: right now, none of these attributes can have spaces in them
    #due to the passing into c-shell. Probably solvable
    # expdict = read_exp_info_file(experiment_info)
    auxinfo, auxdict = parse_aux_info(aux)

    attdict = {}
    attdict['contact'] = "http://nomads.gfdl.noaa.gov/contacts.html"
    attdict['conventions'] = "CF-1.4"
    attdict['downscaling_model_type'] = "statistical"

    attdict['gfdl_experiment_name'] = auxdict['ds_experiment_name']
    #attdict['region_id'] = expdict['region']
    #attdict['predictand_calibration_datset'] = "dummy_data"

    #NCO = "I'm not entiretly sure I need to add this in now."

    att_val_list = [",".join(el) for el in zip(attdict.keys(), attdict.values())]
    #Leaving off because the quotes get added in when writing the atts.
    #If we could strip those, that would be great - but for now,
    #keep an eye out for things that might contain spaces.
    att_val_list = ['"'+el+'"' for el in att_val_list]
    att_val_string = " ".join(att_val_list)

    print("calling set_metadata")
    #set_metadata = "/home/cew/Code/h_testing/FudgeIO_reboot/set_cat_metadata.csh"
    set_metadata = basedir + "/utils/pp/set_cat_metadata.csh"
    set_command = " ".join(["tcsh -f ",set_metadata, xy_file, att_val_string])
    p = subprocess.Popen(set_command, stdout=subprocess.PIPE, shell=True, env=None)
    out0,err0 = p.communicate()
    if (p.returncode != 0):
        print(str(out0))
        print(str(err0))
        print("Error in setting metadata operations for " +  xy_file + "; call to c-shell failed.")
        print("Command that failed: " + set_command)
        sys.exit(1)
        return(None)
    else:
        print(str(out0))



def find_match_string(lett, patt_array, match_array):
    #Finds all unique strings that match a given pattern
    #with letter lett fed into it

    #Two expected patterns: i-index and i_start-i_end
    #
    print("in find_match_string")
    blank_patt = "(ZZ[0-9]*)_|(ZZ[0-9]*-[0-9]*)"
    patt = re.sub('ZZ', lett, blank_patt)
    find_list  = [re.findall(patt, f) for f in patt_array]
    #print("list of matches")
    #print(find_list)
    unwind_list = [el for f in find_list for el in f ]
    if len(set(unwind_list))==1:
        print("No unique matches on this dim; don't concatenate")
        unwind_matches = [el for f in unwind_list for el in f if el != ""] #no unique matches on this dim
        print(unwind_matches)
    else:
        patt_matches = sorted(list(set([el for f in unwind_list for el in f if el != ""])))
        natural_sort(patt_matches)
        unwind_matches = patt_matches
        #####
        #print("patt_matches")
        #print(patt_matches)
        #sort_matches = []
        #for el in patt_matches:
        #	sort_matches.append([f for f in match_array if re.search(el, f) is not None])
        #unwind_matches = [el for f in sort_matches for el in f ]
        #print(unwind_matches)
        #Okay, this is giving weird behavior for the j cases
    return(unwind_matches)

def indices(lst, element):
    #taken from http://stackoverflow.com/questions/6294179/how-to-find-all-occurrences-of-an-element-in-a-list
    result = 0 #[]
    offset = -1
    while True:
        try:
            offset = lst.index(element, offset+1)
        except ValueError:
            return result
        #result.append(offset)
    return result



def call_cat_min(perm_in, perm_out, input_dir, infiles, scratch_dir, outfile, index_str=""):
    #Convenience wrapper for calling cat_min.csh
    print("calling cat_min")
    infile_str = ",".join(infiles)
    #cat_min = "/home/cew/Code/h_testing/FudgeIO_reboot/concatenate_files.csh"
    cat_min = basedir + "/utils/pp/concatenate_files.csh"
    cat_command = " ".join(["tcsh -f ",cat_min, perm_in, perm_out, input_dir, infile_str, scratch_dir, outfile])
    p = subprocess.Popen(cat_command, stdout=subprocess.PIPE, shell=True, env=None)	#, cwd=tdir
    out0,err0 = p.communicate()
    if (p.returncode != 0):
        print(str(out0))
        print(str(err0))
        print("Error in processing concatenation operations for " +  index_str + "; call to c-shell failed.")
        print("Command that failed: " + cat_command)
        sys.exit(1)
        return(None)
    else:
        print(str(out0))
        return(outfile)

def determine_dims_present(nc_file):
    #Determines the dimensions that are present in the input file
    print('determining dimensions present')
    print(nc_file)
    exclude_dims = ['bnds', 'height']
    with Dataset(nc_file, 'r') as nc_fid:
        nc_dims = [dim for dim in nc_fid.dimensions if dim not in exclude_dims]  # list of nc dimensions
    return nc_dims

def get_dim_perm():
    #Gets a dimension permutation for the input file
    print("empty function")

def get_dim_mapping(dimlist):
    #Maps a list of known aliases for each variable that might pop up in our input code
    #into a known naming schema - allows for permutation in the concatentation calls
    #to ncrcat.
    dim_dict  = {'x' : ['lon', 'longitude', 'x'],
                 'y' : ['lat', 'latitude', 'y'],
                 'z' : ['plev', 'height', 'z'],
                 'ens' : ['ensmem', 'ensmbl', 'ens', 'm'],
                 'n' : ['n'],
                 'time' : ['time']}
    maplist = ['zz'] * len(dimlist)
    for n in range(0, len(dimlist)):
        ndim = dimlist[n]
        maplist[n] = [k for k in dim_dict.keys() if any([re.match(ndim, el) is not None for el in dim_dict[k]])]
    #Assume that any els of length 0 are the bounds dimension
    maplist = [el for f in maplist for el in f ]
    return(maplist)

def natural_sort(list, key=lambda s:s):
    """
    Sort the list into natural alphanumeric order.
    Taken from stackoverflow
    """
    def get_alphanum_key_func(key):
        convert = lambda text: int(text) if text.isdigit() else text
        return lambda s: [convert(c) for c in re.split('([0-9]+)', key(s))]
    sort_key = get_alphanum_key_func(key)
    list.sort(key=sort_key)
