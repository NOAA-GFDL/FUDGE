#!/bin/python
#concatenate_files.py
#Takes a directory of netcdf files;
#works in a temporary directory and
#outputs a concatenated file to the dir
#specified

import os,sys,subprocess,shutil,time,copy
from os import path as osp
from subprocess import PIPE
import re
import netCDF4
from netCDF4 import Dataset

S8_PATH = os.environ.get('S8_PATH')
if S8_PATH is None:
    print("Error in python script calculation: $S8_PATH not set. Please source a path to the section 8 tools.")
    sys.exit(1)
sys.path.append(osp.join(S8_PATH, 'shared_code'))
from common_functions import get_nc_dim_lengths, tstamp
import env

def cat_files(input_dir, patt, output_dir, exp_info='none', delim_patt = ".", orig_file="test.nc"):
	#Get all files in the input dir that match a given pattern
	#(note: assumed to start at the beginning of the filename)

	global basedir
	global tdir
	global cat_min_loc
	global S8_PATH
	S8_PATH = os.environ.get('S8_PATH')
	if S8_PATH is None:
		print("Error in batch setup: $S8_PATH not set. Please source a path to the section 8 tools.")
		sys.exit(1)
	basedir = osp.join(S8_PATH, "shared_code/embarassingly_parallel/")
	tdir = env.TMPDIR
	tsp = tstamp()
	tdir_cat = osp.join(tdir, "i_cat-" + str(tsp))
	if not osp.exists(tdir_cat):
		os.makedirs(tdir_cat)
	cat_min_loc = basedir

	print("getting list of files")
	print(patt)
	file_subset = [f for f in os.listdir(input_dir) if re.search(patt, f) is not None]
	#Start dividing the file subset into subsets based on the separators.
	#Each one is going to be in the general form of $capital_letter$start_index-$end_index
	#find_list  = [re.findall("\.(I[0-9]*)_|\.(I[0-9]*-[0-9]*)_", f) for f in file_subset]
	#unwind_list = [el for f in find_list for el in f ]
	#i_matches = sorted([el for f in unwind_list for el in f if el != ""]) #remove duplicates
	# this bit right here assumes that there are only ever gonna be two periods in the 
	#filename - $filename.i_suffix.nc 
	#corrected in 2021 to be separated by a general-purpose delimiter
	file_split = re.split(delim_patt, file_subset[0])
	out_filename = ".".join([file_split[0], ".nc"])
	outfile = osp.join(output_dir, out_filename)
	tmp_outfile = osp.join(tdir, out_filename)
	print("cdo call to all files matching pattern")
	xy_out = call_cat_min_cdo(input_dir, file_subset, tdir_cat, tmp_outfile, "all values", orig_file)
	print("concatenated file: " + xy_out)
	#Final step needs to be finalizing some metadata atts
	if exp_info != 'none':
		print("finalizing file metadata")
		xy_out = finalize_metadata_out(xy_out, exp_info)
	print("copying file to final destination")
	print("cp " + tmp_outfile + " " + outfile)
	shutil.copy2(tmp_outfile, outfile)
	return(outfile)


def cat_files_nco(input_dir, patt, output_dir, exp_info='none', delim_patt = "."):
	#Get all files in the input dir that match a given pattern
	#(note: assumed to start at the beginning of the filename)

	global basedir
	global tdir
	global cat_min_loc
	global S8_PATH
	S8_PATH = os.environ.get('S8_PATH')
	if S8_PATH is None:
		print("Error in batch setup: $S8_PATH not set. Please source a path to the section 8 tools.")
		sys.exit(1)
	basedir = osp.join(S8_PATH, "shared_code/embarassingly_parallel/")
	tdir = env.TMPDIR
	tsp = tstamp()
	tdir_cat = osp.join(tdir, "i_cat-" + str(tsp))
	if not osp.exists(tdir_cat):
		os.makedirs(tdir_cat)
	cat_min_loc = basedir

	print("getting list of files")
	print(patt)
	file_subset = [f for f in os.listdir(input_dir) if re.search(patt, f) is not None]
	#Start dividing the file subset into subsets based on the separators.
	#Each one is going to be in the general form of $capital_letter$start_index-$end_index
	#find_list  = [re.findall("\.(I[0-9]*)_|\.(I[0-9]*-[0-9]*)_", f) for f in file_subset]
	#unwind_list = [el for f in find_list for el in f ]
	#i_matches = sorted([el for f in unwind_list for el in f if el != ""]) #remove duplicates
	# this bit right here assumes that there are only ever gonna be two periods in the 
	#filename - $filename.i_suffix.nc 
	index_subset = [re.split(delim_patt, f)[1] for f in file_subset]
	i_patterns = find_match_string('I', index_subset, file_subset)
	j_patterns = find_match_string('J', index_subset, file_subset)
	if (len(i_patterns) + len(j_patterns)) == 0:
		print("Error in concatenate_files: no patterns match regex! Please check directory " + input_dir)
		sys.exit(1)
	#file_split = re.split("\.", file_subset[0])
	file_split = re.split(delim_patt, file_subset[0])
	print(file_split)
	#out_filename = ".".join([file_split[0], file_split[-1]])
	out_filename = ".".join([file_split[0], ".nc"])
	outfile = osp.join(output_dir, out_filename)
	###Get input dimensions and dim mapping
	dimlist = determine_dims_present(osp.join(input_dir, file_subset[0]))
	#reversing dimlist to try getting permutations right
	#NOTE TO SELF: ncks uses dims in different order than python utils
	dimlist = dimlist[::-1]
	dimmap = get_dim_mapping(dimlist)
	#print(dimmap)
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
			#loop_file = call_cat_min_cdo(input_dir, i_subset, tdir_j, osp.join(outdir_j, i_out_file), i_patt)
		else:
			#Copy to directory that i concatenation expects to find results in
			shutil.copy2(osp.join(input_dir, i_subset[0]), osp.join(outdir_j, i_out_file))							
		if i  %10 == 0:
			print("i index ", i, " of ", len(i_matches), "complete")
		#End i loop
	#Final concatenation operation
	print("final concatenation operation in i")
	x_idx = dimmap.index('x')
	x_perm_in =  dimlist[x_idx] + "," +",".join([x for i,x in enumerate(dimlist) if i!=x_idx])
	x_perm_out = ",".join(dimlist)
	x_perm_out = 'time,lat,lon'
	print("outer call to cat_min")
	xy_out = call_cat_min(x_perm_in, x_perm_out, outdir_j, i_outfiles,
		          tdir_i, tmp_outfile, "all values")
	#xy_out = call_cat_min_cdo(outdir_j, i_outfiles, tdir_i, tmp_outfile, "all values")

	print("concatenated file: " + xy_out)
	#Final step needs to be finalizing some metadata atts
	#experiment_info = "/archive/cew/PROJECTS/DOWNSCALING/PMhCR/downscaled/GFDL-HIRAM-C360-COARSENED/sst2090/day/atmos/day/r1i1p1/v20110601/PMhCRtxp1-EDQM-A01r1e3X01K00/experiment_info.txt"
	if exp_info != 'none':
		print("finalizing file metadata")
		xy_out = finalize_metadata_out(xy_out, exp_info)
	#else:
	#	print("no experiment_info file; metadata unchanged")
	print("copying file to final destination")
	print("cp " + tmp_outfile + " " + outfile)
	shutil.copy2(tmp_outfile, outfile)
	return(outfile)



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

def call_cat_min_cdo(input_dir, infiles, scratch_dir, outfile, index_str="", orig_file=""):
	#Convenience wrapper for calling cat_min_cdo.csh
	print("calling cat_min")
	infile_str = ",".join(infiles)
	cat_min = cat_min_loc + "concatenate_files-cdo.csh"
	'''
	cat_command = " ".join(["tcsh -f",cat_min, input_dir, infile_str, scratch_dir, outfile, orig_file])
	    sys.exit(1)
	'''
	cat_command = " ".join([cat_min, input_dir, infile_str, scratch_dir, outfile, orig_file])
	print(str(len(cat_command)))
	if len(cat_command) > 8191:
	    print("Error in call_cat_min cdo! The command was more than 2^13-1 characters long, which may lead to failure due to command size length!")	
	cat_cmd_file = osp.join(scratch_dir, "cat_cmd.csh")
	with open(cat_cmd_file, 'w') as f:
	    f.write("#!/bin/tcsh\n")
	    f.write(cat_command)
	print(cat_cmd_file)
	print("tcsh -f " + cat_cmd_file)
	p = subprocess.Popen("tcsh -f " + cat_cmd_file, stdout=subprocess.PIPE, shell=True, env=None)
	out0,err0 = p.communicate()
	if (p.returncode != 0):
		print(str(out0))
		print(str(err0))
		print("Error in processing cdo concatenation operations for " +  index_str + "; call to c-shell failed.")
		print("Command that failed: " + cat_command)
		sys.exit(1)
		return(None)
	else:
		print(str(out0))
		print(str(err0))
		print("returning " + outfile)
		return(outfile)

def call_cat_min(perm_in, perm_out, input_dir, infiles, scratch_dir, outfile, index_str=""):
	#Convenience wrapper for calling cat_min.csh
	print("calling cat_min")
	infile_str = ",".join(infiles)
	cat_min = cat_min_loc + "concatenate_files_nco.csh"
	cat_command = " ".join([cat_min, perm_in, perm_out, input_dir, infile_str, scratch_dir, outfile])
	print(str(len(cat_command)))
	if len(cat_command) > 8191:
	    print("Warning in call_cat_min! The command was more than 2^13-1 characters long, which may lead to errors!")	
	cat_cmd_file = osp.join(scratch_dir, "cat_cmd.csh")
	with open(cat_cmd_file, 'w') as f:
	    f.write("#!/bin/tcsh\n")
	    f.write(cat_command)
	print("tcsh -f " + cat_cmd_file)
	p = subprocess.Popen("tcsh -f " + cat_cmd_file, stdout=subprocess.PIPE, shell=True, env=None)
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
		print(str(err0))
		print("returning " + outfile)
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
