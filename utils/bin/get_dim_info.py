#!/usr/bin/python
#Playing around with getting dimension information from
#looking at the python output from one of these

import netCDF4
from netCDF4 import Dataset
import numpy as np

def get_nc_dim_lengths_sys(nc_file, dimnames):
	#Gets the length of a named dimension from an input netcdf file.
	#Assumes that your system has ncdump available.
	#Yes, this is a bit faster than reading into Python
	# -C 8 is an attempt to get a newline behind every dim
	if isinstance(dimnames, basestring):
		dimnames = [dimnames]
	#system command with ncdump
	sys_cmd = "ncdump -h "+nc_file+" | grep -C 8 'dimensions:'"
	p1 = subprocess.Popen(sys_cmd, shell=True, stdout=PIPE, stdin=PIPE, stderr=PIPE)
	out1,err1=p1.communicate()
	if p1.returncode != 0:
		print(sys_cmd)
		print(out1)
		print(err1)
		sys.exit(1)
	#get dimnames
	coords = {}
	for d in dimnames:
		tmpstr = re.search(d + " \= (.*?)\n",out1).group(0)
		cdim = re.search(r'\d+',tmpstr).group(0)
		coords[d] = int(cdim)
	return(coords)
	
def get_nc_var_vals_sys(nc_file, varname):	
	#Gets the values of a named variable from a netcdf file
	#Assumes that your system has ncdump available.
	#Yes, this is a bit faster than reading into Python
	# ...and this is meant for vals that are going to have a single val 
	# attached to them, and that themselves are integers.
	if isinstance(varnames, basestring):
		varnames = [varnames]
	#system command with ncdump
	sys_cmd = "ncdump -v <VARNAME> "+nc_file+" | grep -A 8 'data:'"
	#get var vals
	vals = {}
	for v in varnames:
		v_cmd = copy.copy(sys_cmd)
		v_cmd.replace("<VARNAME>", v)
		p1 = subprocess.Popen(v_cmd, shell=True, stdout=PIPE, stdin=PIPE, stderr=PIPE)
		out1,err1=p1.communicate()
		if p1.returncode != 0:
			print(v_cmd)
			print(out1)
			print(err1)
			sys.exit(1)
		tmpstr = re.search(v + " \= (.*?)\n",out1).group(0)
		var_vals = re.search(r'\d+',tmpstr).group(0)
		vals[v] = int(var_vals)
	return(vals)
		
	
def get_nc_dim_lengths(nc_file, dimnames):
	'''
	Gets the lengths of given dimensions in an input file. 
	Depends upon knowing what the names of the dimensions are.
	'''
	if isinstance(dimnames, basestring):
		dimnames = [dimnames]	
	coords = {}
	with Dataset(nc_file, 'r') as nc_obj: 
		dims = nc_obj.dimensions
		for d in dimnames:
			try:
				coords[d] = len(dims[d])	
			except KeyError:
				#print("Error in get_nc_dim_lengths: no dim named " + d + "!")
				#sys.exit(1)
				##...it may be a good idea to replace this call with a legth of 0
				coords[d] = 0
	return(coords)
	
def get_nc_var_vals(nc_file, varnames):
	'''
	Gets the values of a variable within a netcdf file. 
	Depends upon knowing the names of the variables; it is assumed 
	that this tool is most often used for getting a single value, 
	like an i_offset . 
	'''	
	if isinstance(varnames, basestring):
		varnames = [varnames]
	vals = {}
	with Dataset(nc_file, 'r') as nc_obj:
		for v in varnames:
			try:
				vals[v] = list(nc_obj.variables[v][:])
			except KeyError:
				#print("Error in get_nc_var_vals: no var named " + v + "!")
				#sys.exit(1)
				vals[v] = []
	return(vals)
					
def main():
	#Check on dimension information when doing subsetting
	#so for each file that you are interested in, you need to get
	# the x and y dims (lat and lon) and then check them 
	# against the x and y dims you currently have access to
	# ... I'm honestly not sure that you need the i and j offset
	# if you aren't working with the start and end coordinates in 
	# the xml .. except for the fact that that's how we specify 
	# partial grid downscaling. You need an i and j offset for 
	# that case. 
	nc_files = ['/home/cew/Code/testing/test_minfiles/9_pt_tasmax.nc', 
	            '/home/cew/Code/testing/test_minfiles/9_pt_tasmin.nc', 
	            '/home/cew/Code/testing/test_minfiles/tasmax_day_GFDL-HIRAM-C360_sst2090_r1to3i1p1_US48_20860101-20951231_ensmbl.I800_J454-567.nc']
	imin = ""
	max_dims = [get_nc_dim_lengths(f, ['lat', 'lon']) for f in nc_files]
	lat_len = list([el['lat'] for el in max_dims])
	lon_len = list([el['lon'] for el in max_dims])
	print(lat_len)
	print(lon_len)
	if any([el != lat_len[0] for el in lat_len]):
		print("Error in this function: one of the x dims in inputs did not match!")
		sys.exit(1)
	if any([el != lon_len[0] for el in lon_len]):
		print("Error in this function: one of the y dims in inputs did not match!")
		sys.exit(1)
	#...that gets you those, though. What about the i and j offset?
	#you do need those for figuring out whether you are running a subset 
	#of a larger grid as a job
	i_offsets = [get_nc_var_vals(f, 'i_offset')['i_offset'][0] for f in nc_files]
	j_offsets = [get_nc_var_vals(f, 'j_offset')['j_offset'][0] for f in nc_files]
	if any([el != i_offsets[0] for el in i_offsets]):
		sys.exit(1)
	if any([el != j_offsets[0] for el in j_offsets]):
		sys.exit(1)
	#...so what do we do with these?
	#which i_indices are we going to be looping over?
	i_elements = range(int(i_start), int(i_end)+1, i_by)
	i_elements.append(int(i_end)+1)
	i_elements = list(set(i_elements))
	i_elements = sorted(i_elements)
	#i_offset = int(i_start)  - 1
	i_max = imaxes[0] #int(i_end) - i_offset
	#which j indices are we going to be looping over?
	j_elements = range(int(j_start), int(j_end), j_by)
	j_elements.append(int(j_end)+1)
	j_elements = list(set(j_elements))
	j_elements = sorted(j_elements)
	#j_offset = int(j_start)  - 1
	j_max = jmaxes[0] #int(j_end) - j_offset
	#sys.exit("Examine this out")	
	
	#Python code from make_trend_and_detr_piecewise: keep tinkering with this
	njobs = 7
	if (njobs > i_len):
		print("Number of subjobs (" + str(njobs) + ") is greater than number of i indices (" + str(i_len) + ").")
		print("Submitting single master with all i indices.")
		n_indices = [1, i_len]
	else:
		nmaster = ( i_len / njobs ) + 2
		#The above used to be +1, but I'm pretty sure that this needs to be
		#+2 to avoid edge cases for small n. It's not going to make much
		#difference on large n (not with njobs = 8), but it's going to divide
		#small jobs more evenly.
		#...this does mean that there's no way you can get only 2 jobs
		#submitted to the queue, but that's probably ok.
		n_indices = [int(el) for el in np.linspace(1, i_len, num=nmaster)]
	
if __name__ == '__main__':
	main()

	
	
