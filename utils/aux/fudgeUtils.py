#GFDL- or FUDGE-specific utility functions
#likely to be used by more than one of the tools in the postproc workflow.
import os,sys,subprocess,shutil
from os import path as osp
from subprocess import PIPE

def gcp(infile, outfile="TMPDIR", verbose=False, gcp_verbose=False):
	#Wrapper for GFDL's GCP function
	#for copying from /archive to /vftmp
	if (outfile=='TMPDIR'):
		tdir = os.environ.get("TMPDIR")
		outfile = osp.join(tdir,infile.lstrip("/"))
		#os.mkdirs(outpath)
	#outpath = osp.join(tmpdir,osp.dirname(infile))
	if gcp_verbose:
		command = "gcp -v -cd " + infile + " " + outfile
	else:
		command = "gcp -cd " + infile + " " + outfile
	if verbose:	
		print(command)
	g = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
	g_out,g_err = g.communicate()
	if(g.returncode != 0):
		print "Error in gcp: file " + infile + " did not copy to " + outfile
		print(g_out)
		print(g_err)
		print "Trying again with cp"
		if not osp.exists(osp.dirname(outfile)):
			os.makedirs(osp.dirname(outfile))
		shutil.copy2(infile, outfile)
	return(outfile)		
	
def read_exp_info_file(exp_info):
	#Reads data formatted in the general form of an experiment info file
	expdict = {}
	with open(exp_info, 'r') as infile:
		for line in infile:
			#print line
			if "#" not in line[0]:
				try:
					keytmp,entrytmp = line.split(":")
				except:
					keytmp = None
					entrytmp = str(line)	
				entrytmp = entrytmp.rstrip("\n")
				if (keytmp is None):
					try:
						entry.append(entrytmp)
					except:
						entry=[entry, entrytmp]
				else:
					key,entry=keytmp,entrytmp	
				expdict[key] =  entry
	return expdict	
