#!/bin/python
#Versions of the anomaly copying scripts for the 
#trend/detrend workflow
import os,sys,subprocess,shutil
from os import path as osp
from subprocess import PIPE
import argparse
import shlex
import fudgeList
import checker
import smtplib
import re
import netCDF4
from netCDF4 import Dataset
import fudgeUtils
from fudgeUtils import send_status_email
from fudgeUtils import gcp

def callCopyToTrendDetr(final_out_dir, exp_name, var, supp_string, mailid=None, sbase="", uinput=""):
	#Copies output to trend+resid output directory and changes the var name
	#Mostly expected for suppementary files (_qcmask, _save)
	out_files = [f for f in os.listdir(final_out_dir) if osp.isfile(osp.join(final_out_dir, f))]
	out_files = [f for f in out_files if re.search(var, f) is not None]
	if len(out_files) > 1:
		print("Error in callCopyToAnom: more than one f in " + final_out_dir + " matches " + var + "!")
		sys.exit(1)
	#Get modified file path
	fpath  = osp.join(final_out_dir, out_files[0])
	#orig_var = var.split('Detr')[0]
	#trendetr_var = re.sub("Detr", "", var)
	try:
		#detr_method = re.match('.*?(Detr.*?)_', var).group(1)
		detr_method = re.match('.*?(Detr.*?)', var).group(1)
	except:
		print("Error in callCopyToTrendDetr! Regular expression failed to find" + \
		      "Detr in expression!")
		sys.exit(1)      	
	#trendetr_var = re.sub('Detr.+?_', '_', var)
	#trendetr_climvar = var.split('Detr')[0]
	trendetr_var = re.sub(detr_method, "", var)
	trendetr_fpath = re.sub(detr_method, "", fpath)
	print("Copying supplementary detrended files to new location!")
	print(fpath)
	print(trendetr_fpath)
	gcp(fpath, trendetr_fpath)
	nco_command = "ncrename -v " +var+","+trendetr_var +" "+trendetr_fpath
	try:
		p = subprocess.Popen(nco_command,shell=True,stdout=subprocess.PIPE, stderr=subprocess.PIPE)
	except: 
		print("Error in subprocess call of callCopyToTrendDetr! Please examine call in more detail.")
		print(nco_command)
		if(mailid is not None):
			send_status_email("failure", 'postProc - copy to trend+detr', exp_name, \
			           traceback = "Error in call to ncrename - call for reference: " + nco_command,\
		               mailid=mailid, logfiles = os.path.join(sbase, "log/out.*"), \
		               outfiles = final_out_dir)
		sys.exit(1)	
	error,output = p.communicate()
	print(error)
	print(output)
	if(p.returncode != 0):
		err_message = "error in call to ncrename"
		print ("Error in renaming! There was an " + err_message)
		print(r_command)
		if(mailid is not None):
			send_status_email("failure", 'postProc - copy to trend+detr', exp_name, \
			           traceback = "Error in call to ncrename - call for reference: " + nco_command,\
	                   mailid=mailid, logfiles = os.path.join(sbase, "log/out.*"), \
	                   outfiles = final_out_dir)
	else:
		print "gcp and rename successful"
	return(trendetr_fpath)		
		
		
	
def callAddResid(final_out_dir,exp_name,var,mailid=None,sbase="",uinput=""):
	#Figure out the final output in the output dir:
	out_files = [f for f in os.listdir(final_out_dir) if osp.isfile(osp.join(final_out_dir, f))]
	#If there is more that one file present, select the one with the shortest name
	detr_file = min(out_files, key=len)
	detr_file = osp.join(final_out_dir, detr_file)
	#Find the location for the trend + detr file
	#note: need to remove 'Detr' AND the method afterwards
	trend_detr_var = var.split('Detr')[0]
	trend_detr_file = re.sub(var,trend_detr_var,detr_file)
	print(final_out_dir)
	print(exp_name)
	print(var)
	print(trend_detr_var)
	print(trend_detr_file)
	with Dataset(detr_file, 'r') as nc_obj:
		try:
			trend = nc_obj.__dict__['trend']
		except KeyError:
			trend_error = 'Trend not found in input file ' + detr_file
			print("Error in trend addition! " + trend_error + " ;exiting")
			if( mailid is not None):
				send_status_email("failure", 'callAddResid', total_exp_name, \
							traceback = trend_error,\
		                   mailid=mailid, logfiles = os.path.join(sbase, "log/out.*"), \
		                   exp_info_file = os.path.join(sbase, 'experiment_info.txt'), \
		                   outfiles = final_out_dir, xml = uinput)	
			exit(1)
			
	#Remove temporary directories from beginning of pathname, 
	#going as far as needed to complete
	trend_split = trend.split("/")
	s_idx = 0
	trend_exists = False
	while(s_idx < len(trend_split)):
		if(osp.exists("/".join(trend_split[s_idx:len(trend_split)]))):
			trend_exists = True
			trend_file = "/".join(trend_split[s_idx:len(trend_split)])
			break
		else:
			s_idx += 1		
	if(not trend_exists):
		trend_error = "trend file " + trend + ' not found on system'
		print ("Error in anomaly addition!" + trend_error + " ; exiting")
		if( mailid is not None):
			#send_anom_message('failure', trend_error, exp_name,mailid,
			#						anom_file,trend,"None",trend_err,"")
			send_status_email("failure", 'postProc', total_exp_name, \
			               traceback = "trendatology file " + trend + "not found on system",\
		                   mailid=send_to, logfiles = os.path.join(sbase, "log/out.*"), \
		                   #exp_info_file = os.path.join(sbase, 'experiment_info.txt'), \
		                   outfiles = final_out_dir)						
		exit(1)
	else:
		if( not osp.exists(osp.dirname(trend_detr_file)) ):
			print 'Making output anom + trend directory'
			os.makedirs(osp.dirname(trend_detr_file))
		#print(os.environ)
		try:		
			s8_orig = os.environ.get("S8_PATH")
		except:
			s8_orig=""
		#s8_path = "/home/cew/Code/section_8_tools/"
		#os.putenv("S8_PATH", s8_path)
		if (s8_orig is None):
			print("Warning in anomaly addition: $S8_PATH not set.")
			print("resetting to empty string")
			s8_orig=""
		tdir = os.environ.get("TMPDIR")
		trend_detr_file_out = osp.join(tdir, trend_detr_file.lstrip("/"))
		if not osp.exists(osp.dirname(trend_detr_file_out)):
			os.makedirs(osp.dirname(trend_detr_file_out))
		print(trend_file)
		trend_file_out = gcp(trend_file, 'TMPDIR')
		#print(trend_detr_file)	
		detr_file_out = gcp(detr_file,'TMPDIR')	
		#s8_orig = os.environ["S8_PATH"]
		s8_fudge = os.environ["FUDGE_S8_LOC"]
		os.environ["S8_PATH"] = s8_fudge
		#print(os.environ)
		r_command = "Rscript " + s8_fudge + "/AddResid.R --trend " + str(trend_file_out) + " --resid " + str(detr_file_out) + ' --var ' + str(trend_detr_var) + " -o " + str(trend_detr_file_out) + " --clone_globals"
		print "Calling AddResid.R"
		print(r_command)
		try:
			p = subprocess.Popen(r_command,shell=True,stdout=subprocess.PIPE, stderr=subprocess.PIPE) 
		except: 
			print("Error in subprocess call of callAddResid! Please examine call in more detail.")
			print(r_command)
			os.environ["S8_PATH"] = s8_orig
			sys.exit(1)	
			if(mailid is not None):
				send_status_email("failure", 'postProc - residAdd', exp_name, \
				           traceback = "Error in call to AddResid.R - call for reference: " + r_command,\
		                   mailid=mailid, logfiles = os.path.join(sbase, "log/out.*"), \
		                   outfiles = final_out_dir)
			sys.exit(1)                
		error,output = p.communicate()
		os.environ["S8_PATH"] = s8_orig
		#Check on what this returns with an R error
		print(error)
		print(output)
		if(p.returncode != 0):
			err_message = "error in call to AddResid.R"
			print ("Error in trend addition! There was an " + err_message)
			print(r_command)
			if(mailid is not None):
				send_status_email("failure", 'postProc - residAdd', exp_name, \
				           traceback = "Error in call to AddResid.R - call for reference: " + r_command,\
		                   mailid=mailid, logfiles = os.path.join(sbase, "log/out.*"), \
		                   #exp_info_file = os.path.join(sbase, 'experiment_info.txt'), \
		                   outfiles = final_out_dir)
			sys.exit(1)                
		else:
			print "Trend addition successful"
			print "Copying to final location"			
			gcp(trend_detr_file_out, trend_detr_file)											
	return trend_detr_file	

def send_anomadd_message(status,exp_name,mailid,
						anom_file,clim_file,clim_anom_file,
						err_message="",aux=""):
	#Helper function for sending messages from the anomaly addition code					
	sep = "\n\n"
	subject = "Fudge postProc anomaly addition"  + status + " for"  + exp_name
	if (err_message != ""):
		err_message = "due to " + err_message
	text = "Anomaly addition experienced a " + status + err_message + "."
	text = text + sep + "Anomaly input: " + anom_file
	text = text + sep + "Climatology input: " + clim_file
	text = text + sep + "Reconstructed output: " + clim_anom_file
	#text = text + sep + "Logfiles: " + anom_file	
	if (aux != ""):
		text = text + sep + aux		
	notifyUser(mailid,subject,text)	
