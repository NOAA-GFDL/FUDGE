#GFDL- or FUDGE-specific utility functions
#likely to be used by more than one of the tools in the postproc workflow.
import os,sys,subprocess,shutil,time
from os import path as osp
from subprocess import PIPE
import re

def gcp(infile, outfile="TMPDIR", timestamp=False, verbose=False, gcp_verbose=False):
	#Wrapper for GFDL's GCP function
	#for copying from /archive to /vftmp
	if (outfile=='TMPDIR'):
		tdir = os.environ.get("TMPDIR")
		outfile = osp.join(tdir,infile.lstrip("/"))
	if timestamp: 
		outfile  = outfile +"." + tstamp()	
	if gcp_verbose:
		command = "gcp -v -cd " + infile + " " + outfile
	else:
		command = "gcp -cd " + infile + " " + outfile
	if verbose:	
		print(command)
	modulepath = "source /usr/local/Modules/default/init/sh"	
	mod_command = modulepath + "; module load gcp; module load globus;"	
	g = subprocess.Popen(mod_command + command, stdout=subprocess.PIPE, stderr=subprocess.PIPE,shell=True,env=os.environ)
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
	
def read_exp_info_file(exp_info, convert_to_j = True):
	#Reads data formatted in the general form of an experiment info file
	print(exp_info)
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
	#Bugfix to make sure that single rips are always written as a list
	if isinstance(expdict['output.path'], basestring):
		expdict['output.path'] = [expdict['output.path']]
	#Edit to create check in right location for j-style parsing
	tmp = expdict['output.path']
	oned_dirs = ['OneD', 'ZeroD', 'grid0']
	if not any([re.search(el, tmp[0]) is not None for el in oned_dirs]):
		expdict['output.path'] = [el + '/OneD' for el in expdict['output.path']]
	#	for t in range(len(tmp)):
	#		tmp[t] = [osp.join(tmp[t], o) for o in os.listdir(tmp[t]) if osp.isdir(osp.join(tmp[t], o)) and o in oned_dirs]
	return expdict	
	
def tstamp():
	#Returns a simple timestamp for the output file
	return(re.sub("\\.", "_", str(time.time())))	
	
def notifyUser(mailid, subject, text):
	#Implemenation of email notifications 
	#using the mailx utility instead.
	#Since mailx is used for FRE, this should be less
	#likely to break.	
	print('entering notifyUser')
	#uinfo = os.environ.get('USER')
	tmpdir = os.environ.get("TMPDIR")
	hostnam = "princeton.rdhpcs.noaa.gov"
	#send_from = uinfo+"@"+hostnam
	send_to = mailid
	if (mailid is not None):
		text_file = osp.join(tmpdir, "fudge_mail.tmp")
		f = open(text_file, 'wb')
		f.write(text)
		f.close()
		command = "mailx -r " + 'fudge.esd'+'@'+"noaa.gov" +" -s '" + subject +  "' '" + mailid + "' < " + text_file
		#command = "mailx -r " + 'oar.gfdl.esd'+'@'+"noaa.gov" +" -s '" + subject +  "' '" + mailid + "' < " + text_file
		m = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
		m_out,m_err = m.communicate()
	print("message sent and completed")		

def notifyUser_py(mailid,subject,text):
	#Implementation of email notifications using python's 
	#smtp package.
		# email header
		print('entering notifyUser')
		uinfo = os.environ.get('USER')
		hostnam = "princeton.rdhpcs.noaa.gov"
		send_from = uinfo+"@"+hostnam
		send_to = mailid
		msg = MIMEMultipart()
		#msg['From'] = 'aparna.radhakrishnan@noaa.gov' #todo change to esd
		msg['From'] = 'oar.gfdl.esd'+'@'+"noaa.gov" #todo change to esd
		msg['To'] = mailid
		msg['Date'] = formatdate(localtime=True)
		msg['Subject'] = subject
		msg.attach( MIMEText(text))
		if (mailid is not None):
			 smtp = smtplib.SMTP("localhost")
			 smtp.sendmail(send_from, [send_to], msg.as_string())
			 smtp.close() 
			 
def send_status_email(status,step,exp_name,mailid, \
						traceback="",exp_info_file="", logfiles = "", 
						outfiles = "", note="", xml=""):
	#Sends an email message to a user explaining the current
	#status of the workflow. 	
	text = ""				
	if status=='failure':
		subject = exp_name + " failed in " + step
		text = "FUDGE " + step + " failed with the following error message:"
		text = text + "\n\n" + traceback + "\n\n"						
	elif status=='success':
		if step == 'complete':
			subject = exp_name + " completed successfully!"
			text = "FUDGE experiment " + exp_name + " completed all steps successfully."
			if note != "":
				text = text + "\n" + note
		else:
			subject = exp_name + " finished step " + step
	if exp_info_file != "":
		text = text + "\nExperiment info file: " + exp_info_file + "\n"
	if xml != "":	
		text = text + "\nExperiment XML:" + xml + "\n"
	if logfiles != "":
		if isinstance(logfiles,basestring):
			logfiles = [logfiles]
		text = text + "\nLogfiles:"
		for l in logfiles:
			text = text + "\n\t" + l
	if outfiles != "":	
		if isinstance(outfiles,basestring):
			outfiles = [outfiles]		
		text = text + "\nOutput files:"
		for o in outfiles:
			text = text + "\n\t" + o
	print(mailid)
	print(subject)
	print(text)						
	notifyUser(mailid, subject,text)
	
def get_nc_dim_lengths(nc_file, dimnames):
	#Gets the length of a named dimension from an input netcdf file.
	#Assumes that your system has ncdump available.
	#Yes, this is a bit faster than reading into Python
	# -C 8 is an attempt to get a newline behind every dim
	#
	#GFDL-specific code for checking tape migration
	dm_cmd = 'dmls -l ' + nc_file
	p0 = subprocess.Popen(dm_cmd, shell=True, stdout=PIPE, stdin=PIPE, stderr=PIPE)
	out0,err0 = p0.communicate()
	if p0.returncode == 0:
		if re.search('(OFL)', out0) is not None:
			p0 = subprocess.Popen("dmget " + nc_file,
			                     shell=True, stdout=PIPE, stdin=PIPE, stderr=PIPE)
			out0,err0 = p0.communicate()
	else:
		print("Error in get_nc_dim_length: file " + nc_file + " not found on disk or tape!")
		sys.exit(1)
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

def get_offsets(nc_file):
	#Gets the value of any dimensions named '*_offset' in the
	#input netcdf file.
	offsets = ['i_offset', 'j_offset']
	offdict = {}
	#system command with ncdump
	for offset in offsets:
		sys_cmd = "ncdump -v " + offset + " " + nc_file+" | grep " + offset + " = "
		p1 = subprocess.Popen(sys_cmd, shell=True, stdout=PIPE, stdin=PIPE, stderr=PIPE)
		out1,err1=p1.communicate()
		#If your return code is not 0, you probably didn't get a hit for the offset name
		if p1.returncode == 0:
			tmpstr = re.search(d + " \= (.*?)",out1).group(0)
			offval = re.search(r'\d+',tmpstr).group(0)
			offdict[offset] = int(offval)
		else:
			offdict[offset] = 1
	return(offdict)			

						 
