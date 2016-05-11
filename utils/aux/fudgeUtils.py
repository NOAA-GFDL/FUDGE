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
		command = "mailx -r " + 'oar.gfdl.esd'+'@'+"noaa.gov" +" -s '" + subject +  "' '" + mailid + "' < " + text_file
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
