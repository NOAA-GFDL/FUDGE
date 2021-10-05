#GFDL- or FUDGE-specific utility functions
#likely to be used by more than one of the tools in the postproc workflow.
import os,sys,subprocess,shutil,time
from os import path as osp
from subprocess import PIPE
import re

# 2019-05-28: gcp is now in its own file, with commandline args
<<<<<<< HEAD
=======
# search for gcp.py
>>>>>>> master

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
    if isinstance(expdict['output.path'], str):
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
    if status == 'failure':
        subject = exp_name + " failed in " + step
        text = "FUDGE " + step + " failed with the following error message:"
        text = text + "\n\n" + traceback + "\n\n"
    elif status == 'success':
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
        if isinstance(logfiles,str):
            logfiles = [logfiles]
        text = text + "\nLogfiles:"
        for l in logfiles:
            text = text + "\n\t" + l
    if outfiles != "":
        if isinstance(outfiles,str):
            outfiles = [outfiles]
        text = text + "\nOutput files:"
        for o in outfiles:
            text = text + "\n\t" + o
    print(mailid)
    print(subject)
    print(text)
    notifyUser(mailid, subject,text)
