#!/bin/python
# Looking into a framework for a watcher script
import os, sys
from os import path as osp
import re
import subprocess
from subprocess import PIPE
import time
from get_ds_info import parse_aux_info
from fudgeUtils import send_status_email
from profile_FUDGE import make_prof_file
import tarfile
from collections import OrderedDict
#import optparse
from optparse import OptionParser
from gcp import gcp
from fudgeUtils import tstamp
from make_all_ds_paths import make_all_ds_paths
from call_next_step_ens import submit_ens_job
import shutil
from pathlib2 import Path


def ParseNextStepArgs():
    '''
    Parses the arguments coming in from the commandline
    '''
    #Input parsing and arg checking for main body of code
    usage = "usage: %prog [-s scriptdir] [--verbose]"
    parser = OptionParser(usage=usage, version='CallNextStep 1.0')
    parser.add_option("-s", "--scriptdir", dest='scriptdir', help="The path to the scripts directory for the experiment.")
    parser.add_option("--sleep", dest='sleep', default=60, help='How long to sleep in between loops. Defaults to 60 if script is submitted to ppan.')
    parser.add_option("--nloops", dest='nloops', default=100, help='How many loops watcher will go through before timing out. Defaults to 100 if script is submitted to ppan.')
    parser.add_option("-m", "--mail", dest='mail', default='none', help="Where to email watcher status messages to. If not set, will first check environment vars for NEMSemail before defaulting to none, which sends no emails.")
    parser.add_option("-v", "--verbose", dest='verbose', action='store_true', default=False, help="Print extra status messages to help debugging; defaults to False")
    # other opts: mailid, timeout, sleep interval
    #parser.add_option("--sev", dest="sev", default=1, help="Level of status messages provided. Defaults to 1 and goes to 3; higher numbers mean more messages.")
    (options, args) = parser.parse_args()
    if(options.mail=="none"):
        try:
            NEMSemail1 = os.environ["NEMSemail"]
        except KeyError:
            print "-m option unused and NEMSemail environmental var not set. If you want email notifications, enter them manually."
            NEMSemail1 = 'none'
    else:
        NEMSemail1 = options.mail
    main(options.scriptdir, options.sleep, options.nloops, NEMSemail1, options.verbose)

def main(scriptdir, sleep=60, nloops=100,
         mailid='Carolyn.Whitlock@noaa.gov', verbose=False):
    '''
    Main looping structure for the watcher
    '''
    statdir = osp.abspath(osp.join(scriptdir, 'status'))
    logdir = osp.abspath(osp.join(scriptdir, 'log'))
    #Get parameters from the auxfile/config_mod file
    auxdir = osp.join(scriptdir, 'config_mod')
    auxfile = osp.join(auxdir, [el for el in os.listdir(auxdir)
                                      if re.search('json', el) is not None][0])
    auxinfo, auxdict = parse_aux_info(auxfile)
    exp_info = osp.join(scriptdir, 'experiment_info.txt')
    exp_name = auxdict["ds_experiment_name"]
    num_ens = len(auxdict['DS']['rips'])
    make_qcmask = auxdict['make_qcmask']
    outfile_dict = make_all_ds_paths(auxfile, None)
    #make_all_ds_paths(auxfile, "/home/cew/PROJECTS/DOWNSCALING/scripts/PMgCR/PMgCRdtrp1-QDM-B00r2e2X01K00/outfile_paths.txt")
    vars = auxdict['DS']['vars']
    # Note to self: these are not currently used by anything
    is_anom = [re.search("Anom", el) is not None for el in vars]
    is_trend = [re.search("Trend", el) is not None for el in vars]
    crossval_patt = ['00r1e2', '00r2e2']
    cv_match = [crossval_patt.index(c) for c in crossval_patt if re.search(c, exp_name) is not None]
    is_cv = (len(cv_match) > 0)
    if is_cv:
        exp_name2,scriptdir2 = get_crossval_properties(crossval_patt, cv_match, exp_name, scriptdir)
    stage = 'master'
    stagedir = osp.join(scriptdir, 'status/stage')
    if not osp.isdir(stagedir):
        os.makedirs(stagedir)
    master_retry = 1
    status = 'wait'
    is_done = False
    t = 0
    t_max = nloops
    #sleepytime = 60 * 10
    sleepytime = float(sleep)
    outfiles = OrderedDict()
    out_expinfo_locations = []

    while not is_done and t < t_max:
        if verbose:
            print("starting watcher loop " + str(t))
            print("in stage " + stage + ", status " + status)
        skip_wait = False
        if stage == 'master':
            print(master_retry)
            status,faillist,master_retry = check_master_status(osp.join(statdir, 'master'),
                                                  osp.join(scriptdir, 'master'),
                                                  master_retry, verbose)
            if status == 'success':
                stage = 'mini'; status = 'wait'
                Path(osp.join(stagedir, "master.success")).touch()
            elif status == 'fail':
                logfile_loc = logdir + "/master." + exp_name + ".*"
                tmessage = "One or more master jobs have failed and unsuccessfully retried; sample master slices include " + (",").join(faillist)
                send_status_email("failure", 'master', exp_name,
                               traceback= tmessage,
                               mailid=mailid, logfiles = logfile_loc,
                               exp_info_file = exp_info, outfiles = "")
                Path(osp.join(stagedir, "master.fail")).touch()
                sys.exit(1)
        elif stage == 'mini':
            status,faillist = check_mini_status(osp.join(statdir, 'minifile'),
                                       osp.join(scriptdir, 'runscript'),
                                       make_qcmask, verbose)
            if status == 'success':
                stage = 'postProc'; status = 'wait'
                Path(osp.join(stagedir, "mini.success")).touch()
                submit_postproc_jobs(scriptdir, verbose)
            elif status == 'fail':
                logfile_loc = logdir + "/out.*"
                tmessage = "One or more of the minifiles/subjob chunks have failed; failed indices include " + (",").join(faillist)
                send_status_email("failure", 'R code', exp_name,
                               traceback= tmessage,
                               mailid=mailid, logfiles = logfile_loc,
                               exp_info_file = exp_info, outfiles = "")
                Path(osp.join(stagedir, "mini.fail")).touch()
                sys.exit(1)
        elif stage == 'postProc':
            # Note to self: concatenation steps include both postproc and anomaly/trend addition
            status,faillist = check_pp_status(osp.join(statdir, 'postproc'),
                                              osp.join(scriptdir, 'postProc'),
                                              verbose)
            if status == 'success':
                if num_ens > 1:
                    stage = 'ensemble'; status = 'wait'
                    #raw_outdir, raw_outfile, rips, vars, exp_name, epoch, trange_string, verbose)
                    raw_outdir = auxdict['DS']['out_dir']
                    raw_outfile = auxdict['DS']['out_file']
                    rips = auxdict['DS']['rips']
                    vars = auxdict['DS']['vars']
                    ens_outfiles = [outfile_dict['ds'][el]['ens'] for el in outfile_dict['ds'].keys()]
                    # submit_ens_job is supposed to do the trend and anomaly stuff;
                    epoch = "fake-epoch"
                    trange_string = "fake-trange"
                    status,faillist,ens_outfiles,exp_info_files = submit_ens_job(raw_outdir, raw_outfile, rips, vars, exp_name, epoch, trange_string, verbose)
                    Path(osp.join(stagedir, "postproc.success")).touch()
                else:
                    stage = 'end'; status = 'success'
                    skip_wait = True
            elif status == 'fail':
                logfile_loc = logdir + "/fudge_pp_*"
                tmessage = "One or more of the postproc jobs failed; failed jobs include " + (",").join(faillist)
                rmessage = '''
                If you want to try re-running the failed postproc scripts to recover, try running the scripts in
                ''' + osp.join(scriptdir, 'postProc/${rip}.${var}.${type}')
                send_status_email("failure", 'postproc', exp_name,
                               traceback= tmessage + "\n" + rmessage,
                               mailid=mailid, logfiles = logfile_loc,
                               exp_info_file = exp_info, outfiles = "")
                Path(osp.join(stagedir, "postproc.failure")).touch()
                sys.exit(1)
        elif stage == 'ensemble':
            if num_ens > 1:
                #status = check_ens_status()
                if status == 'success':
                    stage = 'crossval' ; status = 'submit'
                    Path(osp.join(stagedir, "ensemble.success")).touch()
                elif status == 'fail':
                    logfile_loc = osp.join(logdir, "watcher.*")
                    tmessage = "One or more of the ensemble concatenation jobs failed; failed jobs include " + (",").join(faillist)
                    send_status_email('failure', 'ensemble', exp_name,
                                      traceback=tmessage,
                                      mailid=mailid, logfiles=logfile_loc,
                                      exp_info_file=exp_info, outfiles="")
                    Path(osp.join(stagedir, "ensemble.failure")).touch()
                    sys.exit(1)
            else:
                stage = 'crossval'
                skip_wait = True
        elif stage == 'crossval':
            if is_cv:
                if status == 'submit':
                    status,faillist,cv_outfiles = submit_crossval_job(outfile_dict['cv'], auxdict['DS']['vars'])
                #elif status == 'wait':
                #    status = check_crossval_status(cv_match, exp_name2, scriptdir2)
                elif status == 'success':
                    stage = 'end'; status = 'wait'
                    skip_wait = True
                else:
                    logfile_loc = ""
                    tmessage = ""
                    send_status_email('fail', 'crossval', exp_name,
                                      traceback=tmessage,
                                      mailid=mailid, logfiles=logfile_loc,
                                      exp_info_file=exp_info, outfiles="")
                    sys.exit(1)
            else:
                stage = 'end' ; status = 'wait'
                skip_wait = True
        elif stage == 'end':
            print("done with CallNextStep looping")
            # print_end_message()
            is_done = True
            skip_wait = True
        t += 1
        if not skip_wait:
            print("sleeping for " + str(sleepytime) + " seconds.")
            time.sleep(sleepytime)
    print("Done with watcher looping")
    if t > t_max:
        print("Watcher timed out")
        tmessage = "Wacher timed out; more than " + str(t_max) + " loops took place."
        rmessage = '''
        If you believe that this was a simple timeout with no steps failed, \
        try restarting the watcher script; if that fails, contact Carolyn.
        '''
        send_status_email('fail', 'watcher', exp_name,
                          traceback=tmessage + "\n" + rmessage,
                          mailid=mailid, logfiles=logfile_loc,
                          exp_info_file=exp_info, outfiles="")
    else:
        print("outfile exp_info + script exp_info:")
        script_info = osp.join(scriptdir, 'experiment_info.txt')
        exp_paths = get_exp_info_paths(outfile_dict) + [scriptdir]
        exp_info_files = [osp.join(el, 'experiment_info.txt') for el in exp_paths]
        for el in exp_info_files:
            if not osp.isfile(el):
                shutil.copy2(script_info, el)
        tmessage = "The experiment " + exp_name + "completed successfully!"
        note = "Profiling information for the experiment is located in: " + osp.join(scriptdir, "profile")
        with open(osp.join(scriptdir, 'outfile_paths.txt'), 'r') as f:
            outpath_str = f.read()
        outfiles = "\n" + outpath_str
        outsplit = re.split('\n', outfiles)
        outsplit = [re.split(':', el)[1] for el in outsplit if re.search('\.ens:', el) is not None]
        if len(outsplit) == 0:
            outsplit = [el for el in re.split('\n', outfiles) if el != ""]
            outsplit = [re.split(':', el)[1] for el in outsplit]
        exp_append = "\n" + "#" * 80 + '\n#' + '\n#'.join(outsplit) #outpath_str
        for el in exp_info_files:
            with open(el, 'a') as f:
                f.write(exp_append + "\n")
        send_status_email("success", 'complete', exp_name,
                       traceback= tmessage,
                       mailid=mailid, logfiles = logdir,
                       exp_info_file = exp_info, outfiles = outsplit)
        Path(osp.join(stagedir, "experiment.success")).touch()
        print("File cleanup operations in progress")
        make_prof_file(scriptdir, scriptdir + "/log/")
        compress_logs(scriptdir + "/log/")

def get_exp_info_paths(file_dict, exp_info=[]):
    '''
    Recursively traverse a path of outfiles to return all paths
    that point to a dictionary location
    '''
    for el in file_dict:
        if not isinstance(el, (OrderedDict, dict)):
            # Need a single path to a single file
            if len(el) == 1 and osp.isfile(el):
                fpath = re.split("/", el)[:-3][-1]
                exp_info.append(fpath)
        else:
            exp_info = get_exp_info_paths(el, exp_info)
    return(exp_info)

def submit_crossval_job(outdict, ds_vars, verbose=False):
    '''
    Makes the cross-validated ensembles for an experiment
    returns:
    status: whether job failed or succeeded
    faillist: list of files that failed their step; helps with retry
    outfiles: list of files that succeeded; gets added to the print statement
    '''
    '''
    I think the general assumption for this particular piece of code
    needs to be that the structure is thus:
    cv:
		r1e2:
		r2e2:
		complement:
		DSDEPh_exp:
		DSCVh_exp
		var:
			dsdeph:
				ens:
				input:
			dscvh:
				ens:
				input:
    '''
    basedir = os.environ.get('BASEDIR')
    tdir = os.environ.get('TMPDIR')
    tdir_ens = osp.join(tdir, "proc_cv_ens")
    if not osp.exists(tdir_ens):
        os.makedirs(tdir_ens)
    types = ['dsdeph', 'dscvh']
    qc_vars = [el + "_qcmask" for el in ds_vars]
    anom_vars = [re.sub("Anom", "", el) for el in ds_vars]
    trend_vars = [re.sub("Trend", "", el) for el in ds_vars]
    ds_vars = list(set(ds_vars + qc_vars + anom_vars + trend_vars))
    outfiles = {}
    for v in ds_vars:
        if v in outdict.keys():
            for t in types:
                #print(outdict[v])
                #print(outdict[v][t])
                for f in outdict[v][t]['input']:
                    if not osp.isfile(f):
                        print("Error in submit_crossval_job for var " + v + "! File " + f + " does not exist!")
                orig_inFiles = ",".join(outdict[v][t]['input'])
                inFiles = ",".join([gcp(el) for el in outdict[v][t]['input']])
                outfile = outdict[v][t]['ens']
                vname = v
                rip_string = "fake_rip"
                exp_name = outdict['r2e2']
                cv_type = t
                trange_string = "fake_trange"
                # All below is taken from the cross-valdiation tool
                print("Output file location: " + outfile)
                if not osp.exists(osp.dirname(outfile)):
                    print("Making " + osp.dirname(outfile))
                    os.makedirs(osp.dirname(outfile))
                tmp_outfile = osp.join(tdir_ens,osp.basename(outfile)+tstamp())
                #Finally, call the ensemble generation code:
                ens_min = basedir + "utils/s7/ens_min.csh"
                ens_command = " ".join(["tcsh -f ",ens_min, tmp_outfile, vname,
                                       rip_string, exp_name, cv_type, trange_string,
                                       orig_inFiles, inFiles, "CV"])
                print('calling c-shell code')
                p = subprocess.Popen(ens_command, stdout=subprocess.PIPE, shell=True, env=None, cwd=tdir)
                out0,err0 = p.communicate()
                if (p.returncode != 0):
                    print(str(out0))
                    print(str(err0))
                    print("Error in processing ensemble " +  inFiles + "; call to c-shell failed.")
                    print("Command that failed: " + ens_command)
                    sys.exit(1)
                else:
                    if verbose:
                        print(str(out0))
                        print(str(err0))
                    gcp(tmp_outfile, outfile, verbose, verbose)
                    print("Copied to " + outfile)
                outfiles[v + "_" + t] = outfile
    return('success', [], outfiles)

def check_master_status(master_statdir, masterdir, retry, verbose):
    '''
    Checks the status of an experiment when it's doing things related
    to the master/minifile chunking steps
    '''
    n_master = len(os.listdir(masterdir))
    max_retry = 2
    resubmit = (retry < max_retry)
    status = 'wait'
    faillist = []
    master_statfiles = os.listdir(master_statdir)
    # mini_statfiles = os.listdir(osp.join(statdir, 'mini'))
    n_status = len(master_statfiles)
    n_success = len([el for el in master_statfiles if re.search('success', el) is not None])
    if verbose:
        print("There are " + str(n_status) + "/" + str(n_master) + " master statfiles; " + str(n_success) + " successful")
    if n_status == n_master:
        if n_success == n_master:
            status = 'success'
        elif resubmit:
            submit_retry_scripts(master_statdir, masterdir)
            print("retry: " + str(retry))
            print("master retry: " + str(max_retry))
            print(str(retry < max_retry))
            #if retry < max_retry:
            #    # Re-submit the master scripts in question - this might be a gcp failure
            #    submit_retry_scripts(master_statdir, masterdir)
        else:
            print("Failure in master script and max number of retries met")
            status = 'fail'
            print([em for em in master_statfiles if re.search("fail", em) is not None])
            faillist = [re.sub(".fail", "", el) for el in [em for em in master_statfiles if re.search("fail", em) is not None]]
        retry += 1
    return(status, faillist, retry)


def submit_retry_scripts(statdir, scriptdir):
    failstat = [el for el in os.listdir(statdir) if re.search('fail', el) is not None]
    print(failstat)
    pieces = [re.split('\.', el)[0] for el in failstat]
    failscripts = ["master_script_" + el for el in pieces]
    for stat,scr in zip(failstat, failscripts):
        os.remove(osp.join(statdir, stat))
        submit_cmd = "module load slurm; sbatch -D $HOME " + osp.abspath(osp.join(scriptdir, scr))
        p1 = subprocess.Popen(submit_cmd,shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
        output1, error1 = p1.communicate()
        if(p1.returncode != 0):
            print("Error in submit_retry_scripts - job submission failed!")
            print(submit_cmd)
            print(output1 + "\n" + error1)
            sys.exit(1)


def check_mini_status(mini_statdir, minidir, write_qcmask, verbose):
    '''
    Checks the status of the minifiles that we've run - are there enough of them
    to move onto the next step? Are there any failures?
    '''
    n_mini = len(os.listdir(minidir))
    statfiles = os.listdir(mini_statdir)
    search_strings = ["mini"]
    if write_qcmask:
        search_strings.append("qcmask")
    n_complete = 0
    n_fail = 0
    status = 'wait'
    faillist = []
    for srst in search_strings:
        str_statfiles = [el for el in statfiles if re.search(srst, el) is not None]
        n_status = len(str_statfiles)
        n_success = len([el for el in str_statfiles if re.search('success', el) is not None])
        if verbose:
            print("There are " + str(n_status) + " " + srst + " statfiles; " + str(n_success) + " successful")
        # The below is a hack - but I want to see what the results are
        if n_status >= n_mini:
            if n_success == n_mini:
                n_complete += 1
            else:
                n_fail += 1
    if n_complete == len(search_strings):
        status = 'success'
    if n_fail > 0:
        status = 'fail'
        faillist = [re.sub(".fail", '', el) for el in statfiles if re.search('fail', el) is not None]
    return(status, faillist)

def submit_postproc_jobs(scriptdir, verbose=False):
    '''
    Submits the combined postproc jobs to the pp nodes
    '''
    wscript = scriptdir+"/postProc/msubwrapper"
    if verbose:
        print("submitting " + wscript)
    p = subprocess.Popen(wscript,shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
    output, errors = p.communicate()
    if(p.returncode != 0):
        print("Error in postproc submission: Please consult the logfiles")
        print(output)
        print(errors)
    return(p.returncode)

def check_pp_status(statdir, ppdir, verbose=False):
    '''
    Checks the status of a postproc job
    '''
    n_postproc = len([el for el in os.listdir(ppdir) if osp.isfile(osp.join(ppdir, el))]) - 1
    status = "wait"
    statfiles = os.listdir(statdir)
    n_status = len(statfiles)
    n_success = len([el for el in statfiles if re.search('success', el) is not None])
    faillist = []
    if verbose:
        print("There are " + str(n_status) + " pp statfiles; " + str(n_success) + " successful")
    if n_status == n_postproc:
        if n_success == n_postproc:
            status = 'success'
        else:
            status = 'fail'
            faillist = [re.sub(".failure", "", el) for el in [em for em in statfiles if re.search('failure', em) is not None]]
    return(status, faillist)


def get_crossval_properties(crossval_patt, cv_match, exp_name, scriptdir):
    '''
    Derives the cross-validated complement of an experiment from the naming
    conventions, nased on an ESD-standard naming mattern
    '''
    patt2 = 1 - cv_match[0]
    exp_name2 = re.sub(crossval_patt[cv_match[0]], crossval_patt[patt2], exp_name)
    pdir_split = re.split("/", scriptdir)
    # project = pdir_split[-1]
    subproj = pdir_split[-2]
    user = os.environ.get('USER')
    if (user!='esd'):
        print("Warning in get_crossval_properties: checking for cross-validated experiments assumes an ESD-specific dir structure")
    complement_exp = osp.join("/home/", user, "PROJECTS/DOWNSCALING/scripts", subproj, exp_name2)
    return(exp_name2, complement_exp)

def check_crossval_status(crossval_patt, cv_match, exp_name, scriptdir, exp_name2, scriptdir2):
    '''
    crossval_patt: array containing patterns which indicate whether cross-valdiation
      takes place.
    cv_match: array of len 1, containing which crossval element was called
    '''
    cv_command = "python " + "makeCVEns --exp1 " + exp_name +" --exp2 " + exp_name2 + \
             " --script1 " + scriptdir + " --script2 " + scriptdir2
    if re.search(crossval_patt[-1], exp_name) is not None:
        # Call cross-validation code
        print("MakeCVEns command-line arguments:")
        print(cv_command)
        cv_outfiles = runMakeCVEns(exp_name, exp_name2, scriptdir, scriptdir2, verbose=(verbopt==1))
    else:
        # Warn about the cross-validated complement experiment
        print("\033[1;43mExperiment " + exp_name +" has a cross-validated complement " + exp_name2 + " ; please run complement to get cross-validated ensembles automatically, or run following command on your own:\033[1;m ")
        print(cv_command)
        return('success')

def compress_logs(logdir):
    '''
    For each pattern in pattdict, compresses all files matching the pattern
    into a separate archive and deletes the uncompressed files
    afterwards.
    '''
    print("Archiving R code logfiles...")
    olddir = os.getcwd()
    os.chdir(logdir)
    #compress_files = [osp.join(logdir, el) for el in os.listdir(logdir)]
    pattdict = {'chunk': '^out\.', 'master': '^master\.'}
    for step,patt in pattdict.items():
        lname = osp.join(logdir, step + "_compressed_logfiles.tar.gz")
        lfiles = [el for el in os.listdir(logdir) if re.search(patt,el) is not None]
        tar_log = tarfile.open(lname, "w:gz")
        for name in lfiles:
            tar_log.add(name)
        tar_log.close()
        for name in lfiles:
            os.remove(name)
    os.chdir(olddir)


def write_xml_info(outdirs, xml_dir):
    #Writes the contents of the experiment xml
    #to the end of all experiment info files
    #located in the dirs discussed
    #First, replace tabs in string
    outdirs = [re.sub("\t", "", el) for el in outdirs]
    #Then, get the xml file
    xml_file = [el for el in os.listdir(xml_dir) if re.search("xml", el) is not None]
    if len(xml_file) > 0:
        xml_file = osp.join(xml_dir, xml_file[0])
        cat_string = ''
        with open(xml_file, 'r') as infile:
            for line in infile:
                cat_string = cat_string + "#" + line.strip() + "\n"
        for el in outdirs:
            with open(el, 'a') as outfile:
                outfile.write("\n############ Concatenated experiment XML ######################################\n")
                outfile.write(cat_string)


def final_print(outfiles, exp_info_file, logfile_path, xml_path):
    '''
    Prints the final message pointing to the outfiles for the experiment
    '''
    # ens_list = ens_namemap(ens_list)
    outpath_list = "\n".join([el + ":" + outfiles[el] for el in outfiles.keys()])
    outpath_string = "\n####" + " Downscaled final output " +"#"*47 +"\n"+outpath_list+"\n"
    # concatenate the  xml path onto the tail of every experiment_info.txt
    with open(exp_info_file, 'a') as f:
        f.write(outpath_string)
        if xml_path is not None:
            xml_txt = "sample xml text goes here"
            f.write(xml_txt)
    for ds_out in outfiles:
        exp_dir = osp.join(ds_out.split("/")[:-3])
        exp_file = osp.join(exp_dir, 'experiment_info.txt')
        if osp.isfile(exp_file):
            with open(exp_file, 'a') as f:
                f.write(outpath_string)
                if xml_path is not None:
                    xml_txt = "sample xml text goes here"
                    f.write(xml_txt)


if __name__ == '__main__':
    ParseNextStepArgs()
