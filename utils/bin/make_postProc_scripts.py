#!/bin/python
#make_postProc_scripts
#import expergen
import listVars
import fudgeList
import fudgeUtils
from fudgeUtils import read_exp_info_file
import os,stat,shutil
from os import path as osp
import shlex
import smtplib
import re


def make_postProc_scripts(auxdict, fudgetag, mailid):
    #Okay, this is going to go in a few steps:
    ensembles = auxdict['DS']['rips']
    vars = auxdict['DS']['vars']
    types = ['none']
    if auxdict['make_qcmask']:
        types.append('qcmask')
    # Hook for adding in save files when we get to that point
    make_save = False
    if make_save:
        types.append('save')
    # regexes contain switches for ensemble members and vars;
    # saving off coords is more complex and we aren't doing it now
    indir_regex = osp.join(auxdict['DS']['out_dir'], auxdict['DS']['out_subdir'])
    outdir_regex = auxdict['DS']['out_dir']
    script_dir = auxdict['script_settings']['scriptdir']
    auxdir = osp.join(script_dir, 'config_mod')
    auxfile = osp.join(auxdir, os.listdir(auxdir)[0])
    runtime = auxdict['pp_wallclocktime']
    # postproc dirs
    pp_dir = osp.join(script_dir, 'postProc')
    statdir  = osp.join(script_dir, 'status', 'postproc')
    #pp_source_dir = osp.join(pp_dir, 'aux')
    pp_log_dir = osp.join(script_dir, "log")
    #pp_stat_dir = osp.join(pp_dir, "ppStatus")

    code_list = []
    #for each var, ensemble, type:
    for v in vars:
        indir_loop = re.sub('<VAR>', v, indir_regex)
        outdir_loop = re.sub('<VAR>', v, outdir_regex)
        for m in ensembles:
            indir_loop2 = re.sub('<RIP>', m, indir_loop)
            outdir_loop2 = re.sub('<RIP>', m, outdir_loop)
            for t in types:
                desc = ".".join([m, v, t])
                pp_runcode = osp.join(pp_dir, desc)
                pp_runcode_header = pp_runcode_header = "\n".join(["#!/bin/tcsh",
                                                   "#SBATCH -c 6",
                                                   '#SBATCH -t '+str(runtime),
                                                   "#SBATCH -J fudge_pp_" + str(v) + "." + str(t) + "." + str(m),
                                                   "#SBATCH -o " + pp_log_dir + "/%x.%j",
                                                   "#SBATCH -p batch"]) + "\n"
                if t == 'qcmask':
                    pp_var = v + '_qcmask'
                    indir_loop2 = osp.join(indir_loop2, pp_var)
                elif t == 'save':
                    pp_var = v = '_save'
                    indir_loop2 = osp.join(indir_loop2, pp_var)
                else:
                    pp_var = v
                # postproc command
                pp_cmd_text = " ".join(["python -u $BASEDIR/bin/postProc",
                                        "--aux " + auxfile,
                                        "-d " + indir_loop2,
                                        "-o " + outdir_loop2,
                                        "--var " + pp_var])
                runcode_vars = set_pp_runcode_vars(desc, script_dir,
                                                   statdir,
                                                   indir_loop2, outdir_loop2)
                runcode_cmd = make_pp_runcode_commands(fudgetag, pp_cmd_text)
                # Check on verbosity settings set earlier for message commands
                with open(pp_runcode, 'w') as f:
                    f.write(pp_runcode_header)
                    f.write(runcode_vars)
                    f.write(runcode_cmd)
                code_list.append(pp_runcode)
    # once looping is over, set permissions
    runmode = ( stat.S_IROTH | stat.S_IRGRP |stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR )
    for el in code_list:
        os.chmod(el, runmode)
    #write the msubwrapper
    #As of 2018-03-14, removing the removal step from the msubwrapper
    msub_file = osp.join(pp_dir, 'msubwrapper')
    msub_text = "#!/bin/tcsh -f \n"
    msub_text = msub_text + "rm -f " + statdir + "/*\n"
    # 2019-06-03 - I'm not sure these need to be on a node by themselves. Check?
    # msub_commands = "\n".join(["sbatch --exclusive " + el for el in code_list]) + "\n"
    msub_commands = "\n".join(["sbatch " + el for el in code_list]) + "\n"
    #msub_text = msub_text + msub_commands + "\n"
    msub_text = msub_text + msub_commands + "\n"
    with open(msub_file, 'w') as f:
        f.write(msub_text)
    os.chmod(msub_file, runmode)

def set_pp_runcode_vars(desc, scriptdir, statdir, indir, outdir):
    '''
    Sets up vars needed for the pp runcode; used in runcode commands
      to improve readability.
    '''
    vars = "\nset ".join(["# setting runcode vars",
                  "desc = " + desc,
                  "scriptdir = " + scriptdir,
                  "statdir = " + statdir,
                  "indir = " + indir,
                  "outdir = " + outdir])
    return(vars + "\n")

def make_pp_runcode_commands(fudgetag, postproc_cmd):
    '''
    Sets up the base of a postproc command.
    Assumes you've set some vars in set_pp_runcode_vars
      for readability.
    '''
    cmd = "\n".join(["echo 'Checking for prior postproc jobs...'",
                     "find $statdir/* | grep $desc",
                     "set ppstat = $status",
                     "if ($ppstat == 1) then",
                     "	source $BASEDIR/set_fudge_env",
                     "	eval `/home/esd/bin/checkout-fudge-s8 --fudgetag " + fudgetag + "`",
                     "	" + postproc_cmd,
                     "	if ( $status == 0 ) then",
                     "		 touch $statdir/${desc}.success",
                     "	else",
                     "		 touch $statdir/${desc}.failure",
                     "	endif",
                     "else",
                     "   echo 'statfile found for postproc; exiting'",
                     "endif" + "\n"])
    return(cmd + "\n")
