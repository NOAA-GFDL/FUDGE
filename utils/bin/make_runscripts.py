#!/bin/python

import re
from os import path as osp

def make_runscript(scriptdir, experiment_name, idstring,
                  ds_vars, rips, outdir_regex,
                  i_idx, i_max, i_by, i_offset, j_idx,
                  j_max, j_by, j_offset, auxfile_mod,
                  meta_file, write_qcmask, xtmpdir="/FAKE"):
    '''
    Given input parameters, writes a runscript to file.
    2019-05-24: checks for preexisting files before recalculating
                calls commandline gcp uitlity
    '''
    header = "\n".join(["#!/bin/bash",
                        "set -x",
                        "# -e not recommended; breaks file status detection for preexist",
                        "#set -e",
                        ". /usr/local/Modules/default/init/bash",
                        "source $BASEDIR/set_fudge_env_bash",
                        "if [ ! $?SLURM_JOB_ID ]; then",
                        "  scriptid=`date +%Y%m%d-%H%M`",
                        "else",
                        "	scriptid=$SLURM_JOB_ID",
                        "fi",
                        "scriptdir=" + scriptdir,
                        "statdir=" + osp.join(scriptdir, 'status', 'minifile'),
                        "final_outdir=" + outdir_regex,
                        "xtmp=" + xtmpdir,
                        "index_idstring=" + idstring,
                        "vars=(" + " ".join(ds_vars) + ")",
                        "rips=(" + " ".join(rips) + ")",
                        'dv_idstring="${vars[${#vars[@]}]}_*${index_idstring}"',
                        'qc_idstring="${vars[${#vars[@]}]}_qcmask*${index_idstring}"',
                        "logfile=${scriptdir}/log/out." + experiment_name + ".master.$scriptid.chunk.$index_idstring"])
    header += "\n"
    #var_set = "vars=(" + " ".join(ds_vars) + ")"
    #rip_set = "rips=(" + " ".join(rips) + ")"
    outdir_regex = re.sub("<VAR>", "${var}", outdir_regex)
    outdir_regex = re.sub("<RIP>", "${rip}", outdir_regex)
    complete = "\n".join(["all_complete=0",
                          "for rip in ${rips[@]}; do",
                          "  for var in ${vars[@]}; do",
                          "    #mkdir -p $final_outdir",
                          "    loop_outdir=$xtmpdir/$rip/$var",
                          "    search_files=$(ls $loop_outdir/ | grep $index_idstring)",
                          "    file_exists=$#",
                          "    if [ $file_exists == 0 ]; then",
                          "      all_complete=1",
                          "    fi",
                          "   done",
                          "done",
                          "if [ $all_complete == 0 ]; then",
                          "  echo 'All files that would have been written are written; not recalculating'",
                          "  exit 0",
                          "fi"])
    # Then call the main R code
    master_driver = "${BASEDIR}/Rsuite/Drivers/Master_Driver.R"
    cmd_args = " ".join(str(el) for el in [i_idx, i_max, i_by, i_offset, j_idx,
                                           j_max, j_by, j_offset, auxfile_mod,
                                           meta_file, 'TRUE'])
    r_command = "Rscript " + master_driver + " " + cmd_args + " &> $logfile &" + "\nwait\nsleep 10\n"
    copies = "\n".join(["write_qcmask=" + ("0" if write_qcmask else "1"),
                            "for rip in ${rips[@]}; do",
                            "  for var in ${vars[@]}; do",
                            "    final_outdir=" + outdir_regex,
                            "    xtmp_outdir=$xtmp/$rip/$var/",
                            "    search_files=$(ls $xtmp_outdir/ | grep $dv_idstring)",
                            "    file_exists=$#",
                            "    if [ $file_exists == 0 ]; then",
                            "#these should also have a check for file size greater than 0",
                            "      touch $statdir/mini.${index_idstring}.success",
                            "    else",
                            "      touch $statdir/mini.${index_idstring}.fail",
                            "    fi",
                            "    if [ $write_qcmask == 0 ]; then",
                            "      xtmp_qcdir=$xtmp/$rip/${var}_qcmask",
                            "      search_files=$(ls $xtmp_outdir/ | grep $qc_idstring)",
                            "      ostat=$#",
                            "      if [ $ostat == 0 ]; then",
                            "        touch $statdir/qcmask.${index_idstring}.success",
                            "      else",
                            "        touch $statdir/qcmask.${index_idstring}.fail",
                            "      fi",
                            "    fi",
                            "  done",
                            "done"])

    #return "\n".join([header, var_set, rip_set, complete, r_command, copies]) + "\n"
    return "\n".join([header, complete, r_command, copies]) + "\n"

    '''
    final_outdir = "/archive/cew/PROJECTS/DOWNSCALING/REGj/downscaled/GFDL-ESM2M/rcp85/day/atmos/GFDL-ESM2Mdetr1981-2099PwlSm21St1Ww31rcp85/${rip}/v20120228/REGjtndetrcap1-QDM-D1A48L01K00/${var}/US480p1/OneD/"
    search_files = `ls $final_outdir/ | grep $index_idstring`
    file_exists = $status
    if ( $file_exists != 0 ) then
      all_complete = 1
    fi
    '''

if __name__ == '__main__':
    #If calling from the commandline, run a test
    scriptdir = "/home/cew/PROJECTS/DOWNSCALING/scripts/PMgCR/PMgCRtxp1-QDM-A02r1e3X01K00/"
    experiment_name = "PMgCRtxp1-QDM-A02r1e3X01K00"
    idstring = "I1-2_J1-5"
    ds_vars = ['tasmax']
    rips = ['r1i1p1']
    outdir_regex = "/archive/cew/PROJECTS/DOWNSCALING/PMgCR/downscaled/GFDL-HIRAM-C360-COARSENED/sst2090/day/atmos/day/<RIP>/v20110601/PMgCRtxp1-QDM-A02r1e3X01K00/tasmax/US48"
    i_idx = 1; i_max = 10; i_by = 2; i_offset = 1
    j_idx = 1; j_max = 10; j_by = 5; j_offset = 1
    auxfile_mod = "/home/cew/PROJECTS/DOWNSCALING/scripts/PMgCR/PMgCRtxp1-QDM-A02r1e3X01K00/config_mod/default_mini_US48.json"
    write_qcmask = False
    meta_file = "~/fake_file.json"
    out = make_runscript(scriptdir, experiment_name, idstring,
                      ds_vars, rips, outdir_regex,
                      i_idx, i_max, i_by, i_offset, j_idx,
                      j_max, j_by, j_offset, auxfile_mod,
                      meta_file,
                      write_qcmask)

    print(out)
