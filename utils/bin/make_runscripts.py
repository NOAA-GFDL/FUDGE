#!/bin/python

import re
from os import path as osp

def make_runscript(scriptdir, experiment_name, idstring,
                  ds_vars, rips, outdir_regex,
                  i_idx, i_max, i_by, i_offset, j_idx,
                  j_max, j_by, j_offset, auxfile_mod,
                  meta_file, write_qcmask):
    '''
    Given input parameters, writes a runscript to file.
    2019-05-24: checks for preexisting files before recalculating
                calls commandline gcp uitlity
    '''
    header = "\n".join(["#!/bin/tcsh",
                        "#set echo",
                        "if ( ! $?SLURM_JOB_ID ) then",
                        "  set scriptid = `date +%Y%m%d-%H%M`",
                        "else",
                        "	set scriptid = $SLURM_JOB_ID",
                        "endif",
                        "set scriptdir = " + scriptdir,
                        "set index_idstring = " + idstring,
                        "set logfile = ${scriptdir}/log/out." + experiment_name + ".master.$scriptid.chunk.$index_idstring"])
    header += "\n"
    var_set = "set vars = (" + " ".join(ds_vars) + ")"
    rip_set = "set rips = (" + " ".join(rips) + ")"
    outdir_regex = re.sub("<VAR>", "${var}", outdir_regex)
    outdir_regex = re.sub("<RIP>", "${rip}", outdir_regex)
    complete = "\n".join(["set all_complete = 0",
                          "foreach rip ( $rips )",
                          "  foreach var ( $vars )",
                          "    set final_outdir = " + outdir_regex,
                          "    mkdir -p $final_outdir",
                          "    set search_files = `ls $final_outdir/ | grep $index_idstring`",
                          "    set file_exists = $status",
                          "    if ( $file_exists != 0 ) then",
                          "      set all_complete = 1",
                          "    endif",
                          "  end",
                          "end",
                          "if ( $all_complete == 0 ) then",
                          "  echo 'All files that would have been written are written; not recalculating'",
                          "  exit 0",
                          "endif"])
    # Then call the main R code
    master_driver = "${BASEDIR}/Rsuite/Drivers/Master_Driver.R"
    cmd_args = " ".join(str(el) for el in [i_idx, i_max, i_by, i_offset, j_idx,
                                           j_max, j_by, j_offset, auxfile_mod,
                                           meta_file, 'TRUE'])
    r_command = "Rscript " + master_driver + " " + cmd_args + " >>& $logfile &" + "\nwait\nsleep 10\n"

    copies = "\n".join(["set write_qcmask = " + ("0" if write_qcmask else "1"),
                        "foreach rip ( $rips )",
                        "  foreach var ( $vars )",
                        "    set final_outdir = " + outdir_regex,
                        "    echo 'gcp $TMPDIR/$final_outdir/*${index_idstring}.nc $final_outdir/'",
                        "    python ${BASEDIR}/utils/bin/gcp.py $TMPDIR/$final_outdir/*${index_idstring}.nc $final_outdir/ --retry 2",
                        "    set ostat = $?",
                        "    if ( $ostat == 0 ) then",
                        "      touch $final_outdir/.${index_idstring}.mini.success",
                        "    else",
                        "      touch $final_outdir/.${index_idstring}.mini.fail",
                        "    endif",
                        "    if ( $write_qcmask == 0 ) then",
                        "      echo 'gcp $TMPDIR/$final_outdir/${var}_qcmask/*${index_idstring}.nc $final_outdir/${var}_qcmask/'",
                        "      python ${BASEDIR}/utils/bin/gcp.py $TMPDIR/*$final_outdir/${var}_qcmask/${index_idstring}.nc $final_outdir/${var}_qcmask/ --retry 2",
                        "      set ostat = $?",
                        "      if ( $ostat == 0 ) then",
                        "        touch $final_outdir/${var}_qcmask/.${index_idstring}.mini.success",
                        "      else",
                        "        touch $final_outdir/${var}_qcmask/.${index_idstring}.mini.fail",
                        "      endif",
                        "    endif",
                        "  end",
                        "end"])
    return "\n".join([header, var_set, rip_set, complete, r_command, copies]) + "\n"

    '''
    set final_outdir = "/archive/cew/PROJECTS/DOWNSCALING/REGj/downscaled/GFDL-ESM2M/rcp85/day/atmos/GFDL-ESM2Mdetr1981-2099PwlSm21St1Ww31rcp85/${rip}/v20120228/REGjtndetrcap1-QDM-D1A48L01K00/${var}/US480p1/OneD/"
    set search_files = `ls $final_outdir/ | grep $index_idstring`
    set file_exists = $status
    if ( $file_exists != 0 ) then
      set all_complete = 1
    endif
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
