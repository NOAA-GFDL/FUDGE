import re
import sys
import os, stat, shutil
from os import path as osp
import json
from collections import OrderedDict


def create_auxfile_runscripts(i_start, i_end, i_by, i_offset,
                          j_start, j_end, j_by, j_offset,
                          auxdict, auxfile, scriptdir, fudgetag,
                          not_strict_metadata, metadata_table):
    '''
    Creates runscripts (with dmget commands) based off of the aux input files -
    all files are present already
    '''
    # TODO:
    #So for each element we are interested, set up a dmget command
    dataset_names = ['OH', 'MH', 'MF']
    data_files = []
    for dname in dataset_names:
        data_files += [auxdict[dname][el]['file'] for el in auxdict[dname].keys()]
    #None of these are going to be 'none':
    dmget = "# Data files:\n" + "\n".join(["dmget " + el for el in data_files]) + "\n"
    tmasks = []
    for dname in dataset_names:
        tmasks += [auxdict[dname][el]['tmask'] for el in auxdict[dname].keys()]
    tmask_nones = ['na', 'none']
    if auxdict['DS']['indexing_tmask'] not in tmask_nones:
        tmasks.append(auxdict['DS']['indexing_tmask'])
    tmasks = list(set(tmasks))
    tmask_none = ["na", "none"]
    tmasks = [el for el in tmasks if el not in tmask_none]
    if len(tmasks) > 0:
        dmget += "# Time mask files:\n" + "\n".join(["dmget " + el for el in tmasks]) + "\n"
    geomasks = []
    for dname in dataset_names:
        geomasks += [auxdict[dname][el]['geomask'] for el in auxdict[dname].keys()]
    geomasks = list(set(geomasks))
    geomasks = [el for el in geomasks if el != 'none']
    if len(geomasks) > 0:
        dmget += "# Spatial mask files:\n" + "\n".join(["dmget " + el for el in geomasks]) + "\n"
    # Create the gcps for the script
    file_list = data_files + geomasks + tmasks
    mkdir_commands = ["mkdir -p $TMPDIR/" + osp.dirname(el) for el in file_list]
    gcp_commands = [make_gcp_cmd(el, "$TMPDIR/" + el) for el in file_list]
    gcp_str = "# gcp commands\n"
    for el, em in zip(mkdir_commands, gcp_commands):
        gcp_str += el + "\n" + em + "\n"
    # Copy your auxfile to a final resting location:
    auxfile_mod = scriptdir + "/config_mod/" + osp.basename(auxfile)
    os.makedirs(osp.dirname(auxfile_mod))
    shutil.copyfile(auxfile, auxfile_mod)

    # And finally, do the looping by index and writing of master and runscripts.
    make_master_and_runscripts(i_start, i_end, i_by, i_offset,
                              j_start, j_end, j_by, j_offset,
                              auxdict['OH'].keys()[0], True,
                              dmget, gcp_str, auxdict, auxfile_mod,
                              scriptdir, fudgetag,
                              not_strict_metadata, metadata_table)


def create_xml_runscripts(i_start, i_end, i_by, i_offset,
                          j_start, j_end, j_by, j_offset,
                          njobs, ncpu, fudgetag, scriptdir,
                          OH_dir, MH_dir, MF_dir,
                          oh_timerange, mh_timerange, mf_timerange, predlist,
                          spatmask, OH_tmask, MH_tmask, MF_tmask, trim_tmask,
                          outdir_string, target_var, ds_method, j_range, idirsuffix, multivar_target,
                          auxfile, aux_file_list, wallclocktime,
                          auxdict, ds_minifile, experiment_name,
                          not_strict_metadata, metadata_table):
    '''
    Creates runscripts (with dmget commands) based off of the xml input files -
    we need to build files from xml components
    '''
    is_syn = False
    if idirsuffix == "/grid0/":
        print("Adding synthetic data workaround for grid0")
        idirsuffix = idirsuffix + "/ZeroD/"
        j_elements = [1, 2]
        is_syn = True
    if idirsuffix == '/X1-1/':
        print("Adding other synthetic data workaround for X1-1")
        idirsuffix = idirsuffix
        j_elements = [1, 2]
        is_syn = True
    spatmask_present = (spatmask != 'na')
    tmask_present = (OH_tmask != 'na')
    trim_time_mask_present = (trim_tmask != 'na')
    dmget_string = ""
    if spatmask_present:
        # Need the file located at one level up from the spatmask dir -
        # spatmask itself points only to the OneD dir
        spatmask_dir = osp.dirname(osp.abspath(spatmask))
        spatmask_file = [osp.join(spatmask_dir, el)
                         for el in os.listdir(spatmask_dir)
                         if re.search('masks', el) is not None][0]
        dmget_string = dmget_string + "# Obtain spatial mask" + "\n"
        spatmask_files = [spatmask_file]
        dmget_string = dmget_string + "dmget " + spatmask_file + " \n"
        spatmask_path = spatmask_file
    else:
        spatmask_files = []
        spatmask_path = 'none'
        tmask_files = []

    tmask_files = []
    if tmask_present:
        tmask_files = [OH_tmask, MH_tmask, MF_tmask]
        dmget_string = dmget_string + "# Get all tmasks (OH MH MF trimming) " + "\n"
        if trim_time_mask_present:
            tmask_files.append(trim_tmask)
            # dmget_string = dmget_string + "# Get MF overlap tmask " + "\n"
            # dmget_string = dmget_string + "dmget " + trim_tmask + "\n"
        tmask_dmget = "dmget " + " ".join(tmask_files) + "\n"
        dmget_string = dmget_string + tmask_dmget

    # MH and MF files have an assumption that you can have multiple
    # predictor vars present. At some point, var classification checks
    # (such is is_spaceless for ENSO data) should go in here    .
    # MH and MF paths are currently passed in several dirlevels earlier than OH
    prefix_loc = len(MH_dir.split("/"))
    oh_prefix = "/".join(OH_dir.split("/")[0:prefix_loc])
    oh_suffix = "/".join(OH_dir.split("/")[(prefix_loc+1):-1])
    if not multivar_target:
        oh_vars = [target_var]
        if not is_syn:
            oh_files, oh_dmget = build_xml_input_paths(oh_vars, oh_prefix,
                                                       oh_timerange,
                                                       "/" + oh_suffix,
                                                       False)
        else:
            oh_files, oh_dmget = build_xml_input_paths(oh_vars, oh_prefix,
                                                       oh_timerange,
                                                       idirsuffix, False)
    else:
        # Only triggered by MBC; may change later
        oh_vars = predlist
        oh_files, oh_dmget = build_xml_input_paths(predlist, oh_prefix,
                                                   oh_timerange,
                                                   idirsuffix, True)
    mh_files, mh_dmget = build_xml_input_paths(in_vars=predlist,
                                               in_dir=MH_dir,
                                               trange=mh_timerange,
                                               suffix=idirsuffix,
                                               is_multivar=True)
    mf_files, mf_dmget = build_xml_input_paths(in_vars=predlist,
                                               in_dir=MF_dir,
                                               trange=mf_timerange,
                                               suffix=idirsuffix,
                                               is_multivar=True)
    # Set hooks for is_prime and is_fixed qualities for the vars
    oh_prime = oh_fixed = [False for el in oh_vars]
    mh_prime = mh_fixed = [False for el in predlist]
    mf_prime = mf_fixed = [False for el in predlist]
    # Add modified aux copy & structure addition
    oh_auxdict = make_aux_entry(oh_vars,  oh_files, oh_prime, oh_fixed,
                                stencil=auxdict['OH']['stencil'],
                                geomask=spatmask_path, tmask=OH_tmask)
    mh_auxdict = make_aux_entry(predlist, mh_files, mh_prime, mh_fixed,
                                stencil=auxdict['MH']['stencil'],
                                geomask=spatmask_path, tmask=MH_tmask)
    mf_auxdict = make_aux_entry(predlist, mf_files, mf_prime, mf_fixed,
                                stencil=auxdict['MF']['stencil'],
                                geomask=spatmask_path, tmask=MF_tmask)
    ds_auxdict = make_ds_aux_entry(ds_minifile, outdir_string, oh_vars[0],
                                   tmask=trim_tmask)
    auxdict['OH'] = oh_auxdict
    auxdict['MH'] = mh_auxdict
    auxdict['MF'] = mf_auxdict
    auxdict['DS'] = ds_auxdict
    auxfile_mod = scriptdir + "/config_mod/" + osp.basename(auxfile)
    os.makedirs(osp.dirname(auxfile_mod))
    auxstr = json.dumps(auxdict, indent=4)
    with open(auxfile_mod, 'w') as f:
        #Alternate write syntax
        f.write(auxstr)
        f.close()
    #Make your gcp commands
    gcp_string = "# Copy all input files to $TMPDIR (-cd option creates input directories) " + "\n"
    file_list = spatmask_files + tmask_files + oh_files + mh_files + mf_files + aux_file_list
    print("file_list:")
    print(file_list)
    for f in file_list:
        gcp_string = gcp_string + "mkdir -p $TMPDIR/" + os.path.dirname(f) + " \n"
        gcp_string = gcp_string + make_gcp_cmd(f, "$TMPDIR/"+f)
    gcp_string = gcp_string + "echo 'Copy operations complete.' \n"
    gcp_string = gcp_string + "sleep 10 \n"

    # And finally, do the looping by index and writing of master and runscripts.
    make_master_and_runscripts(i_start, i_end, i_by, i_offset,
                              j_start, j_end, j_by, j_offset,
                              target_var, is_syn,
                              dmget_string, gcp_string, auxdict, auxfile_mod,
                              scriptdir, fudgetag,
                              not_strict_metadata, metadata_table)


def make_master_and_runscripts(i_start, i_end, i_by, i_offset,
                          j_start, j_end, j_by, j_offset,
                          target_var, is_syn,
                          dmget_string, gcp_string,
                          auxdict, auxfile_mod,
                          scriptdir, fudgetag,
                          not_strict_metadata, metadata_table):
    '''
    Common commands for making the runscript and runcodes from the auxdict.
    Note that this now implies that the auxdict contains the scriptdir.
    '''
        # which i_indices are we going to be looping over?
    i_elements = range(int(i_start), int(i_end)+1, i_by)
    i_elements.append(int(i_end)+1)
    i_elements = list(set(i_elements))
    i_elements = sorted(i_elements)
    # i_offset = int(i_start)  - 1
    i_max = int(i_end) - i_offset
    # which j indices are we going to be looping over?
    j_elements = range(int(j_start), int(j_end), j_by)
    if len(j_elements) == 0:
        j_elements = range(int(j_start), int(j_end)+1, j_by)
    j_elements.append(int(j_end)+1)
    j_elements = list(set(j_elements))
    j_elements = sorted(j_elements)
    # j_offset = int(j_start)  - 1
    j_max = int(j_end) - j_offset
    #print(i_elements)
    #print(j_elements)
    #sys.exit("Examine this out")

    if is_syn:
        j_elements = [1,2]
    if len(j_elements) == 1:
        j_elements.append(j_elements[0])

    njobs = auxdict['njobs']
    ncpus = auxdict['ncpus']

    slurm_header = "\n".join(['#!/bin/tcsh',
                              '#SBATCH -t '+auxdict['wallclocktime'],
                              '#SBATCH -o ' + scriptdir+'/log/'+'fudge.%x.%j',
                              #'#SBATCH -J fudge_master',
                              '#SBATCH -c ' + str(ncpus),
                              '#SBATCH -p batch']) + "\n"
    # The path to the checkout script is hard-coded, and probably going to
    # stay that way for now; the load of git is DEFINETLY needed if the
    # checkout script is going to be used.
    module_loads = "\n".join(['module load R',
                              'module load gcp',
                              'module load netcdf',
                              'module load nco',
                              'module load git/2.4.6',
                              'eval `/home/esd/bin/checkout-fudge-s8 --fudgetag ' + fudgetag + '`']) + "\n"
    #Commmands for making/manipulating file paths
    outdirs = [re.sub('<RIP>', el, auxdict['DS']['out_dir']) + "/" + auxdict['DS']['out_subdir'] + "/" for el in auxdict['DS']['rips']]
    outdir_cmd = create_outdir_string(outdirs, scriptdir, target_var)
    outdir_string = create_outdir_string(outdirs, scriptdir, target_var)
    meta_tmp = "$TMPDIR/meta_file.json"
    meta_final = osp.join(scriptdir, "meta_file.json")
    meta_command = make_meta_command(auxfile_mod, meta_final,
                                     scriptdir, not_strict_metadata, metadata_table)
    experiment_name = auxdict['ds_experiment_name']

    # Finally, start opts that need to loop by index available
    runmode = ( stat.S_IROTH | stat.S_IRGRP | stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR )
    # print(j_elements)
    n_tiles = 0
    j_rangeval = len(j_elements)-1 if len(j_elements) > 1 else 1
    for j in range(0, j_rangeval):
        jval = j_elements[j]
        j_string = "J"+str(j_elements[j]) + "-" + str(j_elements[j+1]-1)
        # j_max and j_by are passed in already; they are numeric
        for i in range(0, len(i_elements)-1):
            ival = i_elements[i]
            n_tiles = n_tiles + 1
            i_string = "I" + str(i_elements[i]) + "-" + str(i_elements[i+1]-1)
            print("will we call the master?")
            print(str(i % njobs))
            if i % njobs == 0 :
                # Create the master scriptname
                end_idx = i+njobs-1
                if end_idx > len(i_elements) - 1:
                    end_idx = len(i_elements) - 1
                i_master_string = "I" + str(i_elements[i]) + "-" + str(i_elements[end_idx])
                idx_string = i_master_string + '_' + j_string
                master_scriptname = osp.join(scriptdir, 'master', 'master_script_' + idx_string)
                runscript_files = []
            # Since master is created on first (always), can proceed with rest of runscript calls
            runscript_name = osp.join(scriptdir, 'runscript', "script." + target_var + "." + auxdict['ds_method'] + "." + i_string + "_" + j_string)
            i_idx = i_offset - (i+1 + i*(i_by-1))
            j_idx = j_offset - (j+1 + j*(j_by-1))
            if is_syn:
                i_idx = 1
                imax = 1
                i_by = 1
                j_idx = 1
                jmax = 1
                j_by = 1
            else:
                i_idx = ival - i_offset + 1
                imax = i_max + 1
                j_idx = jval - j_offset + 1
                jmax = j_max + 1
            # 2018-04-09: Need to copy modified auxfile all the time, since that
            # is where the file paths are.
            runscript_code = create_slice_runscript(i_string, j_string, scriptdir,
                                                    i_idx, imax, i_by, i_offset,
                                                    j_idx, jmax, j_by, j_offset, auxfile_mod, meta_tmp, target_var,outdirs)
            with open(runscript_name, 'w') as f:
                f.write(runscript_code)
                f.write('\n')  # needed for slurm POSIX standard
            os.chmod(runscript_name, runmode)    # f now executable
            runscript_files.append(runscript_name)
            if i % njobs == (njobs - 1) or i == (len(i_elements) - 2):
                # Prepare to write all accululated data to file
                executable_code = make_executable_stuff(runscript_files, auxfile_mod, meta_tmp,
                                                        i_idx, imax, i_by, i_offset, j_idx, jmax, j_by, j_offset)
                with open(master_scriptname, 'w') as f:
                    f.write(slurm_header)
                    #f.write("#SBATCH -J " + experiment_name + "." + idx_string + "\n")
                    f.write("#SBATCH -J " + experiment_name + "\n")
                    f.write(module_loads)
                    f.write(dmget_string)
                    f.write(gcp_string)
                    f.write(outdir_cmd)
                    f.write(meta_command)
                    f.write("echo 'Beginning calls to executable'\n")
                    f.write(executable_code)
                    f.write('\n')  # needed for the slurm POSIX standard - final terminator
                    # f.write("wait")
    print('master runscript and mini creation completed')

def make_meta_command(auxfile_mod, meta_final, scriptdir,
                      not_strict_metadata, metadata_table):
    # Makes the metadata commands that can be run as a standalone tool
    strict_str = "" if not_strict_metadata else " --is-strict"
    meta_str = " --metadata-table " + metadata_table
    meta_command = "\n".join(["# Call the metadata tester; copy to NFS filesystem",
                              "set meta_final = " + meta_final,
                              "set meta_tmp = $TMPDIR/meta_file.json",
                              "python ${BASEDIR}/utils/bin/standalone_metadata_tester.py --aux_input " + auxfile_mod + " --output $meta_tmp --verbose" + strict_str + meta_str,
                              "set meta_status = $status",
                              'echo "status of meta call: $meta_status"',
                              "if ( $meta_status == 0 ) then",
                              "  if ( ! -f $meta_final ) then",
                              "  " + make_gcp_cmd("$meta_tmp", meta_final,
                                                  indent="    ",
                                                  newline_end = False),
                              "  sleep 10",
                              "  endif",
                              "else",
                              "  echo 'Error in standalone_metadata_tester: Metadata operations failed!'",
                              "  touch " + scriptdir + "/status/.meta.fail",
                              "  exit 1",
                              "endif"])
    return(meta_command + "\n")


def build_xml_input_paths(in_vars, in_dir, trange, suffix, is_multivar=True):
    '''
    Builds input paths from the components that are present in the XML.
    '''
    in_files = []
    dmget_string = ""
    for v in in_vars:
        f_dir = osp.join(in_dir, v) + suffix
        f_dir_files = [osp.join(f_dir, el) for el in os.listdir(f_dir) if re.search(trange, el) is not None]
        # the above should match one, and exactly one file. Include a check in here?
        try:
            in_file = [el for el in f_dir_files if re.search(".nc$", el) is not None][0] # should only match ONE
            in_files.append(in_file)
        except IndexError:
            print("directory: " + f_dir)
            print("suffix: " + suffix)
            print("trange: " + trange)
            print("No files found with " + trange + " in " + f_dir + "; check path construction!")
            sys.exit(1)
        dmget_string = dmget_string + "dmget "+ in_file + "\n"
    return(in_files, dmget_string)


def build_aux_input_paths(aux_subset):
    '''
    Makes the dmget strings and does file copy opts for a subset of the aux
    dictionary - one of the OH,MH,MF subsets
    '''
    in_files = []
    dmget_string = ""
    for v in aux_subset.keys():
        in_file = aux_subset[v]['file']
        in_files.append(in_file)
        dmget_string += 'dmget ' + in_file + "\n"
    return(in_files, dmget_string)


def make_gcp_cmd(sourcefile, destfile, indent="", newline_end = True):
    '''
    Wraps a gcp command in a status checker; exits with an error if that
    command fails
    '''
    gcp_cmd  = "gcp --sync --verbose " + sourcefile + " " + destfile
    gcp_echo = indent + "echo '" + gcp_cmd + "'"
    gcp_cmd = indent + gcp_cmd
    stat_cmd = ("\n" + indent).join(["if ($status != 0) then",
                                     "   echo 'Error in runscript: gcp from " + sourcefile + " to " + destfile + " failed!'",
                                     "   exit 99",
                                     "endif"])
    stat_cmd = indent + stat_cmd
    # echo command goes first; status of the echo is usually 0
    end = '\n' if newline_end else ''
    return(indent + gcp_echo + "\n" + indent + gcp_cmd + "\n" + stat_cmd + end)


def create_outdir_string(outdirs, scriptdir, target_var):
    # Wrapper for making the outdir creation commands
    outdir_string = "# Creating output directories \n"
    qcmaskdir = "/" + target_var + "_qcmask/"
    for out in outdirs:
        outdir_string = outdir_string + "mkdir -p " + out + "\n"
        outdir_string = outdir_string + "if ($status != 0) then" + "\n"
        outdir_string = outdir_string + "\t echo 'Error in creation of output dir! You may not have write permissions'" + "\n"
        outdir_string = outdir_string + "\t touch " + scriptdir + "/status/.mini.fail" + "\n"
        outdir_string = outdir_string + "endif"    + "\n"
        outdir_string = outdir_string + "mkdir -p $TMPDIR/" + out + "\n"
        outdir_string = outdir_string + "mkdir -p " + out + qcmaskdir + "\n"
        outdir_string = outdir_string + "mkdir -p $TMPDIR/" + out + qcmaskdir + "\n"
    return(outdir_string)

def create_slice_runscript(i_string, j_string, scriptdir, i_idx, i_max, i_by, i_offset, j_idx, j_max, j_by, j_offset, auxfile_mod, meta_file, target_var, outdirs):
    # Creates the runscript code specific to a slice of the grid downscaled
    # by the R code
    # Technically only the shebang is needed, but extra info in the event
    # that we ever split
    # these differently is not going to hurt.
    # i: 1:194
    # j_range: 'J454_576'
    runscript_log = osp.join(scriptdir, 'log') + "/ind.%x.%j"
    runscript_header = "\n".join(['#!/bin/tcsh',
                                  '#SBATCH -c 1',
                                  '#SBATCH -t 6:00:00',
                                  '#SBATCH -o ' + runscript_log,
                                  '#SBATCH -J fudge_run',
                                  '#SBATCH -p batch']) + "\n"
    exp_name = scriptdir.split("/")[-1]
    if(exp_name==''):
        exp_name = scriptdir.split("/")[-2]
    master_driver = "${BASEDIR}/Rsuite/Drivers/Master_Driver.R"
    # runcode = osp.join(scriptdir, 'runcode') + "/code." + target_var + "." + exp_name + "." + i_string + "_" +j_string
    logfile = osp.join(scriptdir, 'log') + "/out."+exp_name+"."+ i_string + "_" +j_string
    # print(runcode)
    # print(logfile)
    # Okay - this defines the list of runcodes that are going to be run.
    # i_idx is in the same space as i_max; i is in minifile space.
    # r_command = "Rscript " + runcode + " " + " ".join([str(el) for el in [i_idx, i_max, auxfile, 'TRUE']]) + " >>& " + logfile # + " &"
    cmd_args = " ".join(str(el) for el in [i_idx, i_max, i_by, i_offset, j_idx,
                                           j_max, j_by, j_offset, auxfile_mod,
                                           meta_file, 'TRUE'])
    r_command = "Rscript " + master_driver + " " + cmd_args + " >>& " + logfile + " &"
    runscript = runscript_header + r_command + "\n" + "wait \n"
    for outdir in outdirs:
        runscript = runscript + create_runscript_copy_ops(outdir, i_string, j_string, target_var)
    return(runscript)

def create_runscript_copy_ops(outdir, i_string, j_string, target_var):
    # Makes the copy operations for the output of the R code
    # Currently assumes a single file of _qcmask output;
    # when we get to writing coordinates, this needs to be updated
    # Note: right now, for a given script job, all J are going to be identical.
    # Updated May 2019: Adding in an alternative copy for the copy
    # operations related to the csv workflow; if the i,j is not found, looks for
    # files ending in .nc without the i,j
    outstr = []
    ij_id = i_string+"_"+j_string
    outstr.append('set file_out = `ls "$TMPDIR/' + outdir + '"`')
    outstr.append('echo $file_out | grep ' + ij_id)
    outstr.append('if ( $status == 0 ) then')
    outstr.append('  set file_id = "/*' + ij_id + '"')
    outstr.append('else')
    outstr.append('  set file_id = "/*"')
    outstr.append('endif')
    outstr.append('echo gcp "$TMPDIR/' + outdir + "${file_id}.nc " + outdir + '"')
    outstr.append("gcp $TMPDIR/" + outdir + "${file_id}.nc " + outdir)
    outstr.append("set ostat = $?")
    outstr.append("if ( $ostat == 0 ) then")
    outstr.append("\t touch " + outdir + "/.mini.success")
    outstr.append("else")
    outstr.append("\t touch " + outdir + "/.mini.fail")
    outstr.append('endif')
    # Now do the same for the qc mask file
    out_mask_dir = outdir + "/" + target_var + "_qcmask/"
    outstr.append('echo "gcp $TMPDIR/' + out_mask_dir + "${file_id}.nc " + out_mask_dir + '"')
    outstr.append("gcp $TMPDIR/" + out_mask_dir + "${file_id}.nc " + out_mask_dir)
    outstr.append("set ostat = $?")
    outstr.append("if ( $ostat == 0 ) then")
    outstr.append("\t touch " + out_mask_dir + "/.mini.success")
    outstr.append("else")
    outstr.append("\t touch " + out_mask_dir + "/.mini.fail")
    outstr.append('endif')
    return("\n".join(outstr) + "\n")

def awful_runcode_hack(runcode_dir, runscript_dir):
    # I don't want to have to tinker with something that we're planning on
    # replacing during this flavor or the next. For now, it's not much of
    # a stretch to copy some of the runcodes to a location that fits the
    # expected pattern in the runscripts.
    runscript_files = [el for el in os.listdir(runscript_dir)
                       if re.search("script", el) is not None]
    runcode_files = [el for el in os.listdir(runcode_dir)
                     if re.search("code", el) is not None]
    runscript_patterns = [re.split("\.", el)[-1]
                          for el in runscript_files
                          if (re.split("\.", el)[-1]).count('-') == 2]
    # print(runscript_patterns)
    runcode_split = re.split("\.", runcode_files[0])
    # print(runcode_split)
    for patt in runscript_patterns:
        tmp_file = runcode_split
        tmp_file[-1] = patt
        tmp_dest = osp.join(runcode_dir, ".".join(tmp_file))
        tmp_src = osp.join(runcode_dir, runcode_files[0])
        shutil.copy2(tmp_src, tmp_dest)

def make_executable_stuff(runscript_files, auxfile_mod, meta_final,
                          ival, imax, i_by, i_offset, jval, jmax, j_by, j_offset):
    '''
    Makes the things that call R code (eventually) in the master.
    The batch branch sources runscripts with pre-defined x,y locations
      Workflow: master -> runscript -> Master_Driver with command-line args
    The interactive branch is activated when master is called with command-line
      arguments, and pops an rstudio terminal with a config file that allows
      access to the command-line params that set the x,y locations in the
      runscripts.
    Workflow: master -> config file -> Master_Driver without command-line args
    '''
    runscript_calls = ["\t" + el + " & \n" for el in runscript_files]
    batch_cmd = "\t sleep 10 \n".join(runscript_calls) + "\twait"
    # Then, do the code that gets used for the alt running
    echo_cmd = "<newline> ".join([
                          "# clean environment before calls",
                          "rm(list=ls())",
                          "# set twindow parameters:",
                          "# # if not set to 'all', you can set to a number from",
                          "# # 1:nmasks to run a specific mask",
                          "twindow_run <<- 'all'",
                          "# set i and j location:",
                          "# # idx is the i/j index from which you start,",
                          "# # stride is the number of indices from that point you step (1 being a single i or j val)",
                          "# # max is the length of the i/j dimension (DONT EDIT) ",
                          "# # offset is the i,j offset (DONT EDIT YET - USED FOR FILENAME CONSTRUCTION)",
                          "i_idx    <<- " + str(ival),
                          "i_max    <<- " + str(imax),
                          "i_stride <<- " + str(i_by),
                          "i_offset <<- " + str(i_offset),
                          "j_idx    <<- " + str(jval),
                          "j_max    <<- " + str(jmax),
                          "j_stride <<- " + str(j_by),
                          "j_offset <<- " + str(j_offset),
                          "# point to the modified auxfile (has full file paths)",
                          "auxfile  <<- '" + auxfile_mod + "'",
                          "# point to the metadata file",
                          "metafile <<- '" + meta_final + "'",
                          "# Source Master_Driver for the R code",
                          "source('$master_driver')"])
    echo_cmd = '"' + echo_cmd + '"'

    int_cmd = "\n\t".join(["\tmodule load rstudio/0.98.1103",
                           "module unload R/2.15.2",
                           "module load R/3.4.4",
                           "set auxfile_mod = " + auxfile_mod,
                           # "set runcode_dir = " + scriptdir + "/" + 'runcode',
                           # "set generic_runcode_base = `ls $runcode_dir | head -n 1`",
                           # "set generic_runcode = $runcode_dir/$generic_runcode_base",
                           "set master_driver = ${BASEDIR}/Rsuite/Drivers/Master_Driver.R",
                           # Below line shouldn't give an error: removes if there,
                           # moves on if not.
                           "rm -rf $TMPDIR/command_line_params.R",
                           "set r_cmd = " + echo_cmd,
                           "set r_header = '<shebang>/bin/Rscript'",
                           "echo $r_header | sed -e 's:<shebang>:# \!:g' >> $TMPDIR/command_line_params.R",
                           "echo $r_cmd | sed -e 's:<newline>:" + r'\n' + ":g' >> $TMPDIR/command_line_params.R",
                           "echo 'Command to run R minifile code from the command line:'",
                           'echo "rstudio $TMPDIR/command_line_params.R &"',
                           "rstudio $TMPDIR/command_line_params.R &"]
                          )
    # And then, join both commands together with ifelse
    executable_stuff = "\nset arrlen = $# argv\n\nif ( $arrlen == 0 ) then\n" + batch_cmd + "\nelse\n" + int_cmd + "\nendif"
    return(executable_stuff)


def make_aux_entry(varnames=['fakevar'], files=['fake_file.nc'],
                   is_prime=[False], is_fixed=[False],
                   stencil='none', tmask='none', geomask='none'):
    '''
    Makes an aux file entry of input data files for one of OH, MH or MF.
    '''
    aux_entry = {}
    for var, f, prime, fixed in zip(varnames, files, is_prime, is_fixed):
        aux_entry[var] = {'file': f,
                          'is_fixed': "TRUE" if is_fixed else "FALSE",
                          'is_prime': "TRUE" if is_prime else "FALSE",
                          'stencil': stencil,
                          'geomask': geomask,
                          'tmask':   tmask}
    # 2017-04-06: R code currently looks for stencil under top-level check,
    # not under the var names.
    # aux_entry['stencil'] = stencil
    return(aux_entry)


# And make the output dict for the json file
def make_ds_aux_entry(out_file, outdir, var, tmask='none'):
    '''
    Makes an output dict for the aux file.
    '''
    outdir_opts = outdir.split("#")
    outdir_path = "/".join(re.split('/', outdir_opts[0].rstrip('/'))[:-1])
    riplist = outdir_opts[2].split(",")
    if (riplist[0] == 'na'):
        riplist = [outdir_path.split("/")[-5]]
    # The parenthesis made sense in c-shell; pythons' sub functions are PICKY
    outrip_patt = "\$\(RIP\)"
    outdir_path = re.sub(outrip_patt, '<RIP>', outdir_path)
    #
    out_subdir = re.split('/', outdir_opts[0].rstrip('/'))[-1]
    out_vars = [var]
    # Edit this when we start adding more in the way of supp. output writing
    out_supp = [var + '_qcmask']
    out_rips = riplist
    dsdict = {'indexing_tmask': tmask,
              'out_dir': outdir_path,
              'out_file': out_file,
              'out_subdir': out_subdir,
              'out_supp': out_supp,
              'vars': out_vars,
              'rips': out_rips}
    return(dsdict)
