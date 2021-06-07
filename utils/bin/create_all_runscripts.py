import re
import sys
import os, stat, shutil
from os import path as osp
import json
from collections import OrderedDict
from make_runscripts import make_runscript
import math


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
    dmget = "\n".join(["dmget " + el for el in data_files]) + "\n"
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
        dmget += "\n".join(["dmget " + el for el in tmasks]) + "\n"
    geomasks = []
    for dname in dataset_names:
        geomasks += [auxdict[dname][el]['geomask'] for el in auxdict[dname].keys()]
    geomasks = list(set(geomasks))
    geomasks = [el for el in geomasks if el != 'none']
    if len(geomasks) > 0:
        dmget +=  "\n".join(["dmget " + el for el in geomasks]) + "\n"
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

    is_syn = False
    if auxdict['output_grid'] in ['X1-1', 'grid0']:
        is_syn = True

    # And finally, do the looping by index and writing of master and runscripts.
    #print(" ".join([str(el) for el in [i_start, i_end, i_by, i_offset, j_start, j_end, j_by, j_offset]]))
    #sys.exit(1)
    make_master_and_runscripts(i_start, i_end, i_by, i_offset,
                              j_start, j_end, j_by, j_offset,
                              auxdict['OH'].keys()[0], is_syn,
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
        #dmget_string = dmget_string + "# Obtain spatial mask" + "\n"
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
        #dmget_string = dmget_string + "# Get all tmasks (OH MH MF trimming) " + "\n"
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
    # Set hooks for is_prime and no_grid qualities for the vars
    # as of 2021, no_grid is referenced by metadata functions
    oh_prime = [False for el in oh_vars]
    mh_prime = [False for el in predlist]
    mf_prime = [False for el in predlist]
    oh_prime[0] = True 
    mh_prime[0] = True
    mf_prime[0] = True 
    oh_nogrid = [False for el in oh_vars]
    mh_nogrid = [False for el in predlist]
    mf_nogrid = [False for el in predlist]
    # Add modified aux copy & structure addition
    oh_auxdict = make_aux_entry(oh_vars,  oh_files, oh_prime, oh_nogrid,
                                stencil=auxdict['OH']['stencil'],
                                geomask=spatmask_path, tmask=OH_tmask)
    mh_auxdict = make_aux_entry(predlist, mh_files, mh_prime, mh_nogrid,
                                stencil=auxdict['MH']['stencil'],
                                geomask=spatmask_path, tmask=MH_tmask)
    mf_auxdict = make_aux_entry(predlist, mf_files, mf_prime, mf_nogrid,
                                stencil=auxdict['MF']['stencil'],
                                geomask=spatmask_path, tmask=MF_tmask)
    ds_auxdict = make_ds_aux_entry(ds_minifile, outdir_string, oh_vars[0],
                                   tmask=trim_tmask)
    auxdict['OH'] = oh_auxdict
    auxdict['MH'] = mh_auxdict
    auxdict['MF'] = mf_auxdict
    auxdict['DS'] = ds_auxdict
    #Adding dmget parameters for data files after timeout associated with gcp
    #dmget_string += "# Dmget data files\n"
    dmget_string += oh_dmget
    dmget_string += mh_dmget
    dmget_string += mf_dmget
    auxfile_mod = scriptdir + "/config_mod/" + osp.basename(auxfile)
    # 2019-05-21: before dumping to file, add params to run from aux
    auxdict['files_in_aux'] = True
    auxdict['script_settings'] = {'scriptdir': scriptdir}
    auxdict['cleanup_settings'] = {'if_preexist': 'move'}
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
    j_elements = range(int(j_start), int(j_end)+1, j_by)
    if len(j_elements) == 0:
        j_elements = range(int(j_start), int(j_end)+1, j_by)
    j_elements.append(int(j_end)+1)
    j_elements = list(set(j_elements))
    j_elements = sorted(j_elements)
    # j_offset = int(j_start)  - 1
    j_max = int(j_end) - j_offset

    if is_syn:
        j_elements = [1,2]
    if len(j_elements) == 1:
        j_elements.append(j_elements[0])

    njobs = auxdict['njobs']
    ncpus = auxdict['ncpus']

    #print(str(i_elements))
    #print(str(j_elements))

    nrunscripts = (len(i_elements) - 1) * (len(j_elements) - 1)
    print("expected number runscripts:")
    print(str(nrunscripts))
    nmaster = int(math.ceil(float(nrunscripts) / float(njobs)))
    print("expected number of master scripts:")
    print(str(nmaster))
    #print(str((float(nrunscripts) / float(njobs))))

    slurm_header = "\n".join(['#!/bin/bash',
                              '#SBATCH -t '+auxdict['wallclocktime'],
                              '#SBATCH -c ' + str(ncpus),
                              '#SBATCH -p batch']) + "\n"
    # The path to the checkout script is hard-coded, and probably going to
    # stay that way for now; the load of git is DEFINETLY needed if the
    # checkout script is going to be used.
    module_loads = "\n".join(["set -x",
                              "set -e",
                              'module load git/2.4.6',
                              'pwd',
                              'eval `/home/esd/bin/checkout-fudge-s8-bash --fudgetag ' + fudgetag + '`', 
                              "source $BASEDIR/set_fudge_env_bash"]) + "\n"
    #Commmands for making/manipulating file paths
    ds_var = auxdict['DS']['vars'][0]
    outdirs = [re.sub('<RIP>', el, auxdict['DS']['out_dir']) + "/" + auxdict['DS']['out_subdir'] + "/" for el in auxdict['DS']['rips']]
    outdir_cmd = create_outdir_string(outdirs, scriptdir, ds_var)
    outdir_string = create_outdir_string(outdirs, scriptdir, ds_var)
    meta_tmp = "$TMPDIR/meta_file.json"
    meta_final = osp.join(scriptdir, "meta_file.json")
    meta_command = make_meta_command(auxfile_mod, meta_final,
                                     scriptdir, not_strict_metadata, metadata_table)
    experiment_name = auxdict['ds_experiment_name']

    #swap out the newlines and dmgets in the dmget command
    dmget_bg_string  = re.sub("\ndmget", " ", dmget_string)
    dmget_bg_string = re.sub("  ", " ", dmget_bg_string)
    
    #Make the command to source the list of files for dmgetting within the script
    script_dmgets = "$BASEDIR/utils/bin/dmget_files " + scriptdir + "/fudge_inputs.txt\n"

    # Finally, start opts that need to loop by index available
    runmode = ( stat.S_IROTH | stat.S_IRGRP | stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR )
    # print(j_elements)
    n_tiles = 0
    j_rangeval = len(j_elements)-1 if len(j_elements) > 1 else 1
    k = 0
    for j in range(0, j_rangeval):
        jval = j_elements[j]
        jby = j_elements[j+1] - jval
        j_string = "J"+str(j_elements[j]) + "-" + str(j_elements[j+1]-1)
        # j_max and j_by are passed in already; they are numeric
        for i in range(0, len(i_elements)-1):
            ival = i_elements[i]
            iby = i_elements[i+1] - ival
            n_tiles = n_tiles + 1
            i_string = "I" + str(i_elements[i]) + "-" + str(i_elements[i+1]-1)
            #print("will we call the master?")
            #print(str(i % njobs))
            if k % njobs == 0 :
                #print("initialize write strings")
                #print("i " + str(i) + " j " + str(j))
                #print("i " + str(i_elements[i]) + " j " + str(j_elements[j]))
                i_master_string = "I" + str(i_elements[i])
                j_master_string = "J" + str(j_elements[j])
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
            #print(" ".join([str(el) for el in [ival, i_idx, imax, i_by, i_offset, jval, j_idx, jmax, j_by, j_offset]]))
            regex_outdir = osp.join(auxdict['DS']['out_dir'], auxdict['DS']['out_subdir'])
            idstring = i_string + "_" + j_string
            runscript_code = make_runscript(scriptdir, experiment_name, idstring,
                                            auxdict['DS']['vars'], auxdict['DS']['rips'], regex_outdir,
                                            i_idx, imax, iby, i_offset,
                                            j_idx, jmax, jby, j_offset,
                                            auxfile_mod, meta_tmp, auxdict['make_qcmask'])
            with open(runscript_name, 'w') as f:
                f.write(runscript_code)
                f.write('\n')  # needed for slurm POSIX standard
            os.chmod(runscript_name, runmode)    # f now executable
            runscript_files.append(runscript_name)
            if k % njobs == (njobs - 1) or k == ( nrunscripts - 1):
                #print("writing master")
                #print("i " + str(i) + " j " + str(j))
                #print("i " + str(i_elements[i+1]-1) + " j " + str(j_elements[j+1]-1))
                # Prepare to write all accululated data to file
                i_master_string += "-" + str(i_elements[i+1]-1)
                j_master_string += "-" + str(j_elements[j+1]-1)
                idx_string = i_master_string + '_' + j_master_string
                master_scriptname = osp.join(scriptdir, 'master', 'master_script_' + idx_string)
                trap_command  = make_trap_command(scriptdir, idx_string)
                executable_code = make_executable_stuff(runscript_files, auxfile_mod, meta_tmp,
                                                        i_idx, imax, i_by, i_offset, j_idx, jmax, j_by, j_offset)
                #idx_string = "I" + str(i_elements) + "_" + j_string
                # idx_string is set above this point. Doesn't get used on the same
                # loop, but it shouldn't vanish.
                logfilename = 'master.%x.' + idx_string + ".%j"
                with open(master_scriptname, 'w') as f:
                    f.write(slurm_header)
                    #f.write("#SBATCH -J " + experiment_name + "." + idx_string + "\n")
                    f.write('#SBATCH -o ' + osp.join(scriptdir, 'log') + "/" + logfilename + "\n")
                    f.write("#SBATCH -J " + experiment_name + "\n")
                    f.write(module_loads)
                    f.write(trap_command)
                    f.write(script_dmgets)
                    f.write(gcp_string)
                    f.write(outdir_cmd)
                    f.write(meta_command)
                    f.write("echo 'Beginning calls to executable'\n")
                    f.write(executable_code)
                    f.write('\n')  # needed for the slurm POSIX standard - final terminator
            k += 1
    #2019-08: Adding sourcable script for dmgetting files
    #2021-03: File is now a text file of all files used by FUDGE
    infile_list = osp.join(scriptdir, "fudge_inputs.txt")
    dmget_list = dmget_bg_string.split(" ")[1:]
    #remove any empty entries and strip whitespace
    dmget_list = [x.strip() for x in dmget_list if x is not ""]
    with open(infile_list, 'w') as f:
    	f.write("\n".join(dmget_list))
    print('master runscript and mini creation completed')

def make_trap_command(scriptdir, idx_string):
    '''
    Makes a command to trap and write status files based off of the master exit status
    '''
    statdir = osp.join(scriptdir, 'status', 'master')
    trap_command = "\n".join(["#Trap code - writes status files for cleanup",
                              "chunk_id='" + idx_string + "'",
                              "function exit_fxn() {",
                              "    # many thanks to https://medium.com/@dirk.avery/the-bash-trap-trap-ce6083f36700",
                              "	echo $1",
                              '	if [ "$1" != "0" ]; then',
                              "	  touch " + statdir + "/${chunk_id}.fail",
                              "	  exit 1",
                              "  	else",
                              "	  touch " + statdir + "/${chunk_id}.success",
                              "	  exit 0",
                              "	fi",
                              "}",
                              "trap 'exit_fxn $?' EXIT"])
    return(trap_command + "\n")

def make_meta_command(auxfile_mod, meta_final, scriptdir,
                      not_strict_metadata, metadata_table):
    # Makes the metadata commands that can be run as a standalone tool
    strict_str = "" if not_strict_metadata else " --is-strict"
    meta_str = " --metadata-table " + metadata_table
    meta_command = "\n".join(["# Call the metadata tester; copy to NFS filesystem",
                              "meta_final=" + meta_final,
                              "meta_tmp=$TMPDIR/meta_file.json",
                              "python -u ${BASEDIR}/utils/bin/standalone_metadata_tester.py --aux_input " + auxfile_mod + " --output $meta_tmp --verbose" + strict_str + meta_str,
                              "meta_status=$?",
                              'echo "status of meta call: $meta_status"',
                              "if [ $meta_status == 0 ]; then",
                              "  if [ ! -f $meta_final ]; then",
                              "  " + make_gcp_cmd("$meta_tmp", meta_final,
                                                  indent="    ",
                                                  newline_end = False),
                              "  sleep 10",
                              "  fi",
                              "else",
                              "  echo 'Error in standalone_metadata_tester: Metadata operations failed!'",
                              "  touch " + scriptdir + "/status/.meta.fail",
                              "  exit 1",
                              "fi"])
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
    #gcp_cmd  = "gcp --sync --verbose " + sourcefile + " " + destfile
    gcp_cmd = "python ${BASEDIR}/utils/bin/gcp.py --gcp-verbose --gcp-sync --retry 2 --final-copy --verbose " + sourcefile + " " + destfile
    gcp_echo = indent + "echo '" + gcp_cmd + "'"
    gcp_cmd = indent + gcp_cmd
    stat_cmd = ("\n" + indent).join(["if [ $? != 0 ]; then",
                                     "   echo 'Error in runscript: gcp from " + sourcefile + " to " + destfile + " failed!'",
                                     "   exit 99",
                                     "fi"])
    stat_cmd = indent + stat_cmd
    # echo command goes first; status of the echo is usually 0
    end = '\n' if newline_end else ''
    return(indent + gcp_echo + "\n" + indent + gcp_cmd + "\n" + stat_cmd + end)


def create_outdir_string(outdirs, scriptdir, ds_var):
    # Wrapper for making the outdir creation commands
    outdir_string = "# Creating output directories \n"
    qcmaskdir = "/" + ds_var + "_qcmask/"
    for out in outdirs:
        outdir_string = outdir_string + "mkdir -p " + out + "\n"
        outdir_string = outdir_string + "if [ $? != 0 ]; then" + "\n"
        outdir_string = outdir_string + "\t echo 'Error in creation of output dir! You may not have write permissions'" + "\n"
        outdir_string = outdir_string + "\t touch " + scriptdir + "/status/.mini.fail" + "\n"
        outdir_string = outdir_string + "fi"    + "\n"
        outdir_string = outdir_string + "mkdir -p $TMPDIR/" + out + "\n"
        outdir_string = outdir_string + "mkdir -p " + out + qcmaskdir + "\n"
        outdir_string = outdir_string + "mkdir -p $TMPDIR/" + out + qcmaskdir + "\n"
    return(outdir_string)

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
                           "auxfile_mod=" + auxfile_mod,
                           "master_driver=${BASEDIR}/Rsuite/Drivers/Master_Driver.R",
                           # Below line shouldn't give an error: removes if there,
                           # moves on if not.
                           "rm -rf $TMPDIR/command_line_params.R",
                           "r_cmd=" + echo_cmd,
                           "r_header='<shebang>/bin/Rscript'",
                           "echo $r_header | sed -e 's:<shebang>:# \!:g' >> $TMPDIR/command_line_params.R",
                           "echo $r_cmd | sed -e 's:<newline>:" + r'\n' + ":g' >> $TMPDIR/command_line_params.R",
                           "echo 'Command to run R minifile code from the command line:'",
                           'echo "rstudio $TMPDIR/command_line_params.R &"',
                           "rstudio $TMPDIR/command_line_params.R &"]
                          )
    # And then, join both commands together with ifelse
    executable_stuff = "\narrlen=$#\n\nif [ $arrlen == 0 ]; then\n" + batch_cmd + "\nelse\n" + int_cmd + "\nfi"
    return(executable_stuff)


def make_aux_entry(varnames=['fakevar'], files=['fake_file.nc'],
                   is_prime=[False], no_grid=[False],
                   stencil='none', tmask='none', geomask='none'):
    '''
    Makes an aux file entry of input data files for one of OH, MH or MF.
    '''
    aux_entry = OrderedDict()
    for var, f, prime, nog in zip(varnames, files, is_prime, no_grid):
        aux_entry[var] = {'file': f,
                          'no_grid': "TRUE" if nog else "FALSE",
                          'is_prime': "TRUE" if prime else "FALSE",
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
