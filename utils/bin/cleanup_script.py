#cleanup_script.py
#performs same functions as the original csh watcher script,
#but with easier file path parsing

#import expergen
import listVars
import shutil, re
import os, sys
from os import path

def get_ensemble_path(outdirs, rips):
    range_list = [int(r[1]) for r in rips]
    newrip = "r" + str(min(range_list)) + "to" + str(max(range_list)) + rips[0][2:]
    r = 0
    match = False
    ens_outdir = None
    while(match==False and r < len(rips)):
        if re.search(rips[r], outdirs[0]) is not None:
            ens_outdir = re.sub(rips[r], newrip, outdirs[0])
            match = True
        else:
            r = r + 1
    if match == False:
        print("Error in get_ensemble_path! Something has gone wrong with the ensemble path generation!")
        sys.exit()
    return(ens_outdir)

def get_crossval_paths(cv_paths):
    #Okay, this is going to be more complicated - this relies on some of the code
    #that was established by the ESD team for dealing with the cross-validated paths
    orig_expname = re.split("/",cv_paths[0])[-1]
    odesc = ['dp', 'cv']
    dtype = ["DSDEPh", "DSCVh"]
    kfold = ["K00", "K02"]
    cv_outpaths = []
    cv_path = cv_paths[0]
    #for cv_path in cv_paths:
    dirsplit = re.split("/", cv_path)
    #ds_rip = dirsplit[-3]
    ds_rip = re.search("(r[0-9]*i[0-9]*p[0-9]*)", cv_path).group()
    for k in range(0,2):
        loop_path = cv_path
        cv_expname = re.sub("A00r[0-9]*e2", "A00"+odesc[k]+"e2", orig_expname)
        cv_expname = re.sub("K00", kfold[k], cv_expname)
        cv_expname = re.sub(ds_rip, dtype[k], cv_expname)
        #
        loop_path = re.sub(orig_expname, cv_expname, loop_path)
        #cv_fname = re.sub(orig_expname, cv_expname, orig_fname)
        #From this point on, make the changes to the new things instead
        loop_path = re.sub(ds_rip, dtype[k], loop_path)
        cv_outpaths.append(loop_path)
    return(cv_outpaths)

def get_file_paths(path, is_csv = False):
    #go up 3 levels, unless in csv workflow (anything goes)
    if not is_csv:
        return(("/").join(re.split("/", path)[:-4]))
    else:
        return(path)

def get_anomadd_paths(anomvar, path):
    targvar = re.sub("anom", '', anomvar)
    return(re.sub(anomvar, targvar, path))

def count_suffix(path):
    #Counts the number of directories with a similar path one
    #level up. Needed to make sure that a messy cleanup still
    #avoids clobbering prior output
    if not os.path.isdir(path):
        print("dir does not exist; returning 0")
        return(0)
    outdir = re.split("/", path.rstrip("/"))[-1]
    basename = ("/").join(re.split("/", path.rstrip("/"))[:-1])
    outsuff = [p.split("~")[-1] for p in os.listdir(basename) if re.search(outdir, p) is not None]
    outnum=[]
    for el in outsuff:
        try:
            outnum.append(int(el))
        except ValueError:
            outnum.append(0)
    if len(outnum) > 0:
        return(max(outnum))
    else:
        return(0)

def parse_outdirs_from_xml(outdir_string):
    '''
    Gets a list of all output dirs from the #RIP taken from parsed XML
    '''
    if re.search("#", outdir_string) is not None:
        #Get outdirs for each rip
        out_components = re.split("#", outdir_string)
        #Expected parenthesis are an utter pain to make regular expressions around
        for el in ("$", ")", "("):
            out_components[1] = re.sub("\\" + el, '\\'+el, out_components[1])
        rips = out_components[2]
        rip_list = re.split(",", rips)
        outdirs = []
        for rip in rip_list:
            outdirs.append(re.sub(out_components[1], rip, out_components[0]))
        outdirs = [get_file_paths(f, is_csv=False) for f in outdirs]
    else:
        outdirs = [outdir_string]
        out_split = outdir_string.split("/")
        rip_list = [out_split[-3]]
    return(outdirs,rip_list)

def clean_prior_experiments(outdirs, scriptdir, rip_list, basedir, opt, is_csv):
#Clobbers or moves the script directories and all output directories,
#depending on the settings of the xml passed in
#Good news! Expergen checks for running on a workstation versus the analysis
#nodes. This check should be safe.
#Check for being on an analysis node:
    '''
    output = listVars.listVars(xml, basedir)
    outdir = output[27]
    if re.search("#", outdir) is not None: #Check to see how this handles on single-rip datasets
        #Get outdirs for each rip
        out_components = re.split("#", outdir)
        #Expected parenthesis are an utter pain to make regular expressions around
        for el in ("$", ")", "("):
            out_components[1] = re.sub("\\" + el, '\\'+el, out_components[1])
        rips = out_components[2]
        rip_list = re.split(",", rips)
        outdirs = []
        for rip in rip_list:
            outdirs.append(re.sub(out_components[1], rip, out_components[0]))
        outdirs = [get_file_paths(f, is_csv) for f in outdirs]
    #Script path
    scriptpath = output[54]
    targvar = output[25]
    #Ensemble obtain
    '''
    is_ensemble = False
    is_crossval = False
    cv_warn=False
    dirpaths = [scriptdir] + outdirs
    if len(outdirs) > 1:
        is_ensemble = True
        dirpaths.append(get_ensemble_path(outdirs, rip_list))
        crossval_patt = ['00r1e2', '00r2e2']
        cv_match = [crossval_patt.index(c) for c in crossval_patt if re.search(c, outdirs[1]) is not None]
        if (len(cv_match) == 1):
            is_crossval=True
            dirpaths = dirpaths + get_crossval_paths(outdirs)
            print(cv_match)
            if cv_match[0]==0:
                cv_warn = True
    if cv_warn:
        print("\033[1;43mWarning in cleanup_script! The cross-validated ensembles " +
              "generated with this experiment will not be regenerated " +
              "automatically! Run other member of experiment to regenerate.\033[1;m ")
    print "Starting cleanup file operations with opt " + opt
    exist_dirs = [el for el in dirpaths if os.path.exists(el)]
    if len(exist_dirs) > 0 and is_csv:
        print("Error in clean_prior_experiments: csv experiments don't enforce dir structure. " +
              "Please manually clean up the components of this experiment - right now, " +
              "it's a guard against unexpected file deletion/moving.")
        print("\n\t".join(exist_dirs))
        sys.exit(1)
    if opt=='exit':
        if len(exist_dirs) > 0:
            print("Error in clean_prior_experiments: one or more directories that would " +
                  "be written to by the current experiment already exist. Please " +
                  "clean up directories and re-run, or change preeexist settings in config files.")
            print("Directories that exist: " + "\n\t".join(exist_dirs))
            sys.exit(1)
    if opt=='erase':
        for el in dirpaths:
            if os.path.exists(el):
                print "removing " + str(el)
                shutil.rmtree(el) #check on this - does it work for non-empty directories?
    elif opt=='move':
        #set suffix to move new paths to:
        suffix = str(max([count_suffix(el) for el in dirpaths]) + 1)
        for el in dirpaths:
            if os.path.exists(el):
                dest = "~".join([el.rstrip("/"), suffix])
                print "moving " + str(el) + " to " + dest
                shutil.move(el, dest)
                #except:
                #"Move failed; indexes are uneven. Trying again."
                #dest = 	"~".join([el.rstrip("/"), str(int(suffix)+1)])
                #shutil.move(el, dest)
