#cleanup_xtmp.py
#cleans up a specific experiment or a subset of directories in /xtmp/$USER before the latest timestamp

import os
from os import path as osp
import sys
import shutil
import optparse
from optparse import OptionParser
from datetime import datetime
import time

#in the section 8 tools, this uses S8_path instead
BASEDIR = os.environ.get('BASEDIR')
if BASEDIR is None:
    print("Error in python script calculation: $BASEDIR not set. Please source a path to the section 8 tools.")
    sys.exit(1)
sys.path.append(osp.join(BASEDIR, 'utils/shared_code'))

import env
import xtmp_functions


def main():
    '''
    Invocation of cleanup_xtmp from command line; accepts command-line args
    '''
    parser = OptionParser()
    parser.add_option("--clean-dir", default = None, help="Path to directory to clean at the end of an experiment; if set, check to make sure all files are mirrored in /archive and deletes the directory")
    parser.add_option("--clean-dirs-before", default = None, help="string in yyyymmdd form used to identify dates before a date to remove from xtmp")
    parser.add_option("--noop", action='store_true', default=False, dest="noop", help="If set, prints dirst that would be deleted rather than actually deleting dirs")
    (options,args) = parser.parse_args()
    if options.clean_dir is None:
        if options.clean_dirs_before is None:
            print("Error in clean_xtmp: one of clean-dir or clean-dirs-before should be used!")
            sys.exit(1)
        else:
            #parse the string as a date
            print(options.clean_dirs_before)
            xtmp_datestr = options.clean_dirs_before.format("%Y-%m-%d")
            print(xtmp_datestr)
            xtmp_date = datetime.strptime(xtmp_datestr, "%Y-%m-%d")
            clean_by_date(xtmp_date, options.noop)
    elif options.clean_dirs_before is None:
        clean_xtmp_dir(options.clean_dir)
    else:
        print("Error in clean_xtmp: both clean-dir and clean-dirs-before were specificed! Please only use one.")
        sys.exit(1)


def clean_xtmp_dir(xtmp_dirpath, noop=True):
    '''
    Removes a directory from /xtmp after checking to make SURE that all files in the output directories
    are properly mirrored in /archive; used in the postlude process of fudge before zipping logfiles
    Meant to be called from call_next_step
    '''
    print("cleaning xtmp experiment dir " + xtmp_dirpath)
    #Okay - I would add more comprehensive checks on files being written here,
    #but that functionality is pretty redundant with the gcp checks in the
    #ensemble workflow
    if noop:
        print("This is where we would remove " + xtmp_dirpath)
    else:
        shutil.rmtree(xtmp_dirpath)


def clean_by_date(xdate, noop=True):
    '''
    Takes a date in yyyymmdd form and removes all entries in /work/$USER/xtmp prior to that date. Useful for cleaning experiments that did not correctly terminate.
    Optional argument noop prints dirs that *would* be deleted without deleting; defaults to this behavior.
    Note that this is meant to be run as the user who owns the xtmp dir in question.
    '''
    ptmp = xtmp_functions.make_ptmp_path()
    esd_dir = osp.join(ptmp, "PROJECTS/DOWNSCALING/") #subprojects
    #list all dirs 2 dirs below this dir - should be the grid + experiment name
    exp_dirs = []
    dir_depth = 1
    print(esd_dir)
    for r,dirs,f in os.walk(esd_dir):
        if r[len(esd_dir):].count(os.sep) == dir_depth:
            for d in dirs:
                exp_dirs.append(osp.join(r, d))
    exp_dirs.sort(key = lambda x: osp.getmtime(x))
    print("list of all dirs:")
    print("\n".join(exp_dirs))
    exp_before_date = [el for el in exp_dirs if osp.getmtime(el) <= xdate.timestamp()]
    print("dirs modified before date " + format(xdate, "%Y-%m-%d") + ":")
    print("\n" + "\n".join(exp_before_date))
    if noop:
        print("The above dirs will NOT be removed at this time. Run without the --noop flag to delete dirs.")
    else:
        for exp in exp_before_date:
            print("deleting " + exp)
            time.sleep(2)
            shutil.rmtree(exp)
        print("cleanup complete")


#Main method invocation
if __name__ == '__main__':
    main()
