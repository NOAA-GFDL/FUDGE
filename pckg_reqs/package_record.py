#!/bin/python
import pip
import subprocess
import os,sys
from os import path as osp
global BASEDIR
BASEDIR = os.environ.get('BASEDIR')
if BASEDIR is None:
    print("Error in pacakge-recording setup: no version of fudge is checked out! Please check out a jamoca-or-later version of fudge.")
    sys.exit(1)

if __name__ == '__main__':
    '''
    Simple script for getting installed pacakges in R and python
    Assumes that you've got teh version of R you're interested in loaded
    '''
    py_file = osp.join(BASEDIR, 'pckg_reqs/py_installed.txt')
    print("writing python packages installed to " + py_file)
    cmd = [sys.executable, '-m', 'pip', 'freeze']
    print(" ".join(cmd))
    py_reqs = subprocess.check_output(cmd)
    with open(py_file, 'w') as f:
        f.write(sys.executable + "\n")
        f.write(py_reqs)
    r_file = osp.join(BASEDIR, 'pckg_reqs/r_vers.txt')
    print("writing R pacakges installed to " + r_file)
    cmd = ["R", "--version"]; print(" ".join(cmd))
    r_vers = subprocess.check_output(cmd)
    with open(r_file, 'w') as f:
        f.write(r_vers + "\n")
    r_file = osp.join(BASEDIR, 'pckg_reqs/r_installed.csv')
    cmd = ["R", "-e",
           "write.csv(installed.packages(), file='" + r_file + "')"]; print(" ".join(cmd))
    r_reqs = subprocess.check_output(cmd)
    print("all is written")
    sys.exit(0)
