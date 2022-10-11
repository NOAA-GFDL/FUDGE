#!/bin/python
import os,sys

TMPDIR = os.environ.get('TMPDIR')
if TMPDIR is None:
    TMPDIR = os.environ.get('S8TMPDIR')
if TMPDIR is None:
    print("Error in python script calculation: neither $TMPDIR nor $S8TMPDIR is set. Please set a path to the tmpdir.")
    sys.exit(1)
#This one shouldn't be None on any Unix system

USER = os.environ.get('USER')

BASEDIR = os.environ.get('BASEDIR')

PTMPDIR = os.path.join('/work', USER)
print("setting USER, TMPDIR and PTMPDIR (/work)")
