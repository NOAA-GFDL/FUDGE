#!/bin/python

import csv
from os import path as osp


def get_grid_params(grid, FUDGEROOT):
    '''
    Glorified lookup table with error checking. Looks up grid and returns
    relevant subsetting parameters for making the runscripts.
    Note: this assumes that we won't run on subsets of a grid.
    We don't have those args in the auxfile yet.
    For now (getting csv workflow before j wrapup), that's probably okay, but we
    need to address this later.
    '''
    grid_table = osp.join(FUDGEROOT, "utils/aux/grid_lookup.csv")
    # Establish default if lookup not found
    istart = iend = ioff = jstart = jend = joff = 1
    with open(grid_table, 'r') as gtab:
        reader = csv.DictReader(gtab)
        for row in reader:
            if row['grid'] == grid:
                istart = int(row['istart'])
                iend = int(row['iend'])
                jstart = int(row['jstart'])
                jend = int(row['jend'])
                #ioff = istart - 1 # iend - istart + 1
                #joff = iend - 1 #jend - jstart + 1
    return(istart, iend, ioff, jstart, jend, joff)
