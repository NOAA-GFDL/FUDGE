#!/usr/bin/python
#Writes an experiment_info.txt file given a parsed json input.

import json
import os,sys
from os import path as osp
import re
from itertools import chain

def make_exp_info(input, exp_outpath):
    '''
    Writes an experiment_info.txt file to file.
    in: dictionary-pased auxfile
    exp_outpath:
    '''
    ind,oh,mh,mf = parse_auxfile(input)
    scriptdir = ind['script_settings']['scriptdir']
    xml_files = [el for el in os.listdir(scriptdir + "/config/") if el.endswith('xml')]
    if len(xml_files) > 0:
        xml = osp.join(scriptdir + "/config/", xml_files[0])
    else:
        xml = "not used"
    istart = 0
    # Okay - this right here? It's a hack, and a messy one. Need to fix this later.
    iend = len(os.listdir(osp.join(scriptdir, 'runscript')))
    dsets = ['OH', 'MH', 'MF']
    spatmasks = []
    for d in dsets:
        spatmasks.append([ind[d][el]['geomask'] for el in list(ind[d].keys())])
    spatmasks = list(set(chain.from_iterable(spatmasks))) #if el != 'none'
    spatmask = spatmasks[0]
    in_files = [ind['OH'][oh[0]]['file'], ind['MH'][mh[0]]['file'], ind['MF'][mf[0]]['file']]
    #time_starts, time_ends = [get_file_time_strings(el) for el in in_files]
    timestrings = [get_file_time_strings(el) for el in in_files]
    outdir = ind['DS']['out_dir']
    if len(ind['DS']['rips']) > 0:
        rip = ind['DS']['rips'][-1]
        outdir = re.sub('<RIP>', rip, outdir)
    tmask_list = ["target.time.window:" + ind['OH'][oh[0]]['tmask'],
                  "hist.time.window:"+ ind['MH'][mh[0]]['tmask'],
                  "fut.time.window:"+ ind['MF'][mf[0]]['tmask']]
    trim_mask = ind['DS']['indexing_tmask']
    if trim_mask != ind['MF'][mf[0]]['tmask']:
        tmask_list.append("trim.time.mask:" + trim_mask)
    tmask_strings = "\n".join(tmask_list)

    oh_inpath_list = []
    mh_inpath_list = []
    mf_inpath_list = []
    for v in range(len(oh)):
        oh_inpath_list.append("target.inpath_" + str(v+1) + ":" + ind['OH'][oh[v]]['file'])
    for v in range(len(mh)):
        mh_inpath_list.append("hist.inpath_" + str(v+1) + ":" + ind['MH'][mh[v]]['file'])
    for v in range(len(mf)):
        mf_inpath_list.append("fut.inpath_" + str(v+1) + ":" + ind['MF'][mf[v]]['file'])

    inpath_list = oh_inpath_list + mh_inpath_list + mf_inpath_list

    inpath_list_string = "\n".join(inpath_list)

    out  = "\n".join(["ds.experiment:" + ind['ds_experiment_name'],
                      "xml.path:" + xml,
                      "FUDGE.version:" + os.environ.get('FUDGETAG'),
                      "region:" + ind['ds_region'],
                      "minifile.I.start:" + str(istart),
                      "minifile.I.end:" + str(iend),
                      "qc.mask:" + ('on' if ind['make_qcmask'] else 'off'),
                      "target:" + ", ".join(oh),
                      inpath_list_string,
                      "output.path:" + outdir,
                      "spat.mask.path_1:" + spatmask,
                      tmask_strings,
                      "target.train.start.year_1:" + timestrings[0][0],
                      "target.train.end.year_1:" + timestrings[0][1],
                      "hist.train.start.year_1:" + timestrings[1][0],
                      "hist.train.end.year_1:" + timestrings[1][1],
                      "fut.train.start.year_1:" + timestrings[2][0],
                      "fut.train.end.year_1:" + timestrings[2][1],
                      "script.base:" + scriptdir,
                      "aux:" + osp.abspath(input) + "\n"])
    with open(exp_outpath, 'w') as f:
        f.write(out)
    return(exp_outpath)


'''
ds.experiment:NEUSAtxp1-QDM-BA10L01K00
xml.path:/home/Carolyn.Whitlock/Code/k_testing/qdmall/xml/exp_B/NEUSAtxp1-QDM-BA10L01K00.xml
FUDGE.version:fudge/cew-dev
region:NEUSA
minifile.I.start:525
minifile.I.end:525
qc.mask:off
target:tasmax
target.inpath_1:/archive/esd/PROJECTS/DOWNSCALING/OBS_DATA/GRIDDED_OBS/livneh/historical/day/atmos/day/r0i0p1/v1p2/tasmax/NEUSA0p1/OneD/tasmax_day_livneh_historical_r0i0p1_NEUSA0p1_19810101-20051231.I*_J177-177.nc
hist.inpath_1:/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/CCSM4/historical/day/atmos/day/r6i1p1/v20130703/tasmax/NEUSA0p1/tasmax_day_CCSM4_historical_r6i1p1_NEUSA0p1_19810101-20051231.I*_J177-177.nc
fut.inpath_1:/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/CCSM4/historical/day/atmos/day/r6i1p1/v20130703/tasmax/NEUSA0p1/tasmax_day_CCSM4_historical_r6i1p1_NEUSA0p1_19810101-20051231.I*_J177-177.nc
output.path:/archive/cew/PROJECTS/DOWNSCALING/NEUSA/downscaled/CCSM4/historical/day/atmos/day/r6i1p1/v20130703/NEUSAtxp1-QDM-BA10L01K00/tasmax/NEUSA/OneD
spat.mask.path_1:/archive/esd/PROJECTS/DOWNSCALING/MASKS/geomasks/neusa_0p1/neusa_0p1_masks.nc
target.time.window:na
hist.time.window:na
fut.time.window:na
target.train.start.year_1:19810101
target.train.end.year_1:20051231
hist.train.start.year_1:19810101
hist.train.end.year_1:20051231
fut.train.start.year_1:19810101
fut.train.end.year_1:20051231
script.base:/nbhome/cew/PROJECTS/DOWNSCALING/scripts/NEUSA/NEUSAtxp1-QDM-BA10L01K00
aux:/home/Carolyn.Whitlock/Code/k_testing/qdmall/aux/NEUSA.default.singlept.json
NEMSemail:Carolyn.Whitlock@noaa.gov
'''

def parse_auxfile(input):
    '''
    Parses a FUDGE-formatted json file.
    '''
    with open(input, 'r') as f:
        out = json.load(f)
    OHvar = list(out['OH'].keys())
    MHvar = list(out['MH'].keys())
    MFvar = list(out['MF'].keys())
    return(out, OHvar, MHvar, MFvar)

def get_file_time_strings(f):
    '''
    Given a file in standard ESD-style structure, parses the file to get the
    file start and file end dates
    '''
    fstart = "0"
    fend = "0"
    fsplit = re.split("_", osp.basename(f))
    if len(fsplit) > 1:
        if re.search("\.nc$", fsplit[-1]) is not None:
            lastsplit = re.split('-', re.sub("\.nc", "", fsplit[-1]))
            if len(lastsplit) == 2:
                fstart = lastsplit[0]; fend = lastsplit[1]
    return(fstart, fend)
