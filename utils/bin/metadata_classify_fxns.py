#!/bin/python
import os, sys, time, re
from os import path as osp
import json
import netCDF4
from netCDF4 import Dataset
from optparse import OptionParser
import ConfigParser
BASEDIR = os.environ.get('BASEDIR')
if BASEDIR is None:
    print("Error in metadata setup: no version of fudge is checked out! Please check out a jamoca-or-later version of fudge.")
    sys.exit(1)
sys.path.append(osp.join(BASEDIR, 'utils/aux'))
from fudgeUtils import gcp


def get_experiment_metadata(gd, pd, OH_file, MF_file, ds_experiment_name,
                            running_standalone=False, is_strict=True,
                            verbose=False):
    # Sets up parsing for OH and MF experiment attributes, based off of classification.
    if running_standalone:
        OH_file = gcp(OH_file, verbose=verbose)
        MF_file = gcp(MF_file, verbose=verbose)
    # Parse and add OH
    oh_dict, oh_class = get_file_metadata(OH_file, 'OH', is_strict, verbose)
    gd['ds_OH_source'] = oh_dict['dataset']
    pd['ds_OH_source'] = oh_class
    # Parse and add attributes from MF
    mf_dict, mf_class = get_file_metadata(MF_file, 'MF', is_strict, verbose)
    gd['ds_MF_source'] = mf_dict['dataset']
    pd['ds_MF_source'] = mf_class
    # I briefly called this ds_MF_scenario; switching back
    # to maintain backwards-compatability
    gd['ds_scenario'] = mf_dict['scenario']
    pd['ds_scenario'] = mf_class
    gd['title'] = mf_dict['orig_experiment'] + " downscaled by FUDGE " + gd['fudgetag'] + " " + ds_experiment_name
    pd['title'] = mf_class
    return(gd, pd)


def get_file_metadata(vfile, dtype="MF", is_strict=True, verbose=False):
    # Gets (and checks) file metadata atts depending on classification
    if verbose:
        print("Getting metadata for " + vfile)
    dkeys = ['dataset'] if dtype is "OH" else ['dataset', 'scenario', 'orig_experiment']
    vclass = classify_data(vfile, verbose)
    if vclass == "Unclassified":
        if is_strict:
            print("Error in get_file_metadata: classification failed with strict parsing!")
            sys.exit(1)
        else:
            rdict = {k: val for (k, val) in zip(dkeys, ['not found'] * len(dkeys))}
    else:
        rdict = get_attributes(vfile, vclass, verbose)
        if is_strict:
            for k in dkeys:
                if rdict[k] == "not found":
                    print("Error in get_file_metadata: key " + k + " not found with strict parsing!")
                    sys.exit(1)
            rdict = {k: rdict[k] for k in dkeys}
        else:
            rdict = {k: rdict[k] for k in dkeys}
    return(rdict, vclass)

def classify_data(vfile, verbose=False):
    # Classifies datasets by attribute
    if verbose:
        print("Classifying " + vfile)
    vclass = "Unclassified"
    # First do classification based off of filename (only one case so far)
    fsplit = re.split("_", osp.basename(vfile))
    if len(fsplit) > 2:
        if (fsplit[2] == 'TPLM2'):
            vclass = "CBAY"
    # For remaining atts, there are 3 components: class, has_attname, has_attname_of_value
    classval = ['HIRAM',  'CORDEX', 'CMIP-6',
                'Livneh', 'Prism', 'Daymet', 'HIRAM', 'CCSM4', 'MIROC5', 'MPI-ESM-LR', 'GFDL-FLORB01-p2FA', 'KB_SYN']
    attnames = ['gfdl_experiment_name', 'project_id', 'conventions',
                'title', 'title', 'title', 'model_id', 'title', 'title', 'title', 'title', 'experiment_id']
    attgrep = ['HIRAM', "CORDEX", 'CMIP-6',
               'livneh', 'prism', 'daymet', 'HIRAM', 'CCSM4', 'MIROC5', 'MPI-ESM-LR', 'GFDL-FLORB01-p2FA', 'QUANTILEMAP']
    if vclass == "Unclassified":
        with Dataset(vfile, 'r') as nc_obj:
            v_attnames = nc_obj.__dict__.keys()
            for nm,sch,cval in zip(attnames, attgrep, classval):
                if verbose:
                    print(" ".join([nm, sch, cval]))
                    #print(nm)
                    #print(v_attnames)
                # Account for case-insensitve search
                # i.e. conventions and Conventions
                v_search = [el for el in nc_obj.__dict__.keys()
                            if re.search(nm, el, re.IGNORECASE) is not None]
                if len(v_search) > 0:
                    sch_nm = v_search[0] if len(v_search) == 1 else nm
                    # if nm in v_attnames:
                    if sch != "":
                        # Next 2 lines are for case-insensitve search
                        v_att = nc_obj.__dict__[sch_nm]
                        v_search = re.search(sch, v_att, re.IGNORECASE)
                        if verbose: print(sch + " " + v_att); print(v_search)
                        if v_search is not None:
                            vclass = cval; break
                    else:
                        vclass = cval; break
    return(vclass)


def get_attributes(vfile, vclass, verbose):
    # Sequence for a fixed key/value pair
    # or splitting of filenames:
    if verbose:
        print("Getting attributes of " + vfile)
        print("Classified as " + vclass)
    attdict = {'dataset': "not found", "scenario": "not found", "orig_experiment": "not found"}
    title_datasets = ['Livneh', 'Prism', 'Daymet', 'CCSM4', "MIROC5", "MPI-ESM-LR", "GFDL-FLORB01-p2FA"]
    if vclass == "CBAY":
        attdict['dataset'] = "TPLM2"
        return(attdict)
    elif vclass == 'KB_SYN':
        with Dataset(vfile, 'r') as nc_obj:
            atts = atts = nc_obj.__dict__
        attdict['dataset'] = atts['model_id']
        attdict['scenario'] = atts['experiment_id']
        attdict['orig_experiment'] = atts['model_id'] + " " + atts['experiment_id']
    elif vclass in title_datasets:
        attdict['dataset'] = vclass
        # The FLOR case needs more attention; however, we're not running those
        # right now, so that's a lower priority
        if vclass in ['CCSM4', 'MIROC5', "MPI-ESM-LR"]:
            fname = osp.basename(vfile)
            fsplit = re.split("_", fname)
            with Dataset(vfile, 'r') as nc_obj:
                atts = nc_obj.__dict__
                attdict['scenario'] = atts['experiment_id']
                attlist = ['model_id', 'experiment', 'realization']
                #att_reg = ["", "", "_(r[:num:])"]
                att_idx = [2,3,4]
                # Adding a workaround for some of our clim/anom cases - they
                # might not thave a rip in the metadata
                alist = []
                for a,idx in zip(attlist, att_idx):
                    if a in atts.keys():
                        alist.append(str(atts[a]))
                    else:
                        alist.append(str(fsplit[idx]))
                #attdict['orig_experiment'] = " ".join([str(atts[a]) for a in attlist])
                attdict['orig_experiment'] = " ".join(alist)
            return(attdict)
        elif vclass in ["GFDL-FLORB01-p2FA"]:
            with Dataset(vfile, 'r') as nc_obj:
                atts = nc_obj.__dict__
                attdict['dataset'] = "FLORB01"
                attdict['scenario'] = atts['experiment_id']
                attlist = ['model_id', 'experiment_id']
                # We'd normally include 'realizations' as well, but the short
                # form of those is not present in the metadata
                # , 'realizations'
                attdict['orig_experiment'] = " ".join([str(atts[a]) for a in attlist])
            # ... so we need to do a split on the filename instead.
            fname = osp.basename(vfile)
            fsplit = re.split("_", fname)
            attdict['orig_experiment'] += " " + str(fsplit[4])
            return(attdict)
    # Otherwise, we need to split metadata atts
    with Dataset(vfile, 'r') as nc_obj:
        atts = nc_obj.__dict__
        if vclass == "HIRAM":
            if 'gfdl_experiment_name' in atts.keys():
                exp_name = atts['gfdl_experiment_name']
                exp_split = re.split("_", exp_name)
                print(exp_name)
                print(exp_split)
                if len(exp_split) == 5:
                    attdict['scenario'] = exp_split[3]
                    attdict['dataset'] = "_".join([exp_split[0],
                                                   exp_split[1], exp_split[2]])
                elif len(exp_split) == 3:
                    attdict['scenario'] = exp_split[2]
                    attdict['dataset'] = "_".join([exp_split[0], exp_split[1]])
                else:
                    print("Error in get_attributes: parse of hiram had too many elements!")
                    sys.exit(1)
            else:
                attdict['dataset'] = atts['model_id']
                attdict['scenario'] = atts['experiment_id']
            #Second parse is neccessary to replace old functionality
            attdict['orig_experiment'] = atts['model_id'] + " " + atts['experiment_id']
            search_patt = 'GFDL-HIRAM-C360-COARSENED'
            if re.search(search_patt, attdict['orig_experiment']) is not None:
                scen = atts['experiment_id']
                scendict = {'sst2030': 'RCFP 4.5', 'stt2090': 'RCP 8.5'}
                physdict = {'1': 'ESM', '2': 'CM3'}
                try:
                    exp = " ".join(['GFDL-HIRAM', scendict[str(scen)],
                                    physdict[str(atts['physics'])], scen])
                    attdict['orig_experiment'] += exp
                except KeyError:
                    attdict['orig_experiment'] = atts['model_id'] + " " + atts['experiment_id']
        elif vclass == "CORDEX":
            model_dataset = atts['model_id']
            drive_dataset = atts['driving_model_id']
            attdict['dataset'] = model_dataset + "_driven_by_" + drive_dataset
            attdict['scenario'] = atts['experiment_id']
            attdict['orig_experiment'] = atts['model_id'] + " " + atts['experiment_id']
        elif vclass == "CMIP-6":
            attdict['dataset'] = atts['source_id']
            attdict['scenario'] = atts['experiment_id']
            attdict['orig_experiment'] = atts['title'] #Check on this
    return(attdict)


def send_test(testname, OH_file, MF_file, verbose=True):
    gd = {'fudgetag': 'tag-' + testname}
    pd = {}
    exp = "experiment-"+testname
    md = get_experiment_metadata(gd, pd, OH_file, MF_file, exp,
                                 True, True, verbose)
    meta_text = json.dumps(md, indent=2)
    print(meta_text)
    print("\n")


def run_classify_tests():
    '''
    Runs a set of pre-written tests for the
    '''
    print("HIRAM experiment")
    OH_file = "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/NCPP/GFDL-HIRAM-C360/amip/day/atmos/day/r1to2i1p1/v20110601/pr/US48/pr_day_GFDL-HIRAM-C360_amip_r1to2i1p1_US48_19790101-20081231_ensmbl.nc"
    MF_file = "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/NCPP/GFDL-HIRAM-C360-COARSENED/amip/day/atmos/day/r1to2i1p1/v20110601/pr/US48/pr_day_GFDL-HIRAM-C360-COARSENED_amip_r1to2i1p1_US48_19790101-20081231_ensmbl.nc"
    send_test("HIRAM-01", OH_file, MF_file)

    # Variations on I35 experiments
    print("ccsm4-daymet")
    OH_file = "/archive/esd/PROJECTS/DOWNSCALING/OBS_DATA/GRIDDED_OBS/daymet/historical/day/atmos/day/r0i0p1/v2p1/pr/SCCSC0p1/pr_day_daymet_historical_r0i0p1_SCCSC0p1_19810101-20051231.nc"
    MF_file = "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/CCSM4/rcp26/day/atmos/day/r6i1p1/v20120705/pr/SCCSC0p1/pr_day_CCSM4_rcp26_r6i1p1_SCCSC0p1_20060101-20991231.nc"
    send_test("ccsm4-daymet", OH_file, MF_file)
    print("miroc-prism")
    OH_file = "/archive/esd/PROJECTS/DOWNSCALING/OBS_DATA/GRIDDED_OBS/prism/historical/day/atmos/day/r0i0p1/v1/pr/SCCSC0p1/pr_day_prism_historical_r0i0p1_SCCSC0p1_19810101-20051231.nc"
    MF_file = "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/MIROC5/rcp85/day/atmos/day/r1i1p1/v20120710/pr/SCCSC0p1/pr_day_MIROC5_rcp85_r1i1p1_SCCSC0p1_20060101-20991231.nc"
    send_test("miroc-prism", OH_file, MF_file)
    print("mpi-livneh")
    OH_file = "/archive/esd/PROJECTS/DOWNSCALING/OBS_DATA/GRIDDED_OBS/livneh/historical/day/atmos/day/r0i0p0/v1p2/pr/SCCSC0p1/pr_day_livneh_historical_r0i0p0_SCCSC0p1_19810101-20051231.nc"
    MF_file = "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/MPI-ESM-LR/rcp45/day/atmos/day/r1i1p1/v20111006/pr/SCCSC0p1/pr_day_MPI-ESM-LR_rcp45_r1i1p1_SCCSC0p1_20060101-20991231.nc"
    send_test("mpi-livneh", OH_file, MF_file)

    # CMIP 6 experiment
    print("cmip-6")
    OH_file = "/archive/esd/PROJECTS/DOWNSCALING/OBS_DATA/GRIDDED_OBS/livneh/historical/day/atmos/day/r0i0p1/v2016/tasmax/US480p1/tasmax_day_livneh_historical_r0i0p1_US480p1_19810101-20141231.nc"
    MF_file = "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP6/GFDL-CM4/historical/day/atmos/day/r1i1p1f1/v20181207/tasmax/US480p1/tasmax_day_GFDL-CM4_historical_r1i1p1f1_US480p1_19810101-20141231.nc"
    send_test("cmip-6", OH_file, MF_file)
    # cbay experiment
    print("cbay")
    OH_file = "/archive/esd/PROJECTS/DOWNSCALING/OBS_DATA/GRIDDED_OBS/TPLM2/historical/day/atmos/day/r0i0p0/v1/tas/NEUSA/tas_day_TPLM2_historical_r0i0p0_NEUSA_19860101-20151231.nc"
    MF_file = "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CM2p5/GFDL-FLORB01-p2FA/lead01/day/atmos/day/r1to12i1p1/v1/tas/NEUSA/tas_day_GFDL-FLORB01-p2FA_lead01_r1to12i1p1_NEUSA_19860101-20151231_ensmbl.nc"
    send_test("cbay", OH_file, MF_file)

    print("metadata test cases complete.")


def parse_meta_args():
    '''
    Parses the commandline metadata args and returns --help output
    '''
    usage = "usage: %prog --OH oh_file.nc --MF mf_file.nc" + \
            "[--testtype 'commandline']"
    parser = OptionParser(usage=usage, version='jamoca')
    parser.add_option("--OH", help="Path to a file with OH data. Metadata will be taken from the file in the way that OH attributes normally are.")
    parser.add_option("--MF", help="Path to a file with MF data. Metadata will be taken from the file in the way that MF attributes normally are.")
    parser.add_option("--testtype", default='test', help="Placeholder to show how experiment name and fudgetag interact with metadata. If not set, defaults to 'test'")
    parser.add_option("-v","--verbose", action='store_true', default=False, help="Whether to print extra diagnostic output; defaults to False")
    options,args = parser.parse_args()
    print(options)
    print(options.testtype)
    send_test(testname=options.testtype, OH_file=options.OH, MF_file=options.MF, verbose=options.verbose)


if __name__ == '__main__':
    if len(sys.argv) > 1:
        if (len(sys.argv) >= 2):
            # First arg is OH, second arg is MF
            # assume that you will have to copy them
            # send_test("commandline", sys.argv[1], sys.argv[2])
            parse_meta_args()
        else:
            print("Error in metadata_clasify_fxns: you need at least two args!")
            sys.exit(1)
    else:
        # Call the pre-written tests
        run_classify_tests()
