#!/bin/tcsh
#c-shell script for anomaly addition + metadata

set echo
module load nco
module load gcp

set var = $1
set anom_file = $2
set clim_file = $3
set climanom_file = $4

#set var = "tasmax"
#set anom_file = "/archive/esd/PROJECTS/DOWNSCALING/PM/downscaled/GFDL-HIRAM-C360-COARSENED/sst2090/day/atmos/PManom2086-2095Sbx7/r1i1p1/v20110601/PMtxanp1-BCQM-A01r11TEST1X01K00/tasmaxAnom/US48/tasmaxAnom_day_PMtxanp1-BCQM-A01r11TEST1X01K00_sst2090_r1i1p1_US48_20860101-20951231.nc"
#set clim_file = "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/NCPP/GFDL-HIRAM-C360-COARSENED/sst2090/day/atmos/clim2086-2095Sbx7/r1to3i1p1/v20110601/tasmaxClim/US48/tasmaxClim_day_GFDL-HIRAM-C360-COARSENED_sst2090_r1to3i1p1_US48_20860101-20951231.nc"
#set climanom_file = "/work/cew/testing/fudge_anom/anom_test_01.nc"


set anom_var = "${var}Anom"
set clim_var = "${var}Clim"

pushd $TMPDIR

gcp -cd $anom_file $TMPDIR
set anom_file = $TMPDIR/$anom_file
gcp -cd $clim_file $TMPDIR/$clim_file
set clim_file = $TMPDIR/$clim_file

set climanom_tmp = $TMPDIR/$climanom_file
mkdir -p `dirname $climanom_tmp`

ncrename -h -v "$anom_var,$clim_var" -O $anom_file
# note to self: need header pad
ncbo --op='add' -v $clim_var $anom_file $clim_file -O $climanom_tmp
ncrename -h -v "$clim_var,$var" -O $climanom_tmp

#write climatology-specific metadata (and do a clone)?
Rscript $BASEDIR/utils/pp/write_climatology_metadata.R "anom" $clim_file $anom_file $var $climanom_tmp

#clone all variables of interest
#write select variables of interest

#copy climanom file to final resting location
gcp $climanom_tmp $climanom_file
