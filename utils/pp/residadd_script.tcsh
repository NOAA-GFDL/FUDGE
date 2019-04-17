#!/bin/tcsh
#c-shell script for residaly addition + metadata

set echo
module load nco
module load gcp


set var = ""
set resid_var = ""
set trend_var = ""
set resid_file = ""
set trend_file = ""
set trendresid_file = ""

set var = "tasmin"
set resid_file = "/archive/Oar.Gfdl.Esd_test/PROJECTS/DOWNSCALING/MACAdev/downscaled/GFDL-ESM2M/rcp85/day/atmos/GFDL-ESM2Mdetr1981-2099PwlSm21St1Ww31rcp85/r1i1p1/v20120228/MACAdevtndetrcap1-QDM-D1A48L01K00/tasminDetr/US480p1/tasminDetr_day_MACAdevtndetrcap1-QDM-D1A48L01K00_rcp85_r1i1p1_US480p1_20060101-20991231.nc"
set trend_file = "/archive/Oar.Gfdl.Esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/GFDL-ESM2M/rcp85/day/atmos/GFDL-ESM2Mtrend1981-2099PwlSm21St1Ww31rcp85/r1i1p1/v20120228/tasminTrendPwl/US480p1/tasminTrendPwl_day_GFDL-ESM2M_rcp85_r1i1p1_base1981-2099PwlSm21St1Ww31rcp85_20060101-20991231.nc"
set trendresid_file = "/work/cew/testing/fudge_resid/resid_test_01.nc"
#Note: this implies we need to set trend var, resid var, in script
#or do a more active deduction (like split on var and pass that)
set resid_var = "${var}Detr"
set trend_var = "${var}TrendPwl"

gcp -cd $resid_file $TMPDIR/$resid_file
set resid_file = $TMPDIR/$resid_file
gcp -cd $trend_file $TMPDIR/$trend_file
set trend_file = $TMPDIR/$trend_file

set trendresid_tmp = $TMPDIR/$trendresid_file
mkdir -p `dirname $trendresid_tmp`
mkdir -p `dirname $trendresid_file`

ncrename -h -v "$resid_var,$trend_var" -O $resid_file
# note to self: need header pad
ncbo --op='add' -v $trend_var $resid_file $trend_file -O $trendresid_tmp
ncrename -h -v "$trend_var,$var" -O $trendresid_tmp

#write trend-specific metadata (and do a clone)?
Rscript $BASEDIR/utils/pp/write_climatology_metadata.R "trend" $trend_file $resid_file $var $trend_var $resid_var $trendresid_tmp

#copy trendresid file to final resting location
gcp $trendresid_tmp $trendresid_file
