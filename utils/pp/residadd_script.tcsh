#!/bin/tcsh
#c-shell script for residaly addition + metadata

set echo
source $BASEDIR/set_fudge_env


set var = $1
set resid_var = $2
set trend_var = $3
set resid_file = $4
set trend_file = $5
set trendresid_file = $6

mkdir -p `dirname $TMPDIR/$resid_file`
gcp $resid_file $TMPDIR/$resid_file
set resid_file = $TMPDIR/$resid_file
mkdir -p `dirname $TMPDIR/$trend_file`
gcp -cd $trend_file $TMPDIR/$trend_file
set trend_file = $TMPDIR/$trend_file

set trendresid_tmp = $TMPDIR/$trendresid_file
mkdir -p `dirname $trendresid_tmp`
mkdir -p `dirname $trendresid_file`

ncrename -h -v "$resid_var,$trend_var" -O $resid_file

# note to self: need header pad
# The ncks command both saves off the metadata vars (i_offset, lat_bnds)
# and allows for a header pad. The -A in the second line avoids
# overwriting the metadata we worked so hard to add.
ncks -h --hdr_pad 6000 $resid_file -O $trendresid_tmp
ncbo --op='add' -v $trend_var $resid_file $trend_file -A $trendresid_tmp
ncrename -h -v "$trend_var,$var" $trendresid_tmp

#write trend-specific metadata (and do a clone)?
Rscript $BASEDIR/utils/pp/write_climatology_metadata.R "trend" $trend_file $resid_file $var $trend_var $resid_var $trendresid_tmp

#copy trendresid file to final resting location
gcp $trendresid_tmp $trendresid_file
