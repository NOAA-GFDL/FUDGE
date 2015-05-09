#!/bin/csh -f

# filename file first argument is of form e.g. "pr_day_prism_historical_r0i0p0_SCCSC0p1_19810101-20051231.nc"
# parse to get pieces using awk

unset filename fileVar fileFreq fileSource fileEpoch fileRIP fileRegion tstring fileTrange date1 date2

set filename = $1

set fileVar = `echo $filename | awk 'BEGIN { FS = "_" } ; { print $1 }'`
set fileFreq = `echo $filename | awk 'BEGIN { FS = "_" } ; { print $2 }'`
set fileSource = `echo $filename | awk 'BEGIN { FS = "_" } ; { print $3 }'`
set fileEpoch = `echo $filename | awk 'BEGIN { FS = "_" } ; { print $4 }'`
set fileRIP = `echo $filename | awk 'BEGIN { FS = "_" } ; { print $5 }'`
set fileRegion = `echo $filename | awk 'BEGIN { FS = "_" } ; { print $6 }'`

# get time range start and end dates
 
set tstring = `echo $filename | awk 'BEGIN { FS = "_" } ; { print $7 }'`
set fileTrange = `echo $tstring | awk 'BEGIN { FS = "." } ; { print $1 }'`

set date1 = `echo $fileTrange | awk 'BEGIN { FS = "-" } ; { print $1 }'`
set date2 = `echo $fileTrange | awk 'BEGIN { FS = "-" } ; { print $2  }'`

#echo " "
#echo "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ "
#echo "    Parse filename = $filename"
#echo "    Parsed filename components in variables:fileVar,fileFreq,fileSource,fileEpoch,fileRIP,fileRegion,fileTrange,date1,date2"
#echo "    Parsed filename components in variables:$fileVar,$fileFreq,$fileSource,$fileEpoch,$fileRIP,$fileRegion,$fileTrange,$date1,$date2"
#echo "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ "
#echo " "


