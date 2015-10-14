#!/bin/csh -f

# Parse fullpath.
#
#set echo
set curDir = `pwd`

set fullPath = $1

# parse the subdirectories
set pList = `echo $fullPath | sed 's/\// /g'`
#echo $pList
set root1 = $pList[1]
set root2 = $pList[2]
set root3 = $pList[3]
set root4 = $pList[4]
set tmpDataCategory = $pList[5]
set tmpDataType = $pList[6]
set tmpDataSource = $pList[7]
set tmpEpoch = $pList[8]
set tmpFreq = $pList[9]
set tmpRealm = $pList[10]
set tmpMisc = $pList[11]
set tmpRIP = $pList[12]
set tmpVs = $pList[13]
set tmpVar =  $pList[14]
set tmpRegion =  $pList[15]
set tmpDim =  $pList[16]
set tmpDataSet = "$tmpDataCategory.$tmpDataType.$tmpDataSource.$tmpEpoch.$tmpFreq.$tmpRealm.$tmpMisc.$tmpRIP.$tmpVs"

set filename = $pList[17]

set fileVar = `echo $filename | awk 'BEGIN { FS = "_" } ; { print $1 }'`
set fileFreq = `echo $filename | awk 'BEGIN { FS = "_" } ; { print $2 }'`
set fileSource = `echo $filename | awk 'BEGIN { FS = "_" } ; { print $3 }'`
set fileEpoch = `echo $filename | awk 'BEGIN { FS = "_" } ; { print $4 }'`
set fileRIP = `echo $filename | awk 'BEGIN { FS = "_" } ; { print $5 }'`
set fileRegion = `echo $filename | awk 'BEGIN { FS = "_" } ; { print $6 }'`

# get time range start and end dates
 
set tstring = `echo $filename | awk 'BEGIN { FS = "_" } ; { print $7 }'`
set Jinfo = `echo $filename | awk 'BEGIN { FS = "_" } ; { print $8 }'`
set Jrange = `echo $Jinfo | awk 'BEGIN { FS = "." } ; { print $1 }'`

set fileTrange = `echo $tstring | awk 'BEGIN { FS = "." } ; { print $1 }'`
set Iinfo = `echo $tstring | awk 'BEGIN { FS = "." } ; { print $2 }'`

set date1 = `echo $fileTrange | awk 'BEGIN { FS = "-" } ; { print $1 }'`
set date2 = `echo $fileTrange | awk 'BEGIN { FS = "-" } ; { print $2  }'`

set fileSuffix = "${Iinfo}_${Jinfo}"
if ($fileSuffix != "I1_J1.nc") then
  if ("${Jrange}" != "J1-1") then
    echo "No set up to handle synthetic data files whose suffix is not "I1_J1.nc", single-point time series."
    exit 1
  endif
endif

set lons = 1
set lone = 1
set lats = 1
set late = 1
set file_j_range = "${Jrange}"


#echo " "
#echo "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ "
#echo "    Parse filename = $filename"
#echo "    Parsed filename components in variables:fileVar,fileFreq,fileSource,fileEpoch,fileRIP,fileRegion,fileTrange,date1,date2"
#echo "    Parsed filename components in variables:$fileVar,$fileFreq,$fileSource,$fileEpoch,$fileRIP,$fileRegion,$fileTrange,$date1,$date2"
#echo "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ "
#echo " "



cd $curDir
