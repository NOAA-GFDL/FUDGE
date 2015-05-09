#!/bin/csh -f

# selecting dataset based on varn, dataCategory 
#
# In calling shell, 
#     set variables: varn, dataCategory, dataType
#  output variables: dataSet (subdirectories below /archive/esd/PROJECTS/DOWNSCALING, separated by ".")
#
set curDir = `pwd`
#echo $curDir

alias ff      'find . -name \!* -print '

set varn = $findVar
set dC = $findCat
set dT = $findType
set dS = $findSource
set dRIP = $findRIP
set dEpoch = $findEpoch

cd $inRoot/$dC
#echo "Looking for directories under $inRoot/$dC ..."
set searchString = "./$dT/*"
if ($?dS) set searchString = "./$dT/$dS"
set dirs = (`\ls -d $searchString`)
set fList = ()
foreach d ( $dirs )
  if ($dT != "NCPP" || $dRIP != "") then
    set dlist = ($d/${dEpoch}*/*/*/*/${dRIP}*/v*)
  else
    set dlist = ($d/${dEpoch}*/*/*/*/r?i?p?/v*)
  endif
  foreach dl ($dlist)
    set varCheck = (`\ls -m $dl |grep -w $varn`)
      if ($status == 0) then 
        set dblCheck = `\ls -ld $dl/$varn`
         if ($status != 0)  exit 1
# exclude livneh "v1.2" directory entries
        if ($dC == "OBS_DATA") then
          echo $dl  > /tmp/tfile
          grep 'v1\.2' /tmp/tfile
            if ($status != 0) then
              set fList = ($fList "$dl")
            endif
        else
          if ("$dl" != "$excludedData") then
            set fList = ($fList "$dl")
          endif
        endif
      endif
  end
end

if ($#fList == 0) then
  echo "    No $varn variable found under $inRoot/$dC/$dT/$dS/$dEpoch . Exiting."
  exit 1
else
#  echo "    $varn variable is found under $inRoot/$dC."
endif

# 
# Select from datasets identified
#
cd $curDir
set varvals = ($fList)
set dinfo = "$varn, select one of the following $dC datasets"
source $QueryVals

set dataSet = "$kval"
echo "$dataSet" >> $INtxt
echo "dataSet = $dataSet, find_dataset_based_on_var.csh" >> $INtxt.key
set excludedData = "$dataSet"

sleep 5

#echo "    $dataSet selected"

set dSet = `echo $dataSet | sed 's/.\///'`
set dataSet = `echo $dSet | sed 's/\//./g'`

# parse the subdirectories
set parseList = `echo $dSet | sed 's/\// /g'`
#echo $parseList
set dataType = $parseList[1]
set dataSource = $parseList[2]
set epoch = $parseList[3]
set freq = $parseList[4]
set realm = $parseList[5]
set misc = $parseList[6]
set rip = $parseList[7]
set dataVersion = $parseList[8]
set dataSet = "$dC.$dataSet"

cd $curDir
