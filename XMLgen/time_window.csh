#!/bin/csh -f

# TIME WINDOW
# --------------------

# set echo
  echo " "
#.....................................................................
# A more-generic time-window selection
# by finding all mask files in the timemask archive directory 
# for specific years and calendar type

  set timeMaskDir = /archive/esd/PROJECTS/DOWNSCALING/MASKS/timemasks/
  cd $timeMaskDir
  set varvals = (none)
  set twfiles = (`\ls maskdays*${windowDate1}-${windowDate2}*.nc`)
  if (${timemaskSuffix} != "") then 
    set varvals = (`\ls $twfiles | grep "${timemaskSuffix}.nc"`)
  else 
    set varvals = (`\ls $twfiles | grep -v "NO"`)
  endif
  set varvals = ($varvals "none")
  set dinfo = "Time Window"
  source $QueryVals
  set timeWindow = "$kval"
  echo "$timeWindow" >> $INtxt
  echo "timeWindow = $timeWindow, time_window.csh" >> $INtxt.key

  if ($timeWindow == "none") then
     set TimeWindowFile="na"
  else
     set TimeWindowFile="$timeMaskDir${timeWindow}"
  endif

  if (! -e $TimeWindowFile && $TimeWindowFile != "na") then
    echo "PROBLEM: $TimeWindowFile  does not exist."
    echo " "
  else
    if ("$TimeWindowFile" =~ "*_olap*") then
       set tSeg = (`echo $TimeWindowFile | sed -e 's/_/ /g'`)
       set TimeTrimFile = "$tSeg[1]_$tSeg[2]_$tSeg[4]"
       set tSuf = (`echo $tSeg[$#tSeg]| sed -e 's/\./ /g'`)
       set TimeTrimFile = (`echo $TimeTrimFile.$tSuf[2-] | sed -e 's/ /\./g'`)
    else
       set TimeTrimFile = "na"
    endif
    echo "............................................................................................."
    echo "   Time Window = $TimeWindowFile"
#   echo "   Time Trim Window = $TimeTrimFile"
    echo "............................................................................................."
    echo " "
  endif
