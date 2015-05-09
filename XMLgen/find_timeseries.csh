#!/bin/csh -f

# FIND AVAILABLE TIME SERIES
# --------------------------

  set filePartA = $1
  set filePartB = $2
  set fileDir = $3

  cd $fileDir
  set files = (`\ls ${filePartA}_*-*.${filePartB}`)
  set listDate1 = ()
  set listDate2 = ()
  set listTrange = ()
  cd $cDir
  foreach f ($files)
    source $parseFile $f
    if ($date2 <= 20991231) then
      set listDate1 = ($listDate1 $date1)
      set listDate2 = ($listDate2 $date2)
      if ("$listTrange" == "") then 
        set listTrange = ("$fileTrange")
      else
        set listTrange = ("$listTrange" "$fileTrange")
      endif
    endif
  end

# echo "IN findTS. listTrange = $listTrange $#listTrange"
    
  if ($#listTrange == 0) then
    echo "     No suitable time series are available."
    echo "     File(s) found: "
    \ls -l $fileDir$files
    echo "   "
    echo "    Exiting"
    echo "   "
    exit 1
  else
    set varvals = ($listTrange)
#   echo "in $0, listTrange = |$varvals| # values = $#varvals."
    set dinfo = "Available Time Series"
    source $QueryVals
    set tRange = "$kval"
    echo "$tRange" >> $INtxt
    echo "tRange = $tRange, find_timeseries.csh" >> $INtxt.key
    set Dates = (`echo $tRange | sed 's/-/ /g'`)
    set tsDate1 = $Dates[1]
    set tsDate2 = $Dates[2]
  endif
  exit 0
