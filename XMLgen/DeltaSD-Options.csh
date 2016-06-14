#!/bin/csh -f
echo " "
echo " "
echo "    DeltaSD - Delta Downscaling Method "
echo " "
echo "       options:"
echo " "
echo "         deltatype    : 'median' or 'mean' "
echo "         deltaop      : 'ratio' or 'add' "
echo "         ignore0.Delta  : 'on' or 'off'"
echo " "
echo "    This method works best when all input vectors (target, historic and future) are of the same length. If they are not of the same length, then when there are more historic values than future values, the target (local historical) vector will be truncated to match the length of the future vector (note: the historic predictor vector is not truncated). If there are more future values than historic values, then separate deltas will be calculated and applied for each segment of the future data <= length of the historical data."
echo " "
echo " "

# deltatype
echo "    Option: deltatype "
echo "    ----------------- "
set varvals = ('median' 'mean')
set dinfo = " deltatype, select option. (median is generally preferred)"
source $QueryVals
set deltatype = "$kval"
  echo "$deltatype" >> $INtxt
  echo "deltatype = $deltatype, $0" >> $INtxt.key

# deltaop
echo "    Option: deltaop "
echo "    ----------------- "
set varvals = ('ratio' 'add')
set dinfo = " deltaop, select option. ['add' is preferred for any variables for which negative values are not an error (i.e. temperature); 'ratio' is preferred for everything else (i.e. pr)]"
source $QueryVals
set deltaop = "$kval"
  echo "$deltaop" >> $INtxt
  echo "deltaop = $deltaop, $0" >> $INtxt.key

echo "        <deltatype>"\'$deltatype\'"</deltatype>" >> $workDir/custom_XML
echo "        <deltaop>"\'$deltaop\'"</deltaop>" >> $workDir/custom_XML

# ignore0.Delta
if ($use_pr_adjustment == True) then
  echo "    Option: ignore0.Delta "
  echo "    ----------------- "
  set varvals = ('on' 'off')
  set dinfo = " ignore0.Delta , select option. ['on' ensures that any values in the initial dataset set to 0 - such as you might get for the pr adjustment functions - are kept as 0 for the downscaling process. It is generally preferred to be kept on for precipitation]"
  source $QueryVals
  set ignore0Delta = "$kval"
    echo "$ignore0Delta" >> $INtxt
    echo "ignore0Delta = $ignore0Delta, $0" >> $INtxt.key
  echo "        <ignore0.Delta>"\'$ignore0Delta\'"</ignore0.Delta>" >> $workDir/custom_XML
endif

if ($ignore0Delta == "on") then
   set ignore0Delta = "'on'"
else
   set ignore0Delta = "'off'"
endif
