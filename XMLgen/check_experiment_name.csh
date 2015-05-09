#!/bin/csh -f

echo " "
echo "   CHECKING IF EXPERIMENT and/or related-files ALREADY EXISTS"
echo " "
set expDir = "$scriptDir/scripts/$projectID"

#check if experiment directory exists in expDir
unset preExistOpt
if (-e $expDir/$1) then
# echo "   >> $expDir/$1 exists. Do you want to overwrite?"
  set varvals = (exit erase move)
  set dinfo = "$expDir/$1 exists so exit now, or select to either erase or move exiting data/scripts/logfiles/etc. "
  source $QueryVals
  set preExistOpt = "$kval"
  echo "$preExistOpt" >> $INtxt
  echo "preExistOpt = $preExistOpt, check_experiment_name.csh" >> $INtxt.key

  if ($preExistOpt == "exit") then 
    echo " "
    echo "   OK. You do not want to overwrite $1."
    echo "   Please re-run XMLgen with new experiment name."
    echo "   Exiting."
    echo " "
  endif
  exit 0
else
  set preExistOpt = "exit"
  echo "   $expDir/$1 does not exists."
endif

#
#if experiment directory DOES NOT exist then check for earlier versions of XML & related text files 
# that will be backed-up


set datetime=`date +%m.%d.%Y-%k.%M.%S`

set notfound
if (-e $expDir/XML/$1.xml) then
  echo "   Found $expDir/XML/$1.xml"
  \mv $expDir/XML/$1.xml $expDir/XML/$1.xml.$datetime
  echo "   Moved $expDir/XML/$1.xml to $expDir/XML/$1.xml.$datetime"
  unset notfound
  sleep 2
endif  
if (-e $expDir/XMLtxt/$1.log) then
  \mv $expDir/XMLtxt/$1.log $expDir/XMLtxt/$1.log.$datetime
  echo "   Found $expDir/XMLtxt/$1.log"
  echo "   Moved $expDir/XMLtxt/$1.log to $expDir/XMLtxt/$1.log.$datetime"
  unset notfound
  sleep 2
endif
if (-e $expDir/XMLtxt/$1.input.txt) then
  echo "   Found $expDir/XMLtxt/$1.input.txt"
  \mv $expDir/XMLtxt/$1.input.txt $expDir/XMLtxt/$1.input.txt.$datetime
  echo "   Moved $expDir/XMLtxt/$1.input.txt to $expDir/XMLtxt/$1.input.txt.$datetime"
  unset notfound
  sleep 2
endif

if ($?notfound) echo "   No other XML text files found."
echo "   Proceeding."
echo " "
echo "========================================================== "
echo " "
sleep 5

exit 0
