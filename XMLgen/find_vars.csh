#!/bin/csh -f
#
# Find and return list of variables based on dataCategory/dataType/dataSource/epoch
#
# In calling shell, 
#     set variables: inRoot,dataCategory,dataType,dataSource,epoch
#  output variables: list of variables
#
# e.g. /archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/NCPP/GFDL-HIRAM-C360/amip/day/atmos/day/r1i1p1/v20110601/uas/US48/OneD/
#      ^------------------------------------------------------------------^
#                                     indir
#                                                                           ^-^ ^---^ ^-^ ^----^ ^-------^ ^-^ ^--^ ^--^
# then search                                                                1    2    3    4      5        6   7    8
#   6 levels below to find variables subdirectories.
#   (NOTE: skip *Ensm and *Clim variables)
#
# then only keep variables that have a level 8 "dim" directory 
#

set c1Dir = `pwd`
if (-e $workDir/tmpvarlist) then
  \rm $workDir/tmpvarlist
endif

alias ff      'find . -name \!* -print '

set indir = $1
cd $indir
set varList = ()
set dirs=(*/*/*/*/*)

<<<<<<< HEAD
echo " "
echo "    (Ignore 'ls: No match.' messages below.)"
echo " "
=======
#echo " "
#echo "    (Ignore 'ls: No match.' messages below.)"
#echo " "
>>>>>>> g_bcqmtailadj

foreach dir ($dirs)
  cd $indir/$dir
  set tmp_varList = (`\ls `)
# echo "In $indir/$dir found:"
# echo "$tmp_varList"
  set varList = ($varList $tmp_varList)
end

cd $indir
foreach var ($varList)
<<<<<<< HEAD
# based on darkchocolate input file path structure, 
=======
# based on espresso input file path structure, 
>>>>>>> g_bcqmtailadj
# look for "dim" directories (e.g. OneD) to construct list of available variables
  set dimdirs = (`\ls -l */*/*/*/*/$var/* | awk '/^d/ {print $9}'`) 
  if ("$dimdirs" != "") then
    echo $var >> $workDir/tmpvarlist
  endif
end

set varList = (`sort -u $workDir/tmpvarlist`)
<<<<<<< HEAD
echo " "
echo -n "    FINAL LIST : "
echo "$varList"
echo " "
=======
#echo " "
#echo -n "    FINAL LIST : "
#echo "$varList"
#echo " "
>>>>>>> g_bcqmtailadj
\rm $workDir/tmpvarlist

cd $c1Dir
if ($#varList >= 0) then
<<<<<<< HEAD
  exit 0
else
  echo "No variables found. Exiting."
  exit 1
endif
=======
  foreach var ($varList)
     echo $var >> $workDir/varListFile
  end
  exit 0
else
#  echo "No variables found. Exiting."
  exit 1
endif

>>>>>>> g_bcqmtailadj
