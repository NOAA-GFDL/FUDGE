#!/bin/csh -f

  set curdir = `pwd`
  echo " "
  echo "    Default input file settings for $region data are:"
  echo " "
  echo "      Subdirectory from which input minifiles will be used = $dim"
  echo " "
  echo "      Starting longitude index (lons) = $lons"
  echo "      Ending longitude index   (lone) = $lone"
  echo "      Starting latitude index  (lats) = $lats"
  echo "      Ending latitude index    (late) = $late"
  echo "      Minifile suffix  (file_j_range) = $file_j_range"
  echo " "

  echo "    Checking for all minifile subdirectories (defined by 'dim' in XML).... "
  cd $1
  set dimList = ()
  set dims=`\ls -l  $hpPath | awk '/^d/ {print $9}'`
  if ($#dims == 0) then
     echo "+++ No input file dims (e.g. OneD,ZeroD) are available. Exiting."
     cd $curdir
     exit 1
  endif
  if ($#dims == 1) then
     echo "+++ dim = $dim is the only input file 'dim' directory available."
     cd $curdir
     exit 0
  endif
  echo "    Available 'dim' subdirectories found : "
  echo "    $dims"
  
  echo "    Do you want to select a new subdirectory ?"
  echo "    (NOTE: if so,  you will need to reset the longitude and latitude "
  echo "     starting and ending indices and the minifile 'file_j_range' suffix)"
  echo "    "
  echo -n ">>  Enter y for 'yes'; Enter n or hit the 'Enter' key for 'no' :"
  set opt=$<
  echo $opt


  if ($opt == "y") then
  echo ">>    Enter the subdirectory from which input minifiles will be used  (from $dims): "
  set dim=$<
  echo ">>    Enter Starting longitude index (lons) : "
  set lons=$<
  echo "      Ending longitude index   (lone) = $lone"
  set lone=$<
  echo "      Starting latitude index  (lats) = $lats"
  set lats=$<
  echo "      Ending latitude index    (late) = $late"
  set late=$<
  echo "      Minifile suffix  (file_j_range) = $file_j_range"
  set file_j_range=$<
  echo " "
  echo "      The experiment will now use the following settings: "
  echo " "
  echo "      Subdirectory from which input minifiles will be used = $dim"
  echo " "
  echo "      Starting longitude index (lons) = $lons"
  echo "      Ending longitude index   (lone) = $lone"
  echo "      Starting latitude index  (lats) = $lats"
  echo "      Ending latitude index    (late) = $late"
  echo "      Minifile suffix  (file_j_range) = $file_j_range"
  endif

  cd $curdir
  exit 0
