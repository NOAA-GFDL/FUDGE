#!/bin/csh -f

source /usr/local/Modules/default/init/csh

  set cDir=`pwd`
  set dateIN = `date +%m.%d.%Y-%k.%M.%S`
  set dateMDY = `date +%m.%d.%Y`
#----------
# xmlRootDir = where XMLgen directory resides
#----------
# set xmlRootDir = "${BASEDIR}"
  set xmlRootDir = "/home/esd/MJN_sandbox/darkchocolate"

# create unique workDir  (which is removed at end)
  set workDir=/nbhome/esd/work.XMLgen.$dateIN
#----------
#----------

# Check if running this as "esd". This affects who owns the root directories that ExperGen will use to write scripts/code/downscaled output.
  set owner="$USER"
  echo "    You are running as user $owner  "
  echo " "
  whoami|grep -i esd
  if ($status != 0) then
     echo "    Please run as user esd."
     exit 1
  endif

# set some basic variables that are used by this and child scripts

  if (-e  $workDir) then
    echo "Seriously?? Someone else is running this script at precisely the same moment. What are the odds!?"
    echo "Try again."
    exit 1
  else
    mkdir -p $workDir
  endif

  set INtxt = "$workDir/InputValues.$dateIN.txt"
  if (-e $INtxt) then
    \rm $INtxt
  endif

  cd $workDir

  set xmlGenDir = ${xmlRootDir}/XMLgen

  echo "    Please select which type of XML you wish to create : "
  set genOpt
  while ($genOpt != 1 && $genOpt != 2) 
   echo "    "
   echo " 1  Create SCCSC0p1 or Perfect Model data XML"
   echo " "
   echo " 2  Create Synthetic Data XML "
   echo " "
   echo "    Enter the number, or just hit Enter if you wish to exit."
   echo " "
   set genOpt=$<
   echo "$genOpt" > $INtxt
   echo "genOpt = $genOpt, XMLgen.csh" > $INtxt.key
   if ($genOpt == "") then
      echo "Exiting"
      exit 0
   endif
   if ($genOpt != 1 && $genOpt != 2) echo "You selected $genOpt , which is not valid."
  end
  
# use "source" so that variables set in parent script are passed along

  if ($genOpt == 1) source $xmlGenDir/XMLgen.non-syn.csh
  if ($genOpt == 2) source $xmlGenDir/XMLgen.syn.csh

  exit 0
