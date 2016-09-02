#!/bin/csh -f
#
# Ginger XMLgen

  set flavor = ginger
  echo " "
  echo "   ----------------------------------"
  echo " "
  echo "   This is XMLgen for  $flavor FUDGE."
  echo " "
  echo "   ----------------------------------"
  echo " "

#source /usr/local/Modules/default/init/csh

  set cDir=`pwd`
  set dateIN = `date +%m.%d.%Y-%k.%M.%S`
  set dateMDY = `date +%m.%d.%Y`
  set owner="$USER"
#----------
# xmlRootDir = where XMLgen directory resides
#----------
  if ! $?BASEDIR then
     echo "  Environmental variable BASEDIR is not set. "
     echo "  BASEDIR must be set to the root directory location of XMLgen/XMLgen.csh"
     echo "  "
     echo "  Exiting."
     echo "  "
     exit 1
  endif
  set xmlRootDir = "${BASEDIR}"
  echo "  xmlRootDir = $xmlRootDir "
  echo "  "
# set xmlRootDir = "/home/esd/MJN_sandbox/$flavor"


#----------
# txtDir = where XMLgen editable text files reside, under ESD
#----------
# text files need to be updatable by the user.
# a static copy is made to the permanent repo when a FUDGE version is pushed out, but 
#   the user needs to be able to avail themselves of the most up-to-date listings which
#   are kept in /home/esd in $txtRootDir, editable by 'esd' only.

  set txtDir = "/home/esd/MJN_sandbox/$flavor/XMLgen"

# create unique temporary workDir  (regardless of whether running on PPAN or workstation, which is removed at end unless ctrl-c used)
  set workDir=/nbhome/$owner/work.XMLgen.$dateIN
#----------
#----------

# Location of XMLgen/XMLgen.csh and related scripts
  set xmlGenDir = ${xmlRootDir}/XMLgen

# Check if running this as "esd". This affects who owns the root directories that ExperGen will use to write scripts/code/downscaled output.
  echo "    You are running as user $owner  "
  echo " "

# whoami|grep -i esd
# if ($status != 0) then
#    echo "    Please run as user esd."
#    exit 1
# endif

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


  echo "    Please select which type of XML you wish to create : "
  set genOpt
  while ($genOpt != 1 && $genOpt != 2) 
   echo "    "
   echo " 1  Create SCCSC0p1 (used for 3^5/I35/Red River projects) or Perfect Model data XML"
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
