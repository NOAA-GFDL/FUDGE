#!/bin/csh -f
#source /usr/local/Modules/default/init/csh

  echo " "
  echo "=================================================================================== "
  echo " "
  echo "    Welcome to the FUDGE $flavor XMLgen for creating downscaling experiment XML "
  echo " "
  echo "==================================================================================== "
  echo " "
  echo " "

#============================================================
# ASSUMPTIONS
#============================================================

# xmlGenDir is set in XMLgen.csh


# These are files that will/may be used by this script
  set QueryVals = "$xmlGenDir/QueryVals.csh"
  set QueryMultiVals = "$xmlGenDir/QueryMultiVals.csh"
  set findData = "$xmlGenDir/find_dataset_based_on_var.csh"
  set findDim = "$xmlGenDir/minifile_settings.csh"
  set findTS = "$xmlGenDir/find_timeseries.csh"
  set parseFile = "$xmlGenDir/parse_filename.csh"
  set SetGCMrips = $xmlGenDir/SetGCMrips.csh
  set prAdjustment = "$xmlGenDir/pr_adjustment.csh"
  set qcAdjustment = "$xmlGenDir/QC_adjustment.csh"
  set qcOptions_file = "$xmlGenDir/QC_Options.txt"

# platform ID for PPAN = p1
  set platformID = "p1"

# dataset groups available
  set dataGroups = (SCCSC0p1 PerfectModel)

#============================================================
# END ASSUMPTIONS
#============================================================

# initializing values
  set ignore0Delta = "off"
  set ignore0 = "off"
  set saveQcMask = "off"

  
  sleep 1
# ===========================
# INPUT VALUES 
# ===========================
 
# Input, Output & Experiment Home (aka "Script") root directories
# ---------------------------------------------------------------
    echo " "
    echo " "
    echo "    Input, Output & Experiment "Script" Home root directories "
    echo "    --------------------------------------------------------- "
    echo "   "
    echo "    For FUDGE production experiments, all input data and downscaled output data "
    echo "    are to be stored under a root directory:  /archive/esd/PROJECTS/DOWNSCALING "
    echo "    & the experiment Home "script" under root directory:  /home/$USER/PROJECTS/DOWNSCALING ."
    echo "   "
    echo "    However, a non-production experiment - for development or testing - may use input data from"
    echo "    and/or write downscaled output to user-specified root directories with standardized subdirectory "
    echo "    pathnames below which adhere to the ESD conventions."
    echo "    "
    echo ">>  Q: Do you may wish to read input data from or write the downscaled output to a directory other than"
    echo ">>     /archive/esd/PROJECTS/DOWNSCALING "
    echo ">>  or write experiment info & scripts to somewhere other than /home/$USER/PROJECTS/DOWNSCALING ?"
    echo "    "
    echo -n ">>  Enter y for 'yes'; Enter n or hit the 'Enter' key for 'no' : "
    set ans=$<
    if ($ans == "") set ans = "n"
    echo $ans >> $INtxt
    echo "ans = $ans, Input Root other than /archive/esd/PROJECTS/DOWNSCALING, XMLgen.non-syn.csh" >> $INtxt.key
    if ($ans == "y") then

# INPUT ROOT DIRECTORY
      echo " "
      echo "    Input data need to be in our standard directory structure, i.e. dataCategory/dataType/dataSource/... ,"
      echo "    under the input root directory. (See ESD Team webpage for more information on standard directory structure.)"
      echo "   "
      echo -n ">>  Enter the input root directory if not /archive/esd/PROJECTS/DOWNSCALING, otherwise just hit 'Enter' key. "
      set inRoot=$<
      if ($inRoot == "") then
        set inRoot = "/archive/esd/PROJECTS/DOWNSCALING"
      endif
      echo $inRoot >> $INtxt
      echo "inRoot = $inRoot ,XMLgen.non-syn.csh" >> $INtxt.key
      echo "    Input root directory is $inRoot "
      sleep 2

# OUTPUT ROOT DIRECTORY
      echo " "
      echo "    Downscaled data output will be written in standardized directory structure under the output root directory."
      echo "    For production runs, output root directory is to be /archive/esd/PROJECTS/DOWNSCALING ."
      echo "   "
      echo -n ">>  Enter the output root directory if not /archive/esd/PROJECTS/DOWNSCALING, otherwise just hit 'Enter' key. "
      set outRoot=$<
      if ($outRoot == "") then
        set outRoot = "/archive/esd/PROJECTS/DOWNSCALING"
      endif
      echo $outRoot >> $INtxt
      echo "outRoot = $outRoot ,XMLgen.non-syn.csh" >> $INtxt.key
      echo "    Output root directory is $outRoot "
      sleep 2

# Experiment Home Script ROOT DIRECTORY
      echo " "
      echo "    FUDGE experiment scripts, standard output, XML & experiment_info.txt files will be written "
      echo "    in standardized FUDGE directory structure under the scriptDir root directory."
      echo "    For production runs, experiment scriptDir root directory is to be /home/Oar.Gfdl.Esd/PROJECTS/DOWNSCALING ."
      echo "   "
      echo -n ">>  Enter the experiment scriptDir root directory if not /home/$USER/PROJECTS/DOWNSCALING, otherwise just hit 'Enter' key. "
      set scriptDir=$<
      if ($scriptDir == "") then
        set scriptDir = "/home/$owner/PROJECTS/DOWNSCALING"
      endif
      echo $scriptDir >> $INtxt
      echo "scriptDir = $outRoot ,XMLgen.non-syn.csh" >> $INtxt.key
      if (! -e $scriptDir) then
        mkdir -p $scriptDir
          if ($status != 0) then
             set platform = `gfdl_platform`
             echo "   "
             echo "  Can not create $scriptDir from this platform ($platform). Exiting."
             echo "   "
             exit 1
          endif
      endif
      echo "    Experiment home scriptDir root directory is $scriptDir "
      sleep 2
    else if ($ans == "n") then
      set inRoot = "/archive/esd/PROJECTS/DOWNSCALING"
      set outRoot = "/archive/esd/PROJECTS/DOWNSCALING"
      set scriptDir = "/home/$owner/PROJECTS/DOWNSCALING"
      if (! -e $scriptDir) then
        mkdir -p $scriptDir
          if ($status != 0) then
             set platform = `gfdl_platform`
             echo "   "
             echo "  Can not create $scriptDir from this platform ($platform). Exiting."
             echo "   "
             exit 1
          endif
      endif
    else
      echo "    "
      echo "    You entered  $ans . This is not a valid response. Exiting. "
      echo "    "
      exit 1
    endif

# The directory where the experiment-specific scripts/ascii-files/code will be written once ExperGen executes.
  if (! -e $scriptDir) then
    mkdir -p $scriptDir
    if ($status != 0) then
      echo "mkdir -p $scriptDir  failed. Exiting. "
      if ($owner == $USER) exit 1
    endif
  endif

  echo "======================================================================================================= "
  echo " "
  echo "   This script is set up such that downscaling will be performed with the following characteristics:"
  echo " "
  echo "    -  Scripts/files created by ExperGen will be written under $scriptDir ."
  echo " "
  echo "    -  Input netCDF files will be read from $inRoot ."
  echo " "
  echo "    -  Downscaled netCDF output files will be written to $outRoot ."
  echo " "
  echo "    -  The Downscaling method you choose has its own file in XMLgen which prompts the user for options."
  echo "       (This file must be in $xmlGenDir and have the name of : "
  echo "           {DSMETHOD}-Options.csh where {DSMETHOD} = CDFtmod, BCQM or EDQM, for example.)"
  echo " "
  echo "    -  The target variable = the training variable."
  echo " "
  echo "======================================================================================================= "

  sleep 4
#
# Input Data group
# ----------------
# prompt for Input Data group

  echo " "
  echo " "
  echo "======================================================================================================= "
  echo " "
  echo "    ///////////////"
  echo "    ---------------"
  echo "        Dataset      "
  echo "    ---------------"
  echo "    \\\\\\\\\\\\\\\"
  echo " "
  echo " "
  echo "    You need to pick which Datasets to use for the downscaling experiment."
  echo "    Currently we have 2 distinct resolutions of data available : "
  echo " "
  echo "    SCCSC0p1 data is used for the Red River & 3^5 project. All data are"
  echo "      on a 0.1 x 0.1 grid. Observation datasets available for 'Historical Target'"
  echo "      are Livneh, Daymet and PRISM for the periods 1961-2005 or 1981-2005. "
  echo "      GCMs available for 'Historical Predictors' and 'Future Predictors' are "
  echo "      MPI-ESM-LR, CCSM4, MIROC5. The future predictor time period covers 2006-2099."
  echo " "
  echo "    PerfectModel data are all GCM datasets, either hi-res GFDL-HIRAM-C360 for the 'Historical Target'"
  echo "      and coarsened then regridded versions known as the GFDL-HIRAM-C360-COARSENED for the "
  echo "      'Historical Predictors' and 'Future Predictors'"
  echo "      'Historical Targets' are the amip r1i1p1 & r2i1p1 for 1979-2008."
  echo "      'Historical Predictors' are the coarsened regridded data for amip epoch."
  echo "      'Future Predictors' are for sst2030 epoch for years 2026-2035 or sst2090 epoch for years 2086-2095 ."
  echo " "
  echo "    VERY IMPORTANT"
  echo "    --------------"
  echo "    "
  echo "    Selected Historical Target, Historical Predictor input files and Training Mask MUST ALL have the SAME time range."
  echo "    "
  echo "    --------------"
  echo "    "
  echo " "
  sleep 3

  set varvals = ($dataGroups)
  set dinfo = "Datasets to use as input for Downscaling "
  source $QueryVals
  set dataGroup = "$kval"
  echo "$dataGroup" >> $INtxt
  echo "dataGroup = $dataGroup, XMLgen.non-syn.csh" >> $INtxt.key

# set variables specific to the data group selected, and find variables available
# varList , historical target variables and historical predictor dataCategory & dataType defined here

  source $xmlGenDir/$dataGroup.csh
    if ($status == 1) exit 1

  set htTrange = "${htDate1}-$htDate2"
  echo "    Default Time range for $dataGroup = $htTrange "
  sleep 1

#
# TARGET VARIABLE
# ---------------
# prompt for target variable using varList

  echo " "
  echo "    ///////////////"
  echo "    ---------------"
  echo "    Target Variable"
  echo "    ---------------"
  echo "    \\\\\\\\\\\\\\\"
  echo " "
  set varvals = ($varList)
  set dinfo = "Available Target Variables"
  source $QueryVals
  set targVar = "$kval"
  echo "$targVar" >> $INtxt
  echo "targVar = $targVar, XMLgen.non-syn.csh" >> $INtxt.key


  set varInfo = `grep -w $targVar $txtDir/target_ID_table.txt`
     if ($status != 0) then

       echo "    $targVar has not been chosen before. A 2-3 character ID will need to be provided."
       echo " "
# make backup copy of $txtDir/target_ID_table.txt
       \cp $txtDir/target_ID_table.txt $txtDir/target_ID_table.txt.backup
            if ($status != 0) then
               echo ">> Problem. "
               echo ">> "
               echo ">> If you are not running under esd role account, you can not update the project table text file as needed."
               echo ">> Please either run under esd role account or contact the keeper of this XMLgen script."
               echo ">> "
               echo ">> Exiting."
               echo " "
               exit 1
            endif
       echo " "
       echo "    These are the currently-defined variables, with IDs listed first."
       echo " "
       echo "    ID VariableName "
       cat $txtDir/target_ID_table.txt
       echo " "
       echo " "
       echo -n ">>  Please enter 2-3 character ID for $targVar & we will add it to the variable table. Or ctrl-c to end : "
       set targID=$<
       echo "$targID $targVar" >> $txtDir/target_ID_table.txt
       echo " "
       echo "$targID $targVar     has been added to $txtDir/target_ID_table.txt"
       echo " "
     else
       set targID = $varInfo[1]
     endif

  echo "    Target variable = $targVar" 
  echo "    Target ID       = $targID"
  sleep 1

#  If target is pr or prc, query later for pr_adjustment
  set use_pr_adjustment = "False"
  if ($targVar == "pr" || $targVar == "prc") then
    set use_pr_adjustment = "True"
    set prVars = "$targVar"
  endif


#
# Project 
# -------
# prompt for PROJECT 
     echo " "
     echo "    ///////////////"
     echo "    ---------------"
     echo "    Project"
     echo "    ---------------"
     echo "    \\\\\\\\\\\\\\\"
     echo " "
     echo "    Currently defined projects (ID followed by the descriptive name) are:"
     echo " "
     cat $txtDir/project_ID_table.txt
     echo " "
     echo ">>  Enter a short project ID, new or existing, for the Project."
     echo ">>  (e.g. PM for Perfect Model) "
     set projectID = $<
     echo "$projectID" >> $INtxt
     echo "projectID = $projectID, XMLgen.non-syn.csh" >> $INtxt.key
     set projInfo = `grep -w ^$projectID $txtDir/project_ID_table.txt`
       if ($status != 0) then
            
         echo " "
         echo ">>  $projectID has not been chosen before. "
         echo ">>  "
         echo ">>  You need to supply a new project name. "
         echo ">>  "
         \cp $txtDir/project_ID_table.txt $txtDir/project_ID_table.txt.backup
            if ($status != 0) then
               echo ">> Problem. "
               echo ">> If you are not running under esd role account, you can not update the project table text file as needed."
               echo ">> Please either run under esd role account or contact the keeper of this XMLgen script."
               echo ">> "
               echo ">> Exiting."
               echo " "
               exit 1
            endif
         echo ">>  The name can be multi-word, but you MUST use an underscore character, '_', "
         echo ">>  where you want a space in the project name, e.g. Red_River, Perfect_Model, 3_To_The_5th. (3^5 would be ok as is.). "
         echo ">>  "
         echo ">>  The underscore will be removed before writing metadata."
         echo ">>  "
         set done = "false"
         while ($done == "false")
           echo " "
           echo -n ">>  Please enter project name & it will be added along with projectID to the variable table. Or ctrl-c to end : "
           set project=$<
           if ($project != "") then
             set checkProj = `grep -w $project $txtDir/project_ID_table.txt`
               if ($status != 0) then 
                 set done = "true"
#                echo $project >> $INtxt
#                echo "project = $project , XMLgen.non-syn.csh" >> $INtxt.key
                 echo "$projectID $project" >> $txtDir/project_ID_table.txt
                 echo "$projectID $project     has been added to $txtDir/project_ID_table.txt"
               else 
                 if($checkProj[1] != $projectID) then
                   echo "    Problem. $project is already in use. $checkProj" 
                 endif
               endif
            endif
         end
       else
         set project = $projInfo[2]
         if ($projectID != "") then
           echo "    Project exists."
         else if ($projectID == "RR") then
           echo "$projectID is ONLY for Red River running Cinnamon."
           echo "Please use alternate project ID, e.g. RR2"
           echo "Exiting."
           exit 1
         else
           echo "    Ooops. Exiting."
           exit 1
         endif
       endif
    set prj = `echo $project | sed 's/_/ /g'`
    set project = ($prj)
    echo " "
    echo "    Project    = $project" 
    echo "    Project ID = $projectID"
    echo " "
    sleep 1

# Downscaling Method 
# ---------------
# find available methods by scanning for options files in $xmlGenDir
#
  echo " "
  echo "    //////////////////"
  echo "    ------------------"
  echo "    Downscaling Method"
  echo "    ------------------"
  echo "    \\\\\\\\\\\\\\\\\\"
  echo " "
  set dsMethodList = ()
  cd $xmlGenDir
  foreach f (*-Options.csh)
    set method = `echo "$f" | cut -d '-' -f 1`
    set dsMethodList = ($dsMethodList $method)
  end
# echo "Available downscaling methods = $dsMethodList"

# prompt for downscaling method to use
  set varvals = ($dsMethodList)
  set dinfo = "Downscaling method"
  source $QueryVals
  set dsMethod = "$kval"
  set DSMethod_options =  $xmlGenDir/${dsMethod}-Options.csh
  echo "$dsMethod" >> $INtxt
  echo "dsMethod = $dsMethod , XMLgen.non-syn.csh" >> $INtxt.key
# check if multiPredictor method
  set multiPredictor = "False"
  foreach meth (`cat $xmlGenDir/MultiPredictor_table.txt`)
    if ("$dsMethod" == "$meth") then
       set multiPredictor = "True"
    endif
  end

set predVars = ()
if ($multiPredictor == "True") then
# make backup of full variable list
  \cp $workDir/varListFile $workDir/varListFile.backup

  echo " "
  echo "   Multiple predictors may be used for downscaling method, $dsMethod ."
  echo " "

# Do not assume target variable is a predictor. Ask.

  echo "    Is the target variable, $targVar, a predictor also?"
  echo -n "    Enter n for 'no' if not. Hit Enter or y or 'yes' to include target variable in the predictor list: "
  set ans=$<
  echo "$ans" >> $INtxt
  echo "    Is target also a predictor? answer = $ans , XMLgen.non-syn.csh" >> $INtxt.key
  if ($ans != "n" && $ans != "no") then
    set predVars = ($predVars $targVar)  
        
# remove targVar from varListFile
    grep -v -w "$predVars" $workDir/varListFile > $workDir/tmpvarListFile
    \mv $workDir/tmpvarListFile $workDir/varListFile

# ask if any additional predictors are desired
    echo "    Are any additional predictor variables desired?"
    echo -n "    Enter n for 'no' if not. Hit Enter or y for 'yes': "
    set ans=$<
    echo "$ans" >> $INtxt
    echo "    Additional predictor variables desired? answer = $ans , XMLgen.non-syn.csh" >> $INtxt.key
    if ($ans != "n") then
      set selectVar = "True"
    else
      set selectVar = "False"
    endif
  else
    if ($use_pr_adjustment == "True") then
       set use_pr_adjustment = "False"
       echo " "
       echo "    Even though the target variable is $targVar, you chose not to use $targVar as a predictor."
       echo "    Precip adjustment is not an option. "
       echo "    (Precip adjustment is only an option if 'pr' or 'prc' are both the target and a predictor.)"
       echo " "
       echo "    If you desire precip adjustment anyway, modifications to FUDGE will need to be made."
       echo " "
    endif
    set predVars = ()
    set selectVar = "True"
  endif

  set varvals = (`cat $workDir/varListFile`)
  set dinfo = "Available Variables"
  while ($selectVar == "True") 
    source $QueryMultiVals
    set pickedVars = "$kval"
    echo "$pickedVars" >> $INtxt
    echo "pickedVars = $pickedVars, XMLgen.non-syn.csh" >> $INtxt.key
    echo " "
    echo " "
    echo "    Predictor Variables selected:  $predVars $pickedVars  "

# for each variable selected (multiple variables allowed in this case)
    foreach pickedVar ($pickedVars)
      set predVars = ($predVars $pickedVar)
      if ($pickedVar =~ pr*) then
        set use_pr_adjustment = True
        set prVars = "$prVars,$pickedVar"
      endif
# remove pickedVar from varListFile
      grep -v -w "$pickedVar" $workDir/varListFile > $workDir/tmpvarListFile
      \mv $workDir/tmpvarListFile $workDir/varListFile
    end

    echo " "
    echo "    Enter n for 'no' if done."
    echo -n "    To select another variable, Hit Enter or y for 'yes' : "
    set ans=$<
    echo "$ans" >> $INtxt
    echo "ans = $ans , add more predictor variables, XMLgen.non-syn.csh" >> $INtxt.key
    if ($ans == "n" || $ans == "no") then
       set selectVar = "False"
    endif
  end
# restore full variable list to varListFile
  \cp $workDir/varListFile.backup $workDir/varListFile
else
  set predVars = "$targVar"
endif
set pVars = `echo $predVars | sed 's/ /,/g'`
set predVars = "$pVars"

#
# Historical Target
# --------------------
# 
# For SCCSC0p1, historical target is from an observation dataset, and that is 
#   queried for from within SCCSC0p1.csh.
# For PerfectModel, select is made from existing datasets, with
#   narrowing-down done by defining:
#   htDataCategory, htDataType, htDataSource, htEpoch 
#   in $dataGroup.csh. epochList is also defined $dataGroup.csh for non-synthetic.

  set excludedData = ""
  if ($dataGroup != "SCCSC0p1") then
    echo "    "
    echo "    ///////////////////////////////"
    echo "    ------------------------------"
    echo "    Historical TARGET Dataset"
    echo "    ------------------------------"
    echo "    \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"
    echo "    "
    set findVar = $targVar
    set findCat = $htDataCategory
    set findType = $htDataType
    set findSource = $htDataSource
    set findEpoch = $htEpoch
    set findRIP = ""
    set allowEns = "False"
    source $findData
      if ($status != 0) exit 1
  
    set htEpoch =  $epoch
    set htFreq = $freq
    set htRealm = $realm
    set htMisc = $misc
    set htRIP = $rip
    set htVs = $dataVersion
    set htDataSet = $dataSet
  endif

# 
#
# Historical Predictor
# --------------------
# 
# hpDataCategory, hpDataType, hpDataSource, hpEpoch defined in $dataGroup.csh
# epochList defined $dataGroup.csh
# 

  if ($dataGroup == "SCCSC0p1") then
    echo "    "
    echo "    Check for preferred RIPS to use for $dataGroup GCM data...."
    source $SetGCMrips $hpDataSource $hpEpoch
    echo "    "
    set prefRIP = $myRip
  else
    set prefRIP = "$htRIP"
  endif
  echo " "
  echo "    /////////////////////////////////"
  echo "    ---------------------------------"
  echo "    Historical PREDICTOR Dataset"
  echo "    ---------------------------------"
  echo "    \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"
  echo "  "
  set findVar = $targVar 
  set findCat = $hpDataCategory 
  set findType = $hpDataType 
  set findSource = $hpDataSource 
  set findRIP = $prefRIP
  set findEpoch = ""
  set allowEns = "False"
  if ($?epochList) then
     set findEpoch = $epochList[1]
  endif
  source $findData 
      if ($status != 0) exit 1

  set hpEpoch = $epoch
  set hpFreq = $freq
  set hpRealm = $realm
  set hpMisc = $misc
  set hpRIP = $rip
  set hpVs = $dataVersion
  set hpDataSet = $dataSet

#
# Future Predictor
# --------------------
#  same DataCategory DataType DataSource as Historical Predictor

  set pick
  while ($?pick)
  echo " "
  echo "    /////////////////////////////"
  echo "    -----------------------------"
  echo "    Pick FUTURE PREDICTOR Dataset"
  echo "    -----------------------------"
  echo "    \\\\\\\\\\\\\\\\\\\\\\\\\\\\\"
  echo " "
  echo "    Note: can use historical dataset for 'future', for cross-validation type experiment."
  echo " "
  set fpDataCategory = $hpDataCategory 
  set fpDataType = $hpDataType 
  set fpDataSource = $hpDataSource 
  set prefRIP = ""
  set findVar = $targVar 
  set findCat = $fpDataCategory 
  set findType = $fpDataType 
  set findSource = $fpDataSource 
  set findRIP = $prefRIP
  set findEpoch = ""
  set allowEns = "True"
# set allowEns = "False"

  source $findData
      if ($status != 0) exit 1

  set fpEpoch = $epoch
  set fpFreq = $freq
  set fpRealm = $realm
  set fpMisc = $misc
  set fpRIP = $rip
  set fpVs = $dataVersion
  set fpDataSet = $dataSet
  
  if ($fpEpoch == $hpEpoch && $fpRIP == $hpRIP) then
#    echo "    You have picked the same dataset for the historical predictor"
#    echo "    as for the future predictor. Is that what you intended?"
#    echo -n "    Enter y for 'yes'; Hit Enter or n for 'no' : "
#   set ans=$<
#   echo "$ans" >> $INtxt
#   if ($ans == "") set ans = "n"
#   echo $ans >> $INtxt
#   echo "ans = $ans , same dataset for the historical predictor as for the future predictor, XMLgen.non-syn.csh" >> $INtxt.key
#   if ($ans != "y") then
#     echo " "
#     echo "Try again. "
#   else 
      unset pick
      if ($dataGroup == "SCCSC0p1") then 
        set FutTSeqHistTS
      endif
#   endif
  else
    unset pick
  endif
  end

# full paths to the 2d input files (up one level from the minifile directories)

  set htPath = "$inRoot/$htDataCategory/$htDataType/$htDataSource/$htEpoch/$htFreq/$htRealm/$htMisc/$htRIP/$htVs/$targVar/$region"
  set hpPath = "$inRoot/$hpDataCategory/$hpDataType/$hpDataSource/$hpEpoch/$hpFreq/$hpRealm/$hpMisc/$hpRIP/$hpVs/$targVar/$region"
  set fpPath = "$inRoot/$fpDataCategory/$fpDataType/$fpDataSource/$fpEpoch/$fpFreq/$fpRealm/$fpMisc/$fpRIP/$fpVs/$targVar/$region"
  
# check for other minifile dimensions and enable user to change dim,lons,lone,lats,late,file_j_range if so

  source $findDim $fpPath
    if ($status != 0) exit 1

# add dim to spatial mask directory if it's not set to "na" (none)
# if mask directory doesn't exist for $dim, however, set spatial mask variables to 'na'.

  if ($spatMaskDir != "na") then
    set spatMaskDir = "$spatMaskDir$dim/"
    if (! -e $spatMaskDir) then
       echo "Checking for spatial mask directory $spatMaskDir$dim/ . Does not exist."
       echo "Since you are using dim = $dim, no spatial mask exists. Resetting spatial mask variables to 'na'." 
       set spatMaskDir = "na"
       set maskVar = "na"
       exit 1
    endif
  endif

#
# -------------------
# set Training Window
# -------------------


  echo " "
  echo "    /////////////////////////"
  echo "    -------------------------"
  echo "    TRAINING TIME WINDOW"
  echo "    -------------------------"
  echo "    \\\\\\\\\\\\\\\\\\\\\\\\\"
  echo " "
  echo "    Reminder:  Target time window = $htTrange"
  echo " "

# find all time series available
# until time pruning is added, have to use files that match the target period

  echo " "
  echo "    Available training masks for $hpDataSource :"
  echo "    (Target time window has to match training window.)"

  set hpDir = "$hpPath/$dim/"
  set hpPartA = "${targVar}_${hpFreq}_${hpDataSource}_${hpEpoch}_${hpRIP}_${region}"
  set hpPartB = "I${lons}_${file_j_range}.nc"

  set filePartA = "$hpPartA"
  set filePartB = "$hpPartB"
  set fileDir = "$hpDir"
  set fileTrangeIn = "$htTrange"
  source $findTS 
     if ($status == 1) exit 1
 
  if ($tsDate1 >= $htDate1) then 
     set htFileDate1 = $tsDate1
  else
     set htFileDate1 = $htDate1
  endif
  if ($tsDate2 <= $htDate2) then 
     set htFileDate2 = $tsDate2
  else
     set htFileDate2 = $htDate2
  endif
 
# syncronize the historical target and historical predictor time range
  set hpFileDate1 = $htFileDate1
  set hpFileDate2 = $htFileDate2
  set tsDate1 = $htFileDate1
  set tsDate2 = $htFileDate2
  echo "  "
  echo "     The SAME time range must be used for Historical Target, Historical Predictor input files and Training Mask."
  echo "     The common time range to be used ${htFileDate1}-${htFileDate2}."
  echo "  "

# check if minifiles for the time period selected exist for Historical Target and Predictor 
  set nowDir = `pwd`
  cd $htPath/*/
  set htFile = "${targVar}_${htFreq}_${htDataSource}_${htEpoch}_${htRIP}_${region}_${htFileDate1}-${htFileDate2}.${hpPartB}"
  if (! -e $htFile) then
    pwd
    echo "    $htFile does not exist. Exiting."
    echo "    ( Possible reason: "
    echo "      Did you select the SAME time range for Historical Target, Historical Predictor input files and Training Mask? )"
    exit 1
  endif
  cd $nowDir 

  echo " "
  echo "    /////////////////////////"
  echo "    -------------------------"
  echo "    TRAINING TIME WINDOW MASK"
  echo "    -------------------------"
  echo "    \\\\\\\\\\\\\\\\\\\\\\\\\"
  echo " "
  set windowDate1 = $tsDate1
  set windowDate2 = $tsDate2
  
  source $xmlGenDir/time_window.csh
  set trainTimeWindowFile = "$TimeWindowFile"

# ----------------------------------------------
# "Future" (i.e.Independent sample) TIME WINDOW
# ----------------------------------------------
  echo " "
  echo "    ////////////////////////////"
  echo "    ----------------------------"
  echo "    FUTURE PREDICTOR TIME SERIES"
  echo "    ----------------------------"
  echo "    \\\\\\\\\\\\\\\\\\\\\\\\\\\\"
  echo " "
  set fpDir = "$fpPath/$dim/"
  set fpPartA = "${targVar}_${fpFreq}_${fpDataSource}_${fpEpoch}_${fpRIP}_${region}"
  set fpPartB = "I${lons}_${file_j_range}.nc"

  echo "    Available time period(s) for Future $fpDir :"
  set filePartA = "$fpPartA"
  set filePartB = "$fpPartB"
  set fileDir = "$fpDir"
  if ($?FutTSeqHistTS) then
  set fileTrangeIn = "$htTrange"
  else
  set fileTrangeIn = ""
  endif
  source $findTS
     if ($status == 1) exit 1

  set fpFileDate1 = $tsDate1
  set fpFileDate2 = $tsDate2

  echo " "
  echo "    /////////////////////////////////"
  echo "    ---------------------------------"
  echo "    FUTURE PREDICTOR TIME WINDOW MASK"
  echo "    ---------------------------------"
  echo "    \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"
  echo " "
  set windowDate1 = $tsDate1
  set windowDate2 = $tsDate2
  source $xmlGenDir/time_window.csh

  set futureTimeWindowFile = "$TimeWindowFile"
  set futureTimeTrimFile = "$TimeTrimFile"

  echo -n "   Training Time Window = "
  if ("$trainTimeWindowFile" != "na") then
    \ls -k "$trainTimeWindowFile"
  else
    echo "$trainTimeWindowFile"
  endif
  echo -n "   Future Time Window = "
  if ("$futureTimeWindowFile" != "na") then
    \ls -k "$futureTimeWindowFile"
  else
    echo "$futureTimeWindowFile"
  endif
  echo " "

#
# Experiment Name Construction
# -----------------------------
# dataset-group-specific settings for datasetID and epochID used in Experiment Name construction
# set Experiment "datasetID" and "epochID" differently depending on dataGroup
# if SCCSC0p1, datasetID = 1 for CCSM4, 2 for MPI, 3 for MIROC5 and epochID depends on if future predictor is hist (0), rcp26(2), rcp45(4) or rcp85(8)
# (SCCSC0p1 GCM datasetID set in LivnehInfo.csh - checked when determined which Livneh calendar to use.)
# if PM, datasetID = 00 for amip, 01 = sst2090 physics 1; 02 = sst2090 physics 2; 03 = sst2030 physics 1; 04 = sst2030 physics 2
#

  set fpSuffix = ""
  set outRIP = $fpRIP

  if ($dataGroup == "SCCSC0p1") then
     if ($fpEpoch == "historical") then
         set epochID = "0"
         set fpFileDate1 = $hpFileDate1
         set fpFileDate2 = $hpFileDate2
      else if ($fpEpoch == "rcp26") then
         set epochID = "2"
      else if ($fpEpoch == "rcp45") then
         set epochID = "4"
      else if ($fpEpoch == "rcp85") then
         set epochID = "8"
      endif
   endif

# no cross-validation possible yet
   set kfold = "0"
   set kfoldID = "K00"

# assumes RIPs in form rKi1pM for single-realization files where K=integer realization number and M=physics
#  or for ensembles, RIP = rK1toK2i1pM or rK1-K2-K3i1pM 
#  where K1, K2, Kn =integer realizations, "K1toK2" is a range of realizations from K1 to K2 while
#    hyphen-separated list shows only the realizations included.

# get the historical amip physics number
     set hphysics = `echo $hpRIP|awk '{ print substr(  $0, length($0), length($0) ) }'`

# assumes historical data are always "amip", one realization, either r1i1p1 or r2i1p1
   if ($dataGroup == "PerfectModel") then
     if ($hphysics != 1) then
       echo "PROBLEM. amip RIP = $hpRIP, and physics should be = 1"
       exit 1
     endif
     set histRealz=`echo $hpRIP|awk '{ print substr( $0, 2, 1) }'`
   endif

# decode the RIP to check for ensemble and set variables as needed
   set decodeRIP = "$fpRIP"
   source $xmlGenDir/decodeRIP.csh

# reassign values assigned in decodeRIP.csh
   set fpPhysics = "$physics"
   set fpSuffix = "$suffix"
   set fpPropName = "$propertyName"
   set fpPropValue = "$propertyValue"
 
# --------------------------
# put this in decodeRIP.csh
# more generic. 
# --------------------------
# assumes future predictor data can be any epoch/rip
# get the future physics number
#  set fpPhysics = `echo $fpRIP|awk '{ print substr(  $0, length($0), length($0) ) }'`
#
# separate the realization information from the RIP 
#  set Realz=`echo $fpRIP|awk '{ print substr(  $0, 1, length($0) - 4 ) }'`
#
# check for ensemble or individual realization
#   if (("$Realz" =~ *to*) || ("$Realz" =~ *-*)) then
#      set fpSuffix = "_ensmbl"
#      set fpPropName = "RIP"
#      set outRIP = '$(RIP)'
#      if ("$Realz" =~ *to*) then
#        set ensBeg=`echo $Realz|awk '{ print substr(  $0, 2, 1) }'`
#        set ensEnd=`echo $Realz|awk '{ print substr(  $0, 5, 1) }'`
#        @ nEnsMembers = $ensEnd - $ensBeg + 1
#        set futRealz = "e$nEnsMembers"
#        set k = $ensBeg 
#        set fpPropValue = "r${k}i1p$fpPhysics"
#        while ($k < $ensEnd) 
#          @ k++
#          set fpPropValue = "$fpPropValue,r${k}i1p$fpPhysics"
#        end
#      else if ("$Realz" =~ *-*) then
#        set kmems = `echo $Realz | sed -e 's/-/ /g' -e 's/r/ /g'`
#        set fpPropValue = "r${kmems[1]}i1p$fpPhysics"
#        foreach k ($kmems[2-])
#          set fpPropValue = "$fpPropValue,r${k}i1p$fpPhysics"
#        end
#      endif
#   else
#      set ensBeg=`echo $Realz|awk '{ print substr(  $0, 2, 1) }'`
#      set ensEnd=$ensBeg
#      set futRealz = "$ensBeg"
#   endif
#

# downscaled output experiment name, i.e. A01r13, for Perfect Model indicates 
#   which experiment series (A), epoch (00 = amip, 01 = sst2090 esm, 02 = sst2090 cm3),
#   which realizations (r) were used for training (historical realization 1 and future predictory realization 3)
#   If future predictor is an ensemble, use "eN" were N = # members.
#   So A01r1e3 would be training on amip r1i1p1 vs sst2090 esm ensemble containing 3 members
#
   if ($dataGroup == "PerfectModel") then
     if ($fpEpoch == "amip") then
       set dataID = 0
     else if ($fpEpoch == "sst2090") then
       set dataID = $fpPhysics
     else if ($fpEpoch == "sst2030") then
       @ dataID = $fpPhysics + 2
     endif
     set datasetID = "0$dataID"
     set epochID = "r${histRealz}${futRealz}"
   endif 
 
# Experiment Series ID
# --------------------
  echo " "
  echo "    Unique User-Input Experiment Name Information"
  echo "    ---------------------------------------------"
  echo " "
  echo "   >>  Experiment names are of the form: "
  echo "   >> "
  echo "   >>         {projectID}{varID}{platformID}-{DSmethod}-{expID}{expOptionID}{targetID}{kfold}"
  echo "   >> "
  echo "   >>  The user must set the {expID}, which is usually a letter identifying the Experiment Series."
  echo "   >> "
  echo -n ">>  Enter the Experiment Series (usually one capital letter or User's initials for test cases): "
  set expID=$<
  echo $expID 
  echo " "
  echo $expID >> $INtxt
  echo "expID = $expID , XMLgen.non-syn.csh" >> $INtxt.key
  echo "   >> "
  echo -n ">>  Enter an option string (i.e. {expOptionID}) to be added onto the Experiment Series (i.e. {expID}) name. (Just hit Return if no string is desired): "
  set expOptionID=$<
  echo $expOptionID
  echo " "
  if ($expOptionID == "") then
    echo " " >> $INtxt
  else
    echo $expOptionID >> $INtxt
  endif
  echo "expOptionID = $expOptionID , XMLgen.non-syn.csh" >> $INtxt.key

#   -----------------------------------------------
#   CREATE EXPERIMENT NAME BASED ON INPUTS THUS FAR
#   -----------------------------------------------
  echo "--------------------------------------------------------------------------------------------"
  echo " "
  echo "   CREATE EXPERIMENT NAME BASED ON INPUTS THUS FAR"

  set expHashtag = "${expID}${datasetID}${epochID}${expOptionID}"
  set expLabel = "$expHashtag$htID"
  echo " "
  echo "--------------------------------------------------------------------------------------------"
  echo "............................................................................................."
  echo "............................................................................................."
  echo " "
  echo "   Based on your inputs:"
  echo " "
  echo "   ${expLabel}  <= Experiment Label "
  echo " "
  set expName = "${projectID}${targID}${platformID}-${dsMethod}-${expHashtag}${htID}${kfoldID}"
  echo "   ${projectID}${targID}${platformID}-${dsMethod}-${expLabel}${kfoldID} <= Experiment Name"
  echo " "

  set quitjob = 0
  source $xmlGenDir/check_experiment_name.csh $expName
  if ($quitjob == 1) then
    echo "Exiting XMLgen."
    exit 1
  endif
#
  cd $cDir
  echo " "
  echo "--------------------------------------------------------------------------------------------"
  echo "--------------------------------------------------------------------------------------------"
  echo "............................................................................................."
  echo "............................................................................................."
  echo "                              Creating XML     "
  echo "............................................................................................."
  echo "............................................................................................."
  echo "--------------------------------------------------------------------------------------------"
  echo "--------------------------------------------------------------------------------------------"
  echo " "
cat > $workDir/XMLfile <<EOF
<downscale>
    <!-- Created using $xmlGenDir/XMLgen.csh ; $dateMDY -->
    <!-- xmlGenDir = $xmlGenDir -->
    <!-- FUDGE flavor = ginger -->
    <!-- ifpreexist options: erase, move, exit (default for XML creation) -->

    <ifpreexist>$preExistOpt</ifpreexist>
    <input predictor_list = "$predVars" target = "$targVar" target_ID = "$targID" spat_mask = "$spatMaskDir" maskvar = "$maskVar" spat_mask_ID = "$spatMaskID" in_root = "$inRoot">
        <dim>$dim</dim>
        <grid region = "$region">
            <lons>$lons</lons>
            <lone>$lone</lone>
	    <lats>$lats</lats>
	    <late>$late</late>	
            <file_j_range>"$file_j_range"</file_j_range>
        </grid>
        <training>
            <historical_predictor
                file_start_time = "$hpFileDate1"
                file_end_time = "$hpFileDate2"
                train_start_time = "$hpFileDate1"
                train_end_time = "$hpFileDate2"
 		time_window = '$trainTimeWindowFile'
            >
                <dataset>${hpDataCategory}.${hpDataType}.${hpDataSource}.${hpEpoch}.${hpFreq}.${hpRealm}.${hpMisc}.${hpRIP}.${hpVs}</dataset>
            </historical_predictor>
            <historical_target
                file_start_time = "$htFileDate1"
                file_end_time = "$htFileDate2"
                train_start_time = "$htFileDate1"
                train_end_time = "$htFileDate2"
		time_window = '$trainTimeWindowFile'
             >
                <dataset>${htDataCategory}.${htDataType}.${htDataSource}.${htEpoch}.${htFreq}.${htRealm}.${htMisc}.${htRIP}.${htVs}</dataset>
            </historical_target>
            <future_predictor
                file_start_time = "$fpFileDate1"
                file_end_time = "$fpFileDate2"
                train_start_time = "$fpFileDate1"
                train_end_time = "$fpFileDate2"
                time_window = '$futureTimeWindowFile'
                time_trim_mask = '$futureTimeTrimFile'
            >
                <dataset>${fpDataCategory}.${fpDataType}.${fpDataSource}.${fpEpoch}.${fpFreq}.${fpRealm}.${fpMisc}.${fpRIP}.${fpVs}</dataset>
EOF
if ("$fpSuffix" != "") then
   echo "                <suffix>$fpSuffix</suffix>" >> $workDir/XMLfile
endif
cat >> $workDir/XMLfile <<EOF
            </future_predictor>
        </training>
        <esdgen>
        </esdgen>
    </input>    
EOF

#=====================
# $workDir/core_XML
#=====================

cat >> $workDir/core_XML <<EOF
    <core>
        <method name="$dsMethod"> </method>
	<exper_series>$expLabel</exper_series> 
	<project>$project</project>
	<project_ID>$projectID</project_ID>
        <kfold>$kfold</kfold>
        <output>
EOF

if ($fpPropName != "") then
echo "            <property name="\"$fpPropName\" value=\"$fpPropValue\""/> " >> $workDir/core_XML
endif

cat >> $workDir/core_XML <<EOF
            <out_dir>$outRoot/$projectID/downscaled/${fpDataSource}/${fpEpoch}/${fpFreq}/${fpRealm}/${fpMisc}/${outRIP}/${fpVs}/$expName/$targVar/$spatMaskID/$dim/</out_dir>
            <script_root>$scriptDir</script_root>
        </output>
EOF

#
# pr adjustment XML part of $workDir/core_XML
#

  if ("$use_pr_adjustment" == "True") then
    echo "   "
    echo "   Adding precip adjustment options to XML ... "
    echo "   "
    echo "   (Reminder. Target = $targVar. Predictor variable(s) = $predVars )"
    echo "   "
    source $prAdjustment 
      if($status != 0) then
        echo "PROBLEM.  File $prAdjustment does not exist or has an issue to be checked."
        echo " "
        exit 1
      endif
  endif
   
#=====================
# $workDir/custom_XML SECTION 
#=====================
#
  echo "    <custom>" > $workDir/custom_XML

#
# dsMethod options 
#
  echo "   Adding $dsMethod options, if any,  to XML ... "
  source $DSMethod_options
    if($status != 0) then
      echo "PROBLEM.  File $DSMethod_options does not exist or has an issue to be checked."
      echo " "
      exit 1
    endif
  sleep 1

# echo "  "
# echo "  use_pr_adjustment = $use_pr_adjustment,  ignore0 =  $ignore0"
  echo "  "

  if ("$use_pr_adjustment" == "True" && "$ignore0" == "'on'" && !($dsMethod =~ "CDFt"*)) then
#
# minimum sample size 
#
    echo "======================="
    echo "  Minimum Sample Size  "
    echo "======================="
    echo ">>   Set a minimum sample size for which downscaling will be performed."
    echo ">>   Hit Return if minimum sample size = 30 is desired, "
    echo ">>     otherwise enter a value >= 0 and < your data's total size per time window. "
    echo -n ">>   If 0 is entered, minsamplesize will NOT be set in the XML. : "
    set minSample=$<
    echo $minSample
    echo " "
    if ($minSample == "") then
      set minSample = 30
    else
      @ vtest = $minSample
      if ($vtest << 0) then
        echo "   You set the minimum sample size = $minSample. Value needs to be >= 0." 
        echo "   Exiting."
        exit 1
      endif
    endif
    echo $minSample >> $INtxt
    if ($minSample != 0) then
      echo "minSample = $minSample , XMLgen.non-syn.csh" >> $INtxt.key
      echo "        <minsamplesize>$minSample</minsamplesize>" >> $workDir/custom_XML
    endif

  endif

# END of CUMSTOM section
  echo "    </custom>" >> $workDir/custom_XML

#=====================
# $workDir/pp_XML post-processing
#=====================
  echo "    <pp>" >> $workDir/pp_XML

#
# QC adjustments

# if pr_opts are being used, and ignore0 or ignore0.delta is on, then set qcmask on. No other qc options allowed.

  set used_qc_adjustment = False
  if ($use_pr_adjustment == True) then
    if ($ignore0Delta == "'on'" || ($ignore0 == "'on'" && $dsMethod != BCQM)) then
       echo "       <qc type="\'qc.train\'" qc_mask="\'on\'" adjust_out="\'off\'"></qc>" > $workDir/QC_XML
       set saveQcMask = "on"
       set used_qc_adjustment = True
       echo "  "
       echo "   Since ignore0 = $ignore0 and dsMethod != BCQM, FUDGE will output a mask flagging 'ignore0' points. No QC adjustments can be specified."
    endif
  endif

  if ($used_qc_adjustment == "False") then
    source $qcAdjustment
  endif

  if ($use_pr_adjustment == True && $used_qc_adjustment == False) then
    if ($ignore0 == "'on'") then
       echo "       <qc type="\'qc.train\'" qc_mask="\'on\'" adjust_out="\'off\'"></qc>" > $workDir/QC_XML
       set used_qc_adjustment = True
       echo "   Since ignore0 = $ignore0 and no other QC adjustments are specified, FUDGE will output a mask flagging 'ignore0' points."
    endif
  endif

  if ($used_qc_adjustment == "True") then
    cat $workDir/QC_XML >> $workDir/pp_XML
    \rm $workDir/QC_XML
  endif
    
  echo "    </pp>" >> $workDir/pp_XML

# =====================
# Special Case
# =====================
# Additional mod to $workDir/core_XML if flag_neg desired by another QC Adjustment has been made
#
# If QC Adjustments have been made and pr adjustments have been made, check if want flag_neg
#
  if ("$use_pr_adjustment" == "True") then
    if ("$used_qc_adjustment" == "True") then
      echo ">>  "
      echo ">>  Negative Downscaled Values Adjustment"
      echo ">>  "
      echo ">>  Do you want downscaled values < 0. set = 0 ? "
      echo ">>   (Because of previous settings, you can't set flag.neg adjustment directly "
      echo ">>    as a Section 5 QC adjustment. (NOTE: no mask will be created for this.) "
      echo -n ">> Type y or hit Enter now if set values < 0 equal to zero, otherwise type n. : "
      set ans=$<
      echo " "
      if ($ans == "") then 
        set ans = y
      endif
      echo $ans >> $INtxt
      echo "ans = $ans , turn on <flag_neg> in pr_XML, XMLgen.non-syn.csh" >> $INtxt.key
      echo " "
      echo " flag_neg on ? = $ans"
      echo " "
      if ($ans != n) then
        echo "           <flag_neg>'on'</flag_neg>" >> $workDir/pr_XML
      else
        echo "           <flag_neg>'off'</flag_neg>" >> $workDir/pr_XML
      endif
    endif
    echo "        </pr_opts>" >> $workDir/pr_XML
    cat $workDir/pr_XML >> $workDir/core_XML
    \rm $workDir/pr_XML
  endif

  echo "    </core>" >> $workDir/core_XML

# merge XML segments together
  cat $workDir/core_XML >> $workDir/XMLfile
  cat $workDir/custom_XML >> $workDir/XMLfile
  cat $workDir/pp_XML >> $workDir/XMLfile

# Finalizing $workDir/XMLfile 
cat >> $workDir/XMLfile<< EOF
    <exp_check>$expName</exp_check>
</downscale>
EOF

  echo " "
  echo " "
  echo " "
  echo ">>>  Finalizing $workDir/XMLfile "
  echo " "
  echo " "

  set xmlDir=$scriptDir/scripts/$projectID/XML
    if (! -e $xmlDir) then
       mkdir -p $xmlDir
         if ($status != 0) then
            echo "Problem. Can't create $xmlDir on $platform. Exiting."
            exit 1
         endif
    endif
  set xmltxtDir=$scriptDir/scripts/$projectID/XMLtxt
    if (! -e $xmltxtDir) then
       mkdir -p $xmltxtDir
         if ($status != 0) then
            echo "Problem. Can't create $xmltxtDir on $platform. Exiting."
            exit 1
         endif
    endif
  set datetime=`date +%m.%d.%Y-%k.%M.%S`
  if (-e $xmlDir/$expName.xml) \mv $xmlDir/$expName.xml $xmlDir/$expName.xml.$datetime
  if (-e $xmltxtDir/$expName.input.txt) \mv $xmltxtDir/$expName.input.txt $xmltxtDir/$expName.input.txt.$datetime
  \mv $workDir/XMLfile $xmlDir/$expName.xml
  \mv $INtxt $xmltxtDir/$expName.input.txt
  \mv $INtxt.key $xmltxtDir/$expName.input.txt.key
  echo ">>>  "
  echo ">>>  ......................................................................................."
  echo ">>>  ......................................................................................."
  echo ">>>  "
  echo ">>>  "
  echo ">>>  Your XML file =  $xmlDir/$expName.xml " 
  echo ">>>  "
  echo ">>>  "
  chmod -xw $xmlDir/$expName.xml 
  echo ">>>  Keyboard inputs you entered:  $xmltxtDir/$expName.input.txt "
  echo ">>>  Keyboard inputs with additional info:  $xmltxtDir/$expName.input.txt.key "
  echo ">>>  "
  echo ">>>  ......................................................................................."
  echo ">>>  ......................................................................................."
  echo ">>>  "
  echo ">>>  "
  $xmlGenDir/XMLchecker.py $xmlDir/$expName.xml
  set logfile = `\ls $expName.*log`
  \mv $expName.*log $xmltxtDir
  echo " "
  echo " ***** PLEASE CHECK XMLchecker output: $xmltxtDir/$logfile "
  echo " ***** "
  echo " ***** It will indicate if there are any problems with your inputs."
  echo " ***** (This could easily happen if you edited an *input.txt file and used "
  echo " *****  that as input to XMLgen.csh to create a new XML, e.g.  XMLgen.csh < myfile.input.txt "
  echo " *****  which is NOT recommended unless you know what you are doing.)"
  echo " "
  echo " "

  cd $cDir
  
  echo "Removing work directory $workDir"
  echo " "
  echo "Done."
  echo " "
  sleep 1
  \rm -rf $workDir
