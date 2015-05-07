#!/bin/csh -f
source /usr/local/Modules/default/init/csh


  echo " "
  echo "=================================================================================== "
  echo " "
  echo "    Welcome to the FUDGE XML generator for creating downscaling experiment "
  echo "                XML file using Fudge version darkchocolate. "
  echo " "
  echo "==================================================================================== "
  echo " "
  echo " "

#============================================================
# ASSUMPTIONS
#============================================================

# set echo
# xmlGenDir is set in XMLgen.csh

# The directory where the experiment-specific scripts/ascii-files/code will be written once ExperGen executes.
  set scriptDir = "/home/$owner/PROJECTS/DOWNSCALING"
  if (! -e $scriptDir) then
    mkdir -p $scriptDir
    if ($status != 0) then
      echo "mkdir -p $scriptDir  failed. Exiting. "
      if ($owner == $USER) exit 1
    endif
  endif

# The directory where the downscaled netCDF files will be written.
  set dsFileDir = "/archive/$owner/PROJECTS/DOWNSCALING"

# These are files that will/may be used by this script
  set QueryVals = "$xmlGenDir/QueryVals.csh"
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

  echo "======================================================================================================= "
  echo " "
  echo "   This script assumes that downscaling will be performed with the following characteristics:"
  echo " "
  echo "    -  Scripts/files created by ExperGen will be written under $scriptDir ."
  echo "    -  Downscaled output will be written under $dsFileDir ."
  echo "    -  The Downscaling method you choose has its own file which prompts the user for options."
  echo "       (This file must be in $xmlGenDir and have the name of : "
  echo "           {DSMETHOD}-Options.csh where {DSMETHOD} = CDFt, BCQM or EDQM, for example.)"
  echo "    -  The target variable = the training variable."
  echo " "
  echo "======================================================================================================= "
  
  sleep 1
# ===========================
# INPUT VALUES 
# ===========================
 
# Input root directory
# ---------------
    echo " "
    echo " "
    echo ">>  Is the data input located somewhere other than in the ESD archive under /archive/esd/PROJECTS/DOWNSCALING?"
    echo -n "    Enter y for 'yes'; Enter n or hit the 'Enter' key for 'no' : "
    set ans=$<
    if ($ans == "") set ans = "n"
    echo $ans >> $INtxt
    echo "ans = $ans, Input Root other than /archive/esd/PROJECTS/DOWNSCALING, XMLgen.non-syn.csh" >> $INtxt.key
    if ($ans == "y") then
      echo " "
      echo "    Data will need to be in the proper directory structure, i.e. dataCategory/dataType/dataSource/... ,"
      echo "    under the root directory."
      echo -n ">>  Enter the input root directory. "
      set inRoot=$<
      echo $inRoot >> $INtxt
      echo "inRoot = $inRoot ,XMLgen.non-syn.csh" >> $INtxt.key
    else
      set inRoot = "/archive/esd/PROJECTS/DOWNSCALING"
    endif


#
# Input Data group
# ----------------
# prompt for Input Data group

  echo " "
  echo " "
  echo "======================================================================================================= "
  echo " "
  echo "    You need to pick which Datasets to use for the downscaling experiment."
  echo "    Currently we have 3 distinct resolutions of data available : "
  echo " "
  echo "    SCCSC0p1 data which is used for the Red River project. All data are"
  echo "      on a 0.1 x 0.1 grid. Observations available for historical target"
  echo "      is Livneh. GCMs include MPI-ESM-LR, CCSM4, MIROC5."
  echo " "
  echo "    PerfectModel data is the GFDL-HIRAM-C360 and GDFL-HIRAM-C360-COARSENED."
  echo "      'Historical Targets' are the amip r1i1p1 & r2i1p1 for 1979-2008."
  echo "      'Historical Predictors' are the coarsened regridded data for amip epoch."
  echo "      'Future Predictors' are for sst2090 epoch for years 2086-2095"
  echo " "
  echo "    VERY IMPORTANT"
  echo "    --------------"
  echo "    "
  echo "    Selected Historical Target, Historical Predictor input files and Training Mask MUST ALL have the SAME time range."
  echo "    "
  echo "    --------------"
  echo "    "
  echo " "

  echo " "
  echo "    ///////////////"
  echo "    ---------------"
  echo "    Dataset      "
  echo "    ---------------"
  echo "    \\\\\\\\\\\\\\\"
  echo " "
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

  sleep 2

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


  set varInfo = `grep -w $targVar $xmlGenDir/target_ID_table.txt`
     if ($status != 0) then
       echo " "
       echo "    $targVar has not been chosen before. A 2-3 character ID will need to be provided."
       echo " "
       echo "    These are the currently-defined variables, with IDs listed first."
       echo " "
       echo "    ID VariableName "
       cat $xmlGenDir/target_ID_table.txt
       echo " "
       echo " "
       echo -n ">>  Please enter 2-3 character ID for $targVar & we will add it to the variable table. Or ctrl-c to end : "
       set targID=$<
#       echo $targID >> $INtxt
#       echo "targID = $targID, XMLgen.non-syn.csh" >> $INtxt.key
       echo "$targID $targVar" >> $xmlGenDir/target_ID_table.txt
       echo "$targID $targVar     has been added to $xmlGenDir/target_ID_table.txt"
     else
       set targID = $varInfo[1]
     endif

  echo "    Target variable = $targVar" 
  echo "    Target ID       = $targID"
  sleep 2


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
     cat $xmlGenDir/project_ID_table.txt
     echo " "
     echo ">>  Enter a 2-3 character project ID, new or existing, for the Project "
     echo ">>  (e.g. PM for Perfect Model) "
     set projectID = $<
     echo "$projectID" >> $INtxt
     echo "projectID = $projectID, XMLgen.non-syn.csh" >> $INtxt.key
     set projInfo = `grep -w ^$projectID $xmlGenDir/project_ID_table.txt`
       if ($status != 0) then
         echo " "
         echo ">>  $projectID has not been chosen before. "
         echo ">>  "
         echo ">>  You need to supply a new project name. "
         echo ">>  The name can be multi-word, but you MUST use an underscore character, '_', "
         echo ">>  where you want a space in the project name, e.g. Red_River, Perfect_Model, 3_To_The_5th. (3^5 would be ok as is.). "
         echo ">>  "
         echo ">>  The underscore will be removed before writing metadata."
         echo "  "
         set done = "false"
         while ($done == "false")
           echo " "
           echo -n ">>  Please enter project name & it will be added along with projectID to the variable table. Or ctrl-c to end : "
           set project=$<
           if ($project != "") then
             set checkProj = `grep -w $project $xmlGenDir/project_ID_table.txt`
               if ($status != 0) then 
                 set done = "true"
#                 echo $project >> $INtxt
#                 echo "project = $project , XMLgen.non-syn.csh" >> $INtxt.key
                 echo "$projectID $project" >> $xmlGenDir/project_ID_table.txt
                 echo "$projectID $project     has been added to $xmlGenDir/project_ID_table.txt"
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
     echo "    You have picked the same dataset for the historical predictor"
     echo "    as for the future predictor. Is that what you intended?"
     echo -n "    Enter y for 'yes'; Enter n or hit the 'Enter' key for 'no' : "
    set ans=$<
    if ($ans == "") set ans = "n"
    echo $ans >> $INtxt
    echo "ans = $ans , same dataset for the historical predictor as for the future predictor, XMLgen.non-syn.csh" >> $INtxt.key
    if ($ans != "y") then
      echo " "
      echo "Try again. "
    else 
      unset pick
    endif
  else
    unset pick
  endif
  end

# set echo
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
    set spatMaskDir = "$spatMaskDir$dim"
    if (! -e $spatMaskDir) then
       echo "Since you are using dim = $dim, no spatial mask exists. Resetting spatial mask variables to 'na'." 
       set spatMaskDir = "na"
       set maskVar = "na"
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
  echo "    Reminder:  Target time window = $htDate1 to $htDate2"
  echo " "

# find all time series available
# until time pruning is added, have to use files that match the training period

  echo " "
  echo "    Available time period(s) for $hpDataSource :"
  echo "    (Target time window will be reset to selected window if possible.)"

  set hpDir = "$hpPath/$dim/"
  set hpPartA = "${targVar}_${hpFreq}_${hpDataSource}_${hpEpoch}_${hpRIP}_${region}"
  set hpPartB = "I${lons}_${file_j_range}.nc"

  source $findTS "$hpPartA" "$hpPartB" "$hpDir"
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
  source $findTS "$fpPartA" "$fpPartB" "$fpDir"
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
# if PM, datasetID = 1 for amip r1i1p1, 2 for amip r2i1p1, and epochID is 3 character "a" for amip, "s" for sst2090; then # of realization and # of physics
#  example C1s32 would be series.

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

   if ($dataGroup == "PerfectModel") then
# assumes historical data are always "amip"
     if ($htRIP == "r1i1p1") then 
        set epochID1 = "1"
     else
        set epochID1 = "2"
     endif
# assumes future predictor data can be any coarsened epoch/rip
     if ($fpEpoch == "amip") then
       set datasetID = "00"
# comment out kfold =2 because darkchocolate does not allow cross-validation, and non-zero kfold implies that in the code.
#      set kfold = "2"
#      set kfoldID = "K02"
       set kfold = "0"
       set kfoldID = "K00"
       if ($fpRIP == "r1i1p1") then 
          set epochID2 = "1"
       else
          set epochID2 = "2"
       endif
     else
# sst2090 p1 = esm... use datasetID '01'
# sst2090 p2 = cm3....use datasetID '02'
       set kfold = "0"
       set kfoldID = "K00"
       if ($fpRIP == "r1i1p1") then 
          set datasetID = "01"
          set epochID2 = "1"
       else if ($fpRIP == "r2i1p1") then
          set datasetID = "01"
          set epochID2 = "2"
       else if ($fpRIP == "r3i1p1") then
          set datasetID = "01"
          set epochID2 = "3"
       else if ($fpRIP == "r1i1p2") then 
          set datasetID = "02"
          set epochID2 = "1"
       else if ($fpRIP == "r2i1p2") then
          set datasetID = "02"
          set epochID2 = "2"
       else if ($fpRIP == "r3i1p2") then
          set datasetID = "02"
          set epochID2 = "3"
       endif
     endif 
     set epochID = "r${epochID1}${epochID2}"
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
  echo -n ">>  Enter the Letter of the Experiment Series: "
    set expID=$<
    echo $expID 
#   stty echo
    echo " "
    echo $expID >> $INtxt
    echo "expID = $expID , XMLgen.non-syn.csh" >> $INtxt.key
    echo "   >> "
    echo -n ">>  Enter an option string (i.e. {expOptionID}) to be added onto the Experiment Series (i.e. {expID}) name. (Just hit Return if no string is desired): "
    set expOptionID=$<
    echo $expOptionID
#   stty echo
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

  source $xmlGenDir/check_experiment_name.csh $expName

  set outDir = "$inRoot/$projectID/downscaled/${fpDataSource}/${fpEpoch}/${fpFreq}/${fpRealm}/${fpMisc}/${fpRIP}/${fpVs}/$expName/$targVar/$spatMaskID/$dim/"
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
cat > XMLfile <<EOF
<downscale>
    <!-- Created using $xmlGenDir/XMLgen.csh ; $dateMDY -->
    <!-- xmlGenDir = $xmlGenDir -->
    <!-- ifpreexist options: erase, move, exit (default for XML creation) -->

    <ifpreexist>$preExistOpt</ifpreexist>
    <input predictor_list = "$targVar" target = "$targVar" target_ID = "$targID" spat_mask = "$spatMaskDir" maskvar = "$maskVar" spat_mask_ID = "$spatMaskID" in_root = "$inRoot">
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
            </future_predictor>
        </training>
        <esdgen>
        </esdgen>
    </input>    
    <core>
        <method name="$dsMethod"> </method>
	<exper_series>$expLabel</exper_series> 
	<project>$project</project>
	<project_ID>$projectID</project_ID>
        <kfold>$kfold</kfold>
        <output>
            <out_dir>$outDir</out_dir>
            <script_root>$scriptDir</script_root>
        </output>
EOF

# ADD PR ADJUSTMENT XML
  if ("$targVar" == "pr" || "$targVar" == "prc") then
    echo "   Adding precip adjustment options to XML ... "
    source $prAdjustment
      if($status != 0) then
        echo "PROBLEM.  File $prAdjustment does not exist or has an issue to be checked."
        echo " "
        exit 1
      endif
    cat pr_XML >> XMLfile
    \rm pr_XML
  endif

  echo "    </core>" >> XMLfile
  echo "    <custom>" >> XMLfile

# ADD dsMethod options XML
  echo "   Adding $dsMethod options, if any,  to XML ... "
  source $DSMethod_options
    if($status != 0) then
      echo "PROBLEM.  File $DSMethod_options does not exist or has an issue to be checked."
      echo " "
      exit 1
    endif
   sleep 2

# END of CUMSTOM section
  echo "    </custom>" >> XMLfile

# post DS processing - QC adjustments
  echo "    <pp>" >> XMLfile
  source $qcAdjustment
  sleep 2
  cat QC_XML >> XMLfile
  \rm QC_XML
  echo "    </pp>" >> XMLfile


# Finalizing XMLfile 
cat >> XMLfile<< EOF
    <exp_check>$expName</exp_check>
</downscale>
EOF


  set xmlDir=$scriptDir/scripts/$projectID/XML
    if (! -e $xmlDir) mkdir -p $xmlDir
  set xmltxtDir=$scriptDir/scripts/$projectID/XMLtxt
    if (! -e $xmltxtDir) mkdir -p $xmltxtDir
  if (-e $xmlDir/$expName.xml) \mv $xmlDir/$expName.xml $xmlDir/$expName.xml.backup
  if (-e $xmltxtDir/$expName.input.txt) \mv $xmltxtDir/$expName.input.txt $xmltxtDir/$expName.input.txt.backup
  \mv XMLfile $xmlDir/$expName.xml
  \mv $INtxt $xmltxtDir/$expName.input.txt
  \mv $INtxt.key $xmltxtDir/$expName.input.txt.key
  echo " >>>  ......................................................................................."
  echo " >>>  ......................................................................................."
  echo " >>>  ......................................................................................."
  echo " >>>  "
  echo " >>>  Your XMLfile =  $xmlDir/$expName.xml " 
  chmod -xw $xmlDir/$expName.xml 
  echo " >>>  "
  echo " >>>  Keyboard inputs you entered are in $xmltxtDir/$expName.input.txt "
  echo " >>>  "
  echo " >>>  Inputs are also in $xmltxtDir/$expName.input.txt.key, with input variable names and the scripts where assigned."
  echo " >>>  "
  echo " >>>  ......................................................................................."
  echo " >>>  ......................................................................................."
  echo " >>>  ......................................................................................."
  $xmlGenDir/XMLchecker.py $xmlDir/$expName.xml
  set logfile = `\ls $expName.*log`
  \mv $expName.*log $xmltxtDir
  echo " "
  echo " *****                     PLEASE CHECK XMLchecker output file :                        ***** "
  echo " *****                                                                                  ***** "
  echo "        $xmltxtDir/$logfile "
  echo " *****                                                                                  ***** "
  echo " *****         It will indicate if there are any problems with your inputs.             ***** "
  echo " *****                                                                                  ***** "
  echo " *****     (This could easily happen if you edited an *input.txt file and used          ***** "
  echo " *****             that as input to XMLgen.csh to create a new XML,                     ***** "
  echo " *****                  e.g.  XMLgen.csh < myfile.input.txt                             ***** "
  echo " *****         which is NOT recommended unless you know what you are doing.)            ***** "
  echo " "
  echo " >>>  ......................................................................................."
  echo " >>>  ......................................................................................."
  echo " >>>  ......................................................................................."

  cd $cDir
  
  echo "Removing work directory $workDir"
  sleep 4
  \rm -rf $workDir
