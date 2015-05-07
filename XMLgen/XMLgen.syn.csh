#!/bin/csh -f
source /usr/local/Modules/default/init/csh

  echo " "
  echo "====================================================================== "
  echo " "
  echo "    Welcome to the FUDGE XML generator for creating XML for "
  echo "            SYNTHETIC downscaling experiment "
  echo "            sing Fudge version darkchocolate. "
  echo " "
  echo "====================================================================== "
  echo " "
  echo " "



#============================================================
# ASSUMPTIONS
#============================================================

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
  set parsePath = "$xmlGenDir/parse_fullpath.csh"
  set SetGCMrips = $xmlGenDir/SetGCMrips.csh
  set prAdjustment = "$xmlGenDir/pr_adjustment.csh"
  set qcAdjustment = "$xmlGenDir/QC_adjustment.csh"
  set qcOptions_file = "$xmlGenDir/QC_Options.txt"

# platform ID for PPAN = p1
  set platformID = "p1"

# dataset groups available
  set dataGroup = (Synthetic)

# calendar = NONE
  set timemaskSuffix = ".NONE"

#============================================================
# END ASSUMPTIONS
#============================================================

  echo "======================================================================================================= "
  echo " "
  echo "   This script assumes that downscaling will be performed with the following characteristics:"
  echo " "
  echo "    -  SYNTHETIC data is being used."
  echo "    -  The User knows which data he/she wants to use."
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
    echo "ans = $ans , XMLgen.syn.csh" >> $INtxt.key
    if ($ans == "y") then
      echo " "
      echo "    Data will need to be in the proper directory structure, i.e. dataCategory/dataType/dataSource/... ,"
      echo "    under the root directory."
      echo -n ">>  Enter the input root directory. "
      set inRoot=$<
      echo $inRoot >> $INtxt
      echo "inRoot = $inRoot, XMLgen.syn.csh" >> $INtxt.key
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
  echo " "
  echo "    Synthetic data can be anything as long as it's stored under "
  echo "       subdirectory SYN_DATA under the input root directory."
  echo " "


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
     echo ">>  (e.g. S2 for Synthetic_test_2) "
     set projectID = $<
     echo "$projectID" >> $INtxt
     echo "projectID = $projectID , XMLgen.syn.csh" >> $INtxt.key
     set projInfo = `grep -w ^$projectID $xmlGenDir/project_ID_table.txt`
       if ($status != 0) then
         echo " "
         echo ">>  $projectID has not been chosen before. "
         echo ">>  "
         echo ">>  You need to supply a new project name. "
         echo ">>  The name can be multi-word, but you MUST use an underscore character, '_', "
         echo ">>  where you want a space in the project name, e.g. Synthetic_Test_2, 3_To_The_5th. (3^5 would be ok as is.). "
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
#                 echo "project = $project , XMLgen.syn.csh" >> $INtxt.key
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
  echo " "
  set varvals = ($dsMethodList)
  set dinfo = "Downscaling method"
  source $QueryVals
  set dsMethod = "$kval"
  set DSMethod_options =  $xmlGenDir/${dsMethod}-Options.csh
  echo "$dsMethod" >> $INtxt
  echo "dsMethod = $dsMethod , XMLgen.non-syn.csh" >> $INtxt.key


#
# Historical / "Obs" / "Oh" Target
# --------------------------------
# 

  echo "    "
  echo "    ///////////////////////////////////////////////////"
  echo "    ---------------------------------------------------"
  echo "    TARGET ('Historical') Dataset Full Path"
  echo "    ---------------------------------------------------"
  echo "    \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"
  echo "    "
  echo "    NOTE: Synthetic data for darkchocolate and beyond"
  echo "          likely uses calendar = "none" . "
  echo "          For Observations and Synthetic data, to distinguish"
  echo "          between various calendar-type data we chose to use "
  echo "          a specific 'RIP' for each specific calendar type:"
  echo "          For:"
  echo "               'none' calendar data  ==  r0i0p2"
  echo "               julian calendar data  ==  r0i0p0"
  echo "               noleap calendar data  ==  r0i0p1"
  echo "    "
  echo "    Root archive directory = $inRoot/SYN_DATA "
  echo "    "

  echo ">>  Search under $inRoot/SYN_DATA for the files you want to use."
  echo "    "
  echo ">>  RECOMMENDATION: in another window use 'find' command to look"
  echo ">>    for files with the calendar RIP (or any other string) of interest:"
  echo ">>  "
  echo ">>    e.g. to find Synthetic data w/ calendar = 'none' (i.e. RIP = r0i0p2) :"
  echo ">>  "
  echo ">>            cd $inRoot/SYN_DATA"
  echo ">>            find . -name \*r0i0p2\*nc -print"
  echo ">>  "
  echo ">>    Then copy and paste full pathnames starting with $inRoot/SYN_DATA "
  echo ">>    at prompts below (or copy/paste to file, then copy/paste to prompts below.)"
  echo ">>  "
  echo -n ">>  Enter the TARGET (Obs/Oh/Synthetic-Local) FULL PATHNAME. Or ctrl-c to end : "
  set htFile=$<
  echo $htFile >> $INtxt
  echo "htFile = $htFile, XMLgen.syn.csh" >> $INtxt.key
 
  source $parsePath $htFile
    if ($status != 0) exit 1
  set htFileDate1 = $date1
  set htFileDate2 = $date2

  set htRoot = "/$root1/$root2/$root3/$root4"
  if ("$htRoot" != "$inRoot") then 
    echo "    PROBLEM. All downscaled input data must be in common root directory = $inRoot."
    echo "    You passed in file with root  = $htRoot."
    exit 1
  endif
  set htDataCategory = $tmpDataCategory
  set htDataType = $tmpDataType
  set htDataSource = $tmpDataSource
  set htEpoch = $tmpEpoch
  set htFreq = $tmpFreq
  set htRealm = $tmpRealm
  set htMisc = $tmpMisc
  set htRIP = $tmpRIP
  set htVs = $tmpVs

  set targVar = $tmpVar
  set region = $tmpRegion
  set dim = $tmpDim

  set htDataSet = "$tmpDataSet"

  echo " "
  echo "    ///////////////"
  echo "    ---------------"
  echo "    Target Variable"
  echo "    ---------------"
  echo "    \\\\\\\\\\\\\\\"
  echo " "
  echo ">>  Target variable = $targVar"

  set varInfo = `grep $targVar $xmlGenDir/target_ID_table.txt`
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
#       echo "targID = $targID , XMLgen.syn.csh" >> $INtxt.key
       echo "$targID $targVar" >> $xmlGenDir/target_ID_table.txt
       echo "$targID $targVar     has been added to $xmlGenDir/target_ID_table.txt"
     else
       set targID = $varInfo[1]
     endif

  echo "    Target variable = $targVar" 
  echo "    Target ID       = $targID"
  sleep 2

# 
#
# Historical predictor/ "GCM" / "Mh" 
# --------------------------------
# 

  echo " "
  echo "    ///////////////////////////////////////////////////"
  echo "    ---------------------------------------------------"
  echo "    PREDICTOR ('Historical') Dataset Full Path"
  echo "    ---------------------------------------------------"
  echo "    \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"
  echo "  "

  echo -n ">>  Please enter the GCM/Mh/Synthetic-GCM full pathname. Or ctrl-c to end : "
  set hpFile=$<
  echo $hpFile >> $INtxt
  echo "hpFile = $hpFile , XMLgen.syn.csh" >> $INtxt.key
 
  source $parsePath $hpFile
    if ($status != 0) exit 1
  set hpFileDate1 = $date1
  set hpFileDate2 = $date2

  set hpRoot = "/$root1/$root2/$root3/$root4"
  if ("$hpRoot" != "$inRoot") then 
    echo "PROBLEM. All downscaled input data must be in common root directory = $inRoot."
    echo "You passed in file with root  = $hpRoot."
    exit 1
  endif
  set hpDataCategory = $tmpDataCategory
  set hpDataType = $tmpDataType
  set hpDataSource = $tmpDataSource
  set hpEpoch = $tmpEpoch
  set hpFreq = $tmpFreq
  set hpRealm = $tmpRealm
  set hpMisc = $tmpMisc
  set hpRIP = $tmpRIP
  set hpVs = $tmpVs
  set hpDataSet = "$tmpDataSet"

  if ("$targVar" != "$tmpVar") then
    echo "PROBLEM. $hpFile has different variable than target."
    echo "$hpFile"
    echo "targVar = $targVar. Filename indicates variable = $tmpVar."
    exit 1
  endif
  if ($region != "$tmpRegion") then
    echo "PROBLEM. $hpFile has different region than target."
    echo "$hpFile"
    echo "target region = $region. Filename indicates region= $tmpRegion."
    exit 1
  endif
  if ($dim != "$tmpDim") then
    echo "PROBLEM. $hpFile has different dim than target."
    echo "$hpFile"
    echo "target dim = $dim. Filename indicates dim = $dim."
    exit 1
  endif

#
# Future Predictor / Mf 
# --------------------

  echo " "
  echo "    ///////////////////////////////////////////////////"
  echo "    ---------------------------------------------------"
  echo "    PREDICTOR ('Future') Dataset Full Path"
  echo "    ---------------------------------------------------"
  echo "    \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"
  echo "  "
  echo -n ">>  Please enter the GCM/Mf/Synthetic-Future full pathname. Or ctrl-c to end : "
  set fpFile=$<
  echo $fpFile >> $INtxt
  echo "fpFile = $fpFile , XMLgen.syn.csh" >> $INtxt.key
 
  source $parsePath $fpFile
    if ($status != 0) exit 1
  set fpFileDate1 = $date1
  set fpFileDate2 = $date2

  set fpRoot = "/$root1/$root2/$root3/$root4"
  if ("$fpRoot" != "$inRoot") then 
    echo "PROBLEM. All downscaled input data must be in common root directory = $inRoot."
    echo "You passed in file with root  = $fpRoot."
    exit 1
  endif

  set fpDataCategory = $tmpDataCategory
  set fpDataType = $tmpDataType
  set fpDataSource = $tmpDataSource
  set fpEpoch = $tmpEpoch
  set fpFreq = $tmpFreq
  set fpRealm = $tmpRealm
  set fpMisc = $tmpMisc
  set fpRIP = $tmpRIP
  set fpVs = $tmpVs
  set fpDataSet = "$tmpDataSet"

  if ("$targVar" != "$tmpVar") then
    echo "PROBLEM. $fpFile has different variable than target."
    echo "$fpFile"
    echo "targVar = $targVar. Filename indicates variable = $tmpVar."
    exit 1
  endif
  if ($region != "$tmpRegion") then
    echo "PROBLEM. $fpFile has different region than target."
    echo "$fpFile"
    echo "target region = $region. Filename indicates region= $tmpRegion."
    exit 1
  endif
  if ($dim != "$tmpDim") then
    echo "PROBLEM. $fpFile has different dim than target."
    echo "$fpFile"
    echo "target dim = $dim. Filename indicates dim = $dim."
    exit 1
  endif
  

# set echo
# full paths to the 2d input files (up one level from the minifile directories)

  set htPath = "$inRoot/$htDataCategory/$htDataType/$htDataSource/$htEpoch/$htFreq/$htRealm/$htMisc/$htRIP/$htVs/$targVar/$region"
  set hpPath = "$inRoot/$hpDataCategory/$hpDataType/$hpDataSource/$hpEpoch/$hpFreq/$hpRealm/$hpMisc/$hpRIP/$hpVs/$targVar/$region"
  set fpPath = "$inRoot/$fpDataCategory/$fpDataType/$fpDataSource/$fpEpoch/$fpFreq/$fpRealm/$fpMisc/$fpRIP/$fpVs/$targVar/$region"
  
# spatial mask not used for Synthetic data

  set spatMaskID = "$region"
  set spatMaskDir = "na"
  set maskVar = "na"


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
 
  set htFileDate1 = $tsDate1
  set htFileDate2 = $tsDate2
 
  set hpFileDate1 = $tsDate1
  set hpFileDate2 = $tsDate2

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
  echo "    FUTURE PREDICTOR TIME SERIES"
  echo "    ----------------------------"
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
# No automatic convention for Synthetic Experiment Name construction exists.
# User must input entire expLabel, which is a user string meant to uniquely identify the experiment.
# Experiment names are of form:    {projectID}{varID}{platformID}-{DSmethod}-{expLabel}K{kfold}"
# 
# e.g.  S2synp1-BCQM-ATest1K00
#
#
# 
# kfold for synthetic data is set to "0"
# if other option desired, add code here to select value. (see XMLgen.non-syn.csh for example.)

  set kfold = "0"
  set kfoldID = "K00"
 

# Experiment Series ID
# --------------------
  echo " "
  echo " "
  echo "    ---------------------------------------------"
  echo "    Unique User-Input Experiment Name Information"
  echo "    ---------------------------------------------"
  echo " "
  echo ">>  Experiment names are of the form: "
  echo ">> "
  echo ">>         {projectID}{varID}{platformID}-{DSmethod}-{expLabel}K{kfold}"
  echo ">> "
  echo ">>  Synthetic data Experiments do NOT have a convention for constructing the unique Experiment Identifier,"
  echo ">>  denoted as '{expLabel}' in the above format. '{expLabel}' should include the Experiment series "
  echo ">>  (usually a capital letter), and some unique string (numbers/letter combination) that could be entered into"
  echo ">>  a look-up table with a description of the experiment & inputs." 
  echo ">>     e.g. S1synp1-BCQM-A987K00, the '{expLabel}' is 'A987' (enter without the single quotes)." 
  echo ">> "
  echo -n ">>  Enter complete {expLabel} Experiment Identifier : "
    set expLabel=$<
    echo "expLabel = $expLabel"
#   stty echo
    echo " "
    if ($expLabel == "") then
      echo " " >> $INtxt
    else
      echo $expLabel >> $INtxt
    endif
    echo "expLabel = $expLabel , XMLgen.syn.csh" >> $INtxt.key


#   -----------------------------------------------
#   CREATE EXPERIMENT NAME BASED ON INPUTS THUS FAR
#   -----------------------------------------------
  echo "--------------------------------------------------------------------------------------------"
  echo " "
  echo "   CREATE EXPERIMENT NAME BASED ON INPUTS THUS FAR"

  echo " "
  echo "--------------------------------------------------------------------------------------------"
  echo "............................................................................................."
  echo "............................................................................................."
  echo " "
  echo "   Based on your inputs:"
  echo " "
  echo "   ${expLabel}     <= Experiment Label "
  echo " "
  set expName = "${projectID}${targID}${platformID}-${dsMethod}-${expLabel}${kfoldID}"
  echo "   ${projectID}${targID}${platformID}-${dsMethod}-${expLabel}${kfoldID} <= Experiment Name"
  echo " "

# check if Experiment exists already
  source $xmlGenDir/check_experiment_name.csh $expName
    if($status != 0) exit 1

  echo "%"
  echo "${expLabel} $expName" >> $workDir/EXPERIMENT_INFO
  echo "   historical target   : $htFile" >> $workDir/EXPERIMENT_INFO
  echo "   historical predictor: $hpFile" >> $workDir/EXPERIMENT_INFO
  echo "   future predictor    : $fpFile" >> $workDir/EXPERIMENT_INFO
  echo "   training time window: $trainTimeWindowFile" >> $workDir/EXPERIMENT_INFO
  echo "   future   time window: $futureTimeWindowFile" >> $workDir/EXPERIMENT_INFO

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
    <!-- Created using  $xmlGenDir/XMLgen.csh ; $dateMDY -->
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
                time_trim_mask='$futureTimeTrimFile'
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
  echo " >>>  Inputs you entered are in $xmltxtDir/$expName.input.txt "
  echo " >>>  "
  $xmlGenDir/XMLchecker.syn.py $xmlDir/$expName.xml
  set logfile = `\ls *log`
  \mv *log $xmltxtDir
  echo "      XMLchecker output is in $xmltxtDir/$logfile "
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
  echo " "

  cd $cDir
  cat $workDir/EXPERIMENT_INFO >> $xmltxtDir/$expName.EXPERIMENT_INFO
  echo "    Experiment Information added to $xmltxtDir/$expName.EXPERIMENT_INFO"
  
  echo "Removing work directory $workDir"
  sleep 4
  \rm -rf $workDir
