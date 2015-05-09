#!/bin/csh -f

#============================================================
# ASSUMPTIONS
#============================================================
  set gcmList = ("CCSM4" "MIROC5" "MPI-ESM-LR")
# prism is from 1981-2005 while livneh is 1961-2005. 
# set htList = ("livneh" "prism")
  set htList = ("livneh")

  set epochList = ("historical" "rcp26" "rcp45" "rcp85")

  set futureDate1 = 20060101
  set futureDate2 = 20991231
  set futPredFileDate1 = $futureDate1
  set futPredFileDate2 = $futureDate2

# ONLY set up so far for kfold = 0, but logic for kfold 
  set kfold = 0
  if ($kfold <= 9) set kfoldID = "K0$kfold"
  if ($kfold >= 10) set kfoldID = "K$kfold"

#============================================================
# END ASSUMPTIONS
#============================================================

  echo "  "
  echo "    ============================"
  echo "    "
  echo "    SCCSC0p1-specific settings"  
  echo "    "
  echo "    ============================"
  echo " "
# prompt for ht/obs to use
  set varvals = ($htList)
  set dinfo = "Observational dataset as Historical Target "
  source $QueryVals
  set htDataSource = "$kval"
  echo "$htDataSource" >> $INtxt
  echo "htDataSource = $htDataSource, SCCSC0p1.csh" >> $INtxt.key

# prompt for gcm to use
  echo " "
  set varvals = ($gcmList)
  set dinfo = "GCM dataset as Historical & Future Predictors "
  source $QueryVals
  set gcmData = "$kval"
  echo "$gcmData" >> $INtxt
  echo "gcmData = $gcmData, SCCSC0p1.csh" >> $INtxt.key

  source $xmlGenDir/${htDataSource}Info.csh

  set htDir = "$inRoot/$htDataCategory/$htDataType/$htDataSource/$htEpoch"
  if (! -e $htDir) then
    echo " "
    echo "    $htDir not found."
    echo " "
    echo "    The root directory = $inRoot . Maybe this is not correct."
    echo "    The root directory contains: "
    \ls -l $inRoot
    echo " "
    echo "    Exiting."
    exit 1
  endif

# find available variables
  source $xmlGenDir/find_vars.csh $htDir
     if ($status != 0) exit 1

# historical predictor information
  set hpDataCategory = "GCM_DATA"
  set hpDataType = "CMIP5"
  set hpDataSource = "$gcmData"
  set hpEpoch = "historical"

# future predictor information
  set fpDataCategory = "GCM_DATA"
  set fpDataType = "CMIP5"
  set fpDataSource = "$gcmData"

  set region = "SCCSC0p1"
  set lons = 181
  set lone = 370
  set lats = 31
  set late = 170
  set file_j_range = "J${lats}-${late}"
  set dim = OneD


# spatial mask  
# set spatMaskDir = "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/geomasks/"
  set spatMaskDir = "/archive/esd/PROJECTS/DOWNSCALING/MASKS/geomasks/"
  cd $spatMaskDir
  set dirs = (`\ls -d *0p1` none)
  set dirList = ()
  foreach d ($dirs)
    if ($d != "sccsc_0p1") then 
      set dirList = ($dirList $d)
    endif
  end
  set dirs = ($dirList)
  echo " "
  echo "    ////////////"
  echo "    ------------"
  echo "    Spatial Mask"
  echo "    ------------"
  echo "    \\\\\\\\\\\\"
  echo " "
  set varvals = ($dirs)
  set dinfo = "available spatial masks for SCCSC0p1"
  source $QueryVals
  set spatMask = "$kval"
  echo "$spatMask" >> $INtxt
  echo "spatMask = $spatMask, SCCSC0p1.csh" >> $INtxt.key
  if ($spatMask != "none" && $spatMask != "sccsc_0p1") then
    set spatMaskDir = "$spatMaskDir$spatMask/"
    set maskVar = "${spatMask}_masks"
    set spinfo = `grep $spatMask $xmlGenDir/spat_mask_ID_table.txt`
    if ($status == 0 || "$spinfo[2]" == "$spatMask") then
# if an ID has already been assigned to the mask, use it.
      set spatMaskID = "$spinfo[1]"
    else
# if an ID has NOT been assigned to the mask, user must assign one and then it is written to table
      set done = "false"
      while ($done == "false")
        echo " "
        echo ">>  A spatial mask ID has not been assigned yet to $maskVar."
        echo -n ">>  Please enter an ID & it will be added along with $maskVar to the spat_mask_ID table. Or ctrl-c to end : "
        set spatMaskID=$<
        echo $spatMaskID >> $INtxt
        echo "spatMaskID = $spatMaskID, SCCSC0p1.csh" >> $INtxt.key
        if ($spatMaskID != "") then
          set checkMaskID = `grep -w $spatMaskID $xmlGenDir/spat_mask_ID_table.txt`
            if ($status != 0) then
              set done = "true"
              echo "$spatMaskID $maskVar" >> $xmlGenDir/spat_mask_ID_table.txt
              echo "$spatMaskID $maskVar     has been added to $xmlGenDir/spat_mask_ID_table.txt"
            else
              if($checkMaskID[1] != $spatMaskID) then
                echo "    Problem. $project is already in use. $checkMaskID"
              endif
            endif
        endif
      end
    endif
  else
    set spatMaskDir = "na"
    set maskVar = "na"
    set spatMaskID = "SCCSC"
  endif
   
  echo "  "
  echo "    =================================="
  echo "    "
  echo "    End of SCCSC0p1-specific settings."
  echo "  "
  echo "    =================================="
  echo "  "
  echo "    "
  
exit 0
