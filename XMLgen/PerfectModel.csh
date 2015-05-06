#!/bin/csh -f

#set echo
#============================================================
# ASSUMPTIONS
  set epochList = (amip sst2090)
# Julian calendar, set timemaskSuffix = ""
# NOLEAP calendar, set timemaskSuffix = ".NOLEAP"
  set timemaskSuffix = ""

  set trainDate1 = 19790101
  set trainDate2 = 20081231
  set htFileDate1 = $trainDate1
  set htFileDate2 = $trainDate2
  set htDate1 = $trainDate1
  set htDate2 = $trainDate2

  set htDataCategory = "GCM_DATA"
  set htDataType = "NCPP"
  set htDataSource = "GFDL-HIRAM-C360"
  set htEpoch = "amip"

# Using First Version of Perfect Model from /archive/Oar.Gfdl.Esd/PROJECTS/DOWNSCALING/NCPP2013/.../v20110601
  set htID = "X01"

  set hpFileDate1 = $trainDate1
  set hpFileDate2 = $trainDate2

  set hpDataCategory = "GCM_DATA"
  set hpDataType = "NCPP"
  set hpDataSource = "GFDL-HIRAM-C360-COARSENED"
  set hpEpoch = "amip"

  set futureDate1 = 20860101
  set futureDate2 = 20951231
  set fpFileDate1 = $futureDate1
  set fpFileDate2 = $futureDate2

  set fpDataCategory = "GCM_DATA"
  set fpDataType = "NCPP"
  set fpDataSource = "GFDL-HIRAM-C360-COARSENED"
  set fpEpoch = "sst2090"

# ONLY set up so far for kfold = 0, but logic for kfold 
  set kfold = 0
  if ($kfold <= 9) set kfoldID = "K0$kfold"
  if ($kfold >= 10) set kfoldID = "K$kfold"

  set inputDir = "$inRoot/$htDataCategory/$htDataType/$htDataSource/$htEpoch"
  if (! -e $inputDir) then
    echo " "
    echo "    $inputDir not found."
    echo " "
    echo "    The root directory = $inRoot . Maybe this is not correct."
    echo "    The root directory contains : "
    \ls -l $inRoot
    echo " "
    echo "    Exiting."
    exit 1
  endif


  set region = "US48"
  set dim = OneD 
  set lons = 748
  set lone = 941
  set lats = 454
  set late = 567
  set file_j_range = "J${lats}-${late}"

  set spatMaskDir = "na" 
  set maskVar = "na" 
  set spatMaskID = "US48"
#============================================================
# END ASSUMPTIONS
#============================================================

# find available variables
  source $xmlGenDir/find_vars.csh $inputDir
    if ($status != 0) exit 1

exit 0
