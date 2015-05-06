#!/bin/csh -f

#============================================================
# ASSUMPTIONS
  set epochList = ()
# Julian calendar, set timemaskSuffix = ""
# NOLEAP calendar, set timemaskSuffix = ".NOLEAP"
# NONE calendar, set timemaskSuffix = ".NONE"
  set timemaskSuffix = ".NONE"

  set trainDate1 = ""
  set trainDate2 = ""
  set htFileDate1 = $trainDate1
  set htFileDate2 = $trainDate2
  set htDate1 = $trainDate1
  set htDate2 = $trainDate2

  set htDataCategory = "SYN_DATA"
  set htDataType = ""
  set htDataSource = ""
  set htEpoch = ""

  set htID = "S01"

  set hpFileDate1 = $trainDate1
  set hpFileDate2 = $trainDate2

  set hpDataCategory = "SYN_DATA"
  set hpDataType = ""
  set hpDataSource = ""
  set hpEpoch = ""

  set futureDate1 = ""
  set futureDate2 = ""
  set fpFileDate1 = $futureDate1
  set fpFileDate2 = $futureDate2

  set fpDataCategory = "SYN_DATA"
  set fpDataType = ""
  set fpDataSource = ""
  set fpEpoch = ""

# ONLY set up so far for kfold = 0, but logic for kfold 
  set kfold = 0
  if ($kfold <= 9) set kfoldID = "K0$kfold"
  if ($kfold >= 10) set kfoldID = "K$kfold"

  set inputDir = "$inRoot/$htDataCategory/$htDataType/$htDataSource/$htEpoch"

  set region = "grid0"
  set dim = ZeroD 
  set lons = 1
  set lone = 1
  set lats = 1
  set late = 1
  set file_j_range = "J${lats}"

  set spatMaskDir = "na" 
  set maskVar = "na" 
  set spatMaskID = "$region"
#============================================================
# END ASSUMPTIONS
#============================================================

# find available variables
  source $xmlGenDir/find_vars.csh $inputDir
    if ($status != 0) exit 1

exit 0
