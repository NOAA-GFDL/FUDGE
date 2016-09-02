#!/bin/csh -f

  echo "    PRISM DATA being used for Target"
  echo "    and has the same grid as Red River "
  echo "    datasets CCSM4, MIROC5 and MPI-ESM-LR,"
  echo "    the common 0.1x0.1 degree grid."
  echo " "
# PRISM version
  set htVs = "v1"
  if ($htVs == "v1") then
     set htID = "P01"
  else
     echo "Version $htVs has not been given an identifier. See $0"
     exit 1
  endif

  set htDate1 = 19810101
  set htDate2 = 20051231

  echo "    "
  echo "    PRISM is currently only available for"
  echo "    the time period $htDate1 to $htDate2 ."
  echo "    "

  set htDataCategory = OBS_DATA
  set htDataType = GRIDDED_OBS
  set htEpoch = historical
  set htFreq = day
  set htRealm = atmos
  set htMisc = day


# Set GCM datasetID, along with PRISM RIP and time-mask suffix based on GCM."
# prism RIP r0i0p0 is Julian calendar data
# prism RIP r0i0p1 is NOLEAP calendar data
  if ("$gcmData" == "CCSM4") then
    set htRIP = "r0i0p1"
    set timemaskSuffix = ".NOLEAP"
    set datasetID = 1
  endif
  if ("$gcmData" == "MIROC5") then
    set htRIP = "r0i0p1"
    set timemaskSuffix = ".NOLEAP"
    set datasetID = 2
  endif
  if ("$gcmData" == "MPI-ESM-LR") then
    set htRIP = "r0i0p0"
    set timemaskSuffix = ""
    set datasetID = 3
  endif

  echo "    Settings for PRISM DATA complete"
  echo "  "

  exit 0
