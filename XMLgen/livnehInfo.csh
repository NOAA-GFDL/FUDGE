#!/bin/csh -f

  echo "    LIVNEH DATA being used for Target"
  echo "    and has the same grid as Red River "
  echo "    datasets CCSM4, MIROC5 and MPI-ESM-LR,"
  echo "    the common 0.1x0.1 degree grid."
  echo " "
# Livneh version
  set htVs = "v1p2"
  if ($htVs == "v1p2") set htID = "L01"


  set htDate1 = 19610101
  set htDate2 = 20051231

  echo "    "
  echo "    LIVNEH is currently only available for"
  echo "    the time period $htDate1 to $htDate2 ."
  echo "    "

  set htDataCategory = OBS_DATA
  set htDataType = GRIDDED_OBS
  set htEpoch = historical
  set htFreq = day
  set htRealm = atmos
  set htMisc = day


# Set GCM datasetID, along with Livneh RIP and time-mask suffix based on GCM."
# livneh RIP r0i0p0 is Julian calendar data
# livneh RIP r0i0p1 is NOLEAP calendar data
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

  echo "    Settings for LIVNEH DATA complete"
  echo "  "

  exit 0
