#!/bin/csh -f

  echo " "
  echo " "
  echo "============================================="
  echo "============================================="
  echo " "
  echo "       QC Adjustment "
  echo " "
  echo "============================================="
  echo "============================================="
  echo " "
  echo "       Only ONE qc adjustment can be specified at this time. "
  echo " "

# set echo

  if (-e $workDir/QC_XML) \rm $workDir/QC_XML
  touch $workDir/QC_XML

  set qclist = `cat $qcOptions_file | cut -d " " -f 1`
  set qclist = ($qclist none)

  if ($dsMethod == "BCQM" || $dsMethod == "PARM" || $dsMethod =~ "CDFt*") then
    echo ">>   "
    echo ">>   IMPORTANT"
    echo ">>   ========="
    echo ">>   "
    echo ">>   You selected $dsMethod as your downscaling method."
    echo ">>   "
    echo ">>   'simpletailadj' tail adjustment options are available."
    if ($dsMethod =~ "CDFt*") then
       set triggertype = 'cdft'
    else
       set triggertype = 'GfGh'
    endif
    echo ">>   'triggertype' = "$triggertype" will be used for downscaling method $dsMethod."
    echo ">>   "
    echo ">>   Please note that a mask will be saved & no other adjustment can be done."
    echo ">>   "
    echo -n ">>   If you DO NOT want this adjustment, please type n for 'no'. Otherwise hit return:"
    set noTailAdj=$<
    if ($noTailAdj != "n") then
       set noTailAdj = "y"
       set saveQcMask = "on"
       echo ">>   "
       echo ">>   simpletailadj will be done."
       echo ">>   "
       echo ">>   Specify 'lastNpts', which is the number, N, of time points from the beginning and end of the distribution to be averaged together to compute deltas/comparisons for tail adjustments."
       echo -n ">>   lastNpts = 1 will simply use min and max values. Enter an integer value for 'lastNpts' : "
       @ lastNpts=$<
       if ("$lastNpts" < 0) then
         echo ">>   ERROR. lastNpts = $lastNpts, but must be >= 1 . Exit."
         exit 1
       endif
       echo ">>  "
       echo ">>   simpletailadj will be done."
       echo "        <qc type="\'simpletailadj\'" qc_mask="\'on\'" adjust_out="\'on\'">" > $workDir/QC_XML
       echo "        <triggertype>"\'$triggertype\'"</triggertype>" >> $workDir/QC_XML
       echo "        <lastNpts>$lastNpts</lastNpts>" >> $workDir/QC_XML
       echo "        </qc>" >> $workDir/QC_XML
       set used_qc_adjustment = True
       echo $noTailAdj >> $INtxt
       echo "noTailAdj = $noTailAdj, QC_adjustment.csh" >> $INtxt.key
       echo $lastNpts >> $INtxt
       echo "lastNpts = $lastNpts, QC_adjustment.csh" >> $INtxt.key
       exit 0
    else 
       echo ">>   "
       echo ">>   simpletailadj will NOT be done."
       echo ">>   "
       echo ">>   You will be able to select a different adjustment or none."
       echo ">>   "
       echo $noTailAdj >> $INtxt
       echo "noTailAdj = $noTailAdj, QC_adjustment.csh" >> $INtxt.key
    endif
  endif 

  set varvals = ($qclist)
  set dinfo = "QC data adjustment"
  source $QueryVals
  set qcName = "$kval"
  echo "$qcName" >> $INtxt
  echo "qcName = $qcName, XMLgen.non-syn.csh" >> $INtxt.key

  if ($qcName == "none") then
     echo "No adjustment will be made."
     exit 0
  endif

# Retrieve any options used by the qc adjustment in the file specified by $qcOptions_file.
  set qcOptions = (`grep $qcName $qcOptions_file`)
    if ($status != 0) then
       echo ">>   IMPORTANT:  $qcName qc adjustment is not listed in $xmlgenDir/QCMask_Options.txt."
       echo ">>   If you are expecting $qcName to be available, please contact Mary Jo"
       echo ">> "
       echo ">>   Exiting."
       exit 1
    endif

# Only set used_qc_adjustment = True if the qc adjustment being done is not flag.neg, since used_qc_adjustment is being used to indicate that some qc adjustment other than flag.neg is being used.

  if ($qcName != "flag.neg") then
    set used_qc_adjustment = True
  endif

# Set qc_mask Options
  set saveQcMask = "off"
  set qcAdjust = "off"
  echo " "
  echo "    /////////////"
  echo "    ------------- "
  echo "    SAVE QC MASK? "
  echo "    ------------- "
  echo "    \\\\\\\\\\\\\"
  echo " "
  echo  ">> Do you want the QC mask to be saved as output?"
  echo " "
  echo -n ">> Enter y for 'yes' ; Enter n or hit the 'Enter' key for 'no' :  "
  set genMask=$<
  echo "You entered $genMask"
  echo $genMask >> $INtxt
  echo "genMask = $genMask, QC_adjustment.csh" >> $INtxt.key
  if($genMask == "y") then
     set saveQcMask = "on"
     echo " "
     echo "   A QC mask will be saved as output."
     echo " "
  else
     echo " "
     echo "   A QC mask will not be saved as output."
     echo " "
  endif
    
  echo " "
  echo "    ///////////////////"
  echo "    ------------------ "
  echo "    MODIFY QC-ed DATA? "
  echo "    ------------------ "
  echo "    \\\\\\\\\\\\\\\\\\\"
  echo " "
  echo ">> Do you want the QC flagged downscaled values to be modified in the downscaled output?"
  echo " "
  echo -n ">> Enter y for 'yes' ; Enter n or hit the 'Enter' key for 'no' :  "
  set qcout=$<
  echo "   You entered $qcout"
  echo $qcout >> $INtxt
  echo "qcout = $qcout, QC_adjustment.csh" >> $INtxt.key

  if ($qcout == "y") then
     set qcAdjust = "on"
     echo " "
     echo "   QC flagged downscaled values will be modified."
     echo " "
  else
     echo " "
     echo "   QC flagged downscaled values will NOT be modified."
     echo " "
     if ($saveQcMask == "off") then
       echo " "
       echo "   No mask will be saved either."
       echo "   Without these options turned on, the $qcName module will not be added to XML. "
       echo " "
       exit 0
     endif
  endif

  echo "        <qc type="\'$qcName\'" qc_mask="\'$saveQcMask\'" adjust_out="\'$qcAdjust\'">" > $workDir/QC_XML

# set saveQcMask = "on" as a flag for later, even if saveQcMask was = off above
  set saveQcMask = "on"

  if ("$qcOptions" != "") then
    @ nopts = $#qcOptions
    set optValues = ()
#  first string is qcName; options start with 2nd string
    set kstring = 2
    while ($kstring <= $nopts)
      set opt = "$qcOptions[$kstring]"
      unset outVal
      if ($opt == "botlim") then 
        set outVal = -6.
        echo " "
        echo ">> Set values for QC $qcName bottom (botlim) and top (toplim) limits."
        echo ">> "
      endif
      if ($opt == "toplim") then 
        set outVal = 6.
      endif
      if ($?outVal) then
        echo ">> "
        echo ">> Usually for tasmax & tasmin & their anomaly variables, $opt = $outVal "
        echo ">> but can be set to any value meaningful for your variable."
        echo ">> "
        echo -n ">> If $opt = $outVal is what you want, hit Enter to continue. Or type the desired value : "
        set ans=$<
        if ($ans != "") then 
          set outVal = $ans
          echo " "
          echo "   You entered $outVal"
          echo " "
        endif
      else
        echo ">> "
        echo -n ">> Input a value for option $opt : " 
        set outVal=$<
        echo " "
        echo "   You entered $outVal"
        echo " "
      endif
      echo $outVal >> $INtxt
      echo "outVal = $outVal, QC_adjustment.csh" >> $INtxt.key
      echo " "
      echo "   You set $qcOptions[$kstring] = $outVal " 
      echo " "
      set optValues = ($optValues "$qcOptions[$kstring]="\"${outVal}\"" ")
      echo "           <$qcOptions[$kstring]>${outVal}</$qcOptions[$kstring]>" >> $workDir/QC_XML
      @ kstring = $kstring + 1
    end
  endif

  echo "        </qc>" >> $workDir/QC_XML


exit 0
