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
<<<<<<< HEAD
=======
  echo "       Only ONE qc adjustment can be specified. "
  echo " "

# set echo
>>>>>>> g_bcqmtailadj

  if (-e QC_XML) \rm QC_XML
  touch QC_XML

<<<<<<< HEAD
# qc adjustment may be used for specific variables. 
# If the qc is not desired - meaning that you do not ask for a mask to be saved 
#  nor any adjustment to be done, no <qc> tag is generated.

  set qcName = ""

  if ($targVar == "pr" || $targVar == "prc") then
    set qcName = "flag.neg"
    echo "    ******************************************************"
    echo "    QC adjustment available for $targVar = $qcName "
    echo "    ******************************************************"
    echo " "
  else
    set qcName = "SBiasCorr"
    echo " "
    echo "    ******************************************************"
    echo "    QC adjustment available for $targVar = $qcName "
    echo "    ******************************************************"
    echo " "
  endif
# if ($targVar == "tasmax" || $targVar == "tasmin") then
#   set qcName = "SBiasCorr"
#   echo " "
#   echo "    ******************************************************"
#   echo "    QC adjustment available for $targVar = $qcName "
#   echo "    ******************************************************"
#   echo " "
# else if ($targVar == "pr" || $targVar == "prc") then
#   set qcName = "flag.neg"
#   echo "    ******************************************************"
#   echo "    QC adjustment available for $targVar = $qcName "
#   echo "    ******************************************************"
#   echo " "
# else
#   echo " "
#   echo "   No QC is yet available for $targVar."
#   echo " "
#   echo " "
#   exit 0
# endif

# IF a QC method is available for the variable, then check and prompt for more options

# Retrieve any options used by the qc adjustment in the file specified by $qcOptions_file.
  set qcOptions = (`grep $qcName $qcOptions_file`)
   if ($status != 0) then
      echo -n ">> IMPORTANT:  $qcName qc mask has no options in $xmlgenDir/QCMask_Options.txt. Continue? (y/n) :"
      set keep_going=$<
      echo "You entered $keep_going"
      if ($keep_going != "y") exit 1
   endif
=======
  set qclist = `cat $qcOptions_file | cut -d " " -f 1`
  set qclist = ($qclist none)

  if ($dsMethod == "BCQM") then
    echo ">>   "
    echo ">>   IMPORTANT"
    echo ">>   ========="
    echo ">>   "
    echo ">>   You selected $dsMethod as your downscaling method."
    echo ">>   "
    echo ">>   It is strongly recommended you select 'BCQM.tail.adj' QC adjustment."
    echo ">>   "
    echo ">>   Please note that no other adjustment can be done."
    echo ">>   "
    echo -n ">>   If you DO NOT want this adjustment, please type 'n' or 'no'. Otherwise hit return:"
    set noTailAdj=$<
    echo $noTailAdj >> $INtxt
    echo "noTailAdj = $noTailAdj, QC_adjustment.csh" >> $INtxt.key
    if ($noTailAdj == "") then
       echo ">>   BCQM.tail.adj will be done."
       echo "         <qc type="\'BCQM.tail.adj\'" qc_mask="\'on\'" adjust_out="\'on\'">" > QC_XML
       echo "         </qc>" >> QC_XML
       exit 0
    else 
       echo ">>   "
       echo ">>   No BCQM.tail.adj will be done."
       echo ">>   "
       echo ">>   You will be able to select a different adjustment or none."
       echo ">>   "
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
       echo ">>   If you are expecting options outside of whether or not to output the mask or "
       echo ">>   adjust the values, stop and please contact Mary Jo"
       echo -n ">>   Continue? (y/n): "
       set keep_going=$<
       echo "You entered $keep_going"
       if ($keep_going != "y") exit 1
    endif
>>>>>>> g_bcqmtailadj
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
    
<<<<<<< HEAD
# do not yet allow this option to be turned on for "flag.neg".

  if ($qcName == "SBiasCorr") then
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
  else
    set qcout = "n"
    echo " "
    echo "   QC adjustment of $targVar downscaled values is not implemented."
    echo " "
  endif
=======
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
>>>>>>> g_bcqmtailadj

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

  echo "         <qc type="\'$qcName\'" qc_mask="\'$saveQcMask\'" adjust_out="\'$qcAdjust\'">" > QC_XML

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
      echo "            <$qcOptions[$kstring]>${outVal}</$qcOptions[$kstring]>" >> QC_XML
      @ kstring = $kstring + 1
    end
  endif

  echo "         </qc>" >> QC_XML


exit 0
