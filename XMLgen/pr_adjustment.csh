#!/bin/csh -f

  echo " "
  echo " "
  echo "============================================="
  echo "============================================="
  echo " "
  echo "    precip Adjustment "
  echo " "
  echo "============================================="
  echo "============================================="
  echo " "

  if (-e $workDir/pr_XML) \rm $workDir/pr_XML

  set pr_opts = (pr_threshold_in pr_conserve_in pr_threshold_out pr_conserve_out pr_freqadj_in)
  set varvals = ("'us_trace'" "'global_trace'" "'zero'")

  if (! $?prVars ) then
    echo "    precip adjustment can not be done for $targVar = $targVar. "
    echo "    No precip variables have been selected for target or predictors."
    exit 0
  else

  echo "    Description of options for Precip INPUT threshold:"
  echo " "
  echo "   'us_trace' == 0.01 in./day"
  echo "   'global_trace' == 0.1 mm/day" 
  echo "   'zero'  == 0 precipitation"
  echo " "
  echo " "

# pr_threshold_in  

  set dinfo = "precip input data, what threshold value is to be used to distinguish between dry days and wet days?"
  source $QueryVals
  set opt = ($kval)
  echo "$opt" >> $INtxt
  set pr_threshold_in = ($opt[1])
  echo "opt = $opt => pr_threshold_in = $pr_threshold_in, pr_adjustment.csh ">> $INtxt.key

# pr_freqadj_in

  echo " "
  echo  ">>  FREQUENCY ADJUSTMENT? "
  echo " "
  echo  ">>  Do you want to adjust the training predictor precip timeseries"
  echo  ">>  to have the same frequency of wet days as the training target timeseries?"
  echo -n ">>  Type y for yes ; Type n or hit the 'Enter' key for no :  "
  set freqadj=$<
  echo " "
  if ($freqadj == "") then
    set freqadj = n
  endif
  echo $freqadj >> $INtxt
  echo "freqadj = $freqadj, pr_adjustment.csh" >> $INtxt.key

  if ($freqadj != "y") then
    set pr_freqadj_in = "'off'"
    echo "    Training predictor timeseries wet day FREQUENCY will not be adjusted."
  else
    set pr_freqadj_in = "'on'"
    echo "    Training predictor timeseries wet day FREQUENCY will be adjusted."
  endif
  echo " "
  sleep 1

# pr_conserve_in

  if ($pr_threshold_in != "'zero'" || $pr_freqadj_in == "'on'") then

    echo  ">>  CONSERVE INPUT? "
    echo " "
    echo  ">>  When adjusting the input precip timeseries, "
    echo  ">>  do you want to conserve total precip amount?"
    echo -n ">>  Type y for yes ; Type n or hit the 'Enter' key for no :  "
    set conserve_in=$<
    echo " "
    if ($conserve_in == "") then
      set conserve_in = n
    endif
    echo $conserve_in >> $INtxt
    echo "conserve_in = $conserve_in, pr_adjustment.csh" >> $INtxt.key
  
    if ($conserve_in != "y") then
      set pr_conserve_in = "'off'"
      echo "  "
      echo "    Input total precip amount will not be conserved."
      echo "  "
    else
      set pr_conserve_in = "'on'"
      echo "  "
      echo "    Input total precip amount will be conserved."
      echo "  "
    endif
    echo " "
    sleep 1

  else
    set pr_conserve_in = "'off'"
  endif

  echo "    Options for Precip downscaled OUTPUT:"
  echo " "

# pr_threshold_out  

  set dinfo = "downscaled precip output timeseries, what threshold value is to be used to distinguish between dry days and wet days?"
  source $QueryVals
  set opt = ($kval)
  echo "$opt" >> $INtxt
  set pr_threshold_out = ($opt[1])
  echo "opt = $opt => pr_threshold_out = $pr_threshold_out, pr_adjustment.csh" >> $INtxt.key

# pr_conserve_out

  if ($pr_threshold_out != "'zero'") then

    echo "  "
    echo ">>  CONSERVE OUTPUT? "
    echo ">>  When adjusting the downscaled output precip timeseries, "
    echo ">>    do you want to conserve total precip amount?"
    echo -n ">>  Type y for yes ; Type n or hit the 'Enter' key for no :  "
    set conserve_out=$<
    echo "  "
    if ($conserve_out == "") then
      set conserve_out = n
    endif
    echo $conserve_out >> $INtxt
    echo "conserve_out = $conserve_out, pr_adjustment.csh" >> $INtxt.key
  
    if ($conserve_out != "y") then
      set pr_conserve_out = "'off'"
      echo "  "
      echo "    Downscaled output total precip amount will not be conserved."
    else
      set pr_conserve_out = "'on'"
      echo "    Downscaled output total precip amount will be conserved."
    endif
    echo " "
    sleep 1

  else
    set pr_conserve_out = "'off'"
  endif

#
# 9/21/2015  Turn ignore0 option on. User beware.
#
  if ($dsMethod != "DeltaSD") then
    echo ">> "
    echo ">>  Ignore0 option."
    echo ">> "
    echo ">>  Do you want to mask where precip = zero? "
    echo ">>  (Your DS Method is $dsMethod . FUDGE will use the correct masking type for your method.)"
    echo -n ">>  Type n for no ; Type y or hit the 'Enter' key for yes :  "
    set ans=$<
    echo "  "
    if ($ans == "") then
      set ans = y
    endif
    echo $ans >> $INtxt
    echo "ignore0? answer = $ans, pr_adjustment.csh" >> $INtxt.key
    
    if ($ans == "n") then
      set ignore0 = "'off'"
      echo "  "
      echo "    precip = zero will NOT be masked for training."
    else
      set ignore0 = "'on'"
      echo "  "
      echo "    precip = zero WILL be masked for training."
    endif
  else
    echo " "
    set ignore0 = "'off'"
    echo "  "
    echo ">>  Your DS Method is $dsMethod . Can not use ignore0 option here. "
  endif
 
  echo " "
  sleep 1


cat > $workDir/pr_XML<<EOF
        <pr_opts>
           <pr_threshold_in>$pr_threshold_in</pr_threshold_in>
           <pr_freqadj_in>$pr_freqadj_in</pr_freqadj_in>
           <pr_conserve_in>$pr_conserve_in</pr_conserve_in>
           <pr_threshold_out>$pr_threshold_out</pr_threshold_out>
           <pr_conserve_out>$pr_conserve_out</pr_conserve_out>
           <ignore0>$ignore0</ignore0>
EOF
  echo ">>  Tranforms"
  echo ">>  "
  echo ">>  Do you wish to apply a transform to the input data: $prVars?"
  echo ">>  Currently the only transform option is 'power'."
  echo -n ">>  Type n for no ; Type y or hit the 'Enter' key for yes :  "
  set transform=$<
  echo "    "
  if ($transform == "") then
    set transform = y
  endif
  echo $transform >> $INtxt
  echo " "
  echo "    transform ? = $transform"
  echo " "
  echo "transform = $transform, pr_adjustment.csh" >> $INtxt.key
  
  if ($transform != "n") then
     echo  ">>  If you want to use value = 1/3, hit the 'Enter' key."
     echo -n ">>  Otherwise type the value of the power you wish to apply (decimal or fraction acceptable):  "
     set power=$<
     echo "    "
     if ($power == "") then 
        set power = "1/3"
     endif
     echo "$power" >> $INtxt
     echo " "
     echo "    power = $power" 
     echo " "
     echo "power = $power , pr_adjustment.csh" >> $INtxt.key

     echo "           <transform>'power'</transform>" >> $workDir/pr_XML
     echo "           <arg>$power</arg>" >> $workDir/pr_XML
     echo "           <vars>'$prVars'</vars>" >> $workDir/pr_XML
  else
     echo "           <transform>'none'</transform>" >> $workDir/pr_XML
  endif

exit 0
