#!/bin/csh -f
echo " "
echo " "
echo "   CDFt - CDF Transformation"
echo " "
echo "   Options: "
echo " "
echo "      'dev'"
echo "      'npas'"
echo " "
set maxdev = 5
echo "   Option: dev "
echo "   ----------- "
set dinfo = ", the coefficient of development (of the difference between the mean of the large-scale historical data and the mean of the large-scale future data to be down-scaled). This development is used to extend range of data on which the quantiles will be calculated for the CDF to be downscaled. (Package developers suggest dev=2, but we often use 1.)"
echo -n "   Please enter the value for 'dev' $dinfo : "

unset ok
while (! $?ok) 
  set dev=$<
  echo $dev
  if ( `echo $dev | grep -P '^\d+$'` != "" ) then
     if ($dev > 0 && $dev <= $maxdev) then 
       set ok
       echo " "
       echo "   You chose $dev ."
       echo " "
     else
       echo "   You entered $dev . Please enter the integer associated with a ${dinfo}."
     endif
  else
     echo "   You entered $dev . Please enter the integer associated with a ${dinfo}."
  endif
end
echo $dev >>  $INtxt
echo "dev = $dev, CDFt-Options.csh" >>  $INtxt.key

echo "   Option: npas "
echo "   ----------- "
echo "   Please enter the value for 'npas', the number of quantile bins to use."
echo "   Enter an integer 1 through maximum # of days per time window or "
echo "   Options: "
echo "      0 = If you want to use maximum # of days per time window - the shorter of the length of the training target or future predictor datasets within a time window."
echo "      100  = commonly-used value for npas"
echo "      Other  = You want to enter a value for npas"
echo "      training_target   = Length of training target dataset within time window  (will duplicate results with 0 from before 10-20-14)"
echo "      future_predictor  = Length of future predictor dataset within time window  (will duplicate results with 0 from 10-20-14 to 11-3-14)"

set dinfo = "npas"
set varvals = (0 100 "Other" "training_target" "future_predictor")
source $QueryVals
set npas = "$kval"
echo "$npas" >> $INtxt
echo "npas = $npas, Query,CDFt-Options.csh" >> $INtxt.key

if ("$npas" == "Other") then

unset ok
while (! $?ok) 
  echo -n "Enter npas value : "
  set npas=$<
  echo $npas
  echo $npas >> $INtxt
  echo "npas = $npas Enter npas value, CDFt-Options.csh" >>  $INtxt.key
  echo " "
  set ok
end

endif

echo "    You are using dev = $dev and npas = $npas ."
cat >> $workDir/custom_XML <<EOF
        <dev>$dev</dev>
        <npas>$npas</npas>
EOF

# for pr and other target variables that might have missing values
# (because of dryness), maxdevretry and retry options will be prompted for.

if ($targVar != "tasmax" && $targVar != "tasmin") then
echo "   "
echo "   Option: maxdevretry "
echo "   ------------------- "
echo "   Please enter the value for 'maxdevretry', the number of retries that CDFt should "
echo "     do before implementing implementing your setting for 'retry' option which "
echo "     determines what should be done if dev = maxdevretry and CDFt still fails."
echo "     (Max value needed for dev has been < 13 in testing, but the default has been 20.)"
echo "  "
echo -n "   Enter an positive integer value for maxdevretry : "
@ maxdevretry = $<
if ($maxdevretry <= 0) then
  echo  "   maxdevretry = $maxdevretry is not acceptable. Must be a positive integer. Exiting."
  exit 1
endif
echo "$maxdevretry" >> $INtxt
echo "maxdevretry = $maxdevretry, Query,CDFt-Options.csh" >> $INtxt.key

echo "   "
echo "   Option: retry "
echo "   ------------- "
echo "   Please enter the value for 'retry', which determines CDFt behavior if "
echo "     dev = maxdevretry is reached and downscaling can not be done."
echo "   Options available are: "
echo "      'stop',    which halts CDFt with a large obvious error message."
echo "      'missing', which returns a vector of all missing values for the problematic grid point output."
echo "      'predictor', which returns the future predictor values (MF) for the problematic grid point output."
echo "  "
set dinfo = "retry"
set varvals = ("'stop'" "'missing'" "'predictor'")
source $QueryVals
set retry = "$kval"
echo "$retry" >> $INtxt
echo "retry = $retry, Query,CDFt-Options.csh" >> $INtxt.key

echo "    You are using maxdevretry = $maxdevretry and retry = $retry ."
cat >> $workDir/custom_XML <<EOF
        <maxdevretry>$maxdevretry</maxdevretry>
        <retry>$retry</retry>
EOF
endif

