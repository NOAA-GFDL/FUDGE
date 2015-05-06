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
echo "      Other  = You want to enter a value for npas"
echo "      training_target   = Length of training target dataset within time window  (will duplicate results with 0 from before 10-20-14)"
echo "      future_predictor  = Length of future predictor dataset within time window  (will duplicate results with 0 from 10-20-14 to 11-3-14)"

set dinfo = "npas"
set varvals = (0 "Other" "training_target" "future_predictor")
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


cat >> XMLfile <<EOF
        <dev>$dev</dev>
        <npas>$npas</npas>
EOF
