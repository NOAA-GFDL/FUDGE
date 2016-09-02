#!/bin/csh -f

# proposed:
# regression_subsample_method  - total_sample_size or climate_variable_range
#  subsample_pct  - 5% used for temperatures ARRM, 10% used for precip ARRM
#  tail_taper_pct - 10% used for ARRM, resulting in lower tail 10% and high tail 90%

echo " "
echo " "
echo ">>  PARM - Parameterized Asynchronous Regression Model (based on ARRM) downscaling method"
echo ">> "
echo ">>  PARM, which is based on ARRM code from TTU, relies on finding breakpoints by calculating"
echo ">>  a regression between the OH and MH datasets for each point x in the dataset and placing "
echo ">>  breakpoints in those locations with the lowest r-squared. To do this, each point x requires"
echo ">>  definition of a range of points over which to calculate the regression. In the code, this is"
echo ">>  defined as a point (x) and a number of points, window_width, farther along the distribution"
echo ">>  to count. PARM relies on using the distribution of the MH dataset to set window_width that "
echo ">>  will be used as an index in the OH and MH datasets. Whether window_width is a fixed subset of "
echo ">>  the total sample size or the climate variable value range depends on the setting of option"
echo ">>  'regression_subsample_method'. The percentage of the range or sample size is determined by option"
echo ">>  'subsample_pct'. The tail taper percentage is specified by 'tail_taper_pct'."
echo ">> "
echo ">>  'regression_subsample_method' determines if the windows are fixed percentage of"
echo ">>        'climate_variable_range' (ARRM-like) or 'total_sample_size' (GFDL's version). "
echo ">> "
echo ">>  'subsample_pct' determines the subsample size in percent. ARRM used 'subsample_pct' = 5 for temperatures "
echo ">>         10 for precipitation."
echo ">> "
echo ">>  'tail_taper_pct' = 0 means no tail tapering will be done. tail_taper_pct > 0 is the percentage of "
echo ">>         the distribution to be considered the low tail and also the high tail."
echo ">> "
echo ">>   If options are not specified, PARM defaults to ARRM-like behavior (sans section 2.5)."
echo ">> "    
echo " "    

# regression_subsample_method
echo " "    
echo ">>  Option: regression_subsample_method "
echo ">>  ------------------------------------ "
echo ">>  'climate_variable_range' = ARRM-like using equal sized subsets of (Max-Min) range of climate variable for determining slopes and breakpoints."
echo ">>  'total_sample_size' = GFDL version using equal sample size subsets of total sample size for determining slopes and breakpoints.."
set varvals = ('climate_variable_range' 'total_sample_size')
set dinfo = " regression_subsample_method, select option."
source $QueryVals
set regression_subsample_method = "$kval"
  echo "$regression_subsample_method" >> $INtxt
  echo "regression_subsample_method = $regression_subsample_method, $0" >> $INtxt.key

# subsample_pct
echo ">> "    
echo ">>  Option: subsample_pct "
echo ">>  --------------------- "
echo ">>  Percentage of total sample (based on either value range or sample size) for each subsample overwhich to compute slope."
echo ">>  ARRM used 5% for temperature, 10% for precipitation."
echo " " 
echo -n ">>  Enter value greater than 0 and less than or equal to 100: "
  set subsample_pct=$<
  echo "$subsample_pct" >> $INtxt
  echo "subsample_pct = $subsample_pct, $0" >> $INtxt.key

# tail_taper_pct
echo " "    
echo ">>  Option: tail_taper_pct "
echo ">>  --------------------- "
echo ">>  Percentage of total range or sample to use for tail taper. Zero means no taper."
echo ">>  ARRM used 10%."
echo ">>" 
echo -n ">>  Enter 0  if no taper is desired. Otherwise integer value for the low( and high) tail percentage: "
  set tail_taper_pct=$<
  echo "$tail_taper_pct" >> $INtxt
  echo "tail_taper_pct = $tail_taper_pct, $0" >> $INtxt.key


echo " "    
echo "        <regression_subsample_method>"\'$regression_subsample_method\'"</regression_subsample_method>" >> $workDir/custom_XML
echo "        <subsample_pct>$subsample_pct</subsample_pct>" >> $workDir/custom_XML
echo "        <tail_taper_pct>$tail_taper_pct</tail_taper_pct>" >> $workDir/custom_XML
