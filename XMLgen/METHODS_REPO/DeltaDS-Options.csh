#!/bin/csh -f
echo " "
echo " "
echo "    DeltaSD - Delta Downscaling Method "
echo " "
echo "       options:
echo " "
echo "         deltatype: 'median' or 'mean' "
echo "         deltaop  : 'ratio' or 'add' "
echo " "
echo "    This method works best when all input vectors (target, historic and future) are of the same length. If they are not of the same length, then when there are more historic values than future values, the target (local historical) vector will be truncated to match the length of the future vector (note: the historic predictor vector is not truncated). If there are more future values than historic values, then separate deltas will be calculated and applied for each segment of the future data <= length of the historical data."
echo " "
echo " "

# deltatype
echo "    Option: deltatype "
echo "    ----------------- "
set varvals = ('median' 'mean')
set dinfo = " deltatype, select option. (median is generally preferred)"
source $QueryVals
set deltatype = "$varvals[$kvar]"

# deltaop
echo "    Option: deltaop "
echo "    ----------------- "
set varvals = ('ratio' 'add')
set dinfo = " deltaop, select option. [add is preferred for any variables for which neagative values are not an error (i.e. temperature); ratio is preferred for everything else (i.e. pr)]"
source $QueryVals
set deltaop = "$varvals[$kvar]"


echo $deltatype >>  $INtxt
echo $deltaop >> $INtxt

cat >> XMLfile <<EOF
        <deltatype>$deltatype</deltatype>
        <deltaop>$deltaop</deltaop>
EOF
