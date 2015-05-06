#!/bin/csh -f
echo " "
echo " "
echo "   CFQM method - Change Factor Quantile Mapping "
echo " "
echo "   Option: sort "
echo "   ------------ "
set dinfo = ", select to sort by 'future', 'historical' or 'target'"

set varvals = (future historical target)
source $QueryVals
set valSort = "$varvals[$kvar]"
echo $kvar >>  $INtxt

cat >> XMLfile <<EOF
        <sort>$valSort</sort>
EOF
