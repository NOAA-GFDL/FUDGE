# !/bin/csh -f -e

set echo 

#Test for i=300th minifiles with original settings, data only
#set origfile = /work/cew/testing/300-301-old/v20140108/tasmax_day_RRtxp1-CDFt-A38-oldL01K00_rcp85_r1i1p1_RR_20060101-20991231.I300_J31-170.nc
#set compfile = /home/cew/Code/testing/reg_tests//tasmax_day_sample-reg-test_rcp85_r1i1p1_RR_20060101-20991231.I300_J31-170.nc
#set runcode = /home/cew/Code/fudge2014/Rsuite/cew_testing_drivers/Regression_tests/300_original_regtest.R

set runcode = $1
set compfile = $2
echo $compfile
set origfile = $3

#Obtain previous file creation date/time of the test file
set create_time=`stat $compfile | grep Change | awk '{print substr($0, 20, 19)}'`
echo $create_time
#set create_time=`echo $create_time | awk '{ print susbstr($0,20,19) }'`
echo "The create time is: $create_time"

#Re-run the R code to see if it is being updated
Rscript $runcode

#Compare against the test case
nccmp -d $origfile $compfile
set ncc_status = $status
#And if it passes the test, *and* the file was actually written to disk....
echo $ncc_status
#echo `expr $ncc_status + 1`
if ($ncc_status == 0) then
	set new_time=`stat $compfile | grep "Change" | awk '{print substr($0, 20, 19)}'`
	echo $new_time
	echo $create_time
	if(${new_time} != ${create_time}) then
		exit 0
	else
		echo "Error: new file not written to previous output"
		exit 1
	endif
else
	echo "Error: new file data not identical to old file data"
	exit 1
endif



#cew:/home/cew/Code/fudge2014> set tempvar=`stat /home/cew/Code/testing/reg_tests//tasmax_day_sample-reg-test_rcp85_r1i1p1_RR_20060101-20991231.I300_J31-170.nc | grep Change | awk '{print substr($0, 20, 19)}'`
#cew:/home/cew/Code/fudge2014> set created=`stat /home/cew/Code/testing/reg_tests//tasmax_day_sample-reg-test_rcp85_r1i1p1_RR_20060101-20991231.I300_J31-170.nc | grep Change | awk '{print susbstr($0, 20, 19)}'`

#cew:/home/cew/Code/fudge2014> set tempvar=`stat /home/cew/Code/testing/reg_tests//tasmax_day_sample-reg-test_rcp85_r1i1p1_RR_20060101-20991231.I300_J31-170.nc | grep Change | awk '{print substr($0, 20, 19)}'`

#...I think I just spent somewhere north of an hour trying to debug a unicode error of some sort. Line 41
#works, and line 42 fails. But on the plus side, this should work as a testing script now.

