#!/usr/local/python/2.7.1/bin/python

#set echo 
echo "all of the arguments are $*"
set last_index = $#argv
echo $last_index

#Test for i=300th minifiles with original settings, data only
#set origfile = /work/cew/testing/300-301-old/v20140108/tasmax_day_RRtxp1-CDFt-A38-oldL01K00_rcp85_r1i1p1_RR_20060101-20991231.I300_J31-170.nc
#set compfile = /home/cew/Code/testing/reg_tests//tasmax_day_sample-reg-test_rcp85_r1i1p1_RR_20060101-20991231.I300_J31-170.nc
#set runcode = /home/cew/Code/fudge2014/Rsuite/cew_testing_drivers/Regression_tests/300_original_regtest.R

set runcode = $1

if (-e $4) then
	#Second argument will be summary of the expected input; 
	#third will be the input and comparison filename
	#fourth and fifth will be directories of comparison file and output file, respectively
	set test_meaning = $2
	set newfile = $4/$3
	echo $compfile
	set oldfile = $5/$3
	set logfile = $4/stdout.log
	set summfile  = $4/test_status.summary
	#Should always exist; touched at beginning of regression testing suite
else
	set compfile = $2
	echo $compfile
	set origfile = $3
endif

#Obtain previous file creation date/time of the test file
#set create_time=`stat $compfile | grep Change | awk '{print substr($0, 20, 19)}'`
#echo $create_time
#set create_time=`echo $create_time | awk '{ print susbstr($0,20,19) }'`
set create_time = `date +%T`
echo "The create time for test $runcode is: $create_time" >> $logfile 

#Re-run the R code to see if it is being updated
Rscript $runcode >> $logfile

#Compare against the test case
echo "nccmp -d $newfile $oldfile" >> $logfile
nccmp -d $newfile $oldfile
set ncc_status = $status
#And if it passes the test, *and* the file was actually written to disk....
#Code for formatting sumamry file
set test_str = `printf "=%-20s=", ${runcode}`
set testtime = `printf "=%-16s=", ${create_time}`
if ($ncc_status == 0) then
	set statstring = `printf "=%-11s=", "PASSED"`
	echo `printf $test_str, $statstring, $testtime, $test_meaning, "\n"` >> $summfile
	exit 0
else
	set statstring = `printf "=%-11s=", "FAILED"`
	echo `printf $test_str, $statstring, $testtime, $test_meaning, "\n"` >> $summfile
	exit 1
endif
