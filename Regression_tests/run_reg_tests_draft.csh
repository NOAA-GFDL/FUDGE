#!/bin/csh -f
#Runs regression tests for FUDGE and writes stdout to an output logfile
#and test results to a summary file

set echo
#module load nco
#module load nccmp

#Test for i=300th minifiles with original settings, data only
#set origfile = /work/cew/testing/300-301-old/v20140108/tasmax_day_RRtxp1-CDFt-A38-oldL01K00_rcp85_r1i1p1_RR_20060101-20991231.I300_J31-170.nc
#set compfile = /home/cew/Code/testing/reg_tests//tasmax_day_sample-reg-test_rcp85_r1i1p1_RR_20060101-20991231.I300_J31-170.nc
#set runcode = /home/cew/Code/fudge2014/Rsuite/cew_testing_drivers/Regression_tests/300_original_regtest.R
set echo

set runcode = $1
set oldfile = $2
set newdir = $3
set summfile = $4
set logfile  = $5
set mode = $6
#do a nccmp on a var if that option presents itself
#if ($argv > 6){
#	set varstring = "-v $7"
#}
#set test_args = "$argv[7-$#argv]"
#set test_args  = "${test_args}"
#set test_meaning = ""
#foreach i ( $test_args )
#	echo $i
#	set test_meaning = `echo "$test_meaning $i"`
#end
#echo $test_meaning

##...how on EARTH am I geting syntax errors from a check for file existance. HOW?
#echo "does the logfile exist? `-e $logfile`"
#if (-e $logfile) then
#do nothing
#else
	touch $logfile
	touch $summfile
#endif
set start_time = `date +%T`
echo "The start time for test $runcode is: $start_time"
echo "The start time for test $runcode is: $start_time" >> $logfile 

#Re-run the R code to see if it is being updated
if ($mode == 'runcode') then
	#Modify for new directory
	cp $runcode $newdir/runcode #This gets overwritten if there is more than one test, but I don't think that's a problem
	sed -i "s@<OUTPUT_DIR>@$newdir@g" $newdir/runcode
	#The runscript should get modified too, for responsibilty reasons
	#cp $runcode.script $newdir/runscript
	#chmod o+rx $newdir/runscript
	#sed -i "s@<OUTPUT_DIR>@$newdir@g" $newdir/runscript
	#Run the R runcode test
	echo "Running the R code test"
	echo "Rscript $newdir/runcode" >>& $logfile
	Rscript $newdir/runcode >>& $logfile
	#tcsh $newdir/runscript $newdir $newdir/runcode $logfile
	#set dsout = `tail $logfile -n 50 | grep -oP "Downscaled output file:\K.*"` #as opposed to version without /vftmp
	sleep 10
	set dsout = `tail $logfile -n 10 | grep -oP "Final Downscaled output file location:\K.*"`
	echo "nccmp -d $dsout $oldfile" >> $logfile
	nccmp -d $dsout $oldfile
	set ncc_status = $status
	echo "Status of the nccmp: $ncc_status" >> $logfile 
else if ($mode == 'xml') then
	echo "This can take up to 10 minutes to run, and will stop if it takes longer"
	#Need postproc command to work before trying to run a test
	#set outdir = dirname $logfile #Not sure this is neccessary. Try it and see if it breaks?
	set temp_file  = "$newdir/fudgelist.tmp"
	set tmp_stdout = "$newdir/fudgelist_out.tmp"
	#TODO: Set up the sed script replacement for the XMLs
	cp $runcode $newdir/xml
	if (! -e $newdir/dsout) then
		#mkdir $newdir/dsout/
		#mkdir $newdir/scripts/
	endif
	#removed 'dsout' and 'scripts' from create calls
	sed -i "s@OUTPUT_DIR@$newdir/@" $newdir/xml
	sed -i "s@SCRIPT_DIR@$newdir/@" $newdir/xml
	echo "python /$BASEDIR/bin/fudgeList.py -i $newdir/xml -o $temp_file -f > $tmp_stdout"
	python $BASEDIR/bin/fudgeList.py -i $newdir/xml -o $temp_file -f > $tmp_stdout
	set lons = `grep "lons" $tmp_stdout | sed -r 's/^.{7}//'` 
	set lone = `grep "lone" $tmp_stdout | sed -r 's/^.{7}//'` 
	set expected_file_count = `expr $lone - $lons + 1`
	if ($expected_file_count == 194 || $expected_file_count == 190) then
		set postproc_step = 0
	else
		set postproc_step = 1
	endif
	#
	#set full_out=`grep -oP 'output.path:\K.*' $temp_file`
	set outdirs=(`sed -e '/output.path:/,/spat.mask.path_1/\\!d' $temp_file`)
	#Edit first entry (will have string prefix) 
	set outcount=`echo $outdirs[1] | wc -m`
	set outcount=`expr $outcount - 12`
	set outdirs[1]=`echo $outdirs[1] | tail -c $outcount`
	#And remove the last dir entry (spat.mask.path_1:na)
	set outdirs_length=$#outdirs
	set outdirs_length=`expr $outdirs_length - 1`
	set outdirs=($outdirs[1-$outdirs_length])
	
	#set target_var = `grep -oP 'target = \K.*'`

	#set varname = `grep -oP '
	#full_out will point to OneD directory
	#Now, finally, run expergen
	echo "Calling expergen" >> $logfile
	#replaced $runcode with $newdir/xml
	echo "python $BASEDIR/bin/expergen.py -i $newdir/xml --msub" >> $logfile
	python $BASEDIR/bin/expergen.py -i $newdir/xml --msub >>& $logfile
	#Obtain the postproc command that will be sourced
	set scriptdir = `grep -oP 'Script directory is: \K.*' $tmp_stdout`
	#set pp_cmnd  = `tail -n 20 $logfile | grep -oP 'postProc command script location: \K.*'`
	#set pp_cmnd = "$scriptdir/postProc/msubwrapper"
	set pp_cmnd = "$scriptdir/postProc/postProc_command_1"
	echo $pp_cmnd
	source $pp_cmnd
	echo "Waiting 1 min; see if done"
	sleep 60
	set isdone = 1
	set attempts = 0
	while ( ${attempts} < 15 )
		echo "output dir 1: $outdirs[1]"
		set num_files = `ls $outdirs[1] | wc -l`
		echo "num_files: $num_files"
		if ($num_files >= $expected_file_count) then
			#Obtain the number of postprocc'ed files in ds output
			set pp_outdir = `dirname $outdirs[1]`
			set outdir_count = `ls $pp_outdir/*.nc | wc -l`
			echo "outdir_count: $outdir_count"
			if ( $postproc_step == 0 ) then
			#Entire thing here needs to be intented
				set pp_status = $outdir_count > 0
				echo "pp_status is: $pp_status"
				if ($pp_status) then #formerly had ==0 in here
					echo "done"
					set attempts = 20
					set isdone = 0
				#Error code associated with calling pp before complete; may apply to other things but need to check
			#	else if( ${pp_status} == 245 ) then 
			#		echo "notdone"
			#		set attempts = `expr ${attempts} + 1`
			#		echo "not done, waiting one more minute" >> $logfile
			#		sleep 60
				else
					source $pp_cmnd >>& $logfile
					echo "notdone"
					set attempts = `expr ${attempts} + 1`
					echo "not done, waiting one more minute" >> $logfile
					sleep 60
			#		echo "Other error present in postproc!" >> $logfile
			#		echo "Exit code of other error present: $pp_status" >> $logfile
			#		set attempts = 20 
				endif
			#else case for postproc cpmmand
			else if( $postproc_step == 1 ) then
				set attempts = 20
				set isdone = 0
			endif	
		else
			echo "not done yet"
			set attempts = 	`expr ${attempts} + 1`
			echo "not done, waiting one more minute" >> $logfile
			sleep 60
		#Endif for files not done yet	
		endif	
	end
	if ( $isdone == 0 ) then
		#The new directory structure is going to make this SO MUCH EASIER, because it doesn't have the version dir
		#set dsout_dir = $full_out/../../
		#set dsout_dir = `dirname $full_out`
		set dsout_dir = `dirname $outdirs[1]`
		echo "dsout_dir: $dsout_dir"
		#For the moment, just assume that you will NEVER run the qcmasks for the regression tests. 
		#It should be accurate enough to buy some room to test things
		#set dsout = `find $dsout_dir`
		ls $dsout_dir
		set dsout = `ls $dsout_dir/*.nc`
		echo $dsout
		echo "nccmp -d $dsout $oldfile" >> $logfile
		nccmp -d $dsout $oldfile
		set ncc_status = $status
		echo "Status of the nccmp: $ncc_status" >> $logfile 
	else
		set dsout = "`dirname $dsout`/*.nc"
		echo $dsout
		echo "Time out error: After 15 minutes, script has not completed. Please check out dir for more information"
		echo "Time out error: After 15 minutes, script has not completed. Please check out dir for more information" >> $logfile
		set ncc_status = 100 
	endif
endif
#And if it passes the test, *and* the file was actually written to disk....
#Code for formatting summary file
set test_str = `printf "%-20s" ${runcode}`
set testtime = `printf "%-16s" ${start_time}`
echo $ncc_status
if ( $ncc_status == 0 ) then
	set statstring = `printf "%-10s" "PASSED"`
	set linevar = `echo "$test_str $statstring $testtime $dsout $oldfile"`
	#echo `printf $test_str, $statstring, $testtime, $test_meaning, "\n"` >> $summfile
	echo $linevar >> $summfile
	exit 0
else
	set statstring = `printf "%-10s" "FAILED"`
	#echo `printf $test_str, $statstring, $testtime, $test_meaning, "\n"` >> $summfile
	set linevar = `echo "$test_str $statstring $testtime $dsout $oldfile"`
	#echo `printf $test_str, $statstring, $testtime, $test_meaning, "\n"` >> $summfile
	echo $linevar >> $summfile
	exit 1
endif
