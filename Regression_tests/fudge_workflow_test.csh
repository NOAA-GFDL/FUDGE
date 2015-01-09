# !/bin/tcsh -f -e
#
# Sample script calling a set of unit tests to run
# against FUDGE code before it is pushed to a remote repo
# Note: this code runs on all branches
# Carolyn Whitlock, November 2014

source /etc/profile.d/modules.csh
module load nco
module load nccmp
module load R
module load python
#source /home/esd/local/.cshrc_esd
#module load fudge/cinnamon
#source setenv_fudge

echo "Location of FUDGE repo is: $BASEDIR"


echo "Starting tests. Be warned, this takes more than ten minutes to complete"

echo "Entire workflow test (~15 min. or more)"
set starttime = `date '+%X'`
echo "Start time: $starttime"

#Note to self: At some point, see how the code reacts to replacing the runcode with an XML and a call to expergen
#Will probably add time - but if it's a single file, it might not be much

#Add check to see if there is enough space in /work or directory of choice before making the temp file

set start_sed  = `echo $starttime | sed "s@:@.@g"`
set TMPDIR = /work/$USER/$start_sed/

mkdir -p $TMPDIR
echo "Default temp file directory for output and scritps: $TMPDIR" 

#set tasmax_xml = /home/cew/Code/test_xml/sample_regtest.xml #make sure universal permissions on this, and add to regression_tests
set tasmax_xml = $BASEDIR/Regression_tests/xml/sample_regtest.xml
gcp -cd $tasmax_xml $TMPDIR/$tasmax_xml
echo "Location of XML: $TMPDIR/$tasmax_xml"
sed -i "s@TMPDIR@$TMPDIR@g" $TMPDIR/$tasmax_xml
set tasmax_xml = $TMPDIR/$tasmax_xml

#obtain output OneD directory from FudgeList.py
set temp_file  = "$TMPDIR/fudgelist.tmp"
echo "/$BASEDIR/bin/fudgeList.py -i $tasmax_xml -o $temp_file -f"
python "/$BASEDIR/bin/fudgeList.py" -i $tasmax_xml -o $temp_file -f

set outdir=`grep 'output.path' $temp_file`
echo $outdir
set outcount=`echo $outdir | wc -m`
set outcount=`expr $outcount - 12`
set outdir=`echo $outdir | tail -c $outcount`
#set outdir=`dirname "$outdir" | xargs dirname | xargs dirname | xargs dirname`

#Now, activate expergen
echo "Activating expergen (~6 minutes)"
python "$BASEDIR/bin/expergen.py" -i $tasmax_xml --msub
echo "Starting job submission (up to 6 min., usually more like 3)"
echo "Waiting 1 min; see if done"
sleep 60
set isdone = 1
set attempts = 0
while (${attempts} < 8)
	#set dircount = `ls /work/cew/testing//downscaled/NOAA-GFDL/MPI-ESM-LR/rcp45/day/atmos/day/r1i1p1/v20111006/RRtxp1-CDFt-B34at-regtest1L01K00/tasmax/RR/OneD/v20140108/ | wc -l`
	set dircount = `ls $outdir | wc -l`
	echo "on try $attempts of 7"
	echo "$dircount"
	if (${dircount} < 190) then
		set minicount = `expr $dircount - 1`
		echo "Process not complete: $minicount/190 tasmax minifiles written. Waiting one more minute."
		echo $attempts
		set attempts = `expr ${attempts} + 1`
		echo $attempts
		sleep 60
		echo "after sleep command"
	else
		echo "190/190 tasmax minifiles written."
		set qc_count = `ls $outdir/tasmax_qcmask | wc -l`
		echo "$qc_count"
		if(${qc_count} < 190) then
			
			echo "Process not complete; $qc_count/190 qc mask files written. Waiting one more minute."
			set attempts = `expr ${attempts} + 1`
			sleep 60
		else
			echo "190/190 qcmask minifiles complete."
			set attempts = 10
			set isdone = 0
		endif
	endif
end

echo "isdone status: $isdone"
if (${isdone} != 0) then
	echo "Minifile writing not complete; exiting. Please check code and try again."
	echo "Removing up temporary directory $TMPDIR. Press ctrl +c to stop." 
	rm -rf $TMPDIR
	exit 1
endif

set gentime = `date '+%X'`
echo "Expergen + msub ran from $starttime to $gentime"

#module unload python/2.7.3
#module load python/2.7.1

echo "Starting postProc (aka post-post-processing) (~5 minutes)"
echo "python $BASEDIR/bin/postProc -fi $tasmax_xml -v tasmax,tasmax_qcmask"
python "$BASEDIR/bin/postProc" -fi $tasmax_xml -v tasmax,tasmax_qcmask

echo "Checking for conformance with the older tasmax file" 
set tasmax_old = /work/cew/testing/regression_tests/tasmax_all_regtest.nc
#Cannot keep as part of git repo; it's simply too big
#set tasmax_old = $BASEDIR/Regression_tests/tasmax_all_regtest.nc
#nccmp -d $tasmax_old /work/cew/testing//downscaled/NOAA-GFDL/MPI-ESM-LR/rcp45/day/atmos/day/r1i1p1/v20111006/RRtxp1-CDFt-B34at-regtest1L01K00/tasmax/RR/OneD/v20140108//tasmax_qcmask//../../../../RR/v20140108/tasmax_day_RRtxp1-CDFt-B34at-regtest1L01K00_rcp45_r1i1p1_RR_20060101-20991231.nc
nccmp -d $tasmax_old $outdir/tasmax_qcmask//../../../../RR/v20140108/tasmax_day_RRtxp1-CDFt-B34at-regtest1L01K00_rcp45_r1i1p1_RR_20060101-20991231.nc
set ncc_status = $status
if (${ncc_status} != 0) then
	echo "Error in tasmax data: nccmp exited with a status of $ncc_status"
	echo "Removing up temporary directory $TMPDIR. Press ctrl +c to stop." 
	rm -rf $TMPDIR
	exit 1
endif
echo "tasmax file matches previous output"

echo "Checking for conformance with the older tasmax_qcmask file"
set tasmax_qcmask_old = /work/cew/testing/regression_tests/tasmax_qcmask_all_regtest.nc
#set tasmax_qcmask_old = $BASEDIR/Regression_tests/tasmax_qcmask_all_regtest.nc
#/work/Carolyn.Whitlock/10.36.20//unit_tests/dsout//downscaled/NOAA-GFDL/MPI-ESM-LR/rcp45/day/atmos/day/r1i1p1/v20111006/RRtxp1-CDFt-B34at-regtest1L01K00/tasmax/RR/OneD/v20140108/
nccmp -d $tasmax_qcmask_old $outdir/tasmax_qcmask//../../../../RR/v20140108/tasmax_qcmask_day_RRtxp1-CDFt-B34at-regtest1L01K00_rcp45_r1i1p1_RR_20060101-20991231.nc
set ncc_status = $status
if (${ncc_status} != 0) then
	echo "Error in qcmask data: nccmp exited with a status of $ncc_status"
	echo "Removing up temporary directory $TMPDIR. Press ctrl +c to stop." 
	rm -rf $TMPDIR
	exit 1
endif
echo "tasmax_qcmask file matches previous output"

echo "Entire workflow test passed. Congratulations!"
set endtime = `date '+%X'`
echo "Script ran from $starttime to $endtime"
echo "Removing up temporary directory $TMPDIR. Press ctrl +c to stop." 
rm -rf $TMPDIR
