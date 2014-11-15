#! /bin/csh -f
#cleanup_script.csh
#cleans up the output directories, assuming a consistent structure
#At present, takes a path to an XML as an argument
#And is assumed to run on the analysis nodes

#set outdir = $1
#set scriptdir = $2
# parentdir, project, experiment_name

unset echo

set opt = $1

set xmlpath = $2

#alias fudgeList = "$BASEDIR/bin/fudgeList.py"

if ( `echo $HOSTNAME | grep "an"` == "" ) then
	echo "$HOSTNAME is not an analysis node. Please log into an analysis node and start the script again." 
	exit 1
endif

set temp_file  = "$TMPDIR/fudgelist.tmp"

set scriptdir=`( python "/$BASEDIR/bin/fudgeList.py" -i $xmlpath -o $temp_file -f ) | grep "sroot"` 
set scriptcount=`echo "$scriptdir" | wc -m`
set scriptcount=`expr $scriptcount - 8`
set scriptdir=`echo $scriptdir | tail -c $scriptcount`
echo $scriptdir

set outdir=`grep 'output.path' $temp_file`
echo $outdir
set outcount=`echo $outdir | wc -m`
set outcount=`expr $outcount - 12`
set outdir=`echo $outdir | tail -c $outcount`
echo $outdir

set exp_name=`grep 'ds.experiment' $temp_file`
echo $exp_name
set expcount=`echo $exp_name | wc -m`
set expcount=`expr $expcount - 14`
set exp_name=`echo $exp_name | tail -c $expcount`
echo $exp_name



if ( $opt == 'd') then
	if (-e $outdir) then
	rm -r $outdir
	else echo "$outdir could not be removed; does not exist"
	endif
	if (-e $scriptdir/scripts/RR/$exp_name) then
	rm -r $scriptdir/scripts/RR/$exp_name
	else echo "$scriptdir could not be removed; does not exist"
	endif
else if ($opt == '-m') then
	echo "Move option is not yet supported"
endif

exit 0



#set out_dir_pref = "/archive/esd/PROJECTS/DOWNSCALING/RR/downscaled/"
#set out_var = "tasmin"
#set version = "v20140108"
#set out_dir = "$out_dir_pref/$method"

#foreach (file $outdir/*)
#	if (-d $file)
#		rm $file/*
#	else
#		rm $file
#
#	endif
#end

##And remove the files in /archive

#rm -rf "$scriptdir/config"
#rm -rf "$scriptidr/log"
#rm -rf "$scriptdir/master"
#rm -rf "$scriptdir/runcode"
#rm -rf "$scriptidr/runscript"
#rm -rf "$scriptdir/experiment_info.txt"

#foreach (file $scriptdir/*)
#	if (-d $file)
#		rm $file/*
#	else
#		rm $file
#	endif
#end

#"/archive/esd/PROJECTS/DOWNSCALING/"
#"RR/downscaled/NOAA-GFDL/MPI-ESM-LR/rcp85/day/atmos/day/r1i1p1/v20111014/RRtnp1-CDFt-B38atL01K00/tasmin/RR/v20140108/tasmin_day_RRtnp1-CDFt-B38atL01K00_rcp85_r1i1p1_RR_20060101-20991231.old.nc"


