#! /bin/csh -f
#cleanup_script.csh
#cleans up the output directories, assuming a consistent structure
#At present, takes a path to an XML as an argument
#And is assumed to run on the analysis nodes

#set outdir = $1
#set scriptdir = $2
# parentdir, project, experiment_name

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
set outdir=`dirname "$outdir" | xargs dirname | xargs dirname ` #| xargs dirname
echo "output path: $outdir"

set exp_name=`grep 'ds.experiment' $temp_file`
set expcount=`echo $exp_name | wc -m`
set expcount=`expr $expcount - 14`
set exp_name=`echo $exp_name | tail -c $expcount`
echo $exp_name

#set project=`grep 'project_ID' $temp_file`
#set regcount=`echo $project | wc -m`
#set regcount=`expr $regcount - 12` #'project_ID = '
#set project=`echo $project | tail -c $regcount`
set project = `echo $exp_name | head -c 2`
echo "the project ID is: $project"



if ( $opt == 'd') then
	if (-e $outdir) then
		rm -r $outdir
	else echo "$outdir could not be removed; does not exist. Has the experiment been run yet?"
	endif
	if (-e $scriptdir/scripts/$project/$exp_name) then
		rm -r $scriptdir/scripts/$project/$exp_name
	else echo "$scriptdir/scripts/$project/$exp_name could not be removed; does not exist"
	endif
else if ($opt == 'm') then
	echo "Move option activated; move in progress"
	#Count number of experiments existing on system with that filename
	set num_exp_existing=`dirname $outdir | xargs ls | grep $exp_name | wc -l`
	set is_written = 3
	while( $is_written == 3)
		set suffix="~$num_exp_existing"
		set new_outdir="$outdir$suffix"
		set new_scriptdir="$scriptdir/scripts/$project/$exp_name$suffix"
		if (-e $new_scriptdir | -e $new_outdir) then
			echo "Error in move option: dir $new_outdir or dir $new_scriptdir already exists. Trying a new name" 
			set num_exp_existing=`expr $num_exp_existing + 1`
#		if (-e $new_scriptdir) then
#			echo "Error in move option: dir $new_scriptdir already exists. Please check and delete."
#			exit 1
		else
			if (-e $outdir) then 
				echo "Moving older output from $outdir to $new_outdir"		
				mv $outdir $new_outdir
				set outdir_written=$status				
			else
				echo "No directory $outdir found; no move performed"
				set outdir_written=0
			endif
			if (-e $scriptdir/scripts/$project/$exp_name) then
				echo "Moving older ouput from $scriptdir/scripts/$project/$exp_name to $new_scriptdir"
				mv $scriptdir/scripts/$project/$exp_name $new_scriptdir
				set scriptdir_written=$status
			else
				echo "No directory $scriptdir found; no move performed"
				set scriptdir_written=0
			endif
			set is_written=`expr $outdir_written + $scriptdir_written`
			if ($is_written>0) then
				echo "One or more of the output or script directories existed and failed to be moved; please check directories" 
				exit $is_written
			endif
		endif	
	end
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


