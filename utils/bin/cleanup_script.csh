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
set tmp_out = "$TMPDIR/fudgelist.out"

python "/$BASEDIR/bin/fudgeList.py" -i $xmlpath -o $temp_file -f > $tmp_out

#set scriptdir=`( python "/$BASEDIR/bin/fudgeList.py" -i $xmlpath -o $temp_file -f ) | grep sroot`
set scriptdir=`grep "Script directory is" $tmp_out`
echo $scriptdir
set scriptcount=`echo "$scriptdir" | wc -m`
set scriptcount=`expr $scriptcount - 21`
set scriptdir=`echo $scriptdir | tail -c $scriptcount`

set scriptdir=`dirname "$scriptdir"`
echo $scriptdir

#set outdir=`grep 'output.path' $temp_file`
#echo $outdir
#set outcount=`echo $outdir | wc -m`
#set outcount=`expr $outcount - 12`
#set outdir=`echo $outdir | tail -c $outcount`
#set outdir=`dirname "$outdir" | xargs dirname | xargs dirname ` #| xargs dirname
#echo "output path: $outdir"

#set outdirs=(`grep -E -o 'output.path:.*' /home/cew/Code/testing/espresso_sample_experiment_info.txt`)
set outdirs=(`sed -e '/output.path:/,/spat.mask.path_1/\\!d' $temp_file`)
#Edit first entry (will have string prefix) 
set outcount=`echo $outdirs[1] | wc -m`
set outcount=`expr $outcount - 12`
set outdirs[1]=`echo $outdirs[1] | tail -c $outcount`
#And remove the last dir entry (spat.mask.path_1:na)
set outdirs_length=$#outdirs
set outdirs_length=`expr $outdirs_length - 1`
set outdirs=($outdirs[1-$outdirs_length])
#Loop over all present dir entries
set dircount=1
while ($dircount <= $outdirs_length)
	set outdirs[$dircount]=`dirname "$outdirs[$dircount]" | xargs dirname | xargs dirname `
	set dircount=`expr $dircount + 1`
end
echo $outdirs

set exp_name=`grep 'ds.experiment' $temp_file`
set expcount=`echo $exp_name | wc -m`
set expcount=`expr $expcount - 14`
set exp_name=`echo $exp_name | tail -c $expcount`
echo $exp_name

if ( $opt == 'd') then
		set dircount=1
		while ($dircount <= $outdirs_length)
			if (-e $outdirs[$dircount] ) then
				echo "Deleting $outdirs[$dircount]"
				rm -r $outdirs[$dircount]
			else echo "$outdirs[$dircount] could not be removed; does not exist. Has the experiment been run yet?"
			endif
			set dircount=`expr $dircount + 1`
		end
	if (-e $scriptdir/$exp_name) then
		rm -r $scriptdir/$exp_name
	else echo "$scriptdir/$exp_name could not be removed; does not exist"
	endif
else if ($opt == 'm') then
	echo "Move option activated; move in progress"
	#Count number of experiments existing on system with that filename
	#Counting from first member is pretty safe bet; can count higher later.
	set num_exp_existing=`dirname $outdirs[1] | xargs ls | grep $exp_name | wc -l`
	#set num_exp_existing=7
	set is_written = 3
	while( $is_written == 3)
		set suffix="~$num_exp_existing"
		set new_scriptdir="$scriptdir/$exp_name$suffix/"
		set outdir_exists = 0
		foreach outdir ($outdirs)
			echo $outdir
			#ls $outdir$suffix
			if (-e "$outdir$suffix") then
			#if ($status != 0) then
				echo 'inside if loop'
				set outdir_exists=`expr $outdir_exists + 1`
			endif
		end	
		echo "after existance check"	
		if (-e $new_scriptdir | $outdir_exists > 0) then
			echo "Error in move option: dir $new_scriptdir or $outdir_exists new output dirs already exists. Trying a new name" 
			set num_exp_existing=`expr $num_exp_existing + 1`
		else
			set outdir_written = 0
			foreach outdir ($outdirs)
				if (-e $outdir) then
					echo "Moving older output from $outdir to $outdir$suffix"
					mv $outdir $outdir$suffix
					set outdir_written=`expr $outdir_written + $status`
				else
					echo "No directory $outdir found; no move performed"
					set outdir_written=`expr $outdir_written + 0`	
				endif
			end		
			if (-e $scriptdir/$exp_name) then
				echo "Moving older ouput from $scriptdir/$exp_name to $new_scriptdir"
				mv $scriptdir/$exp_name $new_scriptdir
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
