#!/bin/tcsh
#Concatenates a set of files sent in as a long array
#into a single file, along the dimensions specified
#in the input

#set perm_in = "y,x,ensmem,time"
#set perm_out = "x,y,ensmem,time"
#set in_files = "/home/cew/Code/testing/temp_file.nc,/home/cew/Code/testing/temp_file.txt"
#set has_record = 0

#echo $argv
#echo "length of argv:"
#echo $#argv

source /usr/local/Modules/3.1.6/init/csh
#set echo

set perm_in = $argv[1]
#lat,time,bnds,lon
#set perm_in = "lon,time,lat"
#set perm_out = "time,lat,lon"
set perm_out = $argv[2]
set in_dir = $argv[3]
set in_files = $argv[4]
set tdir = $argv[5]
set outfile = $argv[6]
if ($#argv > 6) then
  set ncores = $argv[7]
else
  set ncores = 1
endif

#need to add c-shell check for file existance
rm -rf $tdir/*
mkdir $tdir/input
mkdir $tdir/scratch
mkdir $tdir/log
mkdir $tdir/status
mkdir $tdir/scripts
mkdir -p `dirname $outfile`
set out_fname = `basename $outfile`
set tmp_outfile = $tdir/$out_fname

#rootdir is used to get the dir of the parllel execute wrapper
set rootdir = `dirname $0`

##If the files have a record dimension, then the record dimension needs to go
# first when ncrcat is called.
echo "writing $tmp_outfile"
##If, however, there is not a record dimension and you use ncrcat, you need
#to make a record dimension your last dimension, know where that dimension
#is expected to go in the output, and place it there as your last step.
#If you're working within FUDGE, this is not a problem you have to worry about.

set file_list = `echo $in_files:q | sed 's/,/ /g'`
set i = 1
foreach el ($file_list)
	gcp $in_dir/$el $tdir/input/$el
	set i_str = `printf %03d $i`
  touch $tdir/log/${i_str}.txt
  touch $tdir/scripts/${i_str}.tcsh
  #tcsh $rootdir/execute_parallel.tcsh "ncpdq -a $perm_in -O $tdir/input/$el $tdir/scratch/tmpfile_${i_str}.nc >> $tdir/log/${i_str}.txt" $tdir/status/${i_str} &
  echo "set status_root = $tdir/status/${i_str}\n ncpdq -a $perm_in -O $tdir/input/$el $tdir/scratch/tmpfile_${i_str}.nc >> $tdir/log/${i_str}.txt" >> $tdir/scripts/${i_str}.tcsh
  echo 'set commandstat = $status \n  if ( $commandstat == 0) then \n    touch ${status_root}.success \n  else \n    touch ${status_root}.failure\n  endif' >> $tdir/scripts/${i_str}.tcsh
  @ i = $i + 1
end
set i = 1
foreach el (`ls $tdir/scripts/`)
  #echo "$tdir/scripts/${i_str}.tcsh"
  tcsh $tdir/scripts/$el &
  set mod = `expr $i % $ncores`
  if ( $mod == 0 ) then
    wait
    #Check for failures
    set fail_files = `ls -a $tdir/status/ | grep 'failure' `
    set any_failures = $status
    #echo "any_failures: $any_failures"
    if ( $any_failures == 0 ) then
        foreach f ( $fail_files )
          set fail_str = `echo $f | sed -e s:.failure::g `
          set fail_log = $tdir/log/${fail_str}.txt
          #Write output, indended
          #sed -i $fail_log '1 \TAB i\'
          #echo "$fail_log:"
          #cat $fail_log | sed -e s:\n:\n\TAB:g | sed -i
          cat $fail_log
        end
        echo "One or more minifile permutations failed; please check stdout"
        exit 1
    endif
  endif
  @ i = $i + 1
  sleep 2
end
#Need a second wait after loop because not everything divides evenly
wait
#Check for failures
set fail_files = `ls -a $tdir/status/ | grep 'failure' `
set any_failures = $status
#echo "any_failures: $any_failures"
if ( $any_failures == 0 ) then
    echo $fail_files
    foreach f ( $fail_files )
      set fail_str = `echo $f | sed -e s:.failure::g `
      echo $fail_str
      set fail_log = $tdir/log/${fail_str}.txt
      #Write output, indended
      #sed -i $fail_log '1 \TAB i\'
      #echo "$fail_log:"
      #cat $fail_log | sed -e s:\n:\n\TAB:g | sed -i
      cat $fail_log
    end
    echo "One or more minifile permutations failed; please check stdout"
    exit 1
endif

#Concatenate the temporary permuted files
#echo "ncrcat"
#echo "ncrcat -h $tdir/scratch/tmpfile*.nc $tdir/all_unperm_out.nc"
ncrcat -h -O $tdir/scratch/tmpfile*.nc $tdir/all_unperm_out.nc

#Permute concantenated file
ncpdq -h -a $perm_out -O --header_pad 10000 $tdir/all_unperm_out.nc $tdir/all_perm_out.nc

#echo "make record dimension"
ncks -h -O --mk_rec_dmn 'time' --header_pad 10000 $tdir/all_perm_out.nc $tmp_outfile

#Copy to the final output location
gcp $tmp_outfile $outfile
