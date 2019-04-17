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

set perm_in = $argv[1]
#lat,time,bnds,lon
#set perm_in = "lon,time,lat"
#set perm_out = "time,lat,lon"
set perm_out = $argv[2]
set in_dir = $argv[3]
set in_files = $argv[4]
set tdir = $argv[5]
set outfile = $argv[6]

#need to add c-shell check for file existance
rm -rf $tdir/*
mkdir $tdir/input
mkdir $tdir/scratch
set out_fname = `basename $outfile`
set tmp_outfile = $tdir/$out_fname
##If the files have a record dimension, then the record dimension needs to go first when
#ncrcat is called.

##If, however, there is not a record dimension and you use ncrcat, you need
#to make a record dimension your last dimension, know where that dimension
#is expected to go in the output, and place it there as your last step.

set file_list = `echo $in_files:q | sed 's/,/ /g'`
set i = 1
foreach el ($file_list)
	#echo $el
	gcp $in_dir/$el $tdir/input/$el
	set i_str = `printf %03d $i`
	ncpdq -a $perm_in -O $tdir/input/$el $tdir/scratch/"tmpfile_${i_str}.nc"
	#echo $i
	@ i = $i + 1
end

#Concatenate the temporary permuted files
#echo "ncrcat"
echo "ncrcat -h $tdir/scratch/tmpfile*.nc $tdir/all_unperm_out.nc"
ncrcat -h -O $tdir/scratch/tmpfile*.nc $tdir/all_unperm_out.nc

#Permute concantenated file
#echo "ncpdq"
#echo "$tdir/all_unperm_out.nc"
#echo $perm_out
ncpdq -h -a $perm_out -O --header_pad 10000 $tdir/all_unperm_out.nc $tdir/all_perm_out.nc

#echo "make record dimension"
ncks -h -O --mk_rec_dmn 'time' --header_pad 10000 $tdir/all_perm_out.nc $tmp_outfile

#Copy to the final output location
#echo "final copy"
gcp -cd $tmp_outfile $outfile
