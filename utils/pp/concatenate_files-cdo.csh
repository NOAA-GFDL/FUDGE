#!/bin/tcsh
#Concatenates a set of files sent in as a long array
#into a single file, along the dimensions specified
#in the input

source /usr/local/Modules/default/init/tcsh
#module use /app/spack/v0.15/modulefiles/linux-rhel7-x86_64/
module load cdo
module load netcdf-c

#/work/cew/testing/file_chunks/
set in_dir = $argv[1]
#out.I1-10.J5-8.nc,out.I11-15.J5-8.nc,out.I16-20.J5-8.nc,out.I21-30.J5-8.nc
set in_files=$argv[2]
#$TMPDIR/lat_cat/
set tdir = $argv[3]
#/work/cew/testing/final_concatenated_out.nc
set outfile = $argv[4]
#original_file_with_complete_grid_not_always_same_var.nc
set gridsource = $argv[5]

rm -rf $tdir/*
mkdir $tdir/input
set out_fname = `basename $outfile`
set tmp_outfile = $tdir/$out_fname

cd $in_dir
set file_list = `echo $in_files:q | sed 's/,/ /g'`
# dmget all files before gcp command
#honestly, the way we call this it probably isn't neccessary
#but I'd rather be safe than sorry for our archive files
#dmget $file_list

foreach el ($file_list)
	ln -s $in_dir/$el $tdir/input/$el
end

echo "cdo -v --no_history collgrid $tdir/input/* $tmp_outfile >& $tdir/collgrid_out.txt"
cdo -v --no_history collgrid $tdir/input/* $tmp_outfile >& $tdir/collgrid_out.txt
set collstat = $status
if ( $collstat != 0 ) then
  echo "Error in concatenate_files-cdo! cdo concatenation failed!"
  exit 1
endif

grep "calendar >julian< unsupported!" $tdir/collgrid_out.txt
set is_julian = $status

cdo griddes $gridsource > $tdir/griddesc.txt
cdo --hdr_pad 300000 setgrid,$tdir/griddesc.txt $tmp_outfile $tdir/griddesc_out.nc

set metafile = $file_list[1]
echo `date`

ncks -m --trd $metafile | grep ": type" | cut -f 1 -d ":" | sort > $tdir/orig_vars.txt
ncks -m --trd $tdir/griddesc_out.nc | grep ": type" | cut -f 1 -d ":" | sort > $tdir/cat_vars.txt


echo "ncks to copy missing/incomplete vars that are not dimension-related"
echo `date`
#find vars that are not present in the concatenated file but are present in the metadata file
set vars_not_pres = `grep -F -x -v -f $tdir/cat_vars.txt $tdir/orig_vars.txt`
if ($#vars_not_pres > 0) then
   set varstr = $vars_not_pres[1]
   if ($#vars_not_pres > 1) then
      foreach v (`seq 2 $#vars_not_pres`)
         echo "appending var $vars_not_pres[$v]"
         set varstr = "${varstr},$vars_not_pres[$v]"
      end
   endif
   echo $varstr
   ncks --hdr_pad 300000 -h --no_abc -v $varstr -A $metafile $tdir/griddesc_out.nc
endif


  echo "ncks to copy data from lat,lon, respective bnds"
#look, this would be elegant but it doesn't seem to work - the entire file is getting rewritten
#maybe ncks rewrites files regardless?
  ncks -h -A -v lat,lon $gridsource $tdir/griddesc_out.nc

##copy over any bnds vars
#ncks -m --trd $metafile | grep ": type" | grep "_bnds" | cut -f 1 -d ":" | sort > #$tdir/bnds_vars.txt
#set varlist = `cat $bnds_vars.txt`
#set varstr = `echo $varlist | sed -e s:\n:,:g`
#ncks -h -A -v lat,lon $gridsource $tdir/griddesc_out.nc

 echo `date`
#I'm not happy with hard-coding lon_bnds and lat_bnds above, but honestly? data standardization.

 echo "ncks to copy metadata for lat,lon, respective bounds"
 echo `date`
##set varlist = `cat $tdir/orig_vars.txt`
##set varstr = `echo $varlist | sed -e s:\ :,:g`
#Since this is picking up metadata for grid vars, it needs to be from gridsource -
#but also shouldn't include the variable that we are concatenating on, because
#that is how we get the *correct* missval set in the R code overridden with an
#incorrect missval.
#Currently, the metadata for the above varlist/varstr combo is
#not doing that and I'm not sure how to fix it.
## For now, I am hard-coding lat/lon/respective bounds for data standardization
 set varstr = "lat,lon,lat_bnds,lon_bnds"
 ncks -A -C -H --no-abc -v $varstr $gridsource $tdir/griddesc_out.nc >& $tdir/ncks_stdout.stderr.txt

 grep -q "ERROR" $tdir/ncks_stdout.stderr.txt
 set grepstat = $status

if ( $grepstat == 0 ) then
  foreach line ( "`cat $tdir/ncks_stdout.stderr.txt`" )
    echo $line > $tdir/line_tmp.txt
    grep -F -q 'reports user-supplied variable name or regular expression' $tdir/line_tmp.txt
    set lgrepstat = $status
    if ( $lgrepstat != 0 ) then
      echo $line
    endif
  end
endif
#ncks -A -C -H -v one,three_dmn_rec_var ~/in2.nc ~/out.nc

##copy all globals from the metafile - this includes the ISD-lite atts, if included
#ncks -A -x $metafile $tdir/griddesc_out.nc


#This command can safely be last due to a padded header
echo "calendar checks"
if ($is_julian == 0) then
  ncatted -h -a calendar,time,o,c,julian -O $tdir/griddesc_out.nc
else
  set calstring = `ncks -m --trd $tdir/griddesc_out.nc | grep ': calendar,'`
  echo $calstring | grep '365_day'
  set is_365 = $status
  if ($is_365 == 0) then
    #set calval = `ncks -m --trd $metafile | grep ': calendar,' | grep -P "(?<=value = ).*"`
    #Have NOT been able to get grep to work. Horrible series of cuts and reverses works in stead.
    set calval = `ncks -m --trd $metafile | grep ': calendar,' | cut -f 3 -d ',' | rev | cut -f 1 -d ' ' | rev`
    #should change for ALL instances of calendar
    echo $calval
    echo "ncatted -h -a calendar,,m,c,$calval -O $tdir/griddesc_out.nc"
    ncatted -h -a calendar,,m,c,$calval -O $tdir/griddesc_out.nc
  endif
endif

#remove subsetspecs from the file
ncatted -h -a subsetspecs,global,d,, -O $tdir/griddesc_out.nc

#Step 6: Copy to the final output location
echo "final copy of $tdir/griddesc_out.nc to $outfile"
gcp -cd $tdir/griddesc_out.nc $outfile
