#!/bin/tcsh -f
#Minimal script for adding ensemble data together
#needed to allow module loads when not using current environment
source /usr/local/Modules/default/init/csh
#module list
module load netcdf/4.2
#module unload nco
module load nco/4.3.1
#module unload ncarg
module load ncarg/6.1.2
#module load gcp/2.3
#module list
#Please note that the scraper script uses the default python install. If cdat
#is loaded, the python script will fail.
set outFile = $argv[1]
set climvar = $argv[2]
set rips = $argv[3]
set dataSource = $argv[4]
set epoch = $argv[5]
set trange = $argv[6]
set orig_inFiles = $argv[7]
set inFiles = $argv[8]
set enstype = $argv[9]
if ($enstype == "CV") then
	echo "Making cross-validated ensemble"
	set cvtype = $epoch
endif
set inFiles_array = `echo $inFiles | awk 'BEGIN{FS=","}{for (i=1; i<=NF; i++) print $i}'`

#set echo
#echo $outFile
#echo $climvar
#echo $rips
#echo $dataSource
#echo $trange
#echo $inFiles

#set inFiles_short = $inFiles_array
#foreach i (`seq $#inFiles_array`)
#	ls $inFiles_array[$i]
#	set inFiles_short[$i] = "`basename $inFiles_array[$i]`"
#end
#print with commas, remove last comma
#set inFiles_string = "`printf -- "%s," $inFiles_array | sed 's/.$//'`"
#ls `dirname($outFile)`

echo "ncecat --ovr -v $climvar $inFiles_array -o $outFile"
ncecat --ovr -v $climvar $inFiles_array -o $outFile
ncrename -h -d record,ensmem $outFile
ncdump -h $outFile |grep plev
  
  if ($status == 0) then
    ncpdq -O -h -v $climvar -a time,ensmem,plev,lat,lon $outFile tmp.nc
  else
    ncpdq -O -h -v $climvar -a time,ensmem,lat,lon $outFile tmp.nc
  endif
  \mv tmp.nc $outFile
  
  #echo ">>>> use nco tools to refine new file contents "
  set vars2copy = "i_offset,j_offset,lat_bnds,lon_bnds,time_bnds"
  if("$climvar" == "tasmax" || "$climvar" == "tasmin") then
     set vars2copy = "$vars2copy,height"
  endif
  ncks -A -h -v $vars2copy $inFiles_array[1]  $outFile
  
  if ($enstype == "CV") then
  	echo "in cross-validated case"
  	set fname = `basename $outFile`
  	echo $fname
  	set exp_name = `echo $fname | cut -d "_" -f 3`
    echo $exp_name
    ncatted -O -h -a comment,global,a,c,"; \nEnsemble file of $climvar; ensmem= 1 to $#inFiles_array, in order, are from $orig_inFiles" $outFile
    ncatted -O -a title,global,o,c,"$cvtype $exp_name $trange" $outFile
  	ncatted -O -h -a gfdl_experiment_name,global,o,c,"$exp_name" $outFile
  else
  	ncatted -O -h -a comment,global,a,c,"; \nEnsemble file of $climvar containing data from $#inFiles_array files: $orig_inFiles" $outFile
  	ncatted -O -a title,global,o,c,"$dataSource $epoch $trange" $outFile
  endif

# the realization attribute is in all CMIP5 and GFDL model metadata, but "gfdl_experiment_name" is only in GFDL's model data. 
# execute separately so that if either is missing from netCDF files, the other edit is still executed.
  ncatted -O -h -a realization,global,d,, -a realizations,global,c,c,""$rips"" $outFile
  #changed from deleting this attribute 6-24-16; very useful for some metadata opts in section 8
  #ncatted -O -h -a gfdl_experiment_name,global,c,c, $outFile
