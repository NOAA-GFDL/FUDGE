#!/bin/tcsh -f
#Minimal script for adding ensemble data together
#needed to allow module loads when not using current environment
source /usr/local/Modules/default/init/csh
#module list
module unload nco
module load nco/4.3.1
module unload ncarg
module load ncarg/6.2.1
module unload gcp
module load gcp/2.3
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
set inFiles_array = `echo $inFiles | awk 'BEGIN{FS=","}{for (i=1; i<=NF; i++) print $i}'`

#set echo
echo $outFile
echo $climvar
echo $rips
echo $dataSource
echo $trange
echo $inFiles

foreach i (`seq $#inFiles_array`)
	ls $inFiles_array[$i]
end
ls `dirname($outFile)`

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

  ncatted -O -h -a comment,global,m,c,"Ensemble file of $climvar containing data from $#inFiles_array files: $orig_inFiles" -a title,global,m,c,"$climvar, $dataSource $epoch $trange" $outFile

# the realization attribute is in all CMIP5 and GFDL model metadata, but "gfdl_experiment_name" is only in GFDL's model data. 
# execute separately so that if either is missing from netCDF files, the other edit is still executed.
  ncatted -O -h -a realization,global,d,, -a realizations,global,c,c,""$rips"" $outFile
  ncatted -O -h -a gfdl_experiment_name,global,d,, $outFile
