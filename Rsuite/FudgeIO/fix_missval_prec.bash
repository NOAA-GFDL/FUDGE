#!/bin/bash

infile=$1

#infile=/work/cew/testing/dtr_missvals/dtr_results/dtr_day_gridMET_historical_r0i0p0_CONUS24thDv2_19790101-20201231.I40-90.J10-60.r_only.nc
#SAVEIFS=$IFS
#IFS=$'\n'
#out_string=$(ncks -m --trd $infile)
ncvarlist=$(ncks --trd -m $infile | grep -E ': type' | cut -f 1 -d ' ' | sed 's/://' | sort ;)
#out_vars=(`echo $out_string`)
#IFS=$SAVEIFS

#echo $ncvarlisit


nc_atts=("missing_value" "_FillValue")

for ncv in ${ncvarlist[@]}
do
  nctype=$(ncks --trd -m -v $ncv $infile | grep -E -i "^$ncv: type" | cut -f 3 -d ' ' | cut -f 1 -d ',' | sed -e s:" ":"":g ;)
  for n in ${nc_atts[@]}
  do
    natt=$(ncks -m --trd $infile | grep "$ncv attribute" | grep $n)
    #if attribute is present for var, check precision
    if [[ $natt != "" ]] 
    then
      aprec=$(echo $natt | cut -d " " -f 8 | sed -e s:",":"":g)
      if [[ $aprec != $nctype ]] 
      then
        echo "type mismatch between $n att prec $aprec and $ncv var prec $nctype"
        aval=$(echo $natt | cut -d " " -f 11)
        if [[ $nctype == "NC_FLOAT" ]]
        then
          echo "ncatted -h -a $n,$ncv,o,f,${aval}"
          ncatted -h -O -a $n,$ncv,o,f,${aval} $infile
        else
          echo "ncatted -h -a $n,$ncv,o,d,${aval}"
          ncatted -h -O $n,$ncv,o,d,${aval} $infile
        fi 
      fi
    fi
  done
done





