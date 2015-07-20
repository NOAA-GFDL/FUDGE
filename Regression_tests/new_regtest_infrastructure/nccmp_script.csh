#!/bin/csh -f
#Calls nccmp with select settings on two input files
#And actively returns a nearly-parsed log entry with
#the nccmp statuses as part of the workflow

#requires nccmp to be loaded/accessible in current workflow

#set echo

set newfile = $1
set oldfile = $2
if( $#argv > 2) then
	set outfile = $3
else
	#set echo
	set outfile  = $TMPDIR/temp_nccmp_out.txt
endif

#Start nccmp commands
nccmp -d $oldfile $newfile >>& $outfile
set stat_1 = $status
nccmp -d -t 0.00000000001 $oldfile $newfile >>& $outfile
set stat_2 = $status
nccmp -m $oldfile $newfile >>& $outfile
set stat_3 = $status
nccmp -mg -G "history" $oldfile $newfile >>& $outfile
set stat_4 = $status 

#Create summary entry
set header = "\n %-10s %-15s %-10s %-10s"
set format = "\n %-10s %-15s %-10s %-10s \n"
printf "$header" "DATA" "DATA +/ e-11" "METADATA" "GLOBALS"
printf "$format" ${stat_1} ${stat_2} ${stat_2} ${stat_4}

# "DATA" "DATA +/ e-11" "METADATA" "GLOBALS"`
#set headstr = "`printf "%-20s" "data"` `printf "%-20s" "data + tol e-11"` `printf "%-20s" "metadata"` `printf "%-20s" "globals"`"
#set format = `printf "%-20s %-20s %-20s %-20s" ${stat_1} ${stat_2} ${stat_2} ${stat_4}`

#echo $headstr
#echo $resultstr

if( `expr $stat_1 + $stat_2 + $stat_3 + $stat_4` == 0) then
	exit 0
else
	exit 1
endif


