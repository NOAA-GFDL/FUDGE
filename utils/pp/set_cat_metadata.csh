#!/bin/tcsh
#set_cat_metadata.csh
#sets metadata attributes for concantenated files. At this point, there are
#very few that need to be set.

echo $argv
echo "length of argv:"
echo $#argv
echo $argv[1]

set out_file = $argv[1]
#set quote_args = $argv:q
set quote_args = ($argv:q)
echo $quote_args
################################################################################
foreach i (`seq 2 $#argv`)
	set desc_str = $quote_args[$i]
	set desc_atts = (`echo $desc_str | sed 's:,: :g'`)
	set vals = ""
	#Checking to make sure spaces in atts don't throw an error
	if ($#desc_atts > 2) then
		foreach v (`seq 2 $#desc_atts`)
			set vals = "$vals $desc_atts[$v]"
		end
	else
		set vals = $desc_atts[2]	
	endif
	set vals = (`echo $vals | sed -e 's:__: :g'`)
	echo "ncatted -h -a '$desc_atts[1],global,o,c,$vals' $out_file"
	ncatted -h -a "$desc_atts[1],global,o,c,$vals" $out_file
end

