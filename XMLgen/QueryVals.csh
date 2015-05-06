#!/bin/csh -f
# QueryVals.csh is "source"-d from within xmlGen.csh script. It acts like a function
#  which prints to standard output the contents of a preset string variable "dinfo"
#  then awaits the one element from the list as input.
# 
# PRE-SET variables in shell from which QueryVals.csh is "source"-d:
#
#   "varvals" == list of variables the user can select by entering number associated with the variable.
#                1st variable in the list get number "1", 2nd gets number "2", etc.
#   "dinfo"   == string that describes what the selection from the variable list is about.
#
# OUTPUT 
# 
#   "kval"    == the value of the option selected, and is then used in the shell
#                as well as written to a text file. (so user can re-run without having 
#                to re-type all option selections.)
#

if (! $?dinfo) then
echo "Please set 'dinfo' = to a string describing what you are querying ."
exit 1
endif

if (-e $workDir/varvals) then
  \rm $workDir/varvals
endif

if ($?varvals) then
unset ok
unset kval
while (! $?ok) 
  echo "============================================="
  echo "For  ${dinfo}"
  echo "============================================="
  echo " "
  echo "Options = "
  foreach kval ($varvals)
    echo "$kval" >> $workDir/varvals
    echo "$kval"
  end
  echo " "
  echo -n "Enter the option desired: "
  set kval=$<
  
  echo ""

  grep -w $kval $workDir/varvals
    if ($status == 0) then
       set ok
       echo "You chose $kval ."
     else
       echo "You entered $kval . Please enter one of the options listed."
     endif
end

#echo $kval >> $INtxt
sleep 1
else
echo  "You must define options in "varvals"."
exit 1
endif
