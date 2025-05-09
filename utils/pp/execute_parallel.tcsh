#!/bin/tcsh
#execute_parallel.tcsh
#helper script for backgrounding a process

source $BASEDIR/set_fudge_env
#set in_command = ${1}
set status_root = ${2}

eval "$1"
set commandstat = $status
if ( $commandstat == 0) then
  touch ${status_root}.success
else
  touch ${status_root}.failure
endif
