#!/bin/csh -f
#
#	debugger.csh
#
#	this script is invoked by the pvmd when a task is spawned with
#	the PvmTaskDebug flag set.  it execs an xterm with script
#	debugger2 running inside.
#
#	06 Apr 1993  Manchek
#

if ($#argv < 1) then
	echo "usage: debugger command [args]"
	exit 1
endif

# scratch file for debugger commands

set TEMPCMD=gdb$$.cmd
set TEMPLISP=gdb$$.el

# default debugger and flags

#
# run the debugger
#

echo run $argv[2-] > $TEMPCMD
echo "(gdb "'"'"$argv[1] -q -x $TEMPCMD"'")' > $TEMPLISP

emacs -l $TEMPLISP

#rm -f $TEMPCMD $TEMPLISP

exit 0


