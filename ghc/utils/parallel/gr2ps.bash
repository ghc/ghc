#!/usr/local/bin/bash
##############################################################################
#
# Usage: gr2ps [options] <gr-file>
#
# Transform the log file of a GrAnSim run (a .gr file) into a quasi-parallel
# profile (a .qp file) and then into a PostScript file, showing essentially
# the total number of running, runnable and blocked tasks.
#
# Options:
#  -o <file> ... write PS file to <file>
#  -i <int>  ... info level from 1 to 7; number of queues to display
#  -m        ... create mono PostScript file instead a color one.
#  -O        ... optimize the produced .ps w.r.t. size
#		 NB: With this option info is lost. If there are several values
#		     with same x value only the first one is printed, all 
#		     others are dropped.
#  -s <str>  ... print <str> in the top right corner of the generated graph
#  -v        ... be talkative. 
#  -h        ... print help message (this header).
#
##############################################################################

##############################################################################
# Internal comments:
# ----------------------------------------------------------------------
# This version works on both Suns and Alphas -- KH
# Any volunteers to convert it to /bin/sh?
# Next time somebody calls for volunteers I'd better keep my mouth shut ... HWL
##############################################################################

progname="`basename $0`"
args="$*"

verb=0
help=0
mono=""
psfile=""
debug=""
optimize=""
info_level=0
info_mask=""
string=""

getopts "hvmDOSs:o:i:I:" name
while [ "$name" != "?" ] ; do
  case $name in
   h) help=1;;
   v) verb=1;;
   m) mono="-m";;
   D) debug="-D";;
   O) optimize="-O";;
   S) lines="-S";;
   s) string=$OPTARG;;
   i) info_level=$OPTARG;;
   I) info_mask=$OPTARG;;
   o) psfile=$OPTARG;;
  esac 
  getopts "hvmDOSs:o:i:I:" name
done

shift $[ $OPTIND - 1 ]

if [ -z "$1" ]
 then echo "usage: $progname [-m] file[.gr]"
      exit 1;
fi

f="`basename $1 .gr`"
grfile="$f".gr
qpfile="$f".qp
ppfile="$f".pp

if [ -z "$psfile" ]
  then psfile="$f".ps
fi

if [ $help -eq 1 ]
 then no_of_lines=`cat $0 | awk 'BEGIN { n = 0; } \
                                 /^$/ { print n; \
                                        exit; } \
                                      { n++; }'`
      echo "`head -$no_of_lines $0`"
      exit 
fi

if [ $verb -eq 1 ]
  then echo "Input file: $grfile"
       echo "Quasi-parallel file: $qpfile"
       echo "PP file: $ppfile"
       echo "PostScript file: $psfile"
       if [ "$mono" = "-m" ]
         then echo "Producing monochrome PS file"
         else echo "Producing color PS file"
       fi
       if [ "$optimize" = "-O" ]
         then echo "Optimization is ON"
         else echo "Optimization is OFF"
       fi
       if [ "$debug" = "-D" ]
         then echo "Debugging is turned ON"
         else echo "Debugging is turned OFF"
       fi
fi


# unset noclobber
if [ ! -f "$grfile" ] 
 then
  echo "$grfile does not exist"
  exit 1
 else
  rm -f "$qpfile" "$psfile"
  prog=`head -1 "$grfile" | sed -e 's/Granularity Simulation for //'`
  echo "$prog" >| "$qpfile"
  if [ $verb -eq 1 ]; then echo "Executed program: $prog"; fi
  date >> "$qpfile"
  date="`date`"
  cat "$grfile" | gr2qp | ghc-fool-sort | sort -n +0 -1 | ghc-unfool-sort >> "$qpfile"
  # max=`tail -2 "$qpfile" | awk '!/^Number of threads:/ { print $1; }'`
  max=`tail -1 "$qpfile" | awk '{ print $1; }'`
  if [ $verb -eq 1 ]; then echo "Total runtime: $max"; fi
  opts="";
  if [ $info_level -gt 0 ]
    then opts="-i $info_level";
  fi 
  if [ -n "$info_mask" ]
    then opts="-I $info_mask";
  fi 
  tail +3 "$qpfile" | qp2ps $debug $optimize $mono $lines "-s" "$string" $opts  "$max" "$prog" "$date" >| "$psfile"
  rm -f "$qpfile"
fi

  


