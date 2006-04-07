#!/usr/local/bin/bash
##############################################################################
# Last modified: Time-stamp: <95/08/01 02:21:56 hwloidl>
#
# Usage: gr2gran [options] <sim-file>
#
# Create granularity graphs for the GrAnSim profile <sim-file>. This creates
# a bucket statistics and a cumulative runtimes graph.
# This script is derived from the much more complex gran-extr script, which
# also produces such graphs and much more information, too.
#
# Options:
#  -t <file>   ... use <file> as template file (<,> global <.> local template)
#  -p <file>   ... use <file> as gnuplot .gp file (default: gran.gp)
#  -x <x-size> ... of gnuplot graph
#  -y <y-size> ... of gnuplot graph
#  -n <n>      ... use <n> as number of PEs in title 
#  -o <file>   ... keep the intermediate <file> (sorted list of all runtimes)
#  -h          ... help; print this text.
#  -v          ... verbose mode.
#
##############################################################################

progname="`basename $0`"
args="$*"

help=0
verb=0
template=""
plotfile=""
x=""
y=""
n=""
rtsfile=""
keep_rts=0

getopts "hvt:p:x:y:n:o:" name
while [ "$name" != "?" ] ; do
  case $name in
   h) help=1;;
   v) verb=1;;
   t) template="-t $OPTARG";;
   p) plotfile="-p $OPTARG";;
   x) x="-x $OPTARG";;
   y) y="-y $OPTARG";;
   n) n="-n $OPTARG";;
   o) rtsfile="$OPTARG";;
  esac 
  getopts "hvt:p:x:y:n:o:" name
done

shift $[ $OPTIND - 1 ]

if [ $help -eq 1 ]
 then no_of_lines=`cat $0 | awk 'BEGIN { n = 0; } \
                                 /^$/ { print n; \
                                        exit; } \
                                      { n++; }'`
      echo "`head -$no_of_lines $0`"
      exit 
fi

if [ -z "$1" ]
 then echo "Usage: $progname [options] file[.gr]"
      echo "Use -h option for details"
      exit 1;
fi

f="`basename $1 .gr`"
grfile="${f}.gr"
if [ -z "$rtsfile" ]
  then rtsfile="${f}.rts"
       rtsopt="-o $rtsfile"
  else rtsopt="-o $rtsfile"
       keep_rts=1
fi

opts_RTS="$rtsopt "
opts_ps="$template $plotfile $x $y $n "

if [ $verb -eq 1 ]
  then echo "Input file: $grfile"
       if [ ${keep_rts} -eq 1 ]
         then echo "Intermediate file: $rtsfile  (kept after termination)"
         else echo "Intermediate file: $rtsfile  (discarded at end)"
       fi
       verb_opt="-v "
       opts_RTS="${opts_RTS} $verb_opt "
       opts_ps="${opts_ps} $verb_opt "
       echo "Options for gr2RTS: ${opts_RTS}"
       echo "Options for RTS2gran: ${opts_ps}"
fi


# unset noclobber
if [ ! -f "$grfile" ] 
 then
  echo "$grfile does not exist"
  exit 1
 else
  # rm -f "$rtsfile"
  if [ $verb -eq 1 ] 
    then echo "gr2RTS ..."
  fi
  gr2RTS ${opts_RTS} $grfile
  if [ $verb -eq 1 ] 
    then echo "RTS2gran ..."
  fi
  RTS2gran ${opts_ps} $rtsfile
  if [ ${keep_rts} -ne 1 ] 
    then rm -f $rtsfile
  fi
fi
