#!/usr/local/bin/bash
##############################################################################
# Time-stamp: <Wed Jul 24 1996 22:11:13 Stardate: [-31]7859.41 hwloidl>
#
# Usage: gr2ps [options] <gr-file>
#
# Create an overall activity graph from a GrAnSim (or GUM) profile.
# Transform the log file of a GrAnSim run (a .gr file) into a quasi-parallel
# profile (a .qp file) using gr2qp and then into a PostScript file using qp2ps.
# The generated PostScript file shows essentially the number of running, 
# runnable and blocked tasks during the execution of the program.
#
# Options:
#  -o <file> ... write .ps file to <file>
#  -I <str>  ... queues to be displayed (in the given order) with the encoding
#                 'a' ... active (running)
#                 'r' ... runnable
#                 'b' ... blocked
#                 'f' ... fetching
#                 'm' ... migrating
#                 's' ... sparks
#                (e.g. -I "arb" shows active, runnable, blocked tasks)
#  -i <int>  ... info level from 1 to 7; number of queues to display
#  -m        ... create mono PostScript file instead a color one.
#  -O        ... optimise the produced .ps w.r.t. size
#		 NB: With this option info is lost. If there are several values
#		     with same x value only the first one is printed, all 
#		     others are dropped.
#  -s <str>  ... print <str> in the top right corner of the generated graph
#  -S        ... improved version of sorting events
#  -l <int>  ... length of slice in the .ps file; (default: 100)
#                small value => less memory consumption of .ps file & script
#  -d        ... Print date instead of average parallelism
#  -v        ... be talkative. 
#  -h        ... print help message (this header).
#
##############################################################################

progname="`basename $0`"
args="$*"

verb=0
help=0
mono=""
psfile=""
debug=""
optimise=""
info_level=""
info_mask=""
string=""
length=""
force_date=""
hack=""

getopts "hvmDCOHSdl:s:o:i:I:" name
while [ "$name" != "?" ] ; do
  case $name in
   h) help=1;;
   v) verb=1;;
   m) mono="-m";;
   D) debug="-D";;
   C) check="-C";;
   O) optimise="-O";;
   d) force_date="-d";;
   H) hack="-H";;
   S) improved_sort="-S";;
   s) string="-s $OPTARG";;
   l) length="-l $OPTARG";;
   i) info_level="-i $OPTARG";;
   I) info_mask="-I $OPTARG";;
   o) psfile=$OPTARG;;
  esac 
  getopts "hvmDCOHSdl:s:o:i:I:" name
done

opts_qp="$debug $info_level $info_mask $improved_sort "
opts_ps="$debug $check $optimise $mono $string $length $info_level $info_mask $force_date $hack "

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
grfile="$f".gr
qpfile="${TMPDIR:-.}/$f".qp
ppfile="${TMPDIR:-.}/$f".pp

if [ -z "$psfile" ]
  then psfile="$f".ps
fi

if [ $verb -eq 1 ]
  then echo "Input file: $grfile"
       echo "Quasi-parallel file: $qpfile"
       echo "PP file: $ppfile"
       echo "PostScript file: $psfile"
       if [ -n "$mono" ]
         then echo "Producing monochrome PS file"
         else echo "Producing color PS file"
       fi
       if [ -n "$optimise" ]
         then echo "Optimisation is ON"
         else echo "Optimisation is OFF"
       fi
       if [ -n "$debug" ]
         then echo "Debugging is turned ON"
         else echo "Debugging is turned OFF"
       fi
       if [ -n "$improved_sort" ]
         then echo "Improved sort is turned ON"
         else echo "Improved sort is turned OFF"
       fi
       verb_opt="-v "
       opts_qp="${opts_qp} $verb_opt "
       opts_ps="${opts_ps} $verb_opt "
       echo "Options for gr2qp: ${opts_qp}"
       echo "Options for qp2ps: ${opts_ps}"
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
  if [ $verb -eq 1 ] 
    then echo "Executed program: $prog" 
  fi
  date >> "$qpfile"
  #date="`date`"                     # This is the date of running the script
  date="`tail +2 $grfile | head -1 | sed -e 's/Start time: //'`"
  cat "$grfile" | gr2qp ${opts_qp} >> "$qpfile"
  # Sorting is part of gr2qp now.
  #  | ghc-fool-sort | sort -n +0 -1 | ghc-unfool-sort >> "$qpfile"
  # max=`tail -2 "$qpfile" | awk '!/^Number of threads:/ { print $1; }'`
  xmax=`tail -1 "$qpfile" | awk '{ print $2; }'`
  ymax=`tail -1 "$qpfile" | awk '{ print $4; }'`
  if [ $verb -eq 1 ] 
    then echo "Total runtime: $xmax"
	 echo "Maximal number of tasks: $ymax"
  fi
  tail +3 "$qpfile" | qp2ps ${opts_ps} "$xmax" "$ymax" "$prog" "$date" >| "$psfile"
  rm -f "$qpfile"
  if [ $verb -eq 1 ] 
    then echo "Scaling (maybe): ps-scale-y $psfile " 
  fi
  ps-scale-y "$psfile"
fi

  


