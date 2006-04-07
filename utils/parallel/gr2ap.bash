#!/usr/local/bin/bash
##############################################################################
# Time-stamp: <Wed Jul 24 1996 20:53:36 Stardate: [-31]7859.14 hwloidl>
#
# Usage: gr2ap [options] <gr-file>
#
# Create a per-thread activity graph from a GrAnSim (or GUM) profile.
# Transform the log file of a GrAnSim run (a .gr file) into a quasi-parallel
# profile (a .qp file) using gr2qp and then into a PostScript file using qp2ap.
# The generated PostScript file shows one horizontal line for each task. The 
# thickness of the line indicates the state of the thread:
#  thick ... active,  medium ... suspended,  thin ... fetching remote data
#
# Options:
#  -o <file> ... write .ps file to <file>
#  -m        ... create mono PostScript file instead a color one.
#  -O        ... optimise i.e. try to minimise the size of the .ps file.
#  -v        ... be talkative. 
#  -h        ... print help message (this header).
#
##############################################################################

progname="`basename $0`"
args="$*"

verb=0
help=0
mono=""
apfile=""
optimise=""
scale=""
width=""

getopts "hvmo:s:w:OD" name
while [ "$name" != "?" ] ; do
  case $name in
   h) help=1;;
   v) verb=1;;
   m) mono="-m";;
   o) apfile="$OPTARG";;
   s) scale="-s $OPTARG";;
   w) width="-w $OPTARG";;
   O) optimise="-O";;
   D) debug="-D";;
  esac 
  getopts "hvmo:s:w:OD" name
done

opts="$mono $optimise $scale $width"

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

if [ -z "$apfile" ]
  then apfile="$f"_ap.ps
fi

if [ $verb -eq 1 ]
  then echo "Input file: $grfile"
       echo "Quasi-parallel file: $qpfile"
       echo "PostScript file: $apfile"
       echo "Options forwarded to qp2ap: $opts"
       if [ "$mono" = "-m" ]
         then echo "Producing monochrome PS file"
         else echo "Producing color PS file"
       fi
       if [ "$debug" = "-D" ]
         then echo "Debugging is turned ON"
         else echo "Debugging is turned OFF"
       fi
fi


#  unset noclobber

if [ ! -f "$grfile" ] 
 then
  echo "$grfile does not exist"
  exit 1
 else
  # rm -f "$qpfile" "$apfile"
  prog=`head -1 "$grfile" | sed -e 's/Granularity Simulation for //'`
  echo "$prog" >| "$qpfile"
  if [ $verb -eq 1 ]
    then echo "Executed program: $prog"
  fi
  date >> "$qpfile"
  #date="`date`"                     # This is the date of running the script
  date="`tail +2 $grfile | head -1 | sed -e 's/Start time: //'`"
  cat "$grfile" | gr2qp >> "$qpfile"
  # Sorting is part of gr2qp now.
  #  | ghc-fool-sort | sort -n +0 -1 | ghc-unfool-sort >> "$qpfile"
  # max=`tail -2 "$qpfile" | awk '!/^Number of threads:/ { print $1; }'`
  xmax=`tail -1 "$qpfile" | awk '{ print $2; }'`
  ymax=`tail -1 "$qpfile" | awk '{ print $8; }'`
  if [ $verb -eq 1 ]
    then echo "Total runtime: $xmax"
	 echo "Total number of tasks: $ymax"
  fi
  tail +3 "$qpfile" | qp2ap $opts "$xmax" "$ymax" "$prog" "$date" >| "$apfile" 
  rm -f "$qpfile"
  # Old: qp2ap.pl $mono  $max "$prog" "$date" < "$qpfile" > "$apfile"
fi
  
