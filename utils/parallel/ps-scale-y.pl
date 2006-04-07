#!/usr/local/bin/perl
##############################################################################
# Time-stamp: <Wed Jul 24 1996 22:19:02 Stardate: [-31]7859.44 hwloidl>
#
# Usage: ps-scale-y [options] <file>
#
# It is assumed that the last line of <file> is of the format:
#			     %% y_scaling: <f> max: <n>
# where <f> is a floating point number determining the amount of scaling of 
# the y-axis of the graph that is necessary. <n> is the real maximal number
# of tasks in the program (needed to rebuild y-axis). This script replaces the 
# definitions of the PostScript functions scale-y and unscale-y in <file> by 
# new definitions that do the right amount of scaling. 
# The y-axis is rebuilt (using the above maximal number of tasks and a copy 
# of the print_y_axis routine from qp2ps).
# If the above line doesn't exist, <file> is unchanged.
# This script is typically called from gr2ps.
#
##############################################################################

require "getopts.pl";

&Getopts('hv');  

do process_options();

$tmpfile = ",t";
$debug = 0;

# NB: This must be the same as in qp2ps!!

$xmin = 100;
$xmax = 790;

$scalex = $xmin;
$labelx = $scalex - 45;
$markx =  $scalex - 30;
$major = $scalex - 5;
$majorticks = 10;

$mmax = 1;

$amax = 0;
$ymin = 50;
$ymax = 500;

# E
open (GET_SCALING,"cat $file | tail -1 |") || die "Can't open pipe:  $file | tail -1 |\n";

$y_scaling = 1.0;

while (<GET_SCALING>){
    # print STDERR $_;
    if (/^\%\%\s+y_scaling:\s+([0-9\.]+)\s+max:\s+(\d+)/) {
	$y_scaling = $1;
	$pmax = $2;
	$y_translate = 1.0 - $y_scaling;
    }
}
close (GET_SCALING);

if ( $y_scaling != 1.0 ) {
    print STDERR "Scaling $file ($y_scaling; $pmax tasks) ...\n" if $opt_v;
    # print STDERR "SCALING NECESSARY: y_scaling = $y_scaling; y_translate = $y_translate !\n";
} else {
    # No scaling necessary!!
    exit 0;
}


open (IN,"<$file") || die "Can't open file $file\n";
open (OUT,">$tmpfile") || die "Can't open file $tmpfile\n";

$skip = 0;
while (<IN>) {
    $skip = 0 if $skip && /^% End Y-Axis.$/;
    next if $skip;
    if (/\/scale\-y/) { 
	print OUT "/scale-y { gsave\n" .
	          "           0 50 $y_translate mul translate\n" .
		  "           1 $y_scaling scale } def\n";
    }
    elsif (/\/unscale\-y/) {  
	print OUT "/unscale-y { grestore } def \n";
    } else {
	print OUT $_;
    }
    if (/^% Y-Axis:$/) {
	$skip = 1;
	do print_y_axis();
    }
}

close (IN);
close (OUT);

rename($tmpfile,$file);

exit 0;

# ###########################################################################
# Same as in qp2ps (but printing to OUT)!
# ###########################################################################

sub print_y_axis {
    local ($i);
    local ($y, $smax,$majormax, $majorint);

# Y-axis label

    print OUT  "% " . ("-" x 75) . "\n";
    print OUT  "% Y-Axis (scaled):\n";
    print OUT  "% " . ("-" x 75) . "\n";

    print OUT ("%scale-y  % y-axis outside scaled area if ps-scale-y rebuilds it!\n");

    print OUT ("gsave\n");
    print OUT ("HE12 setfont\n");
    print OUT ("(tasks)\n");
    print OUT ("dup stringwidth pop\n");
    print OUT ("$ymax\n");
    print OUT ("exch sub\n");
    print OUT ("$labelx exch\n");
    print OUT ("translate\n");
    print OUT ("90 rotate\n");
    print OUT ("0 0 moveto\n");
    print OUT ("show\n");
    print OUT ("grestore\n");

# Scale

    if ($pmax < $majorticks) {
	$majorticks = $pmax;
    }

    print OUT ("HE12 setfont\n$scalex $ymin moveto\n$scalex $ymax lineto\n");
    print OUT ("% Max number of tasks: $pmax\n");
    print OUT ("% Number of ticks: $majorticks\n");

    print OUT  "0.5 setlinewidth\n";

    $y = $ymax; # (($pmax - $ymin)/$majorticks) * ($majorticks-$i) + $ymin;
    print OUT ("$scalex $y moveto\n$major $y lineto\n");
    print OUT ("$markx $y moveto\n($pmax) show\n");

    $majormax = int($pmax/$majorticks)*$majorticks;
    $smax = $majormax*(($ymax-$ymin)/$pmax)+$ymin;
    $majorint = $majormax/$majorticks;

    for($i=1; $i <= $majorticks; ++$i) {
	$y = (($smax - $ymin)/$majorticks) * ($majorticks-$i) + $ymin;
	$majorval = int($majorint * ($majormax/$majorint-$i));
	print OUT ("$scalex $y moveto\n$major $y lineto\n");
	print OUT ("$markx $y moveto\n($majorval) show\n");
    }

    # print OUT ("$xmin $ymax moveto\n10 0 rlineto\n10 0 rmoveto\n($pmax) show\n");
    print OUT  " stroke\n";
    print OUT  "1 setlinewidth\n";
    print OUT ("%unscale-y\n");
    print OUT ("% End Y-Axis (scaled).\n");
    print OUT "% " . ("-" x 75) . "\n";
}

# ----------------------------------------------------------------------------

sub process_options {

    if ( $opt_h ) {                      
	open(ME,$0) || die "Can't open myself ($0): $!\n";
	$n = 0;
	while (<ME>) {
	    last if $_ =~ /^$/;
	    print $_;
	    $n++;
	}
	close(ME);
	exit ;
    }
    
    if ( $#ARGV != 0 ) {
	print "Usage: $0 [options] <file>\n";
	print "Use -h option to get details\n";
	exit 1;
    }

    $file = $ARGV[0];
}
