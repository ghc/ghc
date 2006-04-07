#!/usr/local/bin/perl
# #############################################################################
#
# Usage: gp-ext-imp [options] [<input-file>] [<output-file>]
#
# A small script to produce half-useful bar graphs from the PostScript
# output produced by gnuplot.  
# Translation is done in the X axis automatically, and should
# be `good enough' for graphs with smallish numbers of bars.
#
# Original version:          Bryan O'Sullivan <bos@dcs.glasgow.ac.uk> 09.94
# New and improved version:  Hans Wolfgang Loidl <hwloidl@dcs.glasgow.ac.uk>
#
# Options:
#  -w <width>      ... width of vertical bars
#  -g <gray-level> ... set gray-level (between 0 and 1; 0 means black)
#  -m <move>       ... move the graph <move> pixels to the right
#  -h              ... help; print this text
#  -v              ... verbose mode
#
# #############################################################################

require "getopts.pl";

&Getopts('hvm:w:g:');  

if ( $opt_h ) {                      
    open(ME,$0) || die "Can't open myself ($0)";
    $n = 0;
    while (<ME>) {
      last if $_ =~ /^$/;
      print $_;
      $n++;
    }
    close(ME);

    exit ;
}

$size = $opt_w ? $opt_w : 200;
$gray = $opt_g ? $opt_g : 0;
$move = $opt_m ? $opt_m : 150;

$from = $#ARGV >= 0 ? $ARGV[0] : "-";
$to = $#ARGV >= 1 ? $ARGV[1] : "-";

if ( $opt_v ) {
    print 70 x "-" . "\n";
    print "\nSetup: \n";
    print " Input file: $from   Output file: $to\n";
    print " Width: $size   Gray level: $gray   Move is " . 
          ($opt_m ? "ON" : "OFF") . " with value $move\n";
    print 70 x "-" . "\n";
}

open(FROM, "<$from") || die "$from: $!";
open(TO, ">$to") || die "$to: $!";

$l = -1;

foreach (<FROM>) {
    if ($l >= 0) {
	$l--;
    }
    if ($l == 0) {
	if ( $opt_m ) {
            # This seems to shift everything a little to the right;
	    print TO "$move 0 translate\n";
        }
	print TO "$gray setgray\n";
	print TO "$size setlinewidth\n";
    }
    if (/^LT0$/) {
	$l = 3;
    } elsif (/^LT1$/) {
	print TO "-150 0 translate\n";
    }
    print TO;
}







