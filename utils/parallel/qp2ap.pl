#! /usr/local/bin/perl
##############################################################################
# Time-stamp: <Wed Jul 24 1996 22:05:31 Stardate: [-31]7859.39 hwloidl>
#
# Usage: qp2ap [options] <max-x> <max-y> <prg> <date>
#
# Filter that transforms a quasi-parallel profile (a .qp file) at stdin to  
# a PostScript file at stdout, showing an activity profile with one horizontal
# line for each task (thickness of the line shows if it's active or suspended).
#
# Options:
#  -o <file> ... write .ps file to <file>
#  -m        ... create mono PostScript file instead a color one.
#  -O        ... optimise i.e. try to minimise the size of the .ps file.
#  -s <n>    ... scaling factor of y axis (default: 1)
#  -w <n>    ... width of lines denoting running threads (default: 2) 
#  -v        ... be talkative. 
#  -h        ... print help message (this header).
#
##############################################################################


require "getopts.pl";

&Getopts('hvms:w:OlD');  

do process_options();

if ( $opt_v ) {
    do print_verbose_message();
}

# ---------------------------------------------------------------------------
# Init
# ---------------------------------------------------------------------------

$y_scaling = 0;
$gtid = 1;               # number of process so far = $gtid-1

$xmin = 100;
$xmax = 790;

$scalex = $xmin;
$labelx = $scalex - 45;
$markx =  $scalex - 30;
$major = $scalex - 5;
$majorticks = 10;

# $pmax = 40;
$ymin = 50;
$ymax = 500;

if ( ($ymax - $ymin)/$pmax < 3 ) {
    print STDERR "Warning: Too many tasks! Distance will be smaller than 3 pixels.\n";
}

if ( !$width ) { 
    $width = 2/3 * ($ymax - $ymin)/$pmax;
}

do write_prolog();
do print_y_axis();

# ---------------------------------------------------------------------------
# Main Part
# ---------------------------------------------------------------------------

while(<STDIN>) {
    next if /^[^0-9]/;   # ignore lines not beginning with a digit (esp. last)
    chop;
    ($time, $event, $tid, $addr, $tid2, $addr2) = split;

    if ( $event eq "*G") {
    	    $TID{$addr} = $gtid++;
	    $START{$addr} = $time;
    }

    elsif ($event eq "*A") {
    	    $TID{$addr} = $gtid++;
	    $SUSPEND{$addr} = $time;
    }

    elsif ($event eq "G*" || $event eq "GR" ) {
	do psout($START{$addr},$time,$TID{$addr},"runlineto");
#	$STOP{$addr} = $time;
    }

    elsif ($event eq "GA" || $event eq "GC" || $event eq "GY") {
	do psout($START{$addr},$time,$TID{$addr},"runlineto");
	$SUSPEND{$addr} = $time;
    }
	
    elsif ($event eq "RA") {
	$SUSPEND{$addr} = $time;
    }

    elsif ($event eq "YR") {
	do psout($SUSPEND{$addr},$time,$TID{$addr},"fetchlineto");
    }

    elsif ($event eq "CA" || $event eq "YA" ) {
	do psout($SUSPEND{$addr},$time,$TID{$addr},"fetchlineto");
	$SUSPEND{$addr} = $time;
    }

    elsif ($event eq "AC" || $event eq "AY" ) {
	do psout($SUSPEND{$addr},$time,$TID{$addr},"suspendlineto");
	$SUSPEND{$addr} = $time;
    }

    elsif ($event eq "RG") {
	$START{$addr} = $time;
    }

    elsif ($event eq "AG") {
	do psout($SUSPEND{$addr},$time,$TID{$addr},"suspendlineto");
	$START{$addr} = $time;
    } 

    elsif ($event eq "CG" || $event eq "YG" ) {
	do psout($SUSPEND{$addr},$time,$TID{$addr},"fetchlineto");
	$START{$addr} = $time;
    } elsif ( $event eq "B*" || $event eq "*B" || $event eq "BB" ) {
	print STDERR "Ignoring spark event $event at $time\n"  if $opt_v;
    } else {
	print STDERR "Unexpected event $event at $time\n";
    }

    print("%% $time: $event $addr $TID{$addr}\n\n")  if $opt_D;
}

# ---------------------------------------------------------------------------

# Logo
print("HE14 setfont\n");
if ( $opt_m ) {
    print("50 550 asciilogo\n");                     
} else {
    print("50 550 logo\n");                          #
}

# Epilogue
print("showpage\n");

if ( $gtid-1 != $pmax ) {
    if ( $pedantic ) {
	die "Error: Calculated max no. of tasks ($gtid-1) does not agree with stated max. no. of tasks ($pmax)\n";
    } else {
	print STDERR  "Warning: Calculated total no. of tasks ($gtid-1) does not agree with stated total no. of tasks ($pmax)\n" if $opt_v;
	$y_scaling = $pmax/($gtid-1); 
    }
}


exit 0;

# ---------------------------------------------------------------------------

sub psout {
    local($x1, $x2, $y, $cmd) = @_;
    print("% ($x1,$y) -- ($x2,$y) $cmd\n")  if $opt_D;
    $x1 = int(($x1/$tmax) * ($xmax-$xmin) + $xmin);
    $x2 = int(($x2/$tmax) * ($xmax-$xmin) + $xmin);
    $y = int(($y/$pmax) * ($ymax-$ymin) + $ymin);
    if ( $x1 == $x2 ) {
	$x2 = $x1 + 1;
    }
    
    if ( $opt_l ) {
	print("newpath\n");
	print("$x1 $y moveto\n");
	print("$x2 $y $cmd\n");
	print("stroke\n");
    } elsif ( $opt_O ) {
	print "$x1 $x2 $y " .
	    ( $cmd eq "runlineto" ? "G RL\n" :
	      $cmd eq "suspendlineto" ? "R SL\n" :
	      $cmd eq "fetchlineto" ? "B FL\n" :
	      "\n% ERROR: Unknown command $cmd\n");

    } else {
	print "$x2 $y $x1 $y " . 
	    ( $cmd eq "runlineto" ? "green run\n" :
	      $cmd eq "suspendlineto" ? "red suspend\n" :
	      $cmd eq "fetchlineto" ? "blue fetch\n" :
	      "\n% ERROR: Unknown command $cmd\n");
    }	    
}

# -----------------------------------------------------------------------------

sub get_date {
    local ($date);

    chop($date = `date`);
    return ($date);
}

# -----------------------------------------------------------------------------

sub write_prolog {
    local ($now);

    $now = do get_date();

    print("%!PS-Adobe-2.0\n");
    print("%%BoundingBox:    0 0 560 800\n");
    print("%%Title:          Per-thread Activity Profile\n");
    print("%%Creator:        qp2ap\n");
    print("%%StartTime:      $date\n");
    print("%%CreationDate:   $now\n");
    print("%%Copyright:      1995, 1996 by Hans-Wolfgang Loidl, University of Glasgow\n");
    print("%%EndComments\n");

    print "% " . "-" x 77 . "\n";
    print "% Tunable Parameters:\n";
    print "% The width of a line representing a task\n";
    print "/width $width def\n";
    print "% Scaling factor for the y-axis (usful to enlarge)\n";
    print "/y-scale $y_scale def\n";
    print "% " . "-" x 77 . "\n";

    print "/total-len $tmax def\n";
    print "/show-len $xmax def\n";
    print "/x-offset $xmin def\n";
    print "/y-offset $ymin def\n";
    print "% normalize is the PS version of the formula: \n" .
	  "%   int(($x1/$tmax) * ($xmax-$xmin) + $xmin) \n" .
	  "% in psout.\n";
    print "/normalize { total-len div show-len x-offset sub mul x-offset add floor } def\n";
    print "/x-normalize { exch show-len mul total-len div exch } def\n";
    print "/y-normalize { y-offset sub y-scale mul y-offset add } def\n";
    print "/str-len 12 def\n";
    print "/prt-n { cvi str-len string cvs \n" .
          "         dup stringwidth pop \n" .
	  "         currentpoint pop 780 gt { 10 sub } { 2 div } ifelse \n" .
          "         neg 0 rmoveto \n" . 
          "         show  } def \n" .
	  "        % print top-of-stack integer centered at the current point\n";
    # print "/prt-n { cvi str-len string cvs \n" .
    #       "         dup stringwidth pop 2 div neg 0 rmoveto \n" . 
    #       "         show  } def \n" .
    #	    "        % print top-of-stack integer centered at the current point\n";

    if ( $opt_l ) {
	print ("/runlineto {1.5 setlinewidth lineto} def\n");
	print ("/suspendlineto {0.5 setlinewidth lineto} def\n");
	print ("/fetchlineto {0.2 setlinewidth lineto} def\n");
    } else {
	    if ( $opt_m ) {
		if ( $opt_O ) {
		    print  "/R { 0 } def\n";
		    print  "/G { 0.5 } def\n";
		    print  "/B { 0.2 } def\n";
		} else {
		    print  "/red { 0 } def\n";
		    print  "/green { 0.5 } def\n";
		    print  "/blue { 0.2 } def\n";
		}
		print  "/set-bg { setgray } def\n";
	    } else {
		if ( $opt_O ) {
		    print  "/R { 0.8 0 0 } def\n";
		    print  "/G { 0 0.9 0.1 } def\n";
		    print  "/B { 0 0.1 0.9 } def\n";
		    print  "/set-bg { setrgbcolor } def\n";
		} else {
		    print  "/red { 0.8 0 0 } def\n";
		    print  "/green { 0 0.9 0.1 } def\n";
		    print  "/blue { 0 0.1 0.9 } def\n";
		    print  "/set-bg { setrgbcolor } def\n";
		}
	    }

	    if ( $opt_O ) {
		print "% RL: runlineto; draws a horizontal line in given color\n";
		print "% Operands: x-from x-to y color\n";
		print "/RL { set-bg   % set color \n" .
		      "      newpath y-normalize  % mangle y val\n" .
		      "      2 index 1 index moveto width setlinewidth \n" .
		      "      lineto pop stroke} def\n";
		print "% SL: suspendlineto; draws a horizontal line in given color (thinner)\n";
		print "% Operands: x-from x-to y color\n";
		print "/SL { set-bg   % set color \n" .
		      "      newpath y-normalize  % mangle y val\n" .
		      "      2 index 1 index moveto width 2 div setlinewidth \n" .
		      "      lineto pop stroke} def\n";
		print "% FL: fetchlineto; draws a horizontal line in given color (thinner)\n";
		print "% Operands: x-from x-to y color\n";
		print "/FL { set-bg   % set color \n" .
		      "      newpath y-normalize  % mangle y val\n" .
		      "      2 index 1 index moveto width " . 
			  ( $opt_m ? " 4 " : " 2 ") . 
		      " div setlinewidth \n" .
		      "      lineto pop stroke} def\n";
	    } else {
		print "/run { set-bg newpath 50 sub y-scale mul 50 add moveto width " .
		    "setlinewidth 50 sub y-scale mul 50 add lineto stroke} def\n";
		print "/suspend { set-bg newpath 50 sub y-scale mul 50 add moveto width " .
		    "2 div setlinewidth 50 sub y-scale mul 50 add lineto stroke} def\n";
		print "/fetch { set-bg newpath 50 sub y-scale mul 50 add moveto width " .
		    ( $opt_m ? " 4 " : " 2 ") .
			"div setlinewidth 50 sub y-scale mul 50 add lineto stroke} def\n";
		#print ("/run { newpath moveto 1.5 setlinewidth lineto stroke} def\n");
		#print ("/suspend { newpath moveto 0.5 setlinewidth lineto stroke} def\n");
	    }
	}

    print  "/printText { 0 0 moveto (GrAnSim) show } def\n";      
    print "/asciilogo { 5 sub moveto HB16 setfont (GrAnSim) show } def\n";
    if ( $opt_m ) {
	print "/logo { asciilogo } def\n";
    } else {
	print "/logo { gsave \n" .
	    "        translate \n" .
		"        .95 -.05 0\n" .
		    "          { dup 1 exch sub 0 exch setrgbcolor printText 1 -.5 translate } for \n" . 
			"        1 0 0 setrgbcolor printText\n" . 
			    "        grestore} def\n";
    }
    print "% For debugging PS uncomment this line and add the file behandler.ps\n";
    print "% $brkpage begin printonly endprint \n";

    print("/HE10 /Helvetica findfont 10 scalefont def\n");
    print("/HE12 /Helvetica findfont 12 scalefont def\n");
    print("/HE14 /Helvetica findfont 14 scalefont def\n");
    print("/HB16 /Helvetica-Bold findfont 16 scalefont def\n");
    print "% " . "-" x 77 . "\n";
    print("newpath\n");

    print("-90 rotate\n");
    print("-785 30 translate\n");
    print("0 8.000000 moveto\n");
    print("0 525.000000 760.000000 525.000000 8.000000 arcto\n");
    print("4 {pop} repeat\n");
    print("760.000000 525.000000 760.000000 0 8.000000 arcto\n");
    print("4 {pop} repeat\n");
    print("760.000000 0 0 0 8.000000 arcto\n");
    print("4 {pop} repeat\n");
    print("0 0 0 525.000000 8.000000 arcto\n");
    print("4 {pop} repeat\n");
    print("0.500000 setlinewidth\n");
    print("stroke\n");
    print("newpath\n");
    print("4.000000 505.000000 moveto\n");
    print("4.000000 521.000000 752.000000 521.000000 4.000000 arcto\n");
    print("4 {pop} repeat\n");
    print("752.000000 521.000000 752.000000 501.000000 4.000000 arcto\n");
    print("4 {pop} repeat\n");
    print("752.000000 501.000000 4.000000 501.000000 4.000000 arcto\n");
    print("4 {pop} repeat\n");
    print("4.000000 501.000000 4.000000 521.000000 4.000000 arcto\n");
    print("4 {pop} repeat\n");
    print("0.500000 setlinewidth\n");
    print("stroke\n");

    print("HE14 setfont\n");
    print("100 505 moveto\n");
    print("($pname ) show\n");
    
    print("($date) dup stringwidth pop 750 exch sub 505.000000 moveto show\n");
    
    # print "/total-len $tmax def\n";
    print("-40 -40 translate\n");

    print "% " . "-" x 77 . "\n";
    print "% Print x-axis:\n";
    print "/y-val $ymin def % { y-offset 40 sub 2 div y-offset add } def\n";
    print "0.5 setlinewidth\n";
    print "x-offset y-val moveto total-len normalize x-offset sub 0 rlineto stroke\n";
    print "0 total-len 10 div total-len\n" .
          " { dup normalize dup y-val moveto 0 -2 rlineto stroke  % tic\n" .
          "   y-val 10 sub moveto HE10 setfont round prt-n  % print label \n" .
	  " } for \n";
    print "1 setlinewidth\n";
    print "% " . "-" x 77 . "\n";

}

# -----------------------------------------------------------------------------

sub print_y_axis {
    local ($i);
    local ($y, $smax,$majormax, $majorint);

# Y-axis label

    print "% " . ("-" x 75) . "\n";
    print "% Y-Axis:\n";
    print "% " . ("-" x 75) . "\n";

    if ( $opt_m ) {
	print "0 setgray\n";
    } else {
	print "0 0 0 setrgbcolor\n";
    }

    print("gsave\n");
    print("HE12 setfont\n");
    print("(tasks)\n");
    print("dup stringwidth pop\n");
    print("$ymax\n");
    print("exch sub\n");
    print("$labelx exch\n");
    print("translate\n");
    print("90 rotate\n");
    print("0 0 moveto\n");
    print("show\n");
    print("grestore\n");

# Scale

    if ($pmax < $majorticks) {
	$majorticks = $pmax;
    }

    print "0.5 setlinewidth\n";

    print("HE12 setfont\n$scalex $ymin moveto\n$scalex $ymax lineto\n");
    print("% Total number of tasks: $pmax\n");
    print("% Number of ticks: $majorticks\n");

    $y = $ymax; # (($pmax - $ymin)/$majorticks) * ($majorticks-$i) + $ymin;
    print("$scalex $y moveto\n$major $y lineto\n");
    print("$markx $y moveto\n($pmax) show\n");

    $majormax = int($pmax/$majorticks)*$majorticks;
    $smax = $majormax*(($ymax-$ymin)/$pmax)+$ymin;
    $majorint = $majormax/$majorticks;

    for($i=0; $i <= $majorticks; ++$i) {
	$y = (($smax - $ymin)/$majorticks) * ($majorticks-$i) + $ymin;
	$majorval = int($majorint * ($majormax/$majorint-$i));
	print("$scalex $y moveto\n$major $y lineto\n");
	print("$markx $y moveto\n($majorval) show\n");
    }

    # print("$xmin $ymax moveto\n10 0 rlineto\n10 0 rmoveto\n($pmax) show\n");
    print " stroke\n";
    print "1 setlinewidth\n";
    print "% " . ("-" x 75) . "\n";
}

# ---------------------------------------------------------------------------

sub print_verbose_message {

    print "Prg Name: $pname  Date: $date\n";
    print "Input: stdin  Output: stdout\n";
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
    
     if ( $opt_s ) {                      
	 $y_scale = $opt_s;
     } else {
	 $y_scale = 1; 
     }

    if ( $#ARGV != 3 ) {
	print "Usage: $0 [options] <max x value> <max y value> <prg name> <date> \n";
	print "Use -h option to get details\n";
	exit 1;
    }

    $tmax = $ARGV[0];
    $pmax = $ARGV[1];
    # GUM uses the absolute path (with '=' instead of '/') of the executed file
    # (for PVM reasons); if you want to have the full path in the generated
    # graph, too, eliminate the substitution below
    ($pname = $ARGV[2]) =~ s/.*=//;
    $date = $ARGV[3];

     if ( $opt_w ) {
	 $width = $opt_w;
     } else {
	 $width = 0;
     }

}
# -----------------------------------------------------------------------------
