#! /usr/local/bin/perl
##############################################################################
# Time-stamp: <Wed Jul 24 1996 22:04:50 Stardate: [-31]7859.39 hwloidl>
#
# Usage: qp2ps [options] <max-x> <max-y> <prg> <date>
#
# Filter that transforms a quasi-parallel profile (a .qp file) at stdin to  
# a PostScript file at stdout, showing essentially the total number of running,
# runnable and blocked tasks.
#
# Options:
#  -o <file> ... write .ps file to <file>
#  -m        ... create mono PostScript file instead a color one.
#  -O        ... compress i.e. try to minimize the size of the .ps file
#  -s <str>  ... print <str> in the top right corner of the generated graph
#  -i <int>  ... info level from 1 to 7; number of queues to display
#  -I <str>  ... queues to be displayed (in the given order) with the encoding
#                 'a' ... active (running)
#                 'r' ... runnable
#                 'b' ... blocked
#                 'f' ... fetching
#                 'm' ... migrating
#                 's' ... sparks
#                (e.g. -I "arb" shows active, runnable, blocked tasks)
#  -l <int>  ... length of a slice in the .ps file; (default: 100)
#                small value => less memory consumption of .ps file & script
#                but slower in generating the .ps file
#  -d        ... Print date instead of average parallelism
#  -v        ... be talkative. 
#  -h        ... print help message (this header).
#
##############################################################################

require "getopts.pl";

&Getopts('hvDCOmdl:s:i:I:H');  

do process_options();

if ( $opt_v ) {
    do print_verbose_message();
}

# ---------------------------------------------------------------------------
# Init
# ---------------------------------------------------------------------------

$y_scaling = 1.0;

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

$active = 0;
$runnable = 0;
$blocked = 0;
$fetching = 0;
$migrating = 0;
$sparks = 0;

#$lines_per_flush = 100;            # depends on the PS implementation you use

%color = ( "a", "green",	# active
	   "r", "amber",        # runnable
	   "b", "red",          # blocked
	   "f", "cyan",		# fetching
	   "m", "blue",         # migrating
	   "s", "crimson" );    # sparks

# ---------------------------------------------------------------------------

do print_prolog();

$otime = -1;
$time_of_second_event = 0;
$samples = 0; 

$T[0] = 0; 
$G[0] = 0; 
$A[0] = 0; 
$R[0] = 0; 
$B[0] = 0;
$Y[0] = 0;

while(<STDIN>) {
    next if /^[^0-9]/;   # ignore lines not beginning with a digit (esp. last)
    chop;
    ($time, $event, $tid, $addr, $tid2, $addr2) = split;
    $time_of_second_event = $time         if $time_of_second_event == 0;

    if($time != $otime) {
	$tottime += $G[$samples] * ($time-$T[$samples]);
	$otime = $time;
    }

    if($active > $amax) {
	$amax = $active;
    }

    if ( $opt_D ) {
	if($G[$samples] < $amax && $A[$samples] > 0) {
	    printf(stderr "%% $otime: G $G[$samples], A $A[$samples], " . 
		   "R $R[$samples], B $B[$samples], " .
		   "Y $Y[$samples]\n");
	}
    }

    # Reality Check
    if($G[$samples] < 0 || $A[$samples] < 0 || 
       $R[$samples] < 0 || $B[$samples] < 0 ||
       $Y[$samples] < 0) {
	printf(stderr "Error: Impossible number of tasks at time " .
	       "$T[$samples] (G $G[$samples], A $A[$samples], ".
	       "R $R[$samples], B $B[$samples], Y $Y[$samples])\n") if $opt_v || $opt_D;
	if ( $opt_H ) {  # HACK
	    $G[$samples] = 0  if $G[$samples] < 0;
	    $A[$samples] = 0  if $A[$samples] < 0;
	    $R[$samples] = 0  if $R[$samples] < 0;
	    $B[$samples] = 0  if $B[$samples] < 0;
	    $Y[$samples] = 0  if $Y[$samples] < 0;
	}
    }
    $samples++;

    $eventfrom = substr($event,0,1);
    $eventto = substr($event,1,1);

    printf(stderr "$time $event $eventfrom $eventto\n")   if 0 && $opt_D;
    
    if ($eventfrom eq '*') {
    }

    elsif ($eventfrom eq 'G') {
	--$active;
    }

    elsif ($eventfrom eq 'A') {
	--$runnable;
    }

    elsif ($eventfrom eq 'R') {
	--$blocked;
    }

    elsif ($eventfrom eq 'B') {
	--$sparks;
    }

    elsif ($eventfrom eq 'C') {
	--$migrating;
    }

    elsif ($eventfrom eq 'Y') {
	--$fetching;
    }

    if ($eventto eq '*') {
    }

    elsif ($eventto eq 'G') {
	++$active;
    }

    elsif ($eventto eq 'A') {
	++$runnable;
	$somerunnable = 1;
    }

    elsif ($eventto eq 'R') {
	++$blocked;
	$someblocked = 1;
    }

    elsif ($eventto eq 'B') {
	++$sparks;
	$somesparks = 1;
    }

    elsif ($eventto eq 'C') {
	++$migrating;
	$somemigratory = 1;
    }

    elsif ($eventto eq 'Y') {
	++$fetching;
	$somefetching = 1;
    }


    #printf(stderr "%% $time: G $active, A $runnable, R $blocked, " .
    #	   "B $sparks, C $migrating\n")  if 1;

    printf(stderr "Error: Trying to write at index 0!\n")  if $samples == 0;
    $T[$samples] = $time;
    do set_values($samples,
		  $active,$runnable,$blocked,$fetching,$sparks,$migrating);

   #$G[$samples] = queue_on_a ? $active : 0;
   #$A[$samples] = queue_on_r ? $runnable : 0;
   #$R[$samples] = queue_on_b ? $blocked : 0;
   #$Y[$samples] = queue_on_f ? $fetching : 0;
   #$B[$samples] = queue_on_s ? $sparks : 0;
   #$C[$samples] = queue_on_m ? $migrating : 0;

    $all = $G[$samples] + $A[$samples] + $R[$samples] + $Y[$samples] +
    	   $B[$samples] + $C[$samples] ;

    if($all > $mmax) {
	$mmax = $all; 
    }

    if ( 0 ) {
	print STDERR "%% $time: (act,runnable,blocked,fetch,mig,sp) = " .
	    "($active, $runnable, $blocked, $fetching, $migrating, $sparks)".
		" max = $all\n" ;
    }

    #print STDERR "Sparks @ $time: $sparks \tAll: $all \tMMax: $mmax\n"  if $opt_D;

    if ( $samples >= $slice_width ) {
	do flush_queues();
	$samples = 0;
    }

} # <STDIN>

do flush_queues();
print "%% End\n"  if $opt_C;

# For debugging only
if ($opt_D) {
    printf(stderr "Queue values after last event: " .
	   "$T[$samples] (G $G[$samples], A $A[$samples], ".
	   "R $R[$samples], B $B[$samples], Y $Y[$samples])\n");
}

if($time != $tmax) {
    if ( $pedantic ) {
	die "Error: Calculated time ($time) does not agree with stated max. time ($tmax)\n";
    } else {			# 
	print STDERR "Warning: Calculated time ($time) does not agree with stated max. time ($tmax)\n" if $opt_v;
    }
}

# HACK warning: 
# The real max-y value ($mmax) might differ from the one that is the input 
# to this script ($pmax). If so, we post-process the generated ps-file 
# and place an appropriate scaling  fct into the header of the ps-file.
# This is done by yet another perl-script: 
#		  ps-scale-y <y-scaling-factor> <ps-file>

if($pmax != $mmax) {
    if ( $pedantic ) {
	die "Error: Calculated max no. of tasks ($mmax) does not agree with stated max. no. of tasks ($pmax)\n";
    } else {
	print STDERR  "Warning: Calculated max no. of tasks ($mmax) does not agree with stated max. no. of tasks ($pmax)\n" if $opt_v;
	$y_scaling = $pmax/$mmax; #((float) $pmax)/((float) $mmax);
    }
}

print "% " . ("-" x 75) . "\n";

if ( $opt_m ) {
	print "0 setgray\n";
} else {
	print "0 0 0 setrgbcolor\n";
}

# Print optional str
    if ( $opt_s ) {
	print("HB16 setfont ($opt_s) dup stringwidth pop 790 exch sub 500 moveto show\n");
    }

    print("unscale-y\n");

# Average Parallelism
if($time > 0) {
    if ( $opt_S ) {        #  HACK warning; is this *always* correct -- HWL
	$avg = ($tottime-$time_of_second_event)/($time-$time_of_second_event);
    } else {
	$avg = $tottime/$time;
    }
    if ( $opt_d ) {        # Print date instead of average parallelism
	print("HE14 setfont ($date) dup stringwidth pop 790 exch sub 515 moveto show\n");
    } else { 
	$avgs=sprintf("Average Parallelism = %0.1f\n",$avg);
	print("HE14 setfont ($avgs) dup stringwidth pop 790 exch sub 515 moveto show\n");
    }
    $rt_str=sprintf("Runtime = %0.0f\n",$tmax);
    print("HE14 setfont ($rt_str) dup stringwidth pop 790 exch sub 20 moveto show\n");
}

# do print_y_axis();

# -----------------------------------------------------------------------------
# Draw axes lines etc
# -----------------------------------------------------------------------------

if ( ! $opt_S ) {

# Draw dashed line for orientation (startup time)   -- HWL

if ( $draw_lines ) {
    local($x, $y);
    $x = int((500000/$tmax) * ($xmax-$xmin) + $xmin);
    $y = int((0/$pmax) * ($ymax-$ymin) + $ymin);
    $h = ($ymax-$ymin);

    print "gsave\n" .
	  "[1 3] 1 setdash\n" .
	  "$x $y moveto 0 $h rlineto stroke\n" .
	  "grestore\n";
}

# and another one at the second event                        -- HWL

print STDERR "Time of second event is: $time_of_second_event"  if 0 && $opt_D;

if ( $draw_lines ) {
    local($x, $y);
    $x = int(($time_of_second_event/$tmax) * ($xmax-$xmin) + $xmin);
    $y = int((0/$pmax) * ($ymax-$ymin) + $ymin);
    $h = ($ymax-$ymin);

    print "gsave\n";
    if ( ! $opt_m ) {
	print "green setrgbcolor\n";
    }
    print "[3 5] 1 setdash\n" .
	  "$x $y moveto 0 $h rlineto stroke\n" .
	  "grestore\n";
}

}

# -----------------------------------------------------------------------------

# Logo
print("HE14 setfont\n");
if ($opt_m) {
    print("50 520 asciilogo\n");                          
} else {
    print("50 520 logo\n");                          
}

# Epilogue
print("showpage\n");

if ( $y_scaling != 1.0 ) {
    print "%% y_scaling: $y_scaling\t max: $mmax\n";
}

exit 0 ;

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# -----------------------------------------------------------------------------
# Draw the current slice of the overall graph. 
# This routine is called if a slice of data is full (i.e. $T[0..$samples],
# $G[0..$slice_width] etc with $samples==$slice_width contain data from the 
# input file) or if the end of the input has been reached (i.e. $samples<=
# $slice_width). Note that the last value of the current slice is stored as
# the first value for the next slice.
# -----------------------------------------------------------------------------

sub flush_queues { 
    local ($x_norm, $y_norm);
    local ($index);
    local ($last_x, $last_y, $in_seq) = (-1, -1, 0);
    local ($foo_x, $foo_y);

    if ( $samples == 0 ) { return ; }

    # print "% First sample: T $T[0] (G $G[0], A $A[0], ".
    #	" R $R[0], B $B[0], Y $Y[0])\n"   if $opt_C;

    $rshow = reverse($show);
    print STDERR "\nReversed info-mask is : $rshow"  if 0 && $opt_D;
    print STDERR "\nMaximal y value is $pmax"        if 0 && $opt_D;
    for ($j=0; $j<length($rshow); $j++) {
	$q = substr($rshow,$j,1);
	# print  "% Queue = $q i.e. " . ($color{$q}) . " counts at first sample: " . &count($q,0) ."\n"  if $opt_C;
	do init_psout($q, $T[0], &count($q,0));
	for($i=1; $i <= $samples; $i++) {
	    do psout($T[$i],&count($q,$i));
	}
	print $color{$q} . " F\n";
	($foo_x, $foo_y) = &normalize($T[$samples],&count($q,$samples));
	print "%% Last " . ($color{$q}) . " is " . &get_queue_val($q,$samples) ."  (" . $T[$samples] . ", " . &count($q,$samples) . ") -> ($foo_x,$foo_y)\n"  if $opt_C;
	# print($color{$q} . " flush-it\n");
	# print("$xmax $ymin L\n");
    }
    do wrap($samples);

    #print "% Last sample  T $T[$samples] (G $G[$samples], A $A[$samples], ".
    #      " R $R[$samples], B $B[$samples], Y $Y[$samples])\n"  if $opt_C;
}

# -----------------------------------------------------------------------------
# Scale the (x,y) point (x is time in cycles, y is no. of tasks) s.t. the 
# x-(time-) axis fits between $xmin and $xmax (range for .ps graph).
# In case of optimization ($opt_O):
#  If there is a sequence of (x,y) pairs with same x value, then just 
#  print the first and the last pair in the seqence. To do that, $last_x
#  always contains the scaled x-val of the last point. $last_y contains
#  the y-val of the last point in the current sequence (it is 0 outside a 
#  sequence!).
# -----------------------------------------------------------------------------

sub normalize {
    local($x, $y ) = @_;
    local($x_norm, $y_norm );

    if ( $opt_S ) {
	$x_norm = int(( ($x-$time_of_second_event)/($tmax-$time_of_second_event)) * ($xmax-$xmin) + $xmin);
    } else {
	$x_norm = int(($x/$tmax) * ($xmax-$xmin) + $xmin);
    }
    $y_norm = int(($y/$pmax) * ($ymax-$ymin) + $ymin);

    return (($x_norm, $y_norm));
}

# -----------------------------------------------------------------------------

sub init_psout {
    local ($q, $x, $y) = @_;
    local ($x_norm, $y_norm);

    ($last_x, $last_y, $in_seq) = (-1, -1, 0);
    ($x_norm, $y_norm) =  &normalize($T[0],&count($q,0));
    $last_x = $x_norm;
    $last_y = $y_norm;
    print "%% Begin " . ($color{$q}) . "  (" . $T[0] . ", " . &count($q,0) . ") -> ($x_norm,$y_norm)\n" if $opt_C;
    print $x_norm, " ", $y_norm, " M\n";

}

# ----------------------------------------------------------------------------

sub psout {
    local($x_in, $y_in ) = @_;
    local($x, $y );

    ($x, $y) = &normalize($x_in, $y_in);
    die "Error in psout: Neg x coordinate\n"  if ($x < 0) ;

    if ( $opt_O ) {
	if ( $last_x == $x ) {      # If seq before $x that then print last pt
	    if ( ! $in_seq ) {
		$in_seq = 1;
		$first_y = $last_y;
	    }
	} else {                    # If seq with same $x val then ignore pts
	    if ( $in_seq ) {        # Seq before that -> print last in seq
		print("$last_x $last_y L\n")  if ($first_y != $last_y);
		$in_seq = 0;
	    }
	    print("$x $y L\n");
	}
	$last_x = $x;
	$last_y = $y;
    } else {
	print("$x $y L\n");
    }
}

# -----------------------------------------------------------------------------

sub queue_on {
    local ($queue) = @_;

    return index($show,$queue)+1;
}

# -----------------------------------------------------------------------------

sub count {
    local ($queue,$index) = @_;
    local ($res);

    $where = &queue_on($queue);
    $res = (($queue_on_a && ($queue_on_a<=$where))  ? $G[$index] : 0) +
	   (($queue_on_r && ($queue_on_r<=$where))  ? $A[$index] : 0) +
	   (($queue_on_b && ($queue_on_b<=$where))  ? $R[$index] : 0) +
	   (($queue_on_f && ($queue_on_f<=$where))  ? $Y[$index] : 0) +
	   (($queue_on_m && ($queue_on_m<=$where))  ? $C[$index] : 0) +
           (($queue_on_s && ($queue_on_s<=$where))  ? $B[$index] : 0);

    return $res;
}
    
# -----------------------------------------------------------------------------

sub set_values {
    local ($samples,
	   $active,$runnable,$blocked,$fetching,$sparks,$migrating) = @_;

    $G[$samples] = $queue_on_a ? $active : 0;   
    $A[$samples] = $queue_on_r ? $runnable : 0; 
    $R[$samples] = $queue_on_b ? $blocked : 0;  
    $Y[$samples] = $queue_on_f ? $fetching : 0; 
    $B[$samples] = $queue_on_s ? $sparks : 0;   
    $C[$samples] = $queue_on_m ? $migrating : 0;
}

# -----------------------------------------------------------------------------

sub set_queue_val {
    local ($queue,$index,$val) = @_;

    if    ( $queue == "a" ) { $G[$index] = $val; }
    elsif ( $queue == "r" ) { $A[$index] = $val; }
    elsif ( $queue == "b" ) { $R[$index] = $val; }
    elsif ( $queue == "f" ) { $Y[$index] = $val; }
    elsif ( $queue == "m" ) { $C[$index] = $val; }
    elsif ( $queue == "s" ) { $B[$index] = $val; }
}

# -----------------------------------------------------------------------------

sub wrap {                # used in flush_queues at the end of a slice
    local ($index) = @_;

    $T[0] = $T[$index];

    $G[0] = $G[$index];
    $A[0] = $A[$index];
    $R[0] = $R[$index];
    $Y[0] = $Y[$index];
    $B[0] = $B[$index];
    $C[0] = $C[$index];
}

# -----------------------------------------------------------------------------

sub get_queue_val {
    local ($queue,$index) = @_;

    if ( $queue == "a" ) { return $G[$index]; }
    elsif ( $queue == "r" ) { return $A[$index]; }
    elsif ( $queue == "b" ) { return $R[$index]; }
    elsif ( $queue == "f" ) { return $Y[$index]; }
    elsif ( $queue == "m" ) { return $C[$index]; }
    elsif ( $queue == "s" ) { return $B[$index]; }
}

# -----------------------------------------------------------------------------

sub get_date {
    local ($date);

    chop($date = `date`);
    return ($date);
}

# -----------------------------------------------------------------------------

sub print_prolog {
    local ($now);

    $now = do get_date();

    print("%!PS-Adobe-2.0\n");
    print("%%BoundingBox:    0 0 560 800\n");
    print("%%Title:          Activity Profile\n");
    print("%%Creator:        qp2ps\n");
    print("%%StartTime:      $date\n");
    print("%%CreationDate:   $now\n");
    print("%%Copyright:      1995, 1996 by Hans-Wolfgang Loidl, University of Glasgow\n");
    print("%%EndComments\n");
    #print ("/greenlineto {1.0 setlinewidth lineto} def\n");
    #print ("/amberlineto {0.5 setlinewidth lineto} def\n");
    #print ("/redlineto {1.5 setlinewidth lineto} def\n");
    #print ("/G {newpath moveto greenlineto stroke} def\n");
    #print ("/A {newpath moveto amberlineto stroke} def\n");
    #print ("/R {newpath moveto redlineto stroke} def\n");

    if ( $opt_m ) {
	print  "/red { 0 } def\n";
	print  "/green { 0.5 } def\n";
	print  "/blue { 0.7 } def\n";
	print  "/crimson { 0.8 } def\n";
	print  "/amber { 0.9 } def\n";
	print  "/cyan { 0.3 } def\n";
    } else {
	print  "/red { 0.8 0 0 } def\n";
	print  "/green { 0 0.9 0.1 } def\n";
	print  "/blue { 0 0.1 0.9 } def\n";
	print  "/crimson { 0.7 0.5 0 } def\n";
	print  "/amber { 0.9 0.7 0.2 } def\n";
	print  "/cyan { 0 0.6 0.9 } def\n";
    }

    print  "/printText { 0 0 moveto (GrAnSim) show } def\n";      
    
    if ( $opt_m ) {
	print "/logo { gsave \n" .
	    "        translate \n" .
	    "        .95 -.05 0\n" .
	    "          { setgray printText 1 -.5 translate } for \n" .
	    "        1 setgray printText\n" . 
	    "        grestore } def\n";
    } else {
	print "/logo { gsave \n" .
	      "        translate \n" .
	      "        .95 -.05 0\n" .
	      "          { dup 1 exch sub 0 exch setrgbcolor printText 1 -.5 translate } for \n" . 
	      "        1 0 0 setrgbcolor printText\n" . 
	      "        grestore} def\n";
    }
    
    print "/asciilogo { 5 sub moveto HB16 setfont (GrAnSim) show } def\n";
    print "/cmpx {pop exch pop eq} def             % compare x-coors of 2 points\n";
    print "/cmpy {exch pop 3 2 roll pop eq} def    % compare y-coors of 2 points\n";
    print "/cmp {2 index eq {exch pop eq}          % compare 2 points\n";
    print "                 {pop pop pop false} ifelse } def\n";

    # Hook for scaling just the graph and y-axis
    print "% " . "-" x 77 . "\n";
    print "/scale-y { } def\n";
    print "/unscale-y { } def\n";
    
    print "% " . "-" x 77 . "\n";
    print "/str-len 12 def\n";
    print "/prt-n { cvi str-len string cvs \n" .
          "         dup stringwidth pop \n" .
	  "         currentpoint pop 780 gt { 10 sub } { 2 div } ifelse \n" .
          "         neg 0 rmoveto \n" . 
          "         show  } def \n" .
	  "        % print top-of-stack integer centered at the current point\n";
    # NB: These PostScript functions must correspond to the Perl fct `normalize'
    #  Currently normalize defines the following trafo on (x,y) values:
    #  $x_norm = int(($x/$tmax) * ($xmax-$xmin) + $xmin);
    #  $y_norm = int(($y/$pmax) * ($ymax-$ymin) + $ymin);

    print "/total-len $tmax def\n";
    print "/show-len $xmax def\n";
    print "/x-offset $xmin def\n";
    print "/y-offset $ymin def\n";
    print "/normalize { total-len div show-len x-offset sub mul x-offset add floor } def\n";
    print "% " . "-" x 77 . "\n";
    print "%/L { lineto } def\n";
    print "%/L {2 copy pop 1 sub currentpoint exch pop lineto lineto} def\n";
    print "/L {2 copy currentpoint cmpx not\n";
    print "     {2 copy pop currentpoint exch pop lineto} if\n";
    print "    2 copy currentpoint cmpy \n";
    print "     {pop pop} \n";
    print "     {lineto} ifelse\n";
    print "} def\n";
    print "/F { % flush a segment of the overall area; Arg: color\n";
    print "            currentpoint pop $ymin lineto closepath\n";
    if ( $opt_m ) {
	print "            setgray fill \n";
    } else {
	print "            setrgbcolor fill \n";
    }
    print "} def\n";
    print "/M {  % Start drawing a slice (vert. line and moveto startpoint)\n";
    print "      % Arg: x y\n";
    print "      newpath 1 index $ymin moveto lineto\n";
    print "} def\n";
    print "% For debugging PS uncomment this line and add the file behandler.ps\n";
    print "% $brkpage begin printonly endprint \n";
    print("/HE10 /Helvetica findfont 10 scalefont def\n");
    print("/HE12 /Helvetica findfont 12 scalefont def\n");
    print("/HE14 /Helvetica findfont 14 scalefont def\n");
    print("/HB16 /Helvetica-Bold findfont 16 scalefont def\n");
    print "% " . "-" x 77 . "\n";

    print("-90 rotate\n");
    print("-785 30 translate\n");
    print("newpath\n");
    print("0 8 moveto\n");
    print("0 525 760 525 8 arcto\n");
    print("4 {pop} repeat\n");
    print("760 525 760 0 8 arcto\n");
    print("4 {pop} repeat\n");
    print("760 0 0 0 8 arcto\n");
    print("4 {pop} repeat\n");
    print("0 0 0 525 8 arcto\n");
    print("4 {pop} repeat\n");
    print("0.500000 setlinewidth\n");
    print("stroke\n");
    print("newpath\n");
    print("4 505 moveto\n");
    print("4 521 752 521 4 arcto\n");
    print("4 {pop} repeat\n");
    print("752 521 752 501 4 arcto\n");
    print("4 {pop} repeat\n");
    print("752 501 4 501 4 arcto\n");
    print("4 {pop} repeat\n");
    print("4 501 4 521 4 arcto\n");
    print("4 {pop} repeat\n");
    print("0.500000 setlinewidth\n");
    print("stroke\n");
    
    print("HE14 setfont\n");
    print("100 505 moveto\n");
    print("($pname ) show\n");
    
    # print("($date) dup stringwidth pop 750 exch sub 505 moveto show\n");
    
    print("4 8 moveto\n");
    print("4 24 756 24 4 arcto\n");
    print("4 {pop} repeat\n");
    print("756 24 756 4 4 arcto\n");
    print("4 {pop} repeat\n");
    print("756 4 4 4 4 arcto\n");
    print("4 {pop} repeat\n");
    print("4 4 4 24 4 arcto\n");
    print("4 {pop} repeat\n");
    print("0.500000 setlinewidth\n");
    print("stroke\n");

# Labels 

# x-range: 100 - 600
# y-value: 

    $x_begin = 100;
    $x_end = 600; 
    $y_label = 10;

    $no_of_labels = length($show);  # $info_level;

    $step = ($x_end-$x_begin)/($no_of_labels);

    $x_now = $x_begin;

    if ( $queue_on_a ) {
	do print_box_and_label($x_now,$y_label,"green","running");
    }

    if ( $queue_on_r  ) {
	$x_now += $step;
	do print_box_and_label($x_now,$y_label,"amber","runnable");
    }

    if ( $queue_on_f ) {
	$x_now += $step;
	do print_box_and_label($x_now,$y_label,"cyan","fetching");
    }

    if ( $queue_on_b ) {
	$x_now += $step;
	do print_box_and_label($x_now,$y_label,"red","blocked");
    }

    if ( $queue_on_m ) {
	$x_now += $step;
	do print_box_and_label($x_now,$y_label,"blue","migrating");
    }

    if ( $queue_on_s ) {
	$x_now += $step;
	do print_box_and_label($x_now,$y_label,"crimson","sparked");
    }
    
    # Print runtime of prg; this is jus a crude HACK; better: x-axis!  -- HWL
    #print("HE10 setfont\n");
    #print("680 10 moveto\n");
    #print("(RT: $tmax) show\n");

    print("-40 -10 translate\n");
    
    do print_x_axis();

    print("$xmin $ymin moveto\n");
    if ( $opt_m ) {
	print "0 setgray\n";
    } else {
	print "0 0 0 setrgbcolor\n";
    }

    do print_y_axis();

    print("scale-y\n");

}

# -----------------------------------------------------------------------------

sub print_box_and_label {
    local ($x,$y,$color,$label) = @_;
    local ($z) = (15);

    print("$x 10 moveto\n");
    print("0 10 rlineto\n");
    print("10 0 rlineto\n");
    print("0 -10 rlineto\n");
    print("closepath\n");
    print("gsave\n");
    if ( $opt_m ) { 
	print("$color setgray\n");
    } else {
	print("$color setrgbcolor\n");
    }
    print("fill\n");
    print("grestore\n");
    print("stroke\n");
    print("HE14 setfont\n");
    print(($x+$z) . " 10 moveto\n");
    print("($label) show\n");

}

# -----------------------------------------------------------------------------

sub print_x_axis {

    print "% " . "-" x 77 . "\n";
    print "% X-Axis:\n";
    print "/y-val $ymin def\n";
    print "0.5 setlinewidth\n";
    print "x-offset y-val moveto total-len normalize x-offset sub 0 rlineto stroke\n";
    print "0 total-len 10 div total-len\n" .
          " { dup normalize dup y-val moveto 0 -2 rlineto stroke  % tic\n" .
          "   y-val 10 sub moveto HE10 setfont round prt-n  % print label \n" .
	  " } for \n";
    print "1 setlinewidth\n";
    print "% End X-Axis:\n";
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

    print("%scale-y  % y-axis outside scaled area if ps-scale-y rebuilds it!\n");

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

    print("HE12 setfont\n$scalex $ymin moveto\n$scalex $ymax lineto\n");
    print("% Max number of tasks: $pmax\n");
    print("% Number of ticks: $majorticks\n");

    print "0.5 setlinewidth\n";

    $y = $ymax; # (($pmax - $ymin)/$majorticks) * ($majorticks-$i) + $ymin;
    print("$scalex $y moveto\n$major $y lineto\n");
    print("$markx $y moveto\n($pmax) show\n");

    $majormax = int($pmax/$majorticks)*$majorticks;
    $smax = $majormax*(($ymax-$ymin)/$pmax)+$ymin;
    $majorint = $majormax/$majorticks;

    for($i=1; $i <= $majorticks; ++$i) {
	$y = (($smax - $ymin)/$majorticks) * ($majorticks-$i) + $ymin;
	$majorval = int($majorint * ($majormax/$majorint-$i));
	print("$scalex $y moveto\n$major $y lineto\n");
	print("$markx $y moveto\n($majorval) show\n");
    }

    # print("$xmin $ymax moveto\n10 0 rlineto\n10 0 rmoveto\n($pmax) show\n");
    print " stroke\n";
    print "1 setlinewidth\n";
    print "%unscale-y\n";
    print "% End Y-Axis.\n";
    print "% " . ("-" x 75) . "\n";
}

# -----------------------------------------------------------------------------

sub print_verbose_message {

    print STDERR "Prg Name: $pname  \nDate: $date  \nInfo-str: $show\n";
    print STDERR "Input: stdin  Output: stdout\n";
    print STDERR "The following queues are turned on: " .
	  ( $queue_on_a ? "active, " : "") .   
	  ( $queue_on_r ? "runnable, " : "") . 
	  ( $queue_on_b ? "blocked, " : "") .  
          ( $queue_on_f ? "fetching, " : "") . 
          ( $queue_on_m ? "migrating, " : "") .
	  ( $queue_on_s ? "sparks" : "") .
          "\n";
    if ( $opt_C ) {
	print STDERR "Inserting check code into .ps file (for check-ps3 script)\n";
    }
    if ( $opt_D )  {
	print STDERR "Debugging is turned ON!\n";
    }
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

    $show = "armfb";
    $draw_lines = 0;

    if ( $opt_i ) { 
	$show = "a"		if info_level == 1;
	$show = "ar"		if info_level == 2;
	$show = "arb"		if info_level == 3;
	$show = "arfb"		if info_level == 4;
	$show = "armfb"		if info_level == 5;
	$show = "armfbs"	if info_level == 6;
    }

    if ( $opt_I ) {
	$show = $opt_I;
    }

    if ( $opt_v ){ 
	$verbose = 1;
    }    

    if ( $opt_l ) {
	$slice_width = $opt_l;
    } else {
	$slice_width = 500;
    }

    $queue_on_a = &queue_on("a");
    $queue_on_r = &queue_on("r"); 
    $queue_on_b = &queue_on("b"); 
    $queue_on_f = &queue_on("f"); 
    $queue_on_s = &queue_on("s"); 
    $queue_on_m = &queue_on("m"); 

# if ($#ARGV == 0) {
#     printf(stderr "usage: qp2ps.pl runtime [prog [date]]\n");
#     exit 1;
# }
}

