#! /usr/local/bin/perl
##############################################################################
#
# Usage: qp2ps.pl [options] <max-x> <prg> <date>
#
# Filter that transforms a quasi-parallel profile (a .qp file) at stdin to  
# a PostScript file at stdout, showing essentially the total number of running,
# runnable and blocked tasks.
#
# Options:
#  -o <file> ... write PS file to <file>
#  -m        ... create mono PostScript file instead a color one.
#  -O        ... compress i.e. try to minimize the size of the .ps file
#  -s <str>  ... print <str> in the top right corner of the generated graph
#  -i <int>  ... info level from 1 to 7; number of queues to display
#  -v        ... be talkative. 
#  -h        ... print help message (this header).
#
##############################################################################

require "getopts.pl";

&Getopts('hvDOmSs:i:I:');  

do process_options();

if ( $opt_v ) {
    do print_verbose_message();
}

# ---------------------------------------------------------------------------
# Init
# ---------------------------------------------------------------------------

$xmin = 100;
$xmax = 790;

$scalex = $xmin;
$labelx = $scalex - 45;
$markx =  $scalex - 30;
$major = $scalex - 5;
$majorticks = 10;

$pmax = 1;
$amax = 0;
$ymin = 50;
$ymax = 500;

$active = 0;
$runnable = 0;
$blocked = 0;
$sparks = 0;
$fetching = 0;

$lines_per_flush = 100;            # depends on the PS implementation you use

%color = ( "a", "green",
           "r", "amber",
	   "b", "red",
	   "f", "cyan",
	   "m", "blue",
	   "s", "crimson" );

# ---------------------------------------------------------------------------

do print_prolog();

$otime = -1;
$last_x = -1;
$last_y = -1;
$in_seq = 0;
$time_of_second_event = 0;

while(<STDIN>) {
    chop;
    ($time, $event, $tid, $addr, $tid2, $addr2) = split;
    $time_of_second_event = $time         if $time_of_second_event == 0;

    if($time != $otime) {
	$tottime += $G[$samples] * ($time-$T[$samples]);

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
		         "R $R[$samples], B $B[$samples], Y $Y[$samples])\n");
       }
	$samples++;
	$otime = $time;
    }

    $eventfrom = substr($event,0,1);
    $eventto = substr($event,1,1);

    printf(stderr "$time $event $eventfrom $eventto\n")   if $opt_D;
    
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

   printf(stderr "%% $time: G $active, A $runnable, R $blocked, " .
	         "B $sparks, C $migrating\n")  if 0;

   $T[$samples] = $time;
   $G[$samples] = &queue_on("a") ? $active : 0;
   $A[$samples] = &queue_on("r") ? $runnable : 0;
   $R[$samples] = &queue_on("b") ? $blocked : 0;
   $Y[$samples] = &queue_on("f") ? $fetching : 0;
   $B[$samples] = &queue_on("s") ? $sparks : 0;
   $C[$samples] = &queue_on("m") ? $migrating : 0;

    $all = $G[$samples] + $A[$samples] + $R[$samples] + $Y[$samples] +
    	   $B[$samples] + $C[$samples] ;

    if($all > $pmax) {
	$pmax = $all; 
    }
}

if($time != $tmax) {
    die "Error: Calculated time ($time) does not agree with stated max. time ($tmax)\n";
}

# Print optional str
    if ( $opt_s ) {
	print("HB16 setfont ($opt_s) dup stringwidth pop 790 exch sub 500 moveto show\n");
    }

# Average Parallelism
if($time > 0) {
    if ( 0 ) {        #  HACK warning; is this *always* correct -- HWL
	$avg = ($tottime-$time_of_second_event)/($time-$time_of_second_event);
    } else {
	$avg = $tottime/$time;
    }
    $avgs=sprintf("Average Parallelism = %0.1f\n",$avg);
    print("HE14 setfont ($avgs) dup stringwidth pop 790 exch sub 525 moveto show\n");
    $rt_str=sprintf("Runtime = %0.0f\n",$tmax);
    print("HE14 setfont ($rt_str) dup stringwidth pop 790 exch sub 30 moveto show\n");
}

# -----------------------------------------------------------------------------
# Draw axes lines etc
# -----------------------------------------------------------------------------

do print_y_axis();

# if ( ! $opt_S ) {

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

print STDERR "Time of second event is: $time_of_second_event"  if $opt_D;

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

# }

# -----------------------------------------------------------------------------
# Draw the different kinds of tasks
# -----------------------------------------------------------------------------

$rshow = reverse($show);
print STDERR "\nReversed info-mask is : $rshow"  if $opt_D;
print STDERR "\nMaximal y value is $pmax"        if $opt_D;
for ($j=0; $j<length($rshow); $j++) {
    $x = substr($rshow,$j,1);
    print STDERR "Queue = $x i.e. " . ($color{$x}) . "\n"  if $opt_D;
    print("$xmin $ymin moveto\n");
    for($i=1; $i <= $samples; $i++) {
	do psout($T[$i],&count($x,$i));
	if ($i % $lines_per_flush == 0) {
	    print($color{$x} . " flush-it\n");
	}
    }
    # print("$xmax $ymin L\n");

    if ( $opt_m ) { 
	print "closepath " . ($color{$x}) . " setgray fill\n";
    } else {
	print "closepath " . ($color{$x}) . " setrgbcolor fill\n";
    }
}

# -----------------------------------------------------------------------------


# Logo
print("HE14 setfont\n");
if ( $opt_m ) {
    print("50 530 asciilogo\n");                          
} else {
    print("50 530 logo\n");                          
}

# Epilogue
print("showpage\n");

exit 0;

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

sub psout {
    local($x, $y ) = @_;
    if ( $opt_S ) {
	$x = int(( ($x-$time_of_second_event)/($tmax-$time_of_second_event)) * ($xmax-$xmin) + $xmin);
    } else {
	$x = int(($x/$tmax) * ($xmax-$xmin) + $xmin);
    }
    $y = int(($y/$pmax) * ($ymax-$ymin) + $ymin);

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

sub count{
    local ($queue,$index) = @_;
    local ($res);

    $where = &queue_on($queue);
    $res = ((&queue_on("a") && (&queue_on("a")<=$where))  ? $G[$index] : 0) +
	   ((&queue_on("r") && (&queue_on("r")<=$where))  ? $A[$index] : 0) +
	   ((&queue_on("b") && (&queue_on("b")<=$where))  ? $R[$index] : 0) +
	   ((&queue_on("f") && (&queue_on("f")<=$where))  ? $Y[$index] : 0) +
	   ((&queue_on("m") && (&queue_on("m")<=$where))  ? $B[$index] : 0) +
           ((&queue_on("s") && (&queue_on("s")<=$where))  ? $C[$index] : 0);

    return $res;
}
    
# -----------------------------------------------------------------------------

sub get_date {
    local ($date);

    open (DATE,"date |") || die ("$!");
    while (<DATE>) {
	$date = $_;
    }
    close (DATE);

    return ($date);
}

# -----------------------------------------------------------------------------

sub print_prolog {
    local ($date);

    $date = do get_date();

    print("%!PS-Adobe-2.0\n");
    print("%%BoundingBox:    0 0 560 800\n");
    print("%%Title:          Activity Profile\n");
    print("%%Creator:        qp2ps.pl\n");
    print("%%CreationDate:   $date\n");
    print("%%EndComments\n");
    #print ("/greenlineto {1.0 setlinewidth lineto} def\n");
    #print ("/amberlineto {0.5 setlinewidth lineto} def\n");
    #print ("/redlineto {1.5 setlinewidth lineto} def\n");
    #print ("/G {newpath moveto greenlineto stroke} def\n");
    #print ("/A {newpath moveto amberlineto stroke} def\n");
    #print ("/R {newpath moveto redlineto stroke} def\n");

    if ( $opt_m ) {
	print  "/red { 0.5 } def\n";
	print  "/green { 0 } def\n";
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
    print "%/L { lineto } def\n";
    print "%/L {2 copy pop 1 sub currentpoint exch pop lineto lineto} def\n";
    print "/L {2 copy currentpoint cmpx not\n";
    print "     {2 copy pop currentpoint exch pop lineto} if\n";
    print "    2 copy currentpoint cmpy \n";
    print "     {pop pop} \n";
    print "     {lineto} ifelse\n";
    print "} def\n";
    print "/flush-it { % draw a segment of the overall area; Arg: color\n";
    print "            currentpoint \n";
    print "            1 index 50 lineto closepath\n";
    if ( $opt_m ) {
	print "            3 2 roll setgray fill \n";
    } else {
	print "            5 2 roll setrgbcolor fill \n";
    }
    print "            1 index 50 moveto lineto \n";
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

    if ( &queue_on("a") ) {
	do print_box_and_label($x_now,$y_label,"green","running");
    }

    if ( &queue_on("r")  ) {
	$x_now += $step;
	do print_box_and_label($x_now,$y_label,"amber","runnable");
    }

    if ( &queue_on("f") ) {
	$x_now += $step;
	do print_box_and_label($x_now,$y_label,"cyan","fetching");
    }

    if ( &queue_on("b") ) {
	$x_now += $step;
	do print_box_and_label($x_now,$y_label,"red","blocked");
    }

    if ( &queue_on("m") ) {
	$x_now += $step;
	do print_box_and_label($x_now,$y_label,"blue","migrating");
    }

    if ( &queue_on("s") ) {
	$x_now += $step;
	do print_box_and_label($x_now,$y_label,"crimson","sparked");
    }
    
    # Print runtime of prg; this is jus a crude HACK; better: x-axis!  -- HWL
    #print("HE10 setfont\n");
    #print("680 10 moveto\n");
    #print("(RT: $tmax) show\n");

    print("-40 -20 translate\n");
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

sub print_y_axis {
    local ($i);

# Y-axis label

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

    if ( $opt_m ) {
	print "0 setgray\n";
    } else {
	print "0 0 0 setrgbcolor\n";
    }

    print("HE12 setfont\n$scalex $ymin moveto\n$scalex $ymax lineto\n");

    if ($pmax < $majorticks) {
	$majorticks = $pmax;
    }

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
}

# -----------------------------------------------------------------------------

sub print_verbose_message {

    print "Prg Name: $pname  Date: $date  Info-str: $show\n";
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
    
    if ( $#ARGV != 2 ) {
	print "Usage: $0 [options] <max y value> <prg name> <date> \n";
	print "Use -h option to get details\n";
	exit 1;
    }

    $tmax = $ARGV[0];
    $pname = $ARGV[1];
    $date = $ARGV[2];

    $show = "armfb";

    if ( $opt_S ) {
	$draw_lines = 1;
    } else {
	$draw_lines = 0;
    }

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

# if ($#ARGV == 0) {
#     printf(stderr "usage: qp2ps.pl runtime [prog [date]]\n");
#     exit 1;
# }
}

# -----------------------------------------------------------------------------
# Old way of drawing areas
# -----------------------------------------------------------------------------

exit 0;

# Blocked Tasks
if ($someblocked && ($info_level >= 3)) {
    print("$xmin $ymin moveto\n");
    for($i=($opt_S ? 2 : 1); $i <= $samples; $i++) {
	do psout($T[$i],$G[$i]+$A[$i]+$C[$i]+$B[$i]+$Y[$i]+$R[$i]);
	if ($i % $lines_per_flush == 0) {
	    print("red flush-it\n");
	}
    }
    # print("$xmax $ymin L\n");

    if ( $opt_m ) { 
	print "closepath red setgray fill\n";
    } else {
	print "closepath red setrgbcolor fill\n";
    }
}

# Fetching Tasks
if ($somefetching && ($info_level >= 4)) {
    print("$xmin $ymin moveto\n");
    for($i=($opt_S ? 2 : 1); $i <= $samples; $i++) {
	do psout($T[$i],$G[$i]+$A[$i]+$C[$i]+$B[$i]+$Y[$i]);
	if ($i % $lines_per_flush == 0) {
	    print("cyan flush-it\n");
	}
    }
    # print("$xmax $ymin L\n");

    if ( $opt_m ) { 
	print "closepath cyan setgray fill\n";
    } else {
	print "closepath cyan setrgbcolor fill\n";
    }
}

# Sparks
if ($somesparks && ($info_level >= 6)) {
    print("$xmin $ymin moveto\n");
    for($i=($opt_S ? 2 : 1); $i <= $samples; $i++) {
	do psout($T[$i],$G[$i]+$A[$i]+$C[$i]+$B[$i]);
	if ($i % $lines_per_flush == 0) {
	    print("crimson flush-it\n");
	}
    }
    # print("$xmax $ymin L\n");

    if ( $opt_m ) { 
	print "closepath crimson setgray fill\n";
    } else {
	print "closepath crimson setrgbcolor fill\n";
    }
}

# Migrating Threads
if ($somemigratory && ($info_level >= 5)) {
    print("$xmin $ymin moveto\n");
    for($i=($opt_S ? 2 : 1); $i <= $samples; $i++) {
	do psout($T[$i],$G[$i]+$A[$i]+$C[$i]);
	if ($i % $lines_per_flush == 0) {
	    print("blue flush-it\n");
	}
    }
    # print("$xmax $ymin L\n");
    # print("closepath\ngsave\n0.9 setgray\nfill\ngrestore\nstroke\n");
    if ( $opt_m ) { 
	print "closepath blue setgray fill\n";
    } else {
	print "closepath blue setrgbcolor fill\n";
    }
}

# Runnable Tasks
if($somerunnable && ($info_level >= 2)) {
    print("$xmin $ymin moveto\n");
    for($i=($opt_S ? 2 : 1); $i <= $samples; $i++) {
	do psout($T[$i],$G[$i]+$A[$i]);
	if ($i % $lines_per_flush == 0) {
	    print("amber flush-it\n");
	}
    }
    # print("$xmax $ymin L\n");
    # print("closepath\ngsave\n0.9 setgray\nfill\ngrestore\nstroke\n");
    if ( $opt_m ) { 
	print "closepath amber setgray fill\n";
    } else {
	print "closepath amber setrgbcolor fill\n";
    }
}

# Active Tasks
if ($info_level >= 1) {
    print("$xmin $ymin moveto\n");
    for($i=($opt_S ? 2 : 1); $i <= $samples; $i++) {
	do psout($T[$i],$G[$i]);
	if ($i % $lines_per_flush == 0) {
	    print("green flush-it\n");
	}
    }
    # print("$xmax $ymin L\n");
    # print("closepath\ngsave\n0.5 setgray\nfill\ngrestore\nstroke\n");
    if ( $opt_m ) { 
	print "closepath green setgray fill\n";
    } else {
	print "closepath green setrgbcolor fill\n";
    }
}

