#!/usr/local/bin/perl 
#                                       (C) Hans Wolfgang Loidl, November 1994
# ############################################################################
# Time-stamp: <Fri Jun 14 1996 20:21:17 Stardate: [-31]7659.03 hwloidl>
#
# Usage: gr2pe [options] <gr-file>
#
# Create per processor activity profile (as ps-file) from a given gr-file.
# 
# Options:
#  -o <file> ... output file (ps file) has name <file>
#  -m        ... produce monochrome output
#  -M        ... produce a migration graph
#  -S        ... produce a spark graph in a separate file (based on the no. of
#                sparks rather than the no. of runnable threads)
#  -t        ... produce trace of runnable, blocked, fetching threads 
#  -i <n>    ... ``infinity'' for number of blocked tasks (default: 20)
#                all values larger than that are shown with the same width
#  -C        ... do consistency check at each event (mainly for debugging)
#  -h        ... print help message (this text)
#  -v        ... be talkative
#  
# ############################################################################

# die "This script is still under development -- HWL\n"; 

# ----------------------------------------------------------------------------
# Command line processing and initialization
# ----------------------------------------------------------------------------

require "getopts.pl";

&Getopts('hvDCMNmSGti:o:l:p:');  

do process_options();

if ( $opt_v ) {
    do print_verbose_message();
}

# ----------------------------------------------------------------------------
# Global Variables
# ----------------------------------------------------------------------------

$RUNNING = "RUNNING";
$RUNNABLE = "RUNNABLE";
$BLOCKED = "BLOCKED";
$START = "START";
$END = "END";

# Modes for hline
#$LITERATE = 1;
#$NORMALIZING = 2;

%GRAY = (
	 $RUNNING, 0.6,
	 $RUNNABLE, 0.3,
	 $BLOCKED, 0,
	 $START, 0,
	 $END, 0.5);

# Special value showing that no task is running on $pe if in $running[$pe] 
$NO_ID = -1;
$NO_LAST_BG = $NO_LAST_BLOCKED = $NO_LAST_START = -1;

# The number of PEs we have
$nPEs = 32;

# Unit (in pts) of the width for BLOCKED and RUNNABLE line segments
$width_unit = 1; 

# Width of line for RUNNING 
$running_width = 1;

# Offset of BLOCKED and RUNNABLE lines from the center line
$offset = 10;

# Left and right border of the picture; Width of the picture
$left_border = 0;
$right_border = 700;
$total_width = $right_border - $left_border;
$x_scale = 1;

# Height of the picture measured from y-val of first to y-val of last PE
$lower_border = 10;
$upper_border = 490;
$total_height = $upper_border - $lower_border;
$y_scale = 1;

# Constant from where shrinking of x-values (+scaling as usual) is enabled
$very_big = 1E8;

# Factor by which the x values are shrunk (if very big)
$shrink_x = 10000;

# Set format of output of numbers
$# = "%.2g";

# Width of stripes in migration graph
$tic_width = 2;

# If no spark profile should be generate we count the number of spark events
# in the profile to inform the user about existing spark information
if ( !$opt_S ) {
    $spark_events = 0;
}

# ----------------------------------------------------------------------------
# The real thing starts here
# ----------------------------------------------------------------------------

open (IN,"<$input") || die "$input: $!\n";
open (OUT,">$output") || die "$output: $!\n";
open (OUT_MIG,">$output_mig") || die "$output_mig: $!\n"  if $opt_M;
open (OUT_SP,">$output_sp") || die "$output_sp: $!\n"  if $opt_S;
# open (OUT_B,">$output_b") || die "$output_b: $!\n";
# open (OUT_R,">$output_r") || die "$output_r: $!\n";

open(OUT_RA, ">$RUNNABLE_file") || die "$RUNNABLE_file: $!\n"  if $opt_t;
print OUT_RA "# Number of Runnable tasks on all PEs $i\n"      if $opt_t;
open(OUT_BA, ">$BLOCKED_file") || die "$BLOCKED_file: $!\n"    if $opt_t;
print OUT_BA "# Number of Blocked tasks on all PEs $i\n"       if $opt_t;
open(OUT_FA, ">$FETCHING_file") || die "$FETCHING_file: $!\n"  if $opt_t;
print OUT_FA "# Number of Fetching tasks on all PEs $i\n"      if $opt_t;

($pname,$pars,$nPEs,$lat) = &skip_header(IN);


# Fill in the y_val table for all PEs
$offset = (&generate_y_val_table($nPEs)/2);

$x_min = 0;
$x_max = &get_x_max($input);
$y_max = $total_height;
#$y_max = $y_val[$nPEs-1] + offset;

$is_very_big = $x_max > $very_big;

# Max width allowed when drawing lines for BLOCKED, RUNNABLE tasks
$max_width = $offset;

# General init
do init($nPEs);

do write_prolog(OUT,$x_max,$y_max);
do write_prolog(OUT_MIG,$x_max,$y_max)  if $opt_M;
do write_prolog(OUT_SP,$x_max,$y_max)  if $opt_S;
# do write_prolog(OUT_B,$x_max,$y_max);
# do write_prolog(OUT_R,$x_max,$y_max);

while (<IN>) {
    next  if /^$/;                                # Omit empty lines;
    next  if /^--/;                               # Omit comment lines;

    ($event, $time, $id, $pe) = &get_line($_);
    $x_max_ = $time  if $time > $x_max_;

    print OUT_RA "TIME: $time  PEs: " . join(", ",@runnable) .
	         "  SUM: " . &list_sum(@runnable) . "\n"      if $opt_t;
    print OUT_BA "TIME: $time  PEs: " . join(", ",@blocked) .
	         "  SUM: " . &list_sum(@blocked) . "\n"       if $opt_t;
    print OUT_FA "TIME: $time  PEs: " . join(", ",@fetching) .
	         "  SUM: " . &list_sum(@fetching) . "\n"      if $opt_t;

    foo : {
	($event eq "START") && do {
	    # do draw_tic($pe, $time, $START);
	    do draw_bg($pe, $time);
	    $last_bg[$pe] = $time;
	    $running[$pe] = $id;
	    # $where{$id} = $pe + 1;
	    last foo;
	};
	($event eq "START(Q)") && do {
	    #do draw_segment($pe, $time, $RUNNABLE);
	    do draw_bg($pe, $time);
	    $last_bg[$pe] = $time;
	    #$last_runnable[$pe] = $time;
	    $runnable[$pe]++;
	    # $where{$id} = $pe + 1;
	    last foo;
	};
	($event eq "STEALING") && do {
	    do draw_bg($pe, $time);
	    $last_bg[$pe] = $time;
	    $runnable[$pe]--;
	    $where{$id} = $pe + 1;
	    if ( $opt_M ) {
		$when{$id} = $time;
		do draw_tic($pe, $time, $event);
	    }
	    last foo;
	};
	($event eq "STOLEN") && do {
	    # do draw_tic($pe, $time, $START);
	    do draw_bg($pe, $time);
	    $last_bg[$pe] = $time;
	    $running[$pe] = $id;
	    if ( $where{$id} ) { 
		# Ok
	    } else {
		$warn++;
		print "WARNING: No previous location for STOLEN task $id found!" .
		     " Check the gr file!\n";
	    }
	    if ( $opt_M ) {
		do draw_tic($pe, $time, $event);
		do draw_arrow($where{$id}-1,$pe,$when{$id},$time);
	    }
	    last foo;
	};
	($event eq "STOLEN(Q)") && do {
	    #do draw_segment($pe, $time, $RUNNABLE);
	    do draw_bg($pe, $time);
	    $last_bg[$pe] = $time;
	    #$last_runnable[$pe] = $time;
	    $runnable[$pe]++;
	    if ( $where{$id} ) { 
		# Ok
	    } else {
		$warn++;
		print "WARNING: No previous location for STOLEN(Q) task $id found!" .
		    " Check the gr file!\n";
	    }
	    if ( $opt_M ) {
		do draw_tic($pe, $time, $event);
		do draw_arrow($where{$id}-1,$pe,$when{$id},$time);
	    }
	    last foo;
	};
	($event eq "BLOCK") && do {
	    do draw_bg($pe, $time);
	    $last_bg[$pe] = $time;
	    do draw_segment($pe, $time, $BLOCKED)  unless $blocked[$pe] == 0 ;
	    $last_blocked[$pe] = $time;
	    #do draw_segment($pe, $time, $RUNNING);
	    $blocked[$pe]++;
	    $running[$pe] = $NO_ID;
	    last foo;
	};
	($event eq "RESUME") && do {
	    # do draw_tic($pe, $time, $START);
	    do draw_bg($pe, $time);
	    $last_bg[$pe] = $time;
	    do draw_segment($pe, $time, $BLOCKED);
	    $last_blocked[$pe] = $time;
	    $blocked[$pe]--;
	    $running[$pe] = $id;
	    last foo;
	};
	($event eq "RESUME(Q)") && do {
	    #do draw_segment($pe, $time, $RUNNABLE);
	    do draw_bg($pe, $time);
	    $last_bg[$pe] = $time;
	    do draw_segment($pe, $time, $BLOCKED);
	    $last_blocked[$pe] = $time;
	    #$last_runnable[$pe] = $time;
	    $blocked[$pe]--;
	    $runnable[$pe]++;
	    last foo;
	};
	($event eq "END") && do {
	    # do draw_tic($pe, $time, $END);
	    do draw_bg($pe, $time);
	    $last_bg[$pe] = $time;
	    $running[$pe] = $NO_ID;
	    # do draw_segment($pe, $time, $RUNNING);
	    # $last_blocked[$pe] = $time;
	    last foo;
	};
	($event eq "SCHEDULE") && do {
	    # do draw_tic($pe, $time);
	    $last_start[$pe] = $time;
	    do draw_bg($pe, $time);
	    $last_bg[$pe] = $time;
	    $runnable[$pe]--;
	    $running[$pe] = $id;
	    last foo;
	};
	# NB: Check these; they are not yet tested
	($event eq "FETCH") && do {
	    # Similar to BLOCK; but don't draw a block segment
	    do draw_bg($pe, $time);
	    $last_bg[$pe] = $time;
	    #do draw_segment($pe, $time, $BLOCKED)  unless $blocked[$pe] == 0 ;
	    #$last_blocked[$pe] = $time;
	    #$blocked[$pe]++;
	    $fetching[$pe]++;
	    $running[$pe] = $NO_ID;
	    last foo;
	};
	($event eq "REPLY") && do {
	    do draw_bg($pe, $time);
	    $last_bg[$pe] = $time;
	    #do draw_segment($pe, $time, $BLOCKED);
	    #$last_blocked[$pe] = $time;
	    #$blocked[$pe]--;
	    $fetching[$pe]--;
	    $blocked[$pe]++;
	    last foo;
	};
	# These are only processed if a spark pofile is generated, too
	(($event eq "SPARK") || ($event eq "SPARKAT") || ($event eq "ACQUIRED")) && do {
	    if ( !opt_S ) {
		$spark_events++;
	        last foo;
	    }
	    do draw_sp_bg($pe, $time);
	    $last_sp_bg[$pe] = $time;
	    $sparks[$pe]++;
	    last foo;
	};

	(($event eq "USED") || ($event eq "PRUNED") || ($event eq "EXPORTED")) && do {
	    if ( !opt_S ) {
		$spark_events++;
	        last foo;
	    }
	    do draw_sp_bg($pe, $time);
	    $last_sp_bg[$pe] = $time;
	    $sparks[$pe]--;
	    if ( $sparks[$pe]<0 ) {
		print STDERR "Error: Neg. number of sparks @ $time\n";
	    }
	    last foo;
	};

	$warn++;
	print "WARNING: Unknown event: $event\n";
    }
    do check_consistency()  if $opt_M;
}

do write_epilog(OUT,$x_max,$y_max);
do write_epilog(OUT_MIG,$x_max,$y_max)   if $opt_M;
do write_epilog(OUT_SP,$x_max,$y_max)    if $opt_S;
# do write_epilog(OUT_B,$x_max,$y_max);
# do write_epilog(OUT_R,$x_max,$y_max);

close(IN);
close(OUT);
# close(OUT_B);
# close(OUT_R);

close(OUT_MIG) if $opt_M;
close(OUT_SP)  if $opt_S;
close(OUT_BA)  if $opt_t;
close(OUT_RA)  if $opt_t;
close(OUT_FA)  if $opt_t;

#for ($i=0; $i<$nPEs; $i++) {
#    close($OUT_BA[$i]);
#    close($OUT_RA[$i]);
#}

if ($x_max != $x_max_ ) {
    print STDERR "WARNING: Max time ($x_max_) is different from time of last event ($x_max)\n";
}

print "Number of suppressed warnings: $warn\n"  if $warn>0;
print "FYI: The file $input contains $spark_events lines of spark information\n"  if !opt_S && ($spark_events>0);

system "gzip -f1 $RUNNABLE_file"  if $opt_t;
system "gzip -f1 $BLOCKED_file"   if $opt_t;
system "gzip -f1 $FETCHING_file"  if $opt_t;

system "fortune -s"  if $opt_v;

exit 0;

# ----------------------------------------------------------------------------
# This translation is mainly taken from gr2qp.awk
# This subroutine returns the event found on the current line together with
# the relevant information for that event. The possible EVENTS are:
#  START, STARTQ, STOLEN, BLOCK, RESUME, RESUMEQ, END, SCHEDULE
# ----------------------------------------------------------------------------

sub get_line {
  local ($line) = @_;
  local ($f, @fs);
  local ($event, $time, $id, $pe);

  @fs = split(/[:\[\]\s]+/,$line);
  $event = $fs[3];
  $time = $fs[2];
  $id = $fs[4];
  $pe = $fs[1];

  print OUT "% > " . $_   if $opt_D;
  print OUT "%   EVENT = $event; TIME = $time; ID = $id; PE = $pe\n" if $opt_D;
  print OUT "%   --> this task comes from PE " . ($where{$id}-1) . "\n"  if $opt_D && $event eq "STOLEN";

  return ($event, $time, $id, $pe);

  # if ($fs[3] eq "START") { 
  #     partprofile = 0; 
  #     print (substr($3,2,length($3)-3))," *G 0 0x" $5; 
  # }
  # if ($fs[3] eq "START(Q)") { 
  #     print (substr($3,2,length($3)-3))," *A 0 0x" $5; 
  # }

 #  if ($fs[3] eq "STOLEN")    { 
  #     print (substr($3,2,length($3)-3))," AG 0 0x" $5; 
  # }

 #  if ($fs[3] eq "BLOCK")     { 
  #     print (substr($3,2,length($3)-3))," GR 0 0x" $5; 
  # }
  # if ($fs[3] eq "RESUME")    { 
  #     print (substr($3,2,length($3)-3))," RG 0 0x" $5, "0 0x0"; 
  # }
  # if ($fs[3] eq "RESUME(Q)") { 
  #     print (substr($3,2,length($3)-3))," RA 0 0x" $5, "0 0x0"; 
  # }
  # if ($fs[3] eq "END")       { 
  #   if (partprofile) {
  #	p rint (substr($9,1,length($9)-1))," *G 0 0x" (substr($5,1,length($5)-1));
  #	p rint (substr($3,2,length($3)-3))," G* 0 0x" (substr($5,1,length($5)-1));
  #   } else {
  #       print (substr($3,2,length($3)-3))," G* 0 0x" (substr($5,1,length($5)-1)); 
  #   }
  # }
  # if ($fs[3] eq "SCHEDULE")  { 
  #     print (substr($3,2,length($3)-3))," AG 0 0x" $5; 
  # }

}

# ----------------------------------------------------------------------------

sub check_consistency {
    local ($i);

    for ($i=0; $i<$nPEs; $i++) {
	if ( $runnable[$i] < 0 ) {
	    print "INCONSISTENCY: PE $i: Size of runnable queue: $runnable[$i] at time $time\n";
            $runnable[$i] = 0 ;
	}
        if  ( $blocked[$i] < 0 ) {
	    print "INCONSISTENCY: PE $i: Size of blocked queue: $blocked[$i] at time $time\n";
            $blocked[$i] = 0 ;
	}
    }
}

# ----------------------------------------------------------------------------

sub get_width {
    local ($n, $type) = @_;

    $warn++   if $n <0;
    print "WARNING: Neg. number of tasks in $type queue: $n!!\n"  if $n <0;
    $n = 0  if $n <0;
    return ( ($type eq $RUNNING) ? ($running_width * $width_unit) : 
	    &min($max_width, $n * $width_unit) );
}

# ----------------------------------------------------------------------------
# Use an intensity between 0 (empty runnable queue) and 1 (`full' runnable
# queue) to abstract from monchrome/color values
# The concrete grayshade/color is computed via PS macros.
# ----------------------------------------------------------------------------

sub get_intensity {
    local ($n) = @_;

    print "SEVERE WARNING: get_intensity: Negative size of runnable queue\n"  if $n<0;

    if ($n >= $inf_block) {
	return 1.0;
    } else {
	return ($n+1)/$inf_block;
    }
}

# ----------------------------------------------------------------------------

sub get_sp_intensity {
    local ($n) = @_;

    print "SEVERE WARNING: get_sp_intensity: Negative size of sparks queue\n"  if $n<0;

    if ($n >= $inf_block) {
	return 1.0;
    } else {
	return ($n+1)/$inf_block;
    }
}

# ----------------------------------------------------------------------------

sub get_shade {
    local ($n) = @_;


    if ($n > $inf_block) {
	return 0.2;
    } else {
	return 0.8 - ($n/$inf_block);
    }
}

# ----------------------------------------------------------------------------

sub max { 
    local($x, $y) = @_;

    return ($x>$y ? $x : $y);
}

# ----------------------------------------------------------------------------

sub min { 
    local($x, $y) = @_;

    return ($x<$y ? $x : $y);
}

# ----------------------------------------------------------------------------

sub list_sum {
    local (@list) = @_;

    local ($sum);

    foreach $x (@list) {
	$sum += $x;
    }

    return ($sum);
}

# ----------------------------------------------------------------------------
# Drawing functions.
# Put on top of funtions that directly generate PostScript.
# ----------------------------------------------------------------------------

sub draw_segment {
    local ($pe, $time, $type) = @_;
    local ($x, $y, $width, $gray);

    if ( $type eq $BLOCKED ) {
	if ( $last_blocked[$pe] == $NO_LAST_BLOCKED ) { return; };
	$width = &get_width($blocked[$pe], $type);
	if ( $width  == 0 ) { return; };
	$y = $stripes_low[$pe] + int($width/2 + 0.5);
	$x = $last_blocked[$pe]; 

	if ( $is_very_big ) {	
	    $x = int($x/$shrink_x) + 1;   # rounded up
	}

	#  $gray = 0.5;  # Ignoring gray level; doesn't change!
	do ps_draw_hline(OUT,$x,$y,$time,$width);   
    } else {
	die "ERROR: Unknow type of line: $type in draw segment\n";
    }

    if ($x < 0 || $y<0) {
	die "Impossiple arguments for ps_draw_hline: ($x,$y); type=$type\n";
    }
    if ($width<0 || $width>$max_width || $gray <0 || $gray > 1) {
	die "Impossible arguments to ps_draw_hline: width=$width; gray=$gray\n";
    }
}

# ----------------------------------------------------------------------------

sub draw_tic {
    local ($pe, $time, $event) = @_;
    local ($x, $y, $lit);

    $ystart = $stripes_low[$pe];
    $yend = $stripes_high[$pe];
    $x = $time;
    if ( $event eq "STEALING" ) {
	$lit = 0;  # i.e. FROM
    } elsif ( ( $event eq "STOLEN") || ( $event eq "STOLEN(Q)" ) ) {
	$lit = 1;  # i.e. TO
    } else {
	die "ERROR: Wrong event $event in draw_tic\n";
    }

    if ( $is_very_big ) {	
	$x = int($x/$shrink_x) + 1;   # rounded up
    }

    if ($x < 0 || $ystart<0 || $yend<0) {
	die "Impossiple arguments for ps_draw_tic: ($x,$ystart,$yend); PE=$pe\n";
    }
    do ps_draw_tic(OUT_MIG,$x,$ystart,$yend,$lit);
}

# ----------------------------------------------------------------------------

sub draw_bg {
    local ($pe,$time) = @_;
    local ($x_start, $x_end, $intensity, $secondary_intensity);

    if ( $last_bg[$pe] == $NO_LAST_BG ) { 
	print OUT "% Omitting BG: NO LAST BG\n" if $opt_D; 
	return; 
    }
    if ( $running[$pe] == $NO_ID ) { 
	print OUT "% BG: NO RUNNING PE -> idle bg\n" if $opt_D; 
	# return;
    }
    $x_start = $last_bg[$pe];  
    $x_end = $time;
    $intensity = ( $running[$pe] == $NO_ID ? 
		      0 : 
	              &get_intensity($runnable[$pe]) );
    $secondary_intensity = ( $running[$pe] == $NO_ID ? 
			        0 : 
	                        &get_intensity($fetching[$pe]) );
    do ps_draw_bg(OUT,$x_start, $x_end, $stripes_low[$pe], $stripes_high[$pe],
		  $intensity,$secondary_intensity);

    if ( $opt_M ) {
	do ps_draw_hline(OUT_MIG, $x_start, $stripes_low[$pe], $x_end, 
			 $mig_width);
    }
    
}

# ----------------------------------------------------------------------------
# Variant of draw_bg; used for spark profile
# ----------------------------------------------------------------------------

sub draw_sp_bg {
    local ($pe,$time) = @_;
    local ($x_start, $x_end, $intensity, $secondary_intensity);

    if ( $last_sp_bg[$pe] == $NO_LAST_BG ) { 
	print OUT_SP "% Omitting BG: NO LAST BG\n" if $opt_D; 
	return; 
    }
    $x_start = $last_sp_bg[$pe];  
    $x_end = $time;
    $intensity = ( $sparks[$pe] <= 0 ? 
		      0 : 
	              &get_sp_intensity($sparks[$pe]) );
    $secondary_intensity = 0; 
    do ps_draw_bg(OUT_SP,$x_start, $x_end, $stripes_low[$pe], $stripes_high[$pe],
		  $intensity,$secondary_intensity);

}

# ----------------------------------------------------------------------------

sub draw_arrow {
    local ($from_pe,$to_pe,$send_time,$arrive_time) = @_;
    local ($ystart,$yend);
    
    $ystart = $stripes_high[$from_pe];
    $yend = $stripes_low[$to_pe];
    do ps_draw_arrow(OUT_MIG,$send_time,$arrive_time,$ystart,$yend);
}

# ----------------------------------------------------------------------------
# Normalize the x value s.t. it fits onto the page without scaling.
# The global values $left_border and $right_border and $total_width 
# determine the borders
# of the graph. 
# This fct is only called from within ps_... fcts. Before that the $x values
# are always times.
# ----------------------------------------------------------------------------

sub normalize {
    local ($x) = @_;

    return (($x-$xmin)/($x_max-$x_min) * $total_width + $left_border);
}

# ----------------------------------------------------------------------------
# PostScript generation functions.
# Lowest level of writing output file.
# Now there is only normalizing mode supported. 
# The following is out of date:
# $mode can be $LITERATE i.e. assuming scaling has been done
#           or $NORMALIZING i.e. no scaling has been done so far (do it in
#                                macros for drawing)
# ----------------------------------------------------------------------------

sub ps_draw_hline {
    local ($OUT,$xstart,$y,$xend,$width) = @_;
    local ($xlen); 

    print $OUT "% HLINE From: ($xstart,$y) to ($xend,$y) (i.e. len=$xlen) with width $width gray $gray\n" if $opt_D; 

    if ( ! $opt_N ) {
	$xstart = &normalize($xstart);
	$xend = &normalize($xend);
    }

    $xlen = $xend - $xstart;

    printf $OUT ("%d %d %d %d L\n",$xstart,$y,$xlen,$width);
    #           ( $mode == $LITERATE ? " L\n" : " N\n");

    # Old version:
    # print $OUT "newpath\n";
    # print $OUT "$GRAY{$type} setgray\n";
    # print $OUT $xend . "  " . $y . " " . $xstart . " " . $y . " " . $width . 
    #    " line\n";
    # print $OUT "stroke\n";
}

# ----------------------------------------------------------------------------

sub ps_draw_vline {
    local ($OUT,$x,$ystart,$yend,$width) = @_;

    print $OUT "% VLINE From: ($x,$ystart) to ($x,$yend) with width $width\n" if $opt_D; 

    if ( ! $opt_N ) {
	$x = &normalize($x);
    }

    print $OUT "newpath\n";
    print $OUT "0 setgray\n";                             # constant gray level
    printf $OUT ("%d %d %d %d %.1g line\n",
		 $x,$yend ,$x,$ystart,$width);
    print $OUT "stroke\n";
}

# ----------------------------------------------------------------------------

sub ps_draw_tic {
    local ($OUT,$x,$ystart,$yend,$lit) = @_;

    print $OUT "% TIC at ($x,$ystart-$yend)\n"   if $opt_D;

    if ( ! $opt_N ) {
	$x = &normalize($x);
    }

    printf $OUT ("%d %d %d %d T\n",$x,$ystart,$yend,$lit);

    # Old version without PostScript macro /tic:
    # print $OUT "newpath\n";
    # print $OUT "ticwidth setlinewidth\n" .
    #	      $x . " " . $y . " ticlen sub moveto\n" .
    #	      $x . " " . $y . " ticlen add lineto\n";
    #print $OUT "stroke\n";
}

# ----------------------------------------------------------------------------

sub ps_draw_arrow {
    local ($OUT,$xstart,$xend,$ystart,$yend) = @_;

    print $OUT "% ARROW from ($xstart,$ystart) to ($xend,$yend)\n"   if $opt_D;

    if ( ! $opt_N ) {
	$xstart = &normalize($xstart);
	$xend = &normalize($xend);
    }

    printf $OUT ("%d %d %d %d A\n",$xstart,$ystart,$xend,$yend);
}

# ----------------------------------------------------------------------------

sub ps_draw_bg {
    local ($OUT,$xstart, $xend, $ystart, $yend, 
	   $intensity, $secondary_intensity) = @_;
    local ($xlen, $ylen);

    print $OUT "% Drawing bg for PE $pe from $xstart to $xend" .
	       "  (intensity: $intensity, $secondary_intensity)\n"  if $opt_D;

    if ( ! $opt_N ) {
	$xstart = &normalize($xstart);
	$xend = &normalize($xend);
    }

    $xlen = $xend - $xstart;
    $ylen = $yend - $ystart;

    printf $OUT ("%d %d %d %d %.2g %.2g R\n",
		 $xstart,$ystart,$xlen,$ylen,$intensity,$secondary_intensity);

    # Old version without PostScript macro /rect:
    #print $OUT "newpath\n";
    #print $OUT " $x_start $y_start moveto\n";
    #print $OUT " $x_end $y_start lineto\n";
    #print $OUT " $x_end $y_end lineto\n";
    #print $OUT " $x_start $y_end lineto\n";
    #print $OUT "closepath\n";
    #print $OUT "$gray setgray\n";
    #print $OUT "fill\n";
}

# ----------------------------------------------------------------------------
# Initialization and such
# ----------------------------------------------------------------------------

sub write_prolog {
    local ($OUT, $x_max, $y_max) = @_;
    local ($date, $dist, $y, $i);

    $date = &get_date();

    if ( $opt_N ) {
      $x_scale = $total_width/$x_max;
      $y_scale = $total_height/$y_max;
    }

    # $tic_width = 2 * $x_max/$total_width;    constant now
    # $tic_len = 4 * $y_max/$total_height;

    print $OUT "%!PS-Adobe-2.0\n";
    print $OUT "%%BoundingBox:   \t0 0 560 800\n";
    print $OUT "%%Title: \t$pname  $pars\n";
    print $OUT "%%Creator: \tgr2pe\n";
    print $OUT "%%CreationDate: \t$date\n";
    # print $OUT "%%Orientation: \tSeascape\n";
    print $OUT "%%EndComments\n";

    # print $OUT "%%BeginSetup\n";
    # print $OUT "%%PageOrientation: \tSeascape\n";
    # print $OUT "%%EndSetup\n";

    print $OUT "%/runlineto {1.5 setlinewidth lineto} def\n";
    print $OUT "%/suspendlineto {0.5 setlinewidth lineto} def\n";
    print $OUT "%/run { newpath moveto 1.5 setlinewidth lineto stroke} def\n";
    print $OUT "%/suspend { newpath moveto 0.5 setlinewidth lineto stroke} def\n";
    print $OUT "\n";
    print $OUT "/total-len $x_max def\n";
    print $OUT "/show-len $total_width def\n";
    print $OUT "/normalize { show-len mul total-len div } def\n";
    print $OUT "/x-normalize { exch show-len mul total-len div exch } def\n";
    print $OUT "/str-len 12 def\n";
    #print $OUT "/prt-n { str-len string cvs show } def" .
    #	       "     % print top-of-stack integer\n";
    print $OUT "/prt-n { cvi str-len string cvs \n" .
	       "         dup stringwidth pop \n" .
	       "         currentpoint pop 780 gt { 10 sub } { 2 div } ifelse \n" .
               "         neg 0 rmoveto \n" . 
               "         show  } def \n" .
	       "        % print top-of-stack integer centered at the current point\n";
    print $OUT "/ticwidth $tic_width def\n";
    print $OUT "%/ticlen $tic_len def     % actually half of the tic-length\n";
    print $OUT "/T    % Draw a tic mark\n" .
               " {    % Operands: x, y-start, y-end of tic, from/to flag \n" .
	       "   newpath\n" .
	       "   0 eq { " . ( $opt_m ? " 0.2 setgray }" 
                                       : " 0 0.7 0.2 setrgbcolor }" ) .
	       "        { " . ( $opt_m ? " 0.8 setgray }" 
                                       : " 0.7 0 0.2 setrgbcolor }" ) . " ifelse\n" .
	       "   ticwidth setlinewidth\n" .
	       "   3 copy pop moveto\n" .
	       "   exch pop lineto\n" .
	       "   stroke\n" .
	       " } def\n";
    #	       "   3 copy pop x-normalize moveto\n" .
    #	       "   exch pop x-normalize lineto\n" .
    #	       "   stroke\n" .
    #	       " } def\n";
    print $OUT "/blocked-gray 0 def\n";
    print $OUT "/idle-gray 1 def\n";
    print $OUT "/blocked-color { 0.2 0.1 0.8 } def\n";
    print $OUT "/idle-color { 0.8 0.1 0.2 } def\n";
    print $OUT "/idle-color-fetch { 0.5 0.6 0.4 } def\n";
    print $OUT "/L              % Draw a line (for blocked tasks)\n" .
	       " {              % Operands: (x,y)-start xlen width\n" .
	       "  newpath \n" .
	       ( $opt_m ? "  blocked-gray setgray\n" : 
                          "  blocked-color setrgbcolor\n") .
	       "         setlinewidth 3 copy pop moveto 0 rlineto pop pop stroke} def\n";
    print $OUT "/N              % Draw a normalized line\n" .
	       " {              % Operands: (x,y)-start xlen width\n" .
	       "  newpath \n" .
	       ( $opt_m ? "  blocked-gray setgray\n" : 
                          "  blocked-color setrgbcolor\n") .
	       "         setlinewidth 3 copy pop x-normalize moveto normalize 0 rlineto pop pop stroke} def\n";
    print $OUT "% /L line def\n";
    print $OUT "/printText { 0 0 moveto (GrAnSim) show } def\n";
    if ( $opt_m ) {
	print $OUT "/logo { gsave \n" .
	           "        translate \n" .
		   "        .95 -.05 0  " .
		   "          { setgray printText 1 -.5 translate } for \n" .
		   "        1 setgray printText\n" . 
		   "        grestore } def\n";
    } else {
	print $OUT "/logo { gsave \n" .
	      "        translate \n" .
	      "        .95 -.05 0\n" .
	      "          { dup 1 exch sub 0 exch setrgbcolor printText 1 -.5 translate } for \n" . 
	      "        1 0 0 setrgbcolor printText\n" . 
	      "        grestore} def\n";
    }

    print $OUT "/asciilogo { 5 sub moveto HB16 setfont (GrAnSim) show } def\n";
    print $OUT  "/starside \n" .
                " {starlen 0 lineto currentpoint translate \n" .
		"    -144 rotate } def\n";

   print $OUT  "/star \n" .
               " { moveto \n" .
	       "   currentpoint translate \n" .
	       "   4 {starside} repeat \n" .
	       "   closepath \n" .
	       "   gsave \n" .
	       "   .7 setgray fill \n" .
	       "   grestore \n" .
	       "   % stroke  \n" .
	       "  } def \n";
    #print $OUT "/get-shade   % compute shade from intensity\n" .
    #	           " { pop 1 exch sub 0.6 mul 0.2 add } def\n";
    if ( $opt_m ) { 
	print $OUT "/from 0.2 def\n";
	print $OUT "/to 0.8 def\n";
	print $OUT "/get-shade   % compute shade from intensity\n" .
	           "  { pop dup 0 eq { pop idle-gray }\n " .
		   "                 { 1 exch sub to from sub mul from add } ifelse } def\n";
	           " { pop 1 exch sub to from sub mul from add } def\n";
    } else {
	print $OUT "/from 0.5 def\n";
	print $OUT "/to 0.9 def\n";
    }
    print $OUT "/epsilon 0.01 def\n";
    print $OUT "/from-blue 0.7 def\n";
    print $OUT "/to-blue   0.95 def\n";
    print $OUT "/m 1 def\n";
    print $OUT "/magnify { m mul dup 1 gt { pop 1 } if } def\n";
    print $OUT "%\n" .
	       "% show no. of runnable threads and the current degree of fetching\n" .
	       "%\n" .
	       "/get-color        % compute color from intensity\n" .
               " { 4 mul dup      % give more weight to second intensity\n" .
	       "   0 eq { pop 0 exch } \n" .
	       "        { from-blue to-blue sub mul from-blue add dup \n" .
	       "          1 gt { pop 1 } if  exch } ifelse \n" .
               "   dup 0 eq { pop pop idle-color }\n" .
               "            { 1 exch sub to from sub mul from add        % green val is top of stack\n" .
	       "              exch 0 3 1 roll  } ifelse } def\n"; 

    print $OUT "%\n";
    print $OUT "% show no. of runable threads only\n";
    print $OUT "%\n";
    print $OUT "/get-color-runnable			% compute color from intensity\n";
    print $OUT "{ pop dup 0 eq { pop idle-color }\n";
    print $OUT "               { 1 exch sub to from sub mul from add   % green val is top of stack\n";
    print $OUT "                 0.2 0 3 1 roll  } ifelse } def\n";

    print $OUT "%\n";
    print $OUT "% show no. of fetching threads only\n";
    print $OUT "%\n";
    print $OUT "/get-color-fetch  			% compute color from intensity\n";
    print $OUT "{ exch pop dup 0 eq { pop idle-color-fetch }\n";
    print $OUT "                    { 1 exch sub to from sub mul from add   % blue val is top of stack\n";
    print $OUT "                      0.2 0.6 3 2 roll  } ifelse } def\n";

    #print $OUT "/get-color    % compute color from intensity\n" .
    #           " { dup 0 eq { pop idle-color }\n" .
    #           "            { 1 exch sub to from sub mul from add 0 exch 0 } ifelse } def\n"; 
    #	       " { dup 0.4 le { 0.4 exch sub 0.2 add 2 mul 0 0 setrgbcolor} " .
    #          "              { 1 exch sub 0.4 add 0 exch 0 setrgbcolor} ifelse \n" .
    print $OUT "/R          % Draw a rectangle \n" .
               " {             % Operands: x y xlen ylen i j \n" .
               "               %    (x,y) left lower start point of rectangle\n" .
               "               %    xlen  length of rec in x direction\n" .
               "               %    ylen  length of rec in y direction\n" .
               "               %    i     intensity of rectangle [0,1] \n" .
               "               %    j     intensity blue to indicate fetching\n" .
               "               %          (ignored in mono mode)\n" .
	       ( $opt_m ? "  get-shade setgray\n" 
		        : "  get-color-runnable setrgbcolor\n" ) .
               "  newpath\n" .
               "  4 copy pop pop moveto\n" .
	       "  1 index  0 rlineto\n" .
	       "  0 index  0 exch rlineto\n" .
	       "  1 index  neg 0 rlineto\n" .
               "  0 index  neg 0 exch rlineto\n" .
               "  pop pop pop pop\n" .
               "  closepath\n" .
               "  fill             % Note: No stroke => no border\n" .
	       " } def\n";
    print $OUT "% /R rect def\n";
    print $OUT "%/A         % Draw an arrow (for migration graph)\n" .
	       "% {         % Operands: x y x' y' \n" .
               "%           %  (x,y)    start point \n" .
               "%           %  (x',y')  end point \n" .
	       ( $opt_m ? "%    0 setgray\n" : "%     0 0 0 setrgbcolor\n" ) .
	       "%  1 setlinewidth\n" .
	       "%  newpath 4 2 roll x-normalize moveto x-normalize lineto stroke } def\n";

    print $OUT "/A         % No arrows \n" .
	       " { pop pop pop pop } def\n";
    print $OUT "-90 rotate\n";
    
    print $OUT "-785 30 translate\n";
    print $OUT "/HE10 /Helvetica findfont 10 scalefont def\n";
    print $OUT "/HE12 /Helvetica findfont 12 scalefont def\n";
    print $OUT "/HE14 /Helvetica findfont 14 scalefont def\n";
    print $OUT "/TI16 /Times-Italic findfont 16 scalefont def\n";
    print $OUT "/HB16 /Helvetica-Bold findfont 16 scalefont def\n";
    print $OUT "% " . "-" x 77 . "\n";

    print $OUT "newpath\n";
    print $OUT "0 8.000000 moveto\n";
    print $OUT "0 525.000000 760.000000 525.000000 8.000000 arcto\n";
    print $OUT "4 {pop} repeat\n";
    print $OUT "760.000000 525.000000 760.000000 0 8.000000 arcto\n";
    print $OUT "4 {pop} repeat\n";
    print $OUT "760.000000 0 0 0 8.000000 arcto\n";
    print $OUT "4 {pop} repeat\n";
    print $OUT "0 0 0 525.000000 8.000000 arcto\n";
    print $OUT "4 {pop} repeat\n";
    print $OUT "0.500000 setlinewidth\n";
    print $OUT "stroke\n";
    print $OUT "newpath\n";
    print $OUT "4.000000 505.000000 moveto\n";
    print $OUT "4.000000 521.000000 752.000000 521.000000 4.000000 arcto\n";
    print $OUT "4 {pop} repeat\n";
    print $OUT "752.000000 521.000000 752.000000 501.000000 4.000000 arcto\n";
    print $OUT "4 {pop} repeat\n";
    print $OUT "752.000000 501.000000 4.000000 501.000000 4.000000 arcto\n";
    print $OUT "4 {pop} repeat\n";
    print $OUT "4.000000 501.000000 4.000000 521.000000 4.000000 arcto\n";
    print $OUT "4 {pop} repeat\n";
    print $OUT "0.500000 setlinewidth\n";
    print $OUT "stroke\n";
    
    print $OUT "% ----------------------------------------------------------\n";
    print $OUT "% Print pallet\n";
    print $OUT "% NOTE: the values for the tics must correspond to start and\n";
    print $OUT "%       end values in /get-color\n";
    print $OUT "gsave \n";
    print $OUT "340 508 translate\n";
    print $OUT "0.0 0.05 1.00 \n";
    print $OUT " { \n";
    print $OUT "  dup dup \n";
    print $OUT "    from epsilon sub gt exch \n";
    print $OUT "    from epsilon add lt \n";
    print $OUT "   and\n";
    print $OUT "    { newpath " .
	       ($opt_m ? "0 setgray " : "0 0 0 setrgbcolor ") .
	       "0 0 moveto 0 -3 rlineto stroke } if\n";
    print $OUT "  dup dup \n";
    print $OUT "    to epsilon 2 mul sub gt exch \n";
    print $OUT "    to epsilon 2 mul add lt \n";
    print $OUT "   and\n";
    print $OUT "    { newpath " . 
	       ($opt_m ? "0 setgray " : "0 0 0 setrgbcolor ") .
	       "10 0 moveto 0 -3 rlineto stroke } if\n";
    print $OUT ($opt_m ? " setgray\n" : "  0 exch 0 setrgbcolor\n");
    print $OUT "  newpath\n";
    print $OUT "  0 0 moveto\n";
    print $OUT "  10 0 rlineto\n";
    print $OUT "  0 10  rlineto\n";
    print $OUT "  -10 0 rlineto\n";
    print $OUT "  closepath\n";
    print $OUT "  fill\n";
    print $OUT "  10 0 translate \n";
    print $OUT " } for\n";
    print $OUT "grestore\n";
 
    print $OUT "% Print pallet for showing fetch\n";
    print $OUT "% NOTE: the values for the tics must correspond to start and\n";
    print $OUT "%       end values in /get-color\n";
    print $OUT "%gsave \n";
    print $OUT "%340 508 translate\n";
    print $OUT "%0.0 0.05 1.00 \n";
    print $OUT "%{ \n";
    print $OUT "%  dup dup \n";
    print $OUT "%    from epsilon sub gt exch \n";
    print $OUT "%    from epsilon add lt \n";
    print $OUT "%   and\n";
    print $OUT "%   { newpath 0 0 0 setrgbcolor 0 0 moveto 0 -3 rlineto stroke } if\n";
    print $OUT "%  dup dup \n";
    print $OUT "%    to epsilon 2 mul sub gt exch \n";
    print $OUT "%    to epsilon 2 mul add lt \n";
    print $OUT "%   and\n";
    print $OUT "%   { newpath 0 0 0 setrgbcolor 10 0 moveto 0 -3 rlineto stroke } if\n";
    print $OUT "%  0.2 exch 0.6 exch setrgbcolor   \n";
    print $OUT "%  newpath\n";
    print $OUT "%  0 0 moveto\n";
    print $OUT "%  10 0 rlineto\n";
    print $OUT "%  0 10  rlineto\n";
    print $OUT "%  -10 0 rlineto\n";
    print $OUT "%  closepath\n";
    print $OUT "%  fill\n";
    print $OUT "%  10 0 translate \n";
    print $OUT "% } for\n";
    print $OUT "% grestore\n";

    print $OUT "% Print double pallet\n";
    print $OUT "% NOTE: the values for the tics must correspond to start and\n";
    print $OUT "%       end values in /get-color\n";
    print $OUT "% gsave \n";
    print $OUT "% 340 500 translate\n";
    print $OUT "% 0.0 0.05 1.00 \n";
    print $OUT "% { \n";
    print $OUT "%   0 exch 0 setrgbcolor   \n";
    print $OUT "%   newpath\n";
    print $OUT "%   0 0 moveto\n";
    print $OUT "%   10 0 rlineto\n";
    print $OUT "%   0 10  rlineto\n";
    print $OUT "%   -10 0 rlineto\n";
    print $OUT "%   closepath\n";
    print $OUT "%   fill\n";
    print $OUT "%   10 0 translate \n";
    print $OUT "% } for\n";
    print $OUT "% grestore\n";
    print $OUT "% gsave \n";
    print $OUT "% 340 510 translate\n";
    print $OUT "% 0.0 0.05 1.00 \n";
    print $OUT "% { \n";
    print $OUT "%   dup dup \n";
    print $OUT "%     from epsilon sub gt exch \n";
    print $OUT "%     from epsilon add lt \n";
    print $OUT "%    and\n";
    print $OUT "%    { newpath 0 0 0 setrgbcolor 0 3 moveto 0 -6 rlineto stroke } if\n";
    print $OUT "%   dup dup \n";
    print $OUT "%     to epsilon 2 mul sub gt exch \n";
    print $OUT "%     to epsilon 2 mul add lt \n";
    print $OUT "%    and\n";
    print $OUT "%    { newpath 0 0 0 setrgbcolor 10 3 moveto 0 -6 rlineto stroke } if\n";
    print $OUT "%    0.7 exch 0 setrgbcolor   \n";
    print $OUT "%    newpath\n";
    print $OUT "%    0 0 moveto\n";
    print $OUT "%    10 0 rlineto\n";
    print $OUT "%    0 10  rlineto\n";
    print $OUT "%    -10 0 rlineto\n";
    print $OUT "%    closepath\n";
    print $OUT "%    fill\n";
    print $OUT "%    10 0 translate \n";
    print $OUT "% } for\n";
    print $OUT "% grestore\n";
    print $OUT "% ----------------------------------------------------------\n";
    print $OUT "HE14 setfont\n";
    print $OUT "100.000000 508.000000 moveto\n";
    print $OUT "($pname  PEs: $nPEs  Lat.: $lat ) show\n";
    
    print $OUT "($date) dup stringwidth pop 750.000000 exch sub 508.000000 moveto show\n";
    print $OUT ( $opt_m ? "5 512 asciilogo\n" : "5 512 logo\n");    
    print $OUT "% 100 500 moveto\n";    

    print $OUT "0 20 translate\n";

    print $OUT "HE14 setfont\n";
    for ($i=0; $i<$nPEs; $i++) {
	$dist = $stripes_high[$i] - $stripes_low[$i];
	$y = $stripes_low[$i] + $dist/2;
	# print $OUT "/starlen $dist def\n";
	# print $OUT "gsave 2 $y star grestore\n";
	print $OUT "  2 " . ($stripes_low[$i]+1) . " moveto ($i) show\n";
    }

    print $OUT "20 0 translate\n";

    print $OUT "% Print x-axis:\n";
    print $OUT "1 setlinewidth\n";
    print $OUT "0 -5 moveto total-len normalize 0 rlineto stroke\n";
    print $OUT "gsave\n" .
	       "[2 4] 1 setdash\n" .
	       "0 0 moveto 0 $total_height rlineto stroke\n" .
	       "% $x_max 0 moveto 0 $total_height rlineto stroke\n" .
	       "grestore\n";
    print $OUT "0 total-len 10 div total-len\n" .
               " { dup normalize dup -5 moveto 0 -2 rlineto stroke  % tic\n" .
               "   -17 moveto HE10 setfont round prt-n  % print label \n" .
	       " } for \n";
    

    print $OUT "$x_scale  $y_scale  scale\n";

    print $OUT "% ++++++++++++++++++++++++++++++++++++++++++++++++++\n\n";
    
    if ( $opt_D ) {
	print $OUT "% Debugging info : \n";

	print $OUT "% Offset is: $offset\n";

	print $OUT "% y_val table: \n";
	for ($i=0; $i<$nPEs; $i++) {
	    print $OUT "% y_val of $i: $y_val[$i]\n";
	}

	print $OUT "% x-max: $x_max; y-max: $y_max\n";
	print $OUT "% Info from header: Prg: $pname; PEs: $nPEs; Lat.: $lat\n";

	print $OUT "% ++++++++++++++++++++++++++++++++++++++++++++++++++\n\n";
    }
}

# ----------------------------------------------------------------------------

sub write_epilog {
    local ($OUT,$x_max, $y_max) = @_;
    local($x_scale,$y_scale);

    print $OUT "showpage\n";
}

# ----------------------------------------------------------------------------

sub get_x_max {
    local ($file) = @_;
    local ($last_line, @fs);

    open (TMP,"tail -1 $file |") || die "tail -1 $file | : $!\n";
    while (<TMP>) {
	$last_line = $_;
    }
    close(TMP);

    @fs = split(/[:\[\]\s]+/,$last_line);

    return $fs[2];
}

# ----------------------------------------------------------------------------
#
#sub get_date {
#    local ($now,$today,@lt);
#
#    @lt = localtime(time);
#    $now = join(":",reverse(splice(@lt,0,3)));
#    $today = join(".",splice(@lt,0,3));
#
#    return $now . " on " . $today;
#}
#
# ----------------------------------------------------------------------------

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

sub generate_y_val_table {
    local ($nPEs) = @_;
    local($i, $y, $dist);

    $dist = int($total_height/$nPEs);
    for ($i=0, $y=1; $i<$nPEs; $i++, $y+=$dist) {
	$y_val[$i] = $y + $lower_border;
	$stripes_low[$i] = $y;
	$stripes_high[$i] = $y+$dist-2;
    }

    # print $OUT "10 5 translate\n";

    return ($dist);
}

# ----------------------------------------------------------------------------

sub init { 
    local ($nPEs) = @_;
    local($i);

    for ($i=0; $i<$nPEs; $i++) {
	if ( $opt_S ) {
	    $sparks[$i] = 0;
	}
	$blocked[$i] = 0;
	$runnable[$i] = 0;
	$fetching[$i] = 0;
	$running[$i] = $NO_ID;
	if ( $opt_S ) {
	    $last_sp_bg[$i] = $NO_LAST_BG;
	}
	$last_bg[$i] = $NO_LAST_BG;
	$last_start[$i] = $NO_LAST_START;
	$last_blocked[$i] = $NO_LAST_BLOCKED;
	$last_runnable[$i] = 0; 
	#open($OUT_RA[$i], "PE". $i . ".dat") || die "PE".$i."-R.dat: $!\n";
	#print $OUT_RA[$i] "# Number of Runnable tasks on PE $i\n";
	#open($OUT_BA[$i], "PE". $i . ".dat") || die "PE".$i."-B.dat: $!\n";
	#print $OUT_BA[$i] "# Number of Blocked tasks on PE $i\n";
    } 
    
}


# ----------------------------------------------------------------------------

sub skip_header {
    local ($FILE) = @_;
    local($prg, $pars, $nPEs, $lat, $fetch, $in_header);

    $in_header = 9;
    while (<$FILE>) {
	if ( $in_header = 9 ) {
	    if (/^=/) {
		$gum_style_gr = 1;
		$in_header = 0;
		$prg = "????";		# 
		$pars = "-b??????";		# 
		$nPEs = $opt_p ? $opt_p : 1; # 
		$lat = $opt_l ? $opt_l : 1;
		return ($prg, $pars, $nPEs, $lat);
	    } else {
		$gum_style_gr = 0;
		$in_header = 1;
	    }
	    
	}
	$prg = $1, $pars = $2   if /^Granularity Simulation for\s+(\w+)\s+(.*)$/;
	$nPEs = $1	        if /^PEs\s+(\d+)/;
	$lat = $1, $fetch = $2  if /^Latency\s+(\d+)[^F]+Fetch\s+(\d+)/;
	die "Can't process GranSim-Light profiles!\n"  if /^GrAnSim-Light$/i;

	last             if /^\+\+\+\+\+/;
    }

    return ($prg, $pars, $nPEs, $lat);
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
	print "Usage: $0 [options] <gr-file>\n";
	print "Use -h option to get details\n";
	exit 1;
    }
    
    $input = $ARGV[0] ;
    $input =~ s/\.gr//;
    $input .= ".gr";

    if ( $opt_o ) {
	($output   = $opt_o) =~ s/\.ps// ;
	$output_b = $output . "_peb.ps";
	$output_r = $output . "_per.ps";
	$output_mig = $output . "_mig.ps" if $opt_M;
	$output_sp = $output . "_sp.ps"   if $opt_S;
	$output   = $output . "_pe.ps";
	#($output_b = $opt_o) =~ s/\./-b./ ;
	#($output_r = $opt_o) =~ s/\./-r./ ;
	#($output_mig = $opt_o) =~ s/\./-mig./  if $opt_M;
	#($output_sp = $opt_o) =~ s/\./-sp./  if $opt_S;
    } else {
	($output = $input) =~ s/\.gr// ;
	$output_b = $output . "_peb.ps";
	$output_r = $output . "_per.ps";
	$output_mig = $output . "_mig.ps" if $opt_M;
	$output_sp = $output . "_sp.ps"   if $opt_S;
	$output   = $output . "_pe.ps";
    }
    
    if ( $opt_v ){ 
	$verbose = 1;
    }    

    if ( $opt_i ) {
	$inf_block = $opt_i;
    } else {
	$inf_block = 20;
    }

    $RUNNABLE_file = $input;
    $RUNNABLE_file =~ s/\.gr//;
    $RUNNABLE_file .= "-R";

    $BLOCKED_file = $input;
    $BLOCKED_file =~ s/\.gr//;
    $BLOCKED_file .= "-B";

    $FETCHING_file = $input;
    $FETCHING_file =~ s/\.gr//;
    $FETCHING_file .= "-F";
}

# ----------------------------------------------------------------------------

sub print_verbose_message {

    print "Input file: $input\n";  
    print "Output files: $output, $output_b, $output_r; ".
          ($opt_M ? "Migration: $output_mig" : "") .
	  ($opt_S ? "Sparks: $output_sp" : "") .
	  "\n";
}

# ----------------------------------------------------------------------------
# Junk from draw_segment:
#
#    if ( $type eq $RUNNING ) { 
#	die "ERROR: This version should never draw a RUNNING segment!";
#	$y = $y_val[$pe];
#	$x = $last_start[$pe]; 
#	$width = &get_width(0, $type);
#	# $gray = 0;
#
#	if ( $is_very_big ) {	
#	    $x = int($x/$shrink_x) + 1;   # rounded up
#	}
#
#	do ps_draw_hline(OUT_B,$x,$y,$time,$width);
#	do ps_draw_hline(OUT_R,$x,$y,$time,$width);  
#
#    } elsif ( $type eq $RUNNABLE ) {
#	die "ERROR: This version should never draw a RUNNABLE segment (shades are used instead)!";
#	$y = $y_val[$pe] + $offset;
#	$x = $last_runnable[$pe];
#	$width = &get_width($runnable[$pe], $type);
#
#	if ( $is_very_big ) {	
#	    $x = int($x/$shrink_x) + 1;   # rounded up
#	}
#
#	# $gray = 0.5;
#	do ps_draw_hline(OUT_R,$x,$y,$time,$width);   
