#!/usr/local/bin/perl
##############################################################################
#
# Usage: gr2java [options]
#
# Filter that transforms a GrAnSim profile (a .gr file) at stdin to  
# a quasi-parallel profile (a .qp file). It is the common front-end for most
# visualization tools (except gr2pe). It collects  running,
# runnable and blocked tasks in queues of different `colours', whose meaning
# is:
#  G ... green; queue of all running tasks
#  A ... amber; queue of all runnable tasks
#  R ... red; queue of all blocked tasks
#  Y ... cyan; queue of fetching tasks 
#  C ... crimson; queue of tasks that are being stolen
#  B ... blue; queue of all sparks
#
# Options:
#  -i <int>  ... info level from 1 to 7; number of queues to count (see qp3ps)
#  -I <str>  ... count tasks that are in one of the given queues; encoding:
#                 'a' ... active (running)
#                 'r' ... runnable
#                 'b' ... blocked
#                 'f' ... fetching
#                 'm' ... migrating
#                 's' ... sparks
#                (e.g. -I "arb" counts sum of active, runnable, blocked tasks)
#  -c        ... check consistency of data (e.g. no neg. number of tasks)
#  -v        ... be talkative. 
#  -h        ... print help message (this header).
#
##############################################################################

require "getopts.pl";

&Getopts('hvDSci:I:');  

do process_options();

if ( $opt_v ) {
    do print_verbose_message();
}

# ---------------------------------------------------------------------------
# Init
# ---------------------------------------------------------------------------

$max = 0;
$pmax = 0;
$ptotal = 0;
$n = 0;

$active = 0;
$runnable = 0;
$blocked = 0;
$fetching = 0;
$migrating = 0;
$sparks = 0;

$improved_sort_option = $opt_S ? "-S" : "";

open (FOOL,"| ghc-fool-sort $improved_sort_option | sort -n +0 -1 | ghc-unfool-sort") || die "FOOL";

$in_header = 9; 
while(<>) {
    if ( $in_header == 9 ) {
        if (/^=/) {
	    $gum_style_gr = 1;
	    $in_header = 0;
	} else {
	    $gum_style_gr = 0;
	    $in_header = 1;
	}
	
    }
    if (/^\++$/) {
	$in_header=0;
	next;
    }
    next if $in_header;
    next if /^$/;
    next if /^=/;
    chop;
    ($PE, $pe, $time, $act, $tid, $rest) = split;
    $time =~ s/[\[\]:]//g;
    # next if $act eq 'REPLY';
    chop($tid) if $act eq 'END';
    $from = $queue{$tid};
    $extra = "";
    if ($act eq 'START') {
	$from = '*';
	$to = 'G';
	$n++;
	if ( $n > $pmax ) { $pmax = $n; }
	$ptotal++;
    } elsif ($act eq 'START(Q)') {
	$from = '*';
	$to = 'A';
	$n++;
	if ( $n > $pmax ) { $pmax = $n; }
	$ptotal++;
    } elsif ($act eq 'STEALING') {
	$to = 'C';
    } elsif ($act eq 'STOLEN') {
	$to = 'G';
    } elsif ($act eq 'STOLEN(Q)') {
	$to = 'A';
    } elsif ($act eq 'FETCH') {
	$to = 'Y';
    } elsif ($act eq 'REPLY') {
	$to = 'R';
    } elsif ($act eq 'BLOCK') {
	$to = 'R';
    } elsif ($act eq 'RESUME') {
	$to = 'G';
	$extra = " 0 0x0";
    } elsif ($act eq 'RESUME(Q)') {
	$to = 'A';
	$extra = " 0 0x0";
    } elsif ($act eq 'END') {
	$to = '*';
	$n--;
	if ( $opt_c && $n < 0 ) { 
	    print STDERR "Error at time $time: neg. number of tasks: $n\n";
	}
    } elsif ($act eq 'SCHEDULE') {
	$to = 'G';
    } elsif ($act eq 'DESCHEDULE') {
	$to = 'A';
    # The following are only needed for spark profiling
    } elsif (($act eq 'SPARK') || ($act eq 'SPARKAT')) {
	$from = '*';
	$to = 'B';
    } elsif ($act eq 'USED') {
	$from = 'B';
	$to = '*';
    } elsif ($act eq 'PRUNED') {
	$from = 'B';
	$to = '*';
    } elsif ($act eq 'EXPORTED') {
	$from = 'B';
	$to = 'B';
    } elsif ($act eq 'ACQUIRED') {
	$from = 'B';
	$to = 'B';
    } else {
	print STDERR "Error at time $time: unknown event $act\n";
    }
    $queue{$tid} = $to;

    if ( $from eq '' ) {
	print STDERRR "Error at time $time: process $tid has no from queue\n";
    }
    if ($to ne $from) {
        print FOOL $time, "  ", $pe, " ",
	  $from, $to, "\n";
    }

    if ($to ne $from) {
	# Compare with main loop in qp3ps
	if ($from eq '*') {
	} elsif ($from eq 'G') {
	    --$active;
	} elsif ($from eq 'A') {
	    --$runnable;
	} elsif ($from eq 'R') {
	    --$blocked;
	} elsif ($from eq 'B') {
	    --$sparks;
	} elsif ($from eq 'C') {
	    --$migrating;
	} elsif ($from eq 'Y') {
	    --$fetching;
	} else {
	    print STDERR "Illegal from char: $from at $time\n";
	}

	if ($to eq '*') {
	} elsif ($to eq 'G') {
	    ++$active;
	} elsif ($to eq 'A') {
	    ++$runnable;
	} elsif ($to eq 'R') {
	    ++$blocked;
	} elsif ($to eq 'B') {
	    ++$sparks;
	} elsif ($to eq 'C') {
	    ++$migrating;
	} elsif ($to eq 'Y') {
	    ++$fetching;
	} else {
	    print STDERR "Illegal to char: $to at $time\n";
	}

    }

    $curr = &count();
    if ( $curr > $max ) {
	$max = $curr;
    }

    if ( 0 ) {
	print STDERR "%% $time: (act,runnable,blocked,fetch,mig,sp) = " .
	    "($active, $runnable, $blocked, $fetching, $migrating, $sparks)".
		" max = $max\n"  ;
    }

    #print STDERR "Sparks @ $time: $sparks \tCurr: $curr \tMax: $max \n"  if $opt_D;
 
    if ( $time > $tmax ) {
	$tmax = $time;
    }
    delete $queue{$tid} if $to eq '*';
    
}

print "Time: ", $tmax, " Max_selected_tasks: ", $max, 
      " Max_running_tasks: ", $pmax, " Total_tasks: ", $ptotal, "\n";

close(FOOL);

exit 0;

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Copied from qp3ps and slightly modified (we don't keep a list for each queue
# but just compute the max value we get out of all calls to count during the
# execution of the script).
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------

sub queue_on {
    local ($queue) = @_;

    return index($show,$queue)+1;
}

# -----------------------------------------------------------------------------

sub count {
    local ($res);

    $res = (($queue_on_a)  ? $active : 0) +
	   (($queue_on_r)  ? $runnable : 0) +
	   (($queue_on_b)  ? $blocked : 0) +
	   (($queue_on_f)  ? $fetching : 0) +
	   (($queue_on_m)  ? $migrating : 0) +
           (($queue_on_s)  ? $sparks : 0);

    return $res;
}
    
# -----------------------------------------------------------------------------
# DaH 'oH lo'lu'Qo'
# -----------------------------------------------------------------------------

sub set_values {
    local ($samples,
	   $active,$runnable,$blocked,$fetching,$sparks,$migrating) = @_;

    $G[$samples] = queue_on_a ? $active : 0;   
    $A[$samples] = queue_on_r ? $runnable : 0; 
    $R[$samples] = queue_on_b ? $blocked : 0;  
    $Y[$samples] = queue_on_f ? $fetching : 0; 
    $B[$samples] = queue_on_s ? $sparks : 0;   
    $C[$samples] = queue_on_m ? $migrating : 0;
}

# -----------------------------------------------------------------------------

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

    $show = "armfb";

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

    $queue_on_a = &queue_on("a");
    $queue_on_r = &queue_on("r"); 
    $queue_on_b = &queue_on("b"); 
    $queue_on_f = &queue_on("f"); 
    $queue_on_s = &queue_on("s"); 
    $queue_on_m = &queue_on("m"); 
}

sub print_verbose_message { 

    print STDERR "Info-str: $show\n";
    print STDERR "The following queues are turned on: " .
	  ( $queue_on_a ? "active, " : "") .   
	  ( $queue_on_r ? "runnable, " : "") . 
	  ( $queue_on_b ? "blocked, " : "") .  
          ( $queue_on_f ? "fetching, " : "") . 
          ( $queue_on_m ? "migrating, " : "") .
	  ( $queue_on_s ? "sparks" : "") .
          "\n";
}
