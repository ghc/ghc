#!/usr/local/bin/perl
# ############################################################################
# Time-stamp: <Fri Aug 25 1995 23:17:43 Stardate: [-31]6189.64 hwloidl>
#                                       (C) Hans Wolfgang Loidl, November 1994
#
# Usage: tf [options] <gr-file>
#
# Show the `taskflow' in the .gr file (especially useful for keeping track of 
# migrated tasks. It's also possible to focus on a given PE or on a given
# event.  
# 
# Options:
#  -p <int> ... Print all events on PE <int>
#  -t <int> ... Print all events that occur on task <int>
#  -e <str> ... Print all <str> events
#  -n <hex> ... Print all events about fetching the node at address <hex>.
#  -s <int> ... Print all events with a spark name <int>
#  -L       ... Print all events with spark queue length information
#  -H       ... Print header of the <gr-file>, too
#  -h       ... print help message (this text)
#  -v       ... be talkative
#
# ############################################################################

# ----------------------------------------------------------------------------
# Command line processing and initialization
# ----------------------------------------------------------------------------

require "getopts.pl";

&Getopts('hvHLp:t:e:n:s:S:');  

do process_options();

if ( $opt_v ) {
    do print_verbose_message();
}

# ----------------------------------------------------------------------------

$in_header = 1;
while (<>) {
    if ( $opt_H && $in_header ) {
	print;
	$in_header = 0 if /^\+\+\+\+\+/;
    }
    next unless /^PE/;
    @c = split(/[\s\[\]:;,]+/);
    if ( ( $check_proc ? $proc eq $c[1] : 1 ) &&
	( $check_event ? $event eq $c[3] : 1 ) &&
	( $check_task ? $task eq $c[4] : 1) &&
	( $check_node ? $node eq $c[5] : 1) &&
	( $check_spark ? (("END" eq $c[3]) && ($spark eq $c[6])) : 1) &&
	( $negated_spark ? (("END" eq $c[3]) && ($spark ne $c[6])) : 1) &&
	( $spark_queue_len ? ($c[5] =~ /sparks/) : 1 ) ) {
	print;
    }
}

exit 0;

# ----------------------------------------------------------------------------

sub process_options { 

 if ( $opt_p ne "" ) {
   $check_proc = 1;
   $proc = $opt_p;
 }

 if ( $opt_t ne "" ) {
   $check_task = 1;
   $task = $opt_t;
 }

 if ( $opt_e ne "" ) {
   $check_event = 1;
   $event = $opt_e;
 }

 if ( $opt_n ne "" ) {
   $check_node = 1;
   $node = $opt_n
 }

 if ( $opt_s ne "" ) {
   $check_spark = 1;
   $spark = $opt_s
 }

 if ( $opt_S ne "" ) {
   $negated_spark = 1;
   $spark = $opt_S
 }

 if ( $opt_L ) {
     $spark_queue_len = 1;
 } else {
     $spark_queue_len = 0;
 }

 if ( $opt_h ) {
     open (ME,$0) || die "!$: $0";
     while (<ME>) {
	 last if /^$/;
	 print;
     }
     close (ME);
     exit 1;
 }
}

# ----------------------------------------------------------------------------

sub print_verbose_message { 

 if ( $opt_p ne "" ) {
   print "Processor: $proc\n";
 }

 if ( $opt_t ne "" ) {
   print "Task: $task\n";
 }

 if ( $opt_e ne "" ) {
   print "Event: $event\n";
 }

 if ( $opt_n ne "" ) {
   print "Node: $node\n";
 }

 if ( $opt_s ne "" ) {
   print "Spark: $spark\n";
 }

 if ( $opt_S ne "" ) {
   print "Negated Spark: $spark\n";
 }

 if ( $opt_L ne "" ) {
   print "Printing spark queue len info.\n";
 }

}

# ----------------------------------------------------------------------------

