#!/usr/local/bin/perl
# ############################################################################
# Time-stamp: <Wed Jun 19 1996 12:26:21 Stardate: [-31]7682.38 hwloidl>
#
# Usage: sn_filter [options] <gr-file> <sn>
#
# Extract all events out of <gr-file> that are related to threads whose 
# spark name component is <sn>.
# 
# Options:
#  -H       ... Print header of the <gr-file>, too
#  -h       ... print help message (this text)
#  -v       ... be talkative
#
# ############################################################################

$gran_dir = $ENV{'GRANDIR'};
if ( $gran_dir eq "" ) {
    print STDERR "Warning: Env variable GRANDIR is undefined\n";
}

push(@INC, $gran_dir, $gran_dir . "/bin");
# print STDERR "INC: " . join(':',@INC) . "\n";

require "get_SN";
require "getopts.pl";

&Getopts('hvH');  

do process_options();
if ( $opt_v ) { do print_verbose_message(); }

# ----------------------------------------------------------------------------

do get_SN($input);

open (FILE,$input) || die "Can't open $file\n";

$in_header = 1;
while (<FILE>) {
    print  if $in_header && $opt_H;
    $in_header = 0 if /^\++$/;
    next if $in_header;
    next unless /^PE\s*\d+\s*\[\d+\]:\s*\w*\s*([0-9a-fx]+)/;
    $id = $1;
    # print STDERR "$id --> " . $id2sn{hex($id)} . " sn: $sn  ==> " . ($sn eq $id2sn{hex($id)}) . "\n";
    print if $sn == $id2sn{hex($id)};
}
	
close (FILE);

exit 0;

# ----------------------------------------------------------------------------

sub process_options { 

    if ( $#ARGV != 1 ) {
	die "Usage: sn_filter <gr-file> <sn>\n";
    }
	
    $input = $ARGV[0];
    $sn = $ARGV[1];

    print STDERR "File: |$file|; sn: |$sn|\n"  if $opt_v;

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

    print "Input: $input\tOutput: stdout\tSN: $sn\n";
    if ( $opt_H ) {
	print "Prepending .gr header to the output.\n";
    }

}

# ----------------------------------------------------------------------------



