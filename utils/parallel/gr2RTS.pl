#!/usr/local/bin/perl
#                                           (C) Hans Wolfgang Loidl, July 1995
##############################################################################
# Time-stamp: <Thu Oct 26 1995 18:40:10 Stardate: [-31]6498.68 hwloidl>
#
# Usage: gr2RTS [options] <sim-file>
#
# Options:
#  -o <file> ... write output to <file>
#  -h        ... help; print this text.
#  -v        ... verbose mode.
#
##############################################################################

# ----------------------------------------------------------------------------
# Command line processing and initialization
# ----------------------------------------------------------------------------

require "getopts.pl";

&Getopts('hvo:');  

do process_options();

if ( $opt_v ) {
    do print_verbose_message ();
}

# ----------------------------------------------------------------------------
# The real thing
# ----------------------------------------------------------------------------

open(INPUT,"<$input") || die "Couldn't open input file $input";
open(OUTPUT,"| sort -n > $output") || die "Couldn't open output file $output";

#do skip_header();

$tot_total_rt = 0;
$tot_rt = 0;

$line_no = 0;
while (<INPUT>) {
    next                     if /^--/;     # Comment lines start with --
    next		     if /^\s*$/;   # Skip empty lines
    $line_no++;
    @fields = split(/[:,]/,$_);
    $has_end = 0;

    foreach $elem (@fields) {
      foo : {
        $pe = $1, $end = $2 , last foo   if $elem =~ /^\s*PE\s+(\d+)\s+\[(\d+)\].*$/;
        $tn = $1, $has_end = 1  , last foo   if $elem =~ /^\s*END\s+(\w+).*$/;
	# $tn = $1	, last foo   if $elem =~ /^\s*TN\s+(\w+).*$/;
	$sn = $1	, last foo   if $elem =~ /^\s*SN\s+(\d+).*$/;
        $start = $1     , last foo   if $elem =~ /^\s*ST\s+(\d+).*$/;
        $is_global = $1 , last foo   if $elem =~ /^\s*EXP\s+(T|F).*$/;
        $bbs = $1       , last foo   if $elem =~ /^\s*BB\s+(\d+).*$/;
        $ha = $1        , last foo   if $elem =~ /^\s*HA\s+(\d+).*$/;
        $rt = $1        , last foo   if $elem =~ /^\s*RT\s+(\d+).*$/;
        $bt = $1, $bc = $2 , last foo if $elem =~ /^\s*BT\s+(\d+)\s+\((\d+)\).*$/;
        $ft = $1, $fc = $2 , last foo if $elem =~ /^\s*FT\s+(\d+)\s+\((\d+)\).*$/;
        $lsp = $1        , last foo   if $elem =~ /^\s*LS\s+(\d+).*$/;
        $gsp = $1        , last foo   if $elem =~ /^\s*GS\s+(\d+).*$/;
        $my = $1        , last foo   if $elem =~ /^\s*MY\s+(T|F).*$/;
      }
    }

    next unless $has_end == 1;

    $total_rt = $end - $start;
    $tot_total_rt += $total_rt;
    $tot_rt += $rt;

    print OUTPUT "$rt\n";
    $sum_rt += $rt;
    $max_rt = $rt  if $rt > $max_rt;
}

close INPUT;
close OUTPUT;

# Hack to  fake a filter
if ( $output eq $filter_output ) {
    system "cat $output";
    system "rm $output";
}

exit 0;

# ---------------------------------------------------------------------------

sub process_options {
    if ( $opt_h ) {                      
	open(ME,$0) || die "Can't open myself ($0)";
	$n = 0;
	while (<ME>) {
	    last if $_ =~ /^$/;
	    print $_;
	    $n++;
	}
	close(ME);
	
	# system "cat $0 | awk 'BEGIN { n = 0; } \
	#                             /^$/ { print n; \
	#                                    exit; } \
	#                                  { n++; }'"
	exit ;
    }

    $input = $#ARGV == -1 ? "-" : $ARGV[0] ;
    
    if ( $#ARGV != 0 ) {
	#print "Usage: gran-extr [options] <sim-file>\n";
	#print "Use -h option to get details\n";
	#exit 1;
	
    }

    $filter_output = $ENV{'TMPDIR'} . "./,gr2RTS-out";
    if ( $opt_o ) {
	$output = $opt_o;
    } else {
	if ( $input eq "-" ) {
	    $output = $filter_output;
	} else {
	    $output = $input; # "RTS";
	    $output =~ s/\.gr$/.rts/g;
        }			# 
    }
}

# ----------------------------------------------------------------------------

sub print_verbose_message {
    print "Input file: $input\t Output file: $output\n";
}

# ----------------------------------------------------------------------------
