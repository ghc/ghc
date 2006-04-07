#!/usr/local/bin/perl
#                                       (C) Hans Wolfgang Loidl, October 1995
#############################################################################
# Time-stamp: <Thu Oct 26 1995 18:30:54 Stardate: [-31]6498.64 hwloidl>
#
# Usage: AVG [options] <gr-file>
#
# A quich hack to get avg runtimes of different spark sites. Similar to SPLIT.
#
# Options:
#  -s <list> ... a perl list of spark names; the given <gr-file> is scanned 
#                for each given name in turn and granularity graphs are 
#                generated for each of these sparks  
#  -O        ... use gr2RTS and RTS2gran instead of gran-extr;
#                this generates fewer output files (only granularity graphs)
#                but should be faster and far less memory consuming
#  -h        ... help; print this text.
#  -v        ... verbose mode.
#
#############################################################################

require "getopts.pl";

&Getopts('hvOs:');  

do process_options();

if ( $opt_v ) { do print_verbose_message(); }

# ---------------------------------------------------------------------------
# Init
# ---------------------------------------------------------------------------

foreach $s (@sparks) {
    # extract END events for this spark-site
    open (GET,"cat $input | tf -s $s | avg-RTS") || die "!$\n";
}

exit 0;

exit 0;

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
    
    if ( $opt_s ) {
	$opt_s =~ s/[\(\)\[\]]//g;
	@sparks = split(/[,;. ]+/, $opt_s);
    } else {
	@sparks = ( 3, 4, 5, 6, 7, 8, 11, 12, 13, 14, 15);
    }

    if ( $#ARGV != 0 ) {
	print "Usage: $0 [options] <gr-file>\n;";
	print "Use -h option to get details\n";
	exit 1;
    }

    $gr_file = $ARGV[0];
    ($basename = $gr_file) =~ s/\.gr//;
    $rts_file = $basename . ".rts";        # "RTS";
    $gran_file = "g.ps"; # $basename . ".ps";
    #$rts_file = $gr_file;
    #$rts_file =~ s/\.gr/.rts/g;

    if ( $opt_o ) {
	$va_file = $opt_o;
	$va_dvi_file = $va_file;
	$va_dvi_file =~ s/\.tex/.dvi/g; 
	$va_ps_file = $va_file;
	$va_ps_file =~ s/\.tex/.ps/g; 
    } else {
	$va_file = "va.tex";
	$va_dvi_file = "va.dvi";
	$va_ps_file = "va.ps";
    }
    
    if ( $opt_t ) {
	$template_file = $opt_t;
    } else {
	$template_file = "TEMPL";
    }

    $tmp_file = ",t";
}

# -----------------------------------------------------------------------------

sub print_verbose_message {
    print "Sparks: (" . join(',',@sparks) . ")\n";
    print "Files: .gr " . $gr_file . "  template " . $template_file .
	    " va " . $va_file . "\n";
}

# -----------------------------------------------------------------------------
