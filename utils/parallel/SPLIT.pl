#!/usr/local/bin/perl
#                                           (C) Hans Wolfgang Loidl, July 1995
#############################################################################
# Time-stamp: <Thu Oct 26 1995 18:23:00 Stardate: [-31]6498.62 hwloidl>
#
# Usage: SPLIT [options] <gr-file>
#
# Generate a set of granularity graphs out of the GrAnSim profile <gr-file>.
# The granularity graphs are put into subdirs of the structure: 
#   <basename of gr-file>-<spark-name>
#
# Options:
#  -s <list> ... a perl list of spark names; the given <gr-file> is scanned 
#                for each given name in turn and granularity graphs are 
#                generated for each of these sparks  
#  -O        ... use gr2RTS and RTS2gran instead of gran-extr;
#                this generates fewer output files (only granularity graphs)
#                but should be faster and far less memory consuming
#  -d <dir>  ... use <dir> as basename for the sub-directories
#  -o <file> ... use <file> as basename for the generated latex files; 
#                the overall result is in  <file>.ps
#  -t <file> ... use <file> as gran-extr type template file 
#                ('.' for local template, ',' for global template)
#  -A        ... surpress generation of granularity profiles for overall .gr
#  -h        ... help; print this text.
#  -v        ... verbose mode.
#
#############################################################################

require "getopts.pl";

&Getopts('hvOAd:o:s:t:');  

do process_options();

if ( $opt_v ) { do print_verbose_message(); }

# ---------------------------------------------------------------------------
# Init
# ---------------------------------------------------------------------------

$latex = "/usr/local/tex/bin/latex2e"; # or "/usr/local/tex/bin/latex2e"

do all()  if !$opt_A;

foreach $s (@sparks) {
    if ( -f $tmp_file ) { system "rm -f $tmp_file"; }
    system "tf -H -s $s $gr_file > $tmp_file" 
	|| die "Can't open pipe: tf -s $s $gr_file > $tmp_file\n";

    if ( $opt_d ) {
	$dir = $opt_d;		
    } else {
	$dir = $gr_file;
    }
    $dir =~ s/\.gr//g;
    $dir .= "-$s";

    if ( ! -d $dir ) {
	mkdir($dir,"755");   # system "mkdir $dir";
	system "chmod u+rwx $dir";
    }

    system "mv $tmp_file $dir/$gr_file";
    chdir $dir;             
    do print_template();
    do print_va("Title",$s);
    if ( -f $va_ps_file ) { 
	local ($old) = $va_ps_file;
	$old =~ s/\.ps/-o.ps/g;
	system "mv $va_ps_file $old";
    }
    if ( $opt_O ) {
	system "gr2RTS -o $rts_file $gr_file; " .
	       "RTS2gran -t $template_file $rts_file; " . 
	       "$latex $va_file; dvips $va_dvi_file > $va_ps_file"; 
    } else {
	system "gran-extr -t $template_file $gr_file; " . 
	    "$latex $va_file; dvips $va_dvi_file > $va_ps_file"; 
    }
    chdir "..";  # system "cd ..";
}

exit 0;

# -----------------------------------------------------------------------------

sub all {

    $dir = $gr_file;
    $dir =~ s/\.gr//g;
    $dir .= "-all";

    if ( ! -d $dir ) {
	mkdir($dir,"755");   # system "mkdir $dir";
	system "chmod u+rwx $dir";
    }

    system "cp $gr_file $dir/$gr_file";
    chdir $dir;             
    do print_template();
    do print_va("All","all");
    if ( -f $va_ps_file ) { 
	local ($old) = $va_ps_file;
	$old =~ s/\.ps/-o.ps/g;
	system "mv $va_ps_file $old";
    }
    if ( $opt_O ) {
	system "gr2RTS -o $rts_file $gr_file; " .
	       "RTS2gran -t $template_file $rts_file; " . 
	       "$latex $va_file; dvips $va_dvi_file > $va_ps_file"; 
    } else {
	system "gran-extr -t $template_file $gr_file; " . 
	    "$latex $va_file; dvips $va_dvi_file > $va_ps_file"; 
    }
    chdir "..";  # system "cd ..";
}

# ---------------------------------------------------------------------------

sub print_template {

    open (TEMPL,">$template_file") || die "Can't open $template_file\n";

    print TEMPL <<EOF;
-- Originally copied from the master template: GrAn/bin/TEMPL 
-- Intervals for pure exec. times
G: (1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000, 40000, 50000, 100000, 200000, 300000)
-- Intervals for communication (i.e. fetch)  times
F: (1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000, 40000, 50000, 100000, 200000, 300000)
-- Intervals for communication percentages 
C: (0, 1, 2, 5, 8, 10, 20, 30, 40, 50, 100)
-- Intervals for no. of sparks
S: (1, 2, 5)
-- Intervals for heap allocations
A: (10,20,30,40,50,100,200,300,400,500,1000,2000,3000)
-- A: (100, 50000, 66000, 100000)


g: g.dat
f: f.dat
c: c.dat
s: s.dat
a: a.dat

-- Select file name corr coeff file
Xcorr: 		CORR

-- Select file names for GNUPLOT data files for cumulative runtime and
-- cluster graphs
Xcumulat-rts: 	cumu-rts.dat
Xcumulat-fts: 	cumu-fts.dat
Xcumulat-has: 	cumu-has.dat
Xcumulat-cps: 	cumu-cps.dat
Xclust-rts: 	clust-rts.dat
Xclust-has: 	clust-has.dat
Xclust-cps: 	clust-cps.dat

-- Select file names for GNUPLOT data files for per proc. runnable time
-- and per spark site runtime
Xpe: 		pe.dat
Xsn: 		sn.dat

-- Select file names for sorted lists of runtimes, heap allocs, number of
-- local and global sparks and communication percentage
XRTS:		RTS
XFTS:		FTS
XHAS:		HAS
XLSPS:		LSPS
XGSPS:		GSPS
XCPS:		CPS
XCCPS:		CPS

-- Std log scaling
L: .
-- ('g',"xy",'Cg',"xy",'Ca',"xy")

-- Gray level of impulses in the graph (0=black)
i: 0.3

-- Number of clusters
k: 2

-- Width of impulses (needed for gp-ext-imp)
e: 150

-- Input file
-- -: soda.gr
EOF

    close(TEMPL);
}

# -----------------------------------------------------------------------------
# NB: different file must be generated for $opt_O and default setup.
# -----------------------------------------------------------------------------

sub print_va {
    local ($title, $spark) = @_;

    open (VA,">$va_file") || die "Can't open $va_file\n";

    if ( $opt_O ) {
    print VA <<EOF;
% Originally copied from master va-file: grasp/tests/va.tex
\\documentstyle[11pt,psfig]{article}

% Page Format
\\topmargin=0cm                %0.5cm
\\textheight=24cm                %22cm
\\footskip=0cm
\\oddsidemargin=0cm            %0.75cm
\\evensidemargin=0cm           %0.75cm
\\rightmargin=0cm            %0.75cm
\\leftmargin=0cm            %0.75cm
\\textwidth=16cm                 %14.5cm

\\title{SPLIT}
\\author{Me}
\\date{Today}

\\pssilent

\\begin{document}

\\pagestyle{empty}
\%\\maketitle

\\nopagebreak

\\begin{figure}[t]
\\begin{center}
\\begin{tabular}{c}
\\centerline{\\psfig{angle=270,width=7cm,file=$gran_file}}
\\end{tabular}
\\end{center}
\\caption{Granularity {\\bf $spark}}
\\end{figure}

\\begin{figure}[t]
\\begin{center}
\\begin{tabular}{cc}
\\psfig{angle=270,width=7cm,file=cumu-rts.ps} &
\\psfig{angle=270,width=7cm,file=cumu-rts0.ps}
\\end{tabular}
\\end{center}
\\caption{Cumulative Execution Times  {\\bf $spark}}
\\end{figure}

\\end{document}
EOF
    } else {
    print VA <<EOF;
% Originally copied from master va-file: grasp/tests/va.tex
\\documentstyle[11pt,psfig]{article}

% Page Format
\\topmargin=0cm                %0.5cm
\\textheight=24cm                %22cm
\\footskip=0cm
\\oddsidemargin=0cm            %0.75cm
\\evensidemargin=0cm           %0.75cm
\\rightmargin=0cm            %0.75cm
\\leftmargin=0cm            %0.75cm
\\textwidth=16cm                 %14.5cm

\\title{$title; Spark: $spark}
\\author{}
\\date{}

\\begin{document}

\\pagestyle{empty}
%\\maketitle

\\nopagebreak

\\begin{figure}[t]
\\begin{center}
\\begin{tabular}{cc}
\\psfig{angle=270,width=7cm,file=$gran_file} &
\\psfig{angle=270,width=7cm,file=a.ps}
\\end{tabular}
\\end{center}
\\caption{Granularity \\& Heap Allocations {\\bf $spark}}
\\end{figure}

\\begin{figure}[t]
\\begin{center}
\\begin{tabular}{cc}
\\psfig{angle=270,width=7cm,file=f.ps} &
\\psfig{angle=270,width=7cm,file=c.ps}
\\end{tabular}
\\end{center}
\\caption{Fetching Profile  {\\bf $spark}}
\\end{figure}

\\begin{figure}[t]
\\begin{center}
\\begin{tabular}{cc}
\\psfig{angle=270,width=7cm,file=cumu-rts.ps} &
\\psfig{angle=270,width=7cm,file=cumu-rts0.ps}
\\end{tabular}
\\end{center}
\\caption{Cumulative Execution Times  {\\bf $spark}}
\\end{figure}

\\end{document}
EOF
}
    close (VA);
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
