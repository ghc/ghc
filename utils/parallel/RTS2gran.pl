#!/usr/local/bin/perl
##############################################################################
# Time-stamp: <Mon May 20 1996 17:22:45 Stardate: [-31]7533.41 hwloidl>
#
# Usage: RTS2gran <RTS-file>
#
# Options:
#  -t <file>   ... use <file> as template file (<,> global <.> local template)
#  -p <file>   ... use <file> as gnuplot .gp file (default: gran.gp)
#  -x <x-size> ... of gnuplot graph
#  -y <y-size> ... of gnuplot graph
#  -n <n>      ... use <n> as number of PEs in title 
#  -h          ... help; print this text.
#  -v          ... verbose mode.
#
##############################################################################

# ----------------------------------------------------------------------------
# Command line processing and initialization
# ----------------------------------------------------------------------------

$gran_dir = $ENV{'GRANDIR'};
if ( $gran_dir eq "" ) {
    print STDERR "RTS2gran: Warning: Env variable GRANDIR is undefined\n";
}

push(@INC, $gran_dir, $gran_dir . "/bin");
# print STDERR "INC: " . join(':',@INC) . "\n";

require "getopts.pl";
require "template.pl";      # contains read_template for parsing template file
require "stats.pl";          # statistics package with corr and friends

&Getopts('hvt:p:x:y:n:Y:Z:');  

$OPEN_INT = 1;
$CLOSED_INT = 0;

do process_options();

if ( $opt_v ) {
    do print_verbose_message ();
}

# ----------------------------------------------------------------------------
# The real thing
# ----------------------------------------------------------------------------

$max_y = &pre_process($input);

open(INPUT,"<$input") || die "Couldn't open input file $input";
open(OUT_CUMU,">$cumulat_rts_file_name") || die "Couldn't open output file $cumulat_rts_file_name";
open(OUT_CUMU0,">$cumulat0_rts_file_name") || die "Couldn't open output file $cumulat0_rts_file_name";

#do skip_header();

$tot_total_rt = 0;
$tot_rt = 0;
$count = 0;
$last_rt = 0;
$last_x = 0;
$last_y = ($logscale{"'g'"} ne "") ? 1 : 0;

$line_no = 0;
while (<INPUT>) {
    $line_no++;
    next                     if /^--/;     # Comment lines start with --
    next		     if /^\s*$/;   # Skip empty lines
    $rt = $1                 if /^(\d+)/;
    $count++;

    if ( $opt_D ) {
	print STDERR "Error @ line $line_no: RTS file not sorted!\n";
    }

    #push(@all_rts,$rt);
    $sum_rt += $rt;

    $index = do get_index_open_int($rt,@exec_times);
    $exec_class[$index]++;

    if ( $last_rt != $rt ) { 
	print OUT_CUMU "$rt \t" . int($last_y/$max_y) . "\n";
	print OUT_CUMU0 "$rt \t$last_y\n";
	print OUT_CUMU "$rt \t" . int($count/$max_y) . "\n";
	print OUT_CUMU0 "$rt \t$count\n";
	$last_x = $rt;
	$last_y = $count;
    }

    $last_rt = $rt;
}
print OUT_CUMU "$rt \t" . int($last_y/$max_y) . "\n";
print OUT_CUMU0 "$rt \t$last_y\n";
print OUT_CUMU "$rt \t" . int($count/$max_y) . "\n";
print OUT_CUMU0 "$rt \t$count\n";

close OUT_CUMU;
close OUT_CUMU0;

$tot_tasks = $count;         # this is y-max in cumulat graph
$max_rt = $rt;               # this is x-max in cumulat graph

$max_rt_class = &list_max(@exec_class);

do write_data($gran_file_name, $OPEN_INT, $logscale{"'g'"}, $#exec_times+1,
              @exec_times, @exec_class);

# ----------------------------------------------------------------------------
# Run GNUPLOT over the data files and create figures
# ----------------------------------------------------------------------------

do gnu_plotify($gp_file_name); 

# ----------------------------------------------------------------------------

if ( $max_y != $tot_tasks ) {
    if ( $pedantic ) {
	die "ERROR: pre-processed number of tasks ($max_y) does not match computed one ($tot_tasks)\n";
    } else {
	print STDERR "Warning: pre-processed number of tasks ($max_y) does not match computed one ($tot_tasks)\n" if $opt_v;
    }
}

exit 0;

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ToDo: Put these routines into an own package
# ----------------------------------------------------------------------------
# Basic Operations on the intervals
# ----------------------------------------------------------------------------

sub get_index_open_int {
    local ($value,@list) = @_;
    local ($index,$right);

    # print "get_index: searching for index of" . $value;
    # print " in " . join(':',@list);

      $index = 0;
      $right = $list[$index];
      while ( ($value >= $right) && ($index < $#list) ) {
  	$index++;
        $right = $list[$index];
      }
    
      return ( ($index == $#list) && ($value > $right) ) ? $index+1 : $index;
}

# ----------------------------------------------------------------------------

sub get_index_closed_int {
    local ($value,@list) = @_;
    local ($index,$right);

      if ( ($value < $list[0]) || ($value > $list[$#list]) ) {
	  return ( -1 );
      }

      $index = 0;
      $left = $list[$index];
      while ( ($left <= $value) && ($index < $#list) ) {
  	$index++;
        $left = $list[$index];
      }
      return ( $index-1 );
}

# ----------------------------------------------------------------------------
# Write operations
# ----------------------------------------------------------------------------

sub write_data {
    local ($file_name, $open_int, $logaxes, $n, @rest) = @_;
    local (@times) = splice(@rest,0,$n);
    local (@class) = @rest;

    open(GRAN,">$file_name") || die "Couldn't open file $file_name for output";

    if ( $open_int == $OPEN_INT ) {

      for ($i=0, 
           $left = ( index($logaxes,"x") != -1 ? int($times[0]/2) : 0 ), 
           $right = 0; 
           $i < $n; 
           $i++, $left = $right) {
         $right = $times[$i];
         print GRAN int(($left+$right)/2) . "  " . 
                    ($class[$i] eq "" ? "0" : $class[$i]) . "\n"; 
      }
      print GRAN $times[$n-1]+(($times[$n-1]-$times[$n-2])/2) . "  " . 
                 ($class[$n] eq "" ? "0" : $class[$n]) . "\n";

     } else {

      print GRAN ( (index($logaxes,"x") != -1) && ($times[0] == 0 ? int($times[1]/2) : ($times[$1] + $times[0])/2 ) .  "  " . $class[0] . "\n");
      for ($i=1; $i < $n-2; $i++) {
         $left = $times[$i];
         $right = $times[$i+1];
         print(GRAN ($left+$right)/2 . "  " . 
                    ($class[$i] eq "" ? "0" : $class[$i]) . "\n"); 
      }
      print GRAN ($times[$n-1]+$times[$n-2])/2 . "  " . $class[$n-2]  if $n >= 2;
    }

    close(GRAN);
}

# ----------------------------------------------------------------------------

sub write_array {
    local ($file_name,$n,@list) = @_;

    open(FILE,">$file_name") || die "$file_name: $!";
    for ($i=0; $i<=$#list; $i++) {
	print FILE $i . "  " .  ( $list[$i] eq "" ? "0" : $list[$i] ) . "\n";
    }

    if ( $opt_D ) {
	print "write_array: (" . join(", ",1 .. $#list) . ")\n for file $file_name returns: \n (0, $#list, &list_max(@list)\n";
    } 

    return ( (0, $#list, &list_max(@list), 
              "(" . join(", ",1 .. $#list) . ")\n") );
}

# ----------------------------------------------------------------------------

sub gnu_plotify {
  local ($gp_file_name) = @_;

  @open_xrange = &range($OPEN_INT,$logscale{"'g'"},@exec_times);

  $exec_xtics = $opt_T ? &get_xtics($OPEN_INT,@exec_times) : "" ;

  open(GP_FILE,">$gp_file_name") || 
      die "Couldn't open gnuplot file $gp_file_name for output\n";

  print GP_FILE "set term postscript \"Roman\" 20\n";
  do write_gp_record(GP_FILE,
                     $gran_file_name, &dat2ps_name($gran_file_name),
                     "Granularity (pure exec. time)", "Number of threads", 
		     $logscale{"'g'"},
                     @open_xrange,$max_rt_class,$exec_xtics);

  do write_gp_lines_record(GP_FILE,
                           $cumulat_rts_file_name, &dat2ps_name($cumulat_rts_file_name),
                           "Cumulative pure exec. times","% of threads",
  			   "",
                           $max_rt, 100, ""); 
                           # $xtics_cluster_rts as last arg?
  
  do write_gp_lines_record(GP_FILE,
                           $cumulat0_rts_file_name, &dat2ps_name($cumulat0_rts_file_name),
                           "Cumulative pure exec. times","Number of threads",
  			   $logscale{"'Cg'"},
                           $max_rt, $tot_tasks, ""); 
                           # $xtics_cluster_rts as last arg?

  close GP_FILE;

  print "Gnu plotting figures ...\n";
  system "gnuplot $gp_file_name";

  print "Extending thickness of impulses ...\n";
  do gp_ext($gran_file_name);
}

# ----------------------------------------------------------------------------

sub gp_ext {
    local (@file_names) = @_;
    local ($file_name);
    local ($ps_file_name);
    local ($prg);

    #$prg = system "which gp-ext-imp";
    #print "  Using script $prg for impuls extension\n";
    $prg = $ENV{GRANDIR} ? $ENV{GRANDIR} . "/bin/gp-ext-imp" 
	                 : $ENV{HOME} . "/bin/gp-ext-imp" ;
    if ( $opt_v ) {
	print "    (using script $prg)\n";
    }

    foreach $file_name (@file_names) {
	$ps_file_name = &dat2ps_name($file_name);
        system "$prg -w $ext_size -g $gray " . 
               $ps_file_name  . " " . 
               $ps_file_name . "2" ;
        system "mv " . $ps_file_name . "2 " . $ps_file_name;
    }
}

# ----------------------------------------------------------------------------

sub write_gp_record {
    local ($file,$in_file,$out_file,$xlabel,$ylabel,$logaxes,
           $xstart,$xend,$ymax,$xtics) = @_;

    if ( $xstart >= $xend ) {
	print ("WARNING: empty xrange [$xstart:$xend] changed to [$xstart:" . $xstart+1 . "]\n")        if ( $pedantic || $opt_v );
	$xend = $xstart + 1;	
    }

    if ( $ymax <=0 ) {
	$ymax = 2;
	print "WARNING: empty yrange changed to [0:$ymax]\n"   if ( $pedantic || $opt_v );
    }

    $str = "set size " . $xsize . "," . $ysize . "\n" .
           "set xlabel \"" . $xlabel . "\"\n" .
           "set ylabel \"" . $ylabel . "\"\n" .
           ($xstart eq "" ? "" 
                          : "set xrange [" . int($xstart) .":" . int($xend) . "]\n") .
           ($opt_Y ? 
	      ("set yrange [" . (index($logaxes,"y") != -1 ? 1 : 0) . ":$opt_Y]\n") :
	      ($ymax eq "" ? "" 
                        : "set yrange [" . (index($logaxes,"y") != -1 ? 1 : 0) . 
                          ":" . &list_max(2,int($ymax+$ymax/5)) . "]\n")) .
           ($xtics ne "" ? "set xtics $xtics" : "") . 
	   "set tics out\n" .
           "set border\n" .
           ( $nPEs!=0 ? "set title \"$nPEs PEs\"\n" : "" ) .
	   "set nokey \n" .
           "set nozeroaxis\n" .
	   "set format xy \"%8.8g\"\n" .
           (index($logaxes,"x") != -1 ? 
               "set logscale x\n" : 
               "set nologscale x\n") .
           (index($logaxes,"y") != -1 ? 
               "set logscale y\n" : 
               "set nologscale y\n") .
           "set output \"" . $out_file . "\"\n" .
	   "plot \"" . $in_file . "\" with impulses\n\n";
    print $file $str;
}

# ----------------------------------------------------------------------------

sub write_gp_lines_record {
    local ($file,$in_file,$out_file,$xlabel,$ylabel,$logaxes,
           $xend,$yend,$xtics) = @_;

    local ($str);

    $str = "set xlabel \"" . $xlabel . "\"\n" .
           "set ylabel \"" . $ylabel . "\"\n" .
	   "set xrange [" . ( index($logaxes,"x") != -1 ? 1 : 0 ) . ":$xend]\n" .
	   "set yrange [" . ( index($logaxes,"y") != -1 ? 1 : 0 ) . 
	       ($yend!=100 && $opt_Z ? ":$opt_Z]\n" : ":$yend]\n") .
           "set border\n" .
           "set nokey\n" .
           ( $xtics ne "" ? "set xtics $xtics" : "" ) .
           (index($logaxes,"x") != -1 ? 
               "set logscale x\n" : 
               "set nologscale x\n") .
           (index($logaxes,"y") != -1 ? 
               "set logscale y\n" : 
               "set nologscale y\n") .
           "set nozeroaxis\n" .
	   "set format xy \"%8.8g\"\n" .
           "set output \"" . $out_file . "\"\n" .
	   "plot \"" . $in_file . "\" with lines\n\n";
    print $file $str;
}


# ----------------------------------------------------------------------------

sub write_gp_simple_record {
    local ($file,$in_file,$out_file,$xlabel,$ylabel,$logaxes,
           $xstart,$xend,$ymax,$xtics) = @_;

    $str = "set size " . $xsize . "," . $ysize . "\n" .
           "set xlabel \"" . $xlabel . "\"\n" .
           "set ylabel \"" . $ylabel . "\"\n" .
           ($xstart eq "" ? "" 
                          : "set xrange [" . int($xstart) .":" . int($xend) . "]\n") .
           ($ymax eq "" ? "" 
                        : "set yrange [" . (index($logaxes,"y") != -1 ? 1 : 0) . 
                          ":" . &list_max(2,int($ymax+$ymax/5)) . "]\n") .
           ($xtics ne "" ? "set xtics $xtics" : "") . 
           "set border\n" .
           "set nokey\n" .
	   "set tics out\n" .
           "set nozeroaxis\n" .
	   "set format xy \"%8.8g\"\n" .
           (index($logaxes,"x") != -1 ? 
               "set logscale x\n" : 
               "set nologscale x\n") .
           (index($logaxes,"y") != -1 ? 
               "set logscale y\n" : 
               "set nologscale y\n") .
           "set output \"" . $out_file . "\"\n" .
	   "plot \"" . $in_file . "\" with impulses\n\n";
    print $file $str;
}

# ----------------------------------------------------------------------------

sub range {
    local ($open_int, $logaxes, @ints) = @_;

    local ($range, $left_margin, $right_margin);

    $range = $ints[$#ints]-$ints[0];
    $left_margin = 0; # $range/10;
    $right_margin = 0; # $range/10;

    if ( $opt_D ) {
       print "\n==> Range: logaxes are $logaxes i.e. " . 
	   (index($logaxes,"x") != -1 ? "matches x axis\n" 
	                              : "DOESN'T match x axis\n"); 
    }
    if ( index($logaxes,"x") != -1 ) {
	if ( $open_int == $OPEN_INT ) {
	    return ( ($ints[0]/2-$left_margin, 
		      $ints[$#ints]+($ints[$#ints]-$ints[$#ints-1])/2+$right_margin) );
	} else {
	    return ( ( &list_max(1,$ints[0]-$left_margin), 
		       $ints[$#ints]+($ints[$#ints]-$ints[$#ints-1])/2+$right_margin) );
	}
    } else {
	if ( $open_int == $OPEN_INT ) {
	    return ( ($ints[0]/2-$left_margin, 
		      $ints[$#ints]+($ints[$#ints]-$ints[$#ints-1])/2+$right_margin) );
	} else {
	    return ( ($ints[0]-$left_margin, 
		      $ints[$#ints]+($ints[$#ints]-$ints[$#ints-1])/2+$right_margin) );
	}
    }
}

# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------

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

    # Default settings:
    $gp_file_name = "gran.gp";
    $gran_file_name = "gran.dat";
    $cumulat_rts_file_name = "cumu-rts.dat";
    $cumulat0_rts_file_name = "cumu-rts0.dat";
    $xsize = 1;
    $ysize = 1;

    if ( $opt_p ) {
	$gp_file_name = $opt_p;
    } else {
	$gp_file_name = "gran.gp";
    }

    #if ( $opt_s ) {
    #	$gp_file_name =~ s|\.|${opt_s}.|;
    #	$gran_file_name =~ s|\.|${opt_s}.|;
    #	$cumulat_rts_file_name =~ s|\.|${opt_s}.|;
    #	$cumulat0_rts_file_name =~ s|\.|${opt_s}.|;
    #}
    
    if ( $opt_x ) {
	$xsize = $opt_x;
    } else {
	$xsize = 1;
    }
    
    if ( $opt_y ) {
	$ysize = $opt_y;
    } else {
	$ysize = 1;
    }
    
    if ( $opt_t ) {
	do read_template($opt_t,$input);
    }
    
}

# ----------------------------------------------------------------------------

sub print_verbose_message { 

    print "-" x 70 . "\n";
    print "Setup: \n";
    print "-" x 70 . "\n";
    print "\nFilenames: \n";
    print "  Input file: $input\n";
    print "  Gran files: $gran_file_name $gran_global_file_name $gran_local_file_name\n"; 
    print "  Comm files: $comm_file_name $comm_global_file_name $comm_local_file_name\n";
    print "  Sparked threads file: $spark_file_name $spark_local_file_name $spark_global_file_name\n";
    print "  Heap file: $ha_file_name\n";
    print "  GNUPLOT file name: $gp_file_name   Correlation file name: $corr_file_name\n";
    print "  Cumulative RT file name: $cumulat_rts_file_name ($cumulat0_rts_file_name)   \n  Cumulative HA file name: $cumulat_has_file_name\n";
    print "  Cluster RT file name: $clust_rts_file_name   \n  Cluster HA file name: $clust_has_file_name\n";
    print "  Cumulative runtimes file name:    $cumulat_rts_file_name\n";
    print "  Cumulative heap allocations file name   $cumulat_has_file_name\n";
    print "  Cluster run times file name:    $clust_rts_file_name\n";
    print "  Cluster heap allocations file name:    $clust_has_file_name\n";
    print "  PE load file name:    $pe_file_name\n";
    print "  Site size file name:    $sn_file_name\n";
    print "\nBoundaries: \n";
    print "  Gran boundaries: (" . join(',',@exec_times) . ")\n";
    print "  Comm boundaries: (" . join(',',@comm_percs) . ")\n";
    print "  Sparked threads boundaries: (" . join(',',@sparks) . ")\n";
    print "  Heap boundaries: (" . join(',',@has) .")\n";
    print "\nOther pars: \n";
    print "  Left margin: $left_margin  Right margin: $right_margin\n";
    print "  GP-extension: $ext_size  GP xsize: $xsize  GP ysize: $ysize\n";
    print "  Gray scale: $gray  Smart x-tics is " . ($opt_T ? "ON" : "OFF") .
	  "  Percentage y-axis is " . ($opt_P ? "ON" : "OFF") . "\n";
    print "  Log. scaling assoc list: ";
    while (($key,$value) = each %logscale) {
        print "$key: $value, ";
    }
    print "\n";
    print "  Active template file: $templ_file\n"              if $opt_t;  
    print "-" x 70 . "\n";
}

# ----------------------------------------------------------------------------

sub pre_process {
    local ($file) = @_;

    open(PIPE,"wc -l $input |") || die "Couldn't open pipe";

    while (<PIPE>) {		
	if (/^\s*(\d+)/) {
	    $res = $1; 
	} else {
	    die "Error in pre-processing: Last line of $file does not match RTS!\n";
	}
    }
    close(PIPE);

    return ($res-1);
}

# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
#
# Old version (eventually delete it)
# New version is in template.pl
# 
# sub read_template {
#     local ($f);
# 
#     if ( $opt_v ) {
# 	print "Reading template file $templ_file_name ...\n";
#     }
# 
#     ($f = ($input eq "-" ? "stdin" : $input)) =~ s/.rts//;
# 
#     open(TEMPLATE,"cat $templ_file_name | sed -e 's/\$0/$f/' |") 
# 	|| die "Couldn't open file $templ_file_name";
# 
#     while (<TEMPLATE>) {
#       next if /^\s*$/ || /^--/;
#       if (/^\s*G[:,;.\s]+([^\n]+)$/) {
# 	  $list_str = $1;
#           $list_str =~ s/[\(\)\[\]]//g;
#           @exec_times = split(/[,;. ]+/, $list_str);
#       } elsif (/^\s*F[:,;.\s]+([^\n]+)$/) {
# 	  $list_str = $1;
#           $list_str =~ s/[\(\)\[\]]//g;
#           @fetch_times = split(/[,;. ]+/, $list_str);
#       } elsif (/^\s*A[:,;.\s]+([^\n]+)$/) {
# 	  $list_str = $1;
#           $list_str =~ s/[\(\)\[\]]//g;
#           @has = split(/[,;. ]+/, $list_str);
#       } elsif (/^\s*C[:,;.\s]+([^\n]+)$/) {
# 	  $list_str = $1;
#           $list_str =~ s/[\(\)\[\]]//g;
#           @comm_percs = split(/[,;. ]+/, $list_str);
#       } elsif (/^\s*S[:,;.\s]+([^\n]+)$/) {
# 	  $list_str = $1;
#           $list_str =~ s/[\(\)\[\]]//g;
#           @sparks = split(/[,;. ]+/, $list_str);
#       } elsif (/^\s*g[:,;.\s]+([\S]+)$/) {
#          ($gran_file_name,$gran_global_file_name, $gran_local_file_name) = 
#            &mk_global_local_names($1);
#       } elsif (/^\s*f[:,;.\s]+([\S]+)$/) {
#          ($ft_file_name,$ft_global_file_name, $ft_local_file_name) = 
#            &mk_global_local_names($1);
#       } elsif (/^\s*c[:,;.\s]+([\S]+)$/) {
#          ($comm_file_name, $comm_global_file_name, $comm_local_file_name) = 
#            &mk_global_local_names($1);
#       } elsif (/^\s*s[:,;.\s]+([\S]+)$/) {
#          ($spark_file_name, $spark_global_file_name, $spark_local_file_name) = 
#            &mk_global_local_names($1);
#       } elsif (/^\s*a[:,;.\s]+([\S]+)$/) {
#          ($ha_file_name, $ha_global_file_name, $ha_local_file_name) = 
#            &mk_global_local_names($1);
#       } elsif (/^\s*p[:,;.\s]+([\S]+)$/) {
#          $gp_file_name = $1;
# 	 $ps_file_name = &dat2ps_name($gp_file_name);
# 
#       } elsif (/^\s*Xcorr[:,;.\s]+([\S]+)$/) {
#          $corr_file_name = $1;
#       } elsif (/^\s*Xcumulat-rts[:,;.\s]+([\S]+)$/) {
#          $cumulat_rts_file_name = $1;
# 	 ($cumulat0_rts_file_name = $1) =~ s/\./0./;
#       } elsif (/^\s*Xcumulat-has[:,;.\s]+([\S]+)$/) {
#          $cumulat_has_file_name = $1;
#       } elsif (/^\s*Xcumulat-fts[:,;.\s]+([\S]+)$/) {
#          $cumulat_fts_file_name = $1;
#       } elsif (/^\s*Xcumulat-cps[:,;.\s]+([\S]+)$/) {
#          $cumulat_cps_file_name = $1;
#       } elsif (/^\s*Xclust-rts[:,;.\s]+([\S]+)$/) {
#          $clust_rts_file_name = $1;
#       } elsif (/^\s*Xclust-has[:,;.\s]+([\S]+)$/) {
#          $clust_has_file_name = $1;
#       } elsif (/^\s*Xclust-fts[:,;.\s]+([\S]+)$/) {
#          $clust_fts_file_name = $1;
#       } elsif (/^\s*Xclust-cps[:,;.\s]+([\S]+)$/) {
#          $clust_cps_file_name = $1;
#       } elsif (/^\s*Xpe[:,;.\s]+([\S]+)$/) {
#          $pe_file_name = $1;
#       } elsif (/^\s*Xsn[:,;.\s]+([\S]+)$/) {
#          $sn_file_name = $1;
# 
#       } elsif (/^\s*XRTS[:,;.\s]+([\S]+)$/) {
# 	  $rts_file_name = $1;
#       } elsif (/^\s*XHAS[:,;.\s]+([\S]+)$/) {
# 	  $has_file_name = $1;
#       } elsif (/^\s*XFTS[:,;.\s]+([\S]+)$/) {
# 	  $fts_file_name = $1;
#       } elsif (/^\s*XLSPS[:,;.\s]+([\S]+)$/) {
# 	  $lsps_file_name = $1;
#       } elsif (/^\s*XGSPS[:,;.\s]+([\S]+)$/) {
# 	  $gsps_file_name = $1;
#       } elsif (/^\s*XCPS[:,;.\s]+([\S]+)$/) {
# 	  $cps_file_name = $1;
#       } elsif (/^\s*XCCPS[:,;.\s]+([\S]+)$/) {
# 	  $ccps_file_name = $1;
# 
#       } elsif (/^\s*\-[:,;.\s]+([\S]+)$/) {
#          $input = $1;
#       } elsif (/^\s*L[:,;\s]+(.*)$/) {
# 	 $str = $1;
# 	 %logscale = ('g',"xy",'a',"xy",'Cg',"xy",'Ca',"xy",'Yp',"y",'Ys',"y") , next  if $str eq ".";
#          $str =~ s/[\(\)\[\]]//g;
#          %logscale = split(/[,;. ]+/, $str);
#       } elsif (/^\s*i[:,;.\s]+([\S]+)$/) {
# 	 $gray = $1;
#       } elsif (/^\s*k[:,;.\s]+([\S]+)$/) {
# 	 $no_of_clusters = $1;
#       } elsif (/^\s*e[:,;.\s]+([\S]+)$/) {
# 	 $ext_size = $1;
#       } elsif (/^\s*v.*$/) {
# 	 $verbose = 1;
#       } elsif (/^\s*T.*$/) {
# 	 $opt_T = 1;
#      }
#   }
#   close(TEMPLATE);
# }      
