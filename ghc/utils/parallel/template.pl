#!/usr/local/bin/perl
##############################################################################
# Time-stamp: <Sat Oct 28 1995 23:00:47 Stardate: [-31]6509.58 hwloidl>
#
# Usage: do read_template(<template_file_name>,<input_file_name>);
#
# Read the template file <template_file_name> as defined in /dev/null.
# Set global variables as defined in the template file.
# This is mainly used in gran-extr and RTS2gran.
#
##############################################################################

require "par-aux.pl";

sub read_template {
    local ($org_templ_file_name,$input) = @_;
    local ($f,$templ_file_name);

    # Resolve name 
    $gran_dir = $ENV{GRANDIR} ? $ENV{GRANDIR} : $ENV{HOME} ;
    $templ_file_name = ( $org_templ_file_name eq '.' ? "TEMPL"     
                                                #^^^   default file name
			 : $org_templ_file_name eq ',' ? $gran_dir . "/bin/TEMPL"     
                                                  #^^^   global master template
			 : $org_templ_file_name eq '/' ? $gran_dir . "/bin/T0"     
                                                  #^^   template, that throws away most of the info
                         : $org_templ_file_name ); 

    if ( $opt_v ) {
	print "Reading template file $templ_file_name ...\n";
    }

    ($f = ($input eq "-" ? "stdin" : $input)) =~ s/.rts//;

    open(TEMPLATE,"cat $templ_file_name | sed -e 's/\$0/$f/' |") 
	|| die "Couldn't open file $templ_file_name";

    while (<TEMPLATE>) {
      next if /^\s*$/ || /^--/;
      if (/^\s*G[:,;.\s]+([^\n]+)$/) {
	  $list_str = $1;
          $list_str =~ s/[\(\)\[\]]//g;
          @exec_times = split(/[,;. ]+/, $list_str);
      } elsif (/^\s*F[:,;.\s]+([^\n]+)$/) {
	  $list_str = $1;
          $list_str =~ s/[\(\)\[\]]//g;
          @fetch_times = split(/[,;. ]+/, $list_str);
      } elsif (/^\s*A[:,;.\s]+([^\n]+)$/) {
	  $list_str = $1;
          $list_str =~ s/[\(\)\[\]]//g;
          @has = split(/[,;. ]+/, $list_str);
      } elsif (/^\s*C[:,;.\s]+([^\n]+)$/) {
	  $list_str = $1;
          $list_str =~ s/[\(\)\[\]]//g;
          @comm_percs = split(/[,;. ]+/, $list_str);
      } elsif (/^\s*S[:,;.\s]+([^\n]+)$/) {
	  $list_str = $1;
          $list_str =~ s/[\(\)\[\]]//g;
          @sparks = split(/[,;. ]+/, $list_str);
      } elsif (/^\s*g[:,;.\s]+([\S]+)$/) {
         ($gran_file_name,$gran_global_file_name, $gran_local_file_name) = 
           &mk_global_local_names($1);
      } elsif (/^\s*f[:,;.\s]+([\S]+)$/) {
         ($ft_file_name,$ft_global_file_name, $ft_local_file_name) = 
           &mk_global_local_names($1);
      } elsif (/^\s*c[:,;.\s]+([\S]+)$/) {
         ($comm_file_name, $comm_global_file_name, $comm_local_file_name) = 
           &mk_global_local_names($1);
      } elsif (/^\s*s[:,;.\s]+([\S]+)$/) {
         ($spark_file_name, $spark_global_file_name, $spark_local_file_name) = 
           &mk_global_local_names($1);
      } elsif (/^\s*a[:,;.\s]+([\S]+)$/) {
         ($ha_file_name, $ha_global_file_name, $ha_local_file_name) = 
           &mk_global_local_names($1);
      } elsif (/^\s*p[:,;.\s]+([\S]+)$/) {
         $gp_file_name = $1;
	 # $ps_file_name = &dat2ps_name($gp_file_name);
      } elsif (/^\s*Xcorr[:,;.\s]+([\S]+)$/) {
         $corr_file_name = $1;
      } elsif (/^\s*Xcumulat-rts[:,;.\s]+([\S]+)$/) {
         $cumulat_rts_file_name = $1;
	 ($cumulat0_rts_file_name = $1) =~ s/\./0./;
      } elsif (/^\s*Xcumulat-has[:,;.\s]+([\S]+)$/) {
         $cumulat_has_file_name = $1;
      } elsif (/^\s*Xcumulat-fts[:,;.\s]+([\S]+)$/) {
         $cumulat_fts_file_name = $1;
      } elsif (/^\s*Xcumulat-cps[:,;.\s]+([\S]+)$/) {
         $cumulat_cps_file_name = $1;
      } elsif (/^\s*Xclust-rts[:,;.\s]+([\S]+)$/) {
         $clust_rts_file_name = $1;
      } elsif (/^\s*Xclust-has[:,;.\s]+([\S]+)$/) {
         $clust_has_file_name = $1;
      } elsif (/^\s*Xclust-fts[:,;.\s]+([\S]+)$/) {
         $clust_fts_file_name = $1;
      } elsif (/^\s*Xclust-cps[:,;.\s]+([\S]+)$/) {
         $clust_cps_file_name = $1;
      } elsif (/^\s*Xpe[:,;.\s]+([\S]+)$/) {
         $pe_file_name = $1;
      } elsif (/^\s*Xsn[:,;.\s]+([\S]+)$/) {
         $sn_file_name = $1;

      } elsif (/^\s*XRTS[:,;.\s]+([\S]+)$/) {
	  $rts_file_name = $1;
      } elsif (/^\s*XHAS[:,;.\s]+([\S]+)$/) {
	  $has_file_name = $1;
      } elsif (/^\s*XFTS[:,;.\s]+([\S]+)$/) {
	  $fts_file_name = $1;
      } elsif (/^\s*XLSPS[:,;.\s]+([\S]+)$/) {
	  $lsps_file_name = $1;
      } elsif (/^\s*XGSPS[:,;.\s]+([\S]+)$/) {
	  $gsps_file_name = $1;
      } elsif (/^\s*XCPS[:,;.\s]+([\S]+)$/) {
	  $cps_file_name = $1;
      } elsif (/^\s*XCCPS[:,;.\s]+([\S]+)$/) {
	  $ccps_file_name = $1;

      } elsif (/^\s*\-[:,;.\s]+([\S]+)$/) {
         $input = $1;
      } elsif (/^\s*L[:,;\s]+(.*)$/) {
	 $str = $1;
	 %logscale = ('g',"xy",'a',"xy",'Cg',"xy",'Ca',"xy",'Yp',"y",'Ys',"y") , next  if $str eq ".";
         $str =~ s/[\(\)\[\]]//g;
         %logscale = split(/[,;. ]+/, $str);
      } elsif (/^\s*i[:,;.\s]+([\S]+)$/) {
	 $gray = $1;
      } elsif (/^\s*k[:,;.\s]+([\S]+)$/) {
	 $no_of_clusters = $1;
      } elsif (/^\s*e[:,;.\s]+([\S]+)$/) {
	 $ext_size = $1;
      } elsif (/^\s*v.*$/) {
	 $verbose = 1;
      } elsif (/^\s*T.*$/) {
	 $opt_T = 1;
     }
  }
  close(TEMPLATE);
}      

# ----------------------------------------------------------------------------

1;
