#!/usr/local/bin/perl
##############################################################################
# Time-stamp: <Sat Oct 28 1995 22:41:09 Stardate: [-31]6509.51 hwloidl>
#
# Usage: do ...
#
# Various auxiliary Perl subroutines that are mainly used in gran-extr and
# RTS2gran. 
# This module contains the following `exported' routines:
#  - mk_global_local_names
#  - dat2ps_name
# The following routines should be local:
#  - basename
#  - dirname
#
##############################################################################

# ----------------------------------------------------------------------------
# Usage:   do mk_global_local_names (<file_name>);
# Returns: (<file_name>,<local_file_name>, <global_file_name>)
#
# Take a filename and create names for local and global variants.
# E.g.: foo.dat -> foo-local.dat and foo-global.dat
# ----------------------------------------------------------------------------

sub mk_global_local_names { 
    local ($file_name) = @_;

    $file_name .= ".dat" unless $file_name =~ /\.dat$/;
    $global_file_name = $file_name; 
    $global_file_name =~ s/\.dat/\-global\.dat/ ;
    $local_file_name = $file_name; 
    $local_file_name =~ s/\.dat/\-local\.dat/ ;

    return ( ($file_name, $global_file_name, $local_file_name) );
}


# ----------------------------------------------------------------------------
# Usage:   do dat2ps(<dat_file_name>);
# Returns: (<ps_file_name>);
# ----------------------------------------------------------------------------

sub dat2ps_name {
    local ($dat_name) = @_;

    $dat_name =~ s/\.dat$/\.ps/;
    return ($dat_name);
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

sub basename {
    local ($in_str) = @_;
    local ($str,$i) ;

    $i = rindex($in_str,"/");
    if ($i == -1) {
	$str = $in_str;
    } else {
	$str = substr($in_str,$i+1) ;
    }

    return $str;
}

# ----------------------------------------------------------------------------

sub dirname {
    local ($in_str) = @_;
    local ($str,$i) ;

    $i = rindex($in_str,"/");
    if ($i == -1) {
	$str = "";
    } else {
	$str = substr($in_str,0,$i+1) ;
    }

    return $str;
}

# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------

1; 
