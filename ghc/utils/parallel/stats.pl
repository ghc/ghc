#!/usr/local/bin/perl
##############################################################################
# Time-stamp: <Sat Oct 28 1995 23:15:13 Stardate: [-31]6509.63 hwloidl>
#
# Usage: do ....
#
# Statistics package that is used in gran-extr, RTS2gran and friends.
# Most of the routines assume a list of integers as input.
# This package contains:
#  - corr
#  - mean_std_dev
#  - cov
#  - list_sum
#  - list_max
#  - list_min
#
##############################################################################

# ----------------------------------------------------------------------------
# Compute correlation of 2 vectors, having their sums precomputed.
# Usage:  do corr(($n, $sum_1, @rest);
#
# Input: $n   ... number of all elements in @list_1 as well as in @list_2
#                 (i.e. $n = $#list_1+1 = $#list_2+1).
#        $sum_1 ... sum of all elements in @list_1
#        @list_1 ... list of integers; first vector
#        $sum_2 ... sum of all elements in @list_2
#        @list_2 ... list of integers; first vector
# Output: correlation of @list_1 and @list_2 
# ----------------------------------------------------------------------------

sub corr {
    local ($n, $sum_1, @rest) = @_;
    local (@list_1) = splice(@rest,0,$n);
    local ($sum_2, @list_2) = @rest;
       
    local ($mean_1,$mean_2,$std_dev_1,$std_dev_2);

    if ( $opt_D ) {
      print "\ncorr: n=$n  sum_1=$sum_1  sum_2=$sum_2\n";
      print "     list_sum of list_1=" . &list_sum(@list_1) . 
              "   list_sum of list_2=" . &list_sum(@list_2) . "\n";
      print "     len of list_1=$#list_1  len of list_2=$#list_2\n";
    }

    ($mean_1, $std_dev_1) = &mean_std_dev($sum_1,@list_1);
    ($mean_2, $std_dev_2) = &mean_std_dev($sum_2,@list_2);

    if ( $opt_D ) {
      print "corr: $mean_1, $std_dev_1; $mean_2, $std_dev_2\n";
    }

    return ( ($std_dev_1 * $std_dev_2) == 0  ?
               0 : 
               &cov($n, $mean_1, @list_1, $mean_2, @list_2) / 
	       ( $std_dev_1 * $std_dev_2 ) );
}

# ----------------------------------------------------------------------------

sub mean_std_dev {
    local ($sum,@list) = @_;
    local ($n, $s, $s_);

    #print "\nmean_std_dev: sum is $sum ; list has length $#list";

    $n = $#list+1;
    $mean_value = $sum/$n;

    $s_ = 0;
    foreach $x (@list) {
	$s_ += $x;
        $s += ($mean_value - $x) ** 2;  
    }
    if ( $sum != $s_ ) {
      print "stat.pl: ERROR in mean_std_dev: provided sum is wrong  " .
            "(provided: $sum; computed: $s_ " . 
            ";list_sum: " . &list_sum(@list) . "\n";
      exit (2);
    }

    return ( ($mean_value, sqrt($s / ($n - 1)) ) );
}

# ----------------------------------------------------------------------------

sub _mean_std_dev {
    return ( &mean_std_dev(&list_sum(@_), @_) );
}

# ----------------------------------------------------------------------------
# Compute covariance of 2 vectors, having their sums precomputed.
# Input: $n   ... number of all elements in @list_1 as well as in @list_2
#                 (i.e. $n = $#list_1+1 = $#list_2+1).
#        $mean_1 ... mean value of all elements in @list_1
#        @list_1 ... list of integers; first vector
#        $mean_2 ... mean value of all elements in @list_2
#        @list_2 ... list of integers; first vector
# Output: covariance of @list_1 and @list_2 
# ----------------------------------------------------------------------------

sub cov {
    local ($n, $mean_1, @rest) = @_;
    local (@list_1) = splice(@rest,0,$n);
    local ($mean_2, @list_2) = @rest;

    local ($i,$s,$s_1,$s_2);

    for ($i=0; $i<$n; $i++) {
	$s_1 += $list_1[$i];
	$s_2 += $list_2[$i];
	$s += ($mean_1 - $list_1[$i]) * ($mean_2 - $list_2[$i]);
    }
    if ( $mean_1 != ($s_1/$n) ) {
      print "stat.pl: ERROR in cov: provided mean value is wrong " . 
            "(provided: $mean_1; computed: " . ($s_1/$n) . ")\n"; 
      exit (2);
    }
    if ( $mean_2 != ($s_2/$n) ) {
      print "stat.pl: ERROR in cov: provided mean value is wrong " .
            "(provided: $mean_2; computed: " . ($s_2/$n) . ")\n"; 
      exit (2);
    }
    return ( $s / ($n - 1) ) ;
}

# ---------------------------------------------------------------------------

sub list_sum {
    local (@list) = @_;
    local ($sum) = (0);

    foreach $x (@list) {
	$sum += $x;
    }

    return ($sum);
}

# ----------------------------------------------------------------------------

sub list_max {
    local (@list) = @_;
    local ($max) = shift;

    foreach $x (@list) {
	$max = $x  if $x > $max;
    }

    return ($max);
}

# ----------------------------------------------------------------------------

sub list_min {
    local (@list) = @_;
    local ($min) = shift;

    foreach $x (@list) {
	$min = $x  if $x < $min;
    }

    return ($min);
}

# ----------------------------------------------------------------------------

1; 
