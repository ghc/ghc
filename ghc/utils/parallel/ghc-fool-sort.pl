##############################################################################
#
# Usage: fool-sort 
#
# Takes a pure (i.e. no header lines) quasi-parallel profile (a .qp file) from
# stdin and inserts a counter as second field to force sort not to change the 
# ordering of lines with the same time stamp. The result is written to stdout.
#
##############################################################################

$last_time = 0;
while (<STDIN>) {
    ($time, @rest) = split;
    if ( $time == $last_time ) {
	$x = ++$count;
    } else {
	$x = $count = 0;
    }
    print $time, " ", $x, " ", join(' ',@rest), "\n";
    $last_time = $time;
}

exit 0;
