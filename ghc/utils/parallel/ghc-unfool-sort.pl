##############################################################################
#
# Usage: unfool-sort 
#
# Reads stdin, elimininates the second field (a dummy counter that has been 
# inserted by fool-sort) of each line and writes the result to stdout.
# See documentation of fool-sort.
#
##############################################################################

while (<STDIN>) {
    ($time, $dummy, @rest) = split;
    print join(' ',$time,@rest) . "\n";
}

exit 0;
