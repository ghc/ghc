# WARNING!
# Note: be careful about running this with an argument > (say) 18 !
# running this script on '27' will chew up ~80 MB of virtual
# ram. and its apetite grows per 1.61803 ** $n.
#
# Your system admin folk would probably be displeased if you trash
# other people's work, or disable systems running this script!
# 
# Usage: perl nfib.prl <number>
#
$n = @ARGV[0];
$f=&fib($n);
print " $n! = $f\n";
sub fib {
    local ($n)=$_[0];
    if ($n==0) {return (0);}
    elsif($n==1) {return(1);}
    return (&fib ($n-1) + &fib($n-2));
}
