#!/usr/local/bin/perl

$n=0;
$sum=0;
$last=0;
while (<>) {
    next unless /^\d+/;
    @c = split;
    $sum += $c[0];
    $last = $c[0];
    $n++;
}

print "Average Runtimes: n=$n; sum=$sum; avg=" . ($sum/$n) . "; max=$last\n";

