#!/usr/bin/env perl

use FindBin;

%DirCount = ();
%ModCount = ();
%DirComments = ();
%ModComments = ();

my $binPath = $FindBin::Bin;

foreach $f ( @ARGV ) {

    if ( $f =~ /\.lhs$/ ) {
        open(INF, "$binPath/../../inplace/lib/unlit $f - |") || die "Couldn't unlit $f!\n";
    } else {
        open(INF, "< $f") || die "Couldn't open $f!\n";
    }
    $cnt = 0;
    while (<INF>) {
        s/--.*//;
        s/{-.*-}//;
        s/\/\/.*//;
        next if /^\s*$/;
        $cnt++;
    }
    close(INF);

    $f_wc = `wc $f`; die "wc failed: $f\n" if $? != 0;
    if ( $f_wc =~ /\s*(\d+)\s*(\d+)\s*(\d+)/ ) {
        $comments = $1 - $cnt;
    } else {
        die "Can't grok wc format: $f_wc";
    }

    if ( $f =~ /(.*)\/(.*)/ ) {
        local($dir) = $1;
        local($mod) = $2;
        $DirCount{$dir} += $cnt;
        $ModCount{$mod} += $cnt;
        $DirComments{$dir} += $comments;
        $ModComments{$mod} += $comments;
    } else {
        print STDERR "not counted in a directory: $f\n";
        $ModCount{$f} += $cnt;
        $ModComments{$f} += $comments;
    }
}

# print the info
$tot = 0;
$totcmts = 0;
printf "\n                      Code  Comments\n";
foreach $d (sort (keys %DirCount)) {
    printf "%-20s %6d %6d\n", $d, $DirCount{$d}, $DirComments{$d};
    $tot     += $DirCount{$d};
    $totcmts += $DirComments{$d};
}
printf "\n%-20s %6d %6d\n\n\n", 'TOTAL:', $tot, $totcmts;

$tot = 0;
$totcmts = 0;
printf "\n                      Code  Comments\n";
foreach $m (sort (keys %ModCount)) {
    printf "%-20s %6d %6d\n", $m, $ModCount{$m}, $ModComments{$m};
    $tot += $ModCount{$m};
    $totcmts += $ModComments{$m};
}
printf "\n%-20s %6d %6d\n", 'TOTAL:', $tot, $totcmts;
