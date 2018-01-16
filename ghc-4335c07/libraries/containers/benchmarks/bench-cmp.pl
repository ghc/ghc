#!/nix/store/89v4apcv1njanvkdgp5cva4jnj0fzgrs-perl-5.24.3/bin/perl
use warnings;
use strict;

@ARGV >= 2 or die "Usage: bench-cmp.pl csv_file_1 csv_file_2";
open (my $f1, "<", $ARGV[0]) or die "Cannot open file $ARGV[0]";
open (my $f2, "<", $ARGV[1]) or die "Cannot open file $ARGV[1]";

my $l1 = <$f1>;
my $l2 = <$f2>;
$l1 eq $l2 or die "CSV files do not correspond -- $l1 and $l2";

while (defined($l1 = <$f1>)) {
  $l2 = <$f2>;

  my @parts1 = split /,/, $l1;
  my @parts2 = split /,/, $l2;

  $parts1[0] eq $parts2[0] or die "CSV files do not correspond -- $parts1[0] and $parts2[0]";
  printf "%s;%+7.2f%%;%.2e\n", $parts1[0], 100 * $parts2[1] / $parts1[1] - 100, $parts1[1];
}

close $f2;
close $f1;
