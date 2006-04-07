#!/usr/local/bin/perl

#
# Convert several .gr files (from the same GUM run) into a single
# .gr file with all times adjusted relative to the earliest start
# time.
#

$count = 0;

foreach $i (@ARGV) {
    open(GR, $i) || die "Can't read $i\n";
    $cmd = <GR>;
    $dateline = <GR>;
    $start = <GR>;
    ($pe, $timestamp) = ($start =~ /PE\s+(\d+) \[(\d+)\]/);
    die "PE $pe too high\n" if $pe > $#ARGV;
    $proc[$count++] = $pe;
    $prog[$pe] = $cmd;
    $time[$pe] = $timestamp;
    close(GR);
}

$basetime = 0;

for($i = 0; $i < $count; $i++) {
    $pe = $proc[$i];
    die "PE $pe missing?\n" if !defined($time[$pe]);
    die "Mismatched .gr files\n" if $pe > 0 && $prog[$pe] ne $prog[$pe - 1];
    $basetime = $time[$pe] if $basetime == 0 || $basetime > $time[$pe];
}

print $cmd;
print $dateline;

for($i = 0; $i < $count; $i++) {
    $pe = $proc[$i];
    $delta = $time[$pe] - $basetime;
    open(GR, $ARGV[$i]) || die "Can't read $ARGV[i]\n";
    $cmd = <GR>;
    $dateline = <GR>;
    $start = <GR>;
    while(<GR>) {
        /PE\s+(\d+) \[(\d+)\]/;
	printf "PE %2u [%lu]%s", $1, $2 + $delta, $';
    }
    close(GR);
}
