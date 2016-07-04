#!/usr/bin/perl

use strict;
use warnings;


my $reading = 0;

# key: 1 <=> single entry (first static then dynamic)
my %thunk_counts = (
        0 => {0 => 0, 1 => 0, 2=>0 },
	1 => {0 => 0, 1 => 0, 2=>0 },
 );
my %dyn_thunk_counts = (
        0 => {0 => 0, 1 => 0, 2=>0 },
	1 => {0 => 0, 1 => 0, 2=>0 },
 );
my %fun_counts = (
        0 => {0 => 0, 1 => 0, 2=>0 },
	1 => {0 => 0, 1 => 0, 2=>0 },
 );

my %reason_counts = ();
my %unique_reason_counts = ();

my @interesting;

while (<>) {
	if ($reading and /^$/) {$reading = 0};
	if (not $reading and /^----------------/) {$reading = 1; next;};
	next unless $reading;

	if (m/^
		 \s+
		 (?<entries>\d+)\s+
		 (?<alloc>\d+)\s+
		 (?<alloced>\d+)\s+
		 (?<nalloc>\d+)\s+
		 (?<single>\d+)\s+
		 (?<multiple>\d+)\s+
		 (?<args>\d+)\s+
		 (?<rest>.*)
		/nx)
	{
		my %vals = %+;


		# ignore constructors
		next if $vals{rest} =~ m/\(con\)/;


		# ignore never allocated things
		next if $vals{nalloc} == 0;

		# ignore static or dead entries
		#next if $vals{single} + $vals{multiple} == 0;

		my ($flags)      = ($vals{rest} =~ m/\((?:thk|fun)(.*)\)/);
		my ($manyreasons) = ($flags =~ m/\((.*)\)/);
		$manyreasons ||= "";
		my @manyreasons = split ",",$manyreasons;

		my $thk = $vals{rest} =~ m/\(thk/;
		my $static_se = $flags =~ m/,se/ ? 1 : 0;
		my $boring = 0;
		my $dynamic_se = $vals{multiple} == 0 ? 1 : 0;
		my $dynamic_dead = ($vals{single} + $vals{multiple} == 0) ? 1 : 0;

		if ($thk) {
			$thunk_counts{$static_se}{$dynamic_se + $dynamic_dead}++;
			$dyn_thunk_counts{$static_se}{$dynamic_se + $dynamic_dead} += $vals{nalloc};
		} else {
			$fun_counts{$static_se}{$dynamic_se + $dynamic_dead}++;
		}

		if ($thk and $dynamic_se and not $static_se and not $boring) {
			$reason_counts{$_} += $vals{nalloc} for @manyreasons;
			if (@manyreasons > 1) {
				$unique_reason_counts{various} += $vals{nalloc};
			} else {
				$unique_reason_counts{$manyreasons[0]} += $vals{nalloc};
			}
			push @interesting, { n => $vals{single}, desc => $vals{rest}};
		}
	} else {
		print "Could not parse $_"
	}
}

sub print_table {
	my ($title, $tab) = @_;

	printf <<__END__,
%s:
	      |  Static s.e. |       Normal |          Sum
Dynamic dead  | %12d | %12d | %12d (Proportion: %4.1f%%)
Dynamic s.e.  | %12d | %12d | %12d (Proportion: %4.1f%%)
Multi entries | %12d | %12d | %12d
Sum           | %12d | %12d | %12d

__END__
		$title,
		$tab->{1}{2},
		$tab->{0}{2},
		$tab->{1}{2} + $tab->{0}{2},
		($tab->{1}{2} + $tab->{0}{2}) ? ($tab->{1}{2} / ($tab->{1}{2} + $tab->{0}{2}) * 100) : 0,
		$tab->{1}{1},
		$tab->{0}{1},
		$tab->{1}{1} + $tab->{0}{1},
		($tab->{1}{1} + $tab->{0}{1}) ? ($tab->{1}{1} / ($tab->{1}{1} + $tab->{0}{1}) * 100) : 0,
		$tab->{1}{0},
		$tab->{0}{0},
		$tab->{1}{0} + $tab->{0}{0},
		$tab->{1}{2} + $tab->{1}{1} + $tab->{1}{0},
		$tab->{0}{2} + $tab->{0}{1} + $tab->{0}{0},
		$tab->{1}{2} + $tab->{0}{2} + $tab->{1}{1} + $tab->{0}{1} + $tab->{1}{0} + $tab->{0}{0};

}

print_table ("Thunks (counted per info table)", \%thunk_counts);
print_table ("Thunks (counted per dynamically allocated instance)", \%dyn_thunk_counts);
print_table ("Functions", \%fun_counts);

@interesting = sort { $b->{n} <=> $a->{n} } @interesting;

# srsly? should have used Haskell...
sub max ($$) { $_[$_[0] < $_[1]] }
sub min ($$) { $_[$_[0] > $_[1]] }


printf <<__END__;
Interesting missed opportunities:
__END__

for (@interesting[0..min(10,$#interesting)]) {
	printf "%10d: %s\n", $_->{n}, $_->{desc};
}

my $total = $dyn_thunk_counts{0}{1} + $dyn_thunk_counts{0}{2};

my @reason_counts = ();
push @reason_counts, { reason => $_, n => $reason_counts{$_} } foreach keys %reason_counts ;
@reason_counts = sort { $b->{n} <=> $a->{n} } @reason_counts;

printf <<__END__;
Most common reasons
__END__

for (@reason_counts[0..min(99,$#reason_counts)]) {
	printf "%10d: (%4.1f%%) %s\n", $_->{n}, $_->{n} / $total * 100, $_->{reason};
}


my @unique_reason_counts = ();
push @unique_reason_counts, { reason => $_, n => $unique_reason_counts{$_} } foreach keys %unique_reason_counts ;
@unique_reason_counts = sort { $b->{n} <=> $a->{n} } @unique_reason_counts;

printf <<__END__;
Most common unique reasons
__END__

for (@unique_reason_counts[0..min(99,$#unique_reason_counts)]) {
	printf "%10d: (%4.1f%%) %s\n", $_->{n}, $_->{n} / $total * 100,  $_->{reason};
}


