#!/usr/bin/perl
my $quote_open = 0;
my $quote_char = '';
my $accum = "";
my $once = 1;
my $c;

# This program generates a partial Haskell list of Strings from
# words passed via stdin suitable for use in package.conf, e.g.:
#
#   foo bar   --> "foo", "bar"
#   "foo bar" --> "foo bar"
#   foo\"bar  --> "foo\"bar"
#
# Invoking genargs.pl with -comma will print an initial comma if
# there's anything to print at all.
#
# Sample application in a Makefile:
#  HSIFIED_EXTRA_LD_OPTS= `echo "$(EXTRA_LD_OPTS)" | $(PERL) genargs.pl`
#  PACKAGE_CPP_OPTS += -DHSIFIED_EXTRA_LD_OPTS="$(HSIFIED_EXTRA_LD_OPTS)"

sub printaccum {
  if ($once) {
    if ($ARGV[0] eq "-comma") {
      print ", ";
    }
  } else {
    print ", ";
  }
  $once=0;
  print '"';
  print $accum;
  print '"';
}

while ($c = getc) {
  if ($quote_open) {
    if ($c eq $quote_char) {
      $quote_open = 0;
    } elsif ($c eq '"') {
      $accum .= '\"';
    } else {
      $accum .= $c;
    }
  } else {
    if (($c eq ' ') || ($c eq "\n")) {
      if (!($accum eq "")) {
	printaccum;
	$accum = "";
      }
    } elsif ($c eq "\\") {
      $accum .= $c;
      $c = getc;
      $accum .= $c;
    } elsif (($c eq '"') || ($c eq "\'")) {
      $quote_open = 1;
      $quote_char = $c;
    } else {
      $accum .= $c
    }
  }
}
