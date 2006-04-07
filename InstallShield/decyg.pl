#!/usr/bin/perl
# Patch a DLL or EXE to change the name of the Cygwin DLL it uses or is, so that we can
# include our own Cygwin DLL that doesn't interfere with the rest of the system, for great justice

@ARGV = ('-') unless @ARGV;
@FILES = @ARGV;
while ($ARGV = shift) {
  $out = $ARGV . ".new";
  open(IN, $ARGV) or warn "Can't open $ARGV: $!\n";
  open(OUT, ">$out") or warn "Can't open $out: $!\n";
  binmode IN;
  while (<IN>) {
    s/cygwin1/aybabtu/g;
    s/c\000y\000g\000w\000i\000n\0001/a\000y\000b\000a\000b\000t\000u/g;
    print OUT;
  }
  close IN;
  close OUT;
  unlink $ARGV;
  rename $out, $ARGV;
}
