#!/usr/bin/perl -i.bak
# Patch the cygwin dll to make a non-clashing version, for great justice

while (<>) {
  s/cygwin1/aybabtu/g;
  s/c\000y\000g\000w\000i\000n\0001/a\000y\000b\000a\000b\000t\000u/g;
  print;
}
