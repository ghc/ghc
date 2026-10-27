#!/usr/bin/env bash

#
# Check the diff between symbols exported from the RTS and those listed in RtsSymbols.c
#

DECLARED_SYMBOLS=declared_symbols.txt
OBSERVED_SYMBOLS=observed_symbols.txt

# Get rts Symbols listed in RtsSymbols.c
./Main dump | sort > $DECLARED_SYMBOLS

# Find the path of the RTS shared object that ./Main is linked against
# Match e.g.:
#    libHSrts-1.0.3-ghc9.15.20260710.so => /home/alice/s p a c e/myApp/lib/libHSrts-1.0.3-ghc9.15.20260710.so (0x7ffebc9a0000)
RTS_DYLIB_PATH=$(ldd ./Main | awk '/^[[:space:]]*libHSrts-.* => .+$/ {
    sub(/^[[:space:]]*libHSrts-.* => /, "")
    sub(/ \([^(]*\)$/, "")
    print
}')

# Dump the exported symbols. Main.hs is responsible for parsing the output.
# The dynamic symbol table of the shared object. We use readelf rather
# than nm as nm classifies symbols by section rather than by symbol
# type, and with tables next to code info tables (objects) live in the
# .text section.
readelf -W --dyn-syms "$RTS_DYLIB_PATH" > $OBSERVED_SYMBOLS

./Main diff $DECLARED_SYMBOLS $OBSERVED_SYMBOLS
