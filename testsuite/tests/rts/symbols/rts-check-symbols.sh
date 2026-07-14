#!/usr/bin/env bash

#
# Check the diff between symbols exported from the RTS and those listed in RtsSymbols.c
#

DECLARED_SYMBOLS=declared_symbols.txt
OBSERVED_SYMBOLS=observed_symbols.txt

# Discover if we're on windows
case "$OSTYPE" in
    "cygwin")
        TYPE="windows"
        ;;

    *)
        TYPE="elf"
        ;;
esac

# Get rts Symbols listed in RtsSymbols.c
./Main dump | sort > $DECLARED_SYMBOLS

# Get rts DLL exports from the DLL
# Match e.g.:
#    libHSrts-1.0.3-ghc9.15.20260710.dll => /home/alice/s p a c e/myApp/bin/libHSrts-1.0.3-ghc9.15.20260710.dll (0x7ffebc9a0000)
RTS_DYLIB_PATH=$(ldd ./Main | awk '/^[[:space:]]*libHSrts-.* => .+$/ {
    sub(/^[[:space:]]*libHSrts-.* => /, "")
    sub(/ \([^(]*\)$/, "")
    print
}')

if [ -z "$RTS_DYLIB_PATH" -o "$RTS_DYLIB_PATH" = "not found"]; then
    echo "Could not find path to RTS DSO: \"$RTS_DYLIB_PATH\"" 1>&2
    exit 1
fi

# Dump the exported symbols. Main.hs is responsible for parsing the output.
case "$TYPE" in
    "elf")
        # The dynamic symbol table of the shared object. We use readelf rather
        # than nm as nm classifies symbols by section rather than by symbol
        # type, and with tables next to code info tables (objects) live in the
        # .text section.
        readelf -W "$RTS_DYLIB_PATH" > $OBSERVED_SYMBOLS
        ;;

    "windows")
        # On windows we inspect the import library (*.dll.a) instead of the dll directly. This is important to get the correct type of info table symbols.
        # Dump the external symbols in POSIX format (one "name type [value [size]]" per line).
        nm -g -P "${RTS_DYLIB_PATH}.a" > $OBSERVED_SYMBOLS
        ;;

    *)
        echo "Unknown TYPE=$TYPE" 1>&2
        exit 1
        ;;
esac

./Main diff "$TYPE" $DECLARED_SYMBOLS $OBSERVED_SYMBOLS
