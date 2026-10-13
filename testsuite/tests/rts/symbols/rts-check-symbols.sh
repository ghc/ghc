#!/usr/bin/env bash

#
# Check the diff between symbols exported from the RTS and those listed in RtsSymbols.c
#

DECLARED_SYMBOLS=declared_symbols.txt
OBSERVED_SYMBOLS=observed_symbols.txt

# Get rts Symbols listed in RtsSymbols.c
./Main dump | sort > $DECLARED_SYMBOLS

# Find the path of the RTS shared object that ./Main is linked against
if command -v ldd > /dev/null 2>&1; then
    # Match e.g.:
    #    libHSrts-1.0.3-ghc9.15.20260710.so => /home/alice/s p a c e/myApp/lib/libHSrts-1.0.3-ghc9.15.20260710.so (0x7ffebc9a0000)
    RTS_DYLIB_PATH=$(ldd ./Main | awk '/^[[:space:]]*libHSrts-.* => .+$/ {
        sub(/^[[:space:]]*libHSrts-.* => /, "")
        sub(/ \([^(]*\)$/, "")
        print
    }')
elif command -v otool > /dev/null 2>&1; then
    # macOS has no ldd. Use otool to find the RTS install name, e.g.:
    #    @rpath/libHSrts-1.0.3-ghc9.15.20260710.dylib (compatibility version 0.0.0, current version 0.0.0)
    RTS_INSTALL_NAME=$(otool -L ./Main | awk '/^[[:space:]]*[^[:space:]].*\/libHSrts-[^\/]*\.dylib \(/ {
        sub(/^[[:space:]]*/, "")
        sub(/ \([^(]*\)$/, "")
        print
        exit
    }')

    case "$RTS_INSTALL_NAME" in
        @rpath/*)
            # Resolve @rpath against the LC_RPATH entries of ./Main e.g.:
            #          cmd LC_RPATH
            #      cmdsize 120
            #         path /Users/alice/s p a c e/lib/rts-1.0.3 (offset 12)
            MAIN_DIR=$(cd "$(dirname ./Main)" && pwd)
            RTS_DYLIB_PATH="not found"
            while IFS= read -r RPATH; do
                RPATH="${RPATH/#@loader_path/$MAIN_DIR}"
                RPATH="${RPATH/#@executable_path/$MAIN_DIR}"
                CANDIDATE="$RPATH/${RTS_INSTALL_NAME#@rpath/}"
                if [ -f "$CANDIDATE" ]; then
                    RTS_DYLIB_PATH="$CANDIDATE"
                    break
                fi
            done < <(otool -l ./Main | awk '
                $1 == "cmd" { in_rpath = ($2 == "LC_RPATH") }
                in_rpath && $1 == "path" {
                    sub(/^[[:space:]]*path /, "")
                    sub(/ \(offset [0-9]+\)$/, "")
                    print
                }')
            ;;
        *)
            RTS_DYLIB_PATH="$RTS_INSTALL_NAME"
            ;;
    esac
else
    echo "Neither ldd nor otool is available to find the RTS DSO" 1>&2
    exit 1
fi

if [ -z "$RTS_DYLIB_PATH" -o "$RTS_DYLIB_PATH" = "not found" ]; then
    echo "Could not find path to RTS DSO: \"$RTS_DYLIB_PATH\"" 1>&2
    exit 1
fi

# Dump the exported symbols. Main.hs is responsible for parsing the output.
# The dynamic symbol table of the shared object. We use readelf rather
# than nm as nm classifies symbols by section rather than by symbol
# type, and with tables next to code info tables (objects) live in the
# .text section.
readelf -W --dyn-syms "$RTS_DYLIB_PATH" > $OBSERVED_SYMBOLS

./Main diff $DECLARED_SYMBOLS $OBSERVED_SYMBOLS
