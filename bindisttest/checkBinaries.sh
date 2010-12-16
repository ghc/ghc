#!/bin/sh

EXPECTED_VERSION="$1"

# Our shared libraries are currently executable (is that a bug?), so
# we exclude anything that looks like a shared library
for f in `find bindisttest/a/b/c -type f -perm -u+x ! -name '*.so' ! -name '*.dylib' ! -name '*.dll'`
do
    if grep -q '("GHC RTS", "YES")' "$f"
    then
        # Looks like a GHC executable. Is it for the right version?
        THIS_VERSION=`./$f +RTS --info | grep '"GHC version"' | sed -e 's/^ ,("GHC version", "//' -e 's/")$//'`
        if [ "$THIS_VERSION" != "$EXPECTED_VERSION" ]
        then
            echo "Bad GHC version '$THIS_VERSION' for '$f'" >&2
            exit 1
        fi
    fi
done
