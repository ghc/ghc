#!/usr/bin/env bash

CABAL=cabal

set -euo pipefail

# readlink on os x, doesn't support -f, to prevent the
# need of installing coreutils (e.g. through brew, just
# for readlink, we use the follownig substitute.
#
# source: http://stackoverflow.com/a/1116890
function rl {
    TARGET_FILE="$1"

    cd "$(dirname "$TARGET_FILE")"
    TARGET_FILE="$(basename "$TARGET_FILE")"

    # Iterate down a (possible) chain of symlinks
    while [ -L "$TARGET_FILE" ]
    do
        TARGET_FILE="$(readlink "$TARGET_FILE")"
        cd "$(dirname "$TARGET_FILE")"
        TARGET_FILE="$(basename "$TARGET_FILE")"
    done

    # Compute the canonicalized name by finding the physical path
    # for the directory we're in and appending the target file.
    PHYS_DIR="$(pwd -P)"
    RESULT="$PHYS_DIR/$TARGET_FILE"
    echo "$RESULT"
}

absoluteRoot="$(dirname "$(rl "$0")")"
cd "$absoluteRoot"

if ! type "$CABAL" > /dev/null; then
    echo "Please make sure 'cabal' is in your PATH"
    exit 2
fi

CABVERSTR=$("$CABAL" --numeric-version)

CABVER=( ${CABVERSTR//./ } )

if [ "${CABVER[0]}" -eq 2 -o "${CABVER[0]}" -eq 1 -a "${CABVER[1]}" -ge 24 ]; then
    # New enough cabal version detected, so
    # let's use the superior 'cabal new-build' mode

    "$CABAL" new-build --disable-profiling --disable-documentation -j exe:hadrian
    "$CABAL" new-run hadrian --        \
        --lint                         \
        --directory "$absoluteRoot/.." \
        "$@"

else
    # The logic below is quite fragile, but it's better than nothing for pre-1.24 cabals
    echo "Old pre cabal 1.24 version detected. Falling back to legacy 'cabal sandbox' mode."

    # Initialize sandbox if necessary
    if ! ( "$CABAL" sandbox hc-pkg list > /dev/null 2>&1); then
        "$CABAL" sandbox init
        "$CABAL" sandbox add-source ../libraries/Cabal/Cabal
        "$CABAL" install                \
            --dependencies-only         \
            --disable-library-profiling \
            --disable-shared
    fi

    "$CABAL" run hadrian --            \
        --lint                         \
        --directory "$absoluteRoot/.." \
        "$@"
fi
