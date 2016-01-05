#!/usr/bin/env bash

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

absoltueRoot="$(dirname "$(rl "$0")")"
cd "$absoltueRoot"

# Initialize sandbox if necessary
if ! ( cabal sandbox hc-pkg list 2>&1 > /dev/null ); then
    cabal sandbox init
    cabal install                   \
        --dependencies-only         \
        --disable-library-profiling \
        --disable-shared
fi

cabal run ghc-shake --             \
    --lint                         \
    --directory "$absoltueRoot/.." \
    --colour                       \
    "$@"
