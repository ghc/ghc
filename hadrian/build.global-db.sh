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

root="$(dirname "$(rl "$0")")"

mkdir -p "$root/bin"

ghc                                      \
    "$root/src/Main.hs"                  \
    -Wall                                \
    -fno-warn-name-shadowing             \
    -XRecordWildCards                    \
    -i"$root/src"                        \
    -i"$root/../libraries/Cabal/Cabal"   \
    -rtsopts                             \
    -with-rtsopts=-I0                    \
    -threaded                            \
    -outputdir="$root/bin" \
    -j -O                                \
    -o "$root/bin/hadrian"

"$root/bin/hadrian"        \
    --lint                 \
    --directory "$root/.." \
    "$@"