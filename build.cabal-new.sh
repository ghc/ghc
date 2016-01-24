#!/usr/bin/env bash

# This wrapper scripts makes use of cabal 1.24+'s nix-store;
# In order to clean/reset, remove the `dist-newstyle/` folder

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

mkdir -p "$root/.shake"

# Notes/Random thoughts:
#
# - if ghc.git had a top-level `cabal.project` file, we could maybe avoid the
# boilerplate above, as we could simply say `cabal exec ghc-shake` from within
# any GHC folder not shadowed by a nearer shadowing `cabal.project` file.

pushd "$root/"

cabal new-build --disable-profiling --disable-documentation -j exe:ghc-shake

PKGVER="$(awk '/^version:/ { print $2 }' shaking-up-ghc.cabal)"

cp -v "$root/dist-newstyle/build/shaking-up-ghc-${PKGVER}/build/ghc-shake/ghc-shake" \
      "$root/.shake/build"

popd

"$root/.shake/build"       \
    --lint                 \
    --directory "$root/.." \
    --colour               \
    "$@"
