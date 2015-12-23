#!/usr/bin/env bash

set -euo pipefail

root="$(dirname "$0")"

mkdir -p "$root/.shake"

ghc                           \
    "$root/src/Main.hs"       \
    -Wall                     \
    -i"$root/src"             \
    -rtsopts                  \
    -with-rtsopts=-I0         \
    -outputdir="$root/.shake" \
    -j -O                      \
    -o "$root/.shake/build"

"$root/.shake/build"       \
    --lint                 \
    --directory "$root/.." \
    --colour               \
    "$@"
