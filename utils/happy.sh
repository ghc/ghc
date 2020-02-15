#!/usr/bin/env bash

set -e

rm -f "$3"
F2=$(mktemp --suffix=".y")
F3=$(mktemp --suffix=".hs")
cp "$2" "$F2"  # needed because happy rejects files that do not have the .y extension
happy -agc --strict --outfile="$F3" "$F2"
ghc -E "$F3" -o "$3"  # needed because happy spits out #if __GLASGOW_HASKELL__ >= 710 conditionals
rm "$F2" "$F3"
