#!/usr/bin/env bash

set -e

rm -f "$3"
F2=$(mktemp --suffix=".x")
F3=$(mktemp --suffix=".hs")
tail -n +2 "$2" > "$F2"
alex -g --latin1 -o "$F3" "$F2"
ghc -E "$F3" -o "$3" # needed because alex spits out ifdef WORDS_BIGENDIAN conditionals
rm "$F2" "$F3"
