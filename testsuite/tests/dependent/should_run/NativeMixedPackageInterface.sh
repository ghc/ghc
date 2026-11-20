#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

ghc=$1
lib=$2
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

# A testsuite run copies the example into the test directory.
cp "$src/ForgetWitness.hs" "$out/"
cat >"$out/Main.hs" <<'HS'
module Main where

import ForgetWitness

main :: IO ()
main = print (map (observe . forget . retained) [True, False])
HS

for level in 1 2 3; do
    dir="$out/$level"
    mkdir -p "$dir/client"
    (
        cd "$dir"
        opts=("-B$lib" -v0 -package-env - -i -i. -O0 -dcore-lint -dstg-lint
              -fomit-interface-pragmas -fno-write-if-simplified-core "-fwrite-if-compression=$level")
        "$ghc" "${opts[@]}" -c "$out/ForgetWitness.hs" -outputdir .
        "$ghc" "-B$lib" --show-iface ForgetWitness.hi >header
        grep -Eq '^ *got +[^[:space:]]+-scoped-sigma-combined-v1$' header
        cp ForgetWitness.hi "$out/Main.hs" client/
        cd client
        "$ghc" "${opts[@]}" -c Main.hs
        "$ghc" "${opts[@]}" Main.o ../ForgetWitness.o -o run
        ./run
    )
done
