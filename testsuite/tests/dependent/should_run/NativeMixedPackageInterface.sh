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
main = print (map (observe . forget . retained) [True, False],
              observe (forget (unknown (error "forced witness"))))
HS

for level in 1 2 3; do
    dir="$out/$level"
    mkdir -p "$dir/client"
    (
        cd "$dir"
        opts=("-B$lib" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
              "-fwrite-if-compression=$level")
        "$ghc" "${opts[@]}" -O0 -fomit-interface-pragmas -fno-write-if-simplified-core -c "$out/ForgetWitness.hs" -outputdir .
        "$ghc" "-B$lib" --show-iface ForgetWitness.hi >header
        grep -Eq '^ *got +[^[:space:]]+-scoped-sigma-combined-v1$' header
        cp ForgetWitness.hi "$out/Main.hs" client/
        cd client
        for opt in -O0 -O1; do
            "$ghc" "${opts[@]}" "$opt" -fforce-recomp -c Main.hs
            "$ghc" "${opts[@]}" Main.o ../ForgetWitness.o -o run
            ./run
        done
    )
done

cd "$out"
cat >Incompatible.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module Incompatible where

import Data.Proxy
import GHC.Exists
import GHC.Sigma (Sigma(Pair))

type Both = some (b :: Bool) | (Proxy b, Proxy b)
type NeedsTrue = exists (b :: Bool) | (Proxy b, Proxy 'True)

bad :: Both -> NeedsTrue
bad (Pair _ fields) = Pack _ fields
HS
if "$ghc" "-B$lib" -v0 -O0 -package-env - -fdiagnostics-color=never -c Incompatible.hs >bad.log 2>&1; then
    echo "Accepted an unknown retained index where True was required" >&2
    exit 1
fi
grep -Eq '^Incompatible.hs:[0-9]+:[0-9]+: error: \[GHC-83865\]' bad.log || { cat bad.log >&2; exit 1; }
grep -Eq "Proxy '?True" bad.log || { cat bad.log >&2; exit 1; }
