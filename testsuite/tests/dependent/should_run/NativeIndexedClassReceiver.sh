#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

src=$(cd "$(dirname "$0")" && pwd)
ghc=("$1" "-B$2" -v0 -O0 -package-env - -dcore-lint -dstg-lint
     -fdiagnostics-color=never -fomit-interface-pragmas -fno-write-if-simplified-core)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"

mkdir provider client
cp "$src/IndexedMap.hs" provider/
(cd provider && "${ghc[@]}" -c IndexedMap.hs)
cp "$src/"{IndexedMapClient,NativeIndexedClassReceiver}.hs provider/IndexedMap.hi client/
cd client

cat >Underdetermined.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, TypeApplications #-}
module Underdetermined where

import GHC.Matchability (U)
import IndexedMap (Batch(..), getCells, mapIndexed)

ambiguous :: IO ()
ambiguous = print (getCells (mapIndexed @U id (Batch [])))
HS

cat >ExplicitMatchable.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, TypeApplications #-}
module ExplicitMatchable where

import GHC.Matchability (M)
import IndexedMap (MatchableBatch, familyInput, mapIndexed)

wrong :: MatchableBatch
wrong = mapIndexed @M (fmap not) familyInput
HS

# Reuse the same stripped interface with and without Quick Look.
for mode in NoImpredicativeTypes ImpredicativeTypes; do
    "${ghc[@]}" "-X$mode" -fforce-recomp -c IndexedMapClient.hs NativeIndexedClassReceiver.hs
    "${ghc[@]}" IndexedMapClient.o NativeIndexedClassReceiver.o ../provider/IndexedMap.o -o observe
    [[ $(./observe) == '([Just True,Nothing],[Just True,Nothing])' ]]

    if "${ghc[@]}" "-X$mode" -c Underdetermined.hs >ambiguous.log 2>&1 ||
       ! grep -Fq '[GHC-39999]' ambiguous.log; then
        cat ambiguous.log >&2
        exit 1
    fi
    if "${ghc[@]}" "-X$mode" -c ExplicitMatchable.hs >wrong.log 2>&1 ||
       ! grep -Fq 'familyInput' wrong.log ||
       ! grep -Eq '\[GHC-(83865|18872)\]' wrong.log; then
        cat wrong.log >&2
        exit 1
    fi
done
