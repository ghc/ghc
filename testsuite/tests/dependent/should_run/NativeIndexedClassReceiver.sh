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

cat >../provider/ReceiverProducer.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module ReceiverProducer (makeReceiver) where

import IndexedMap (FamilyBatch, familyInput)

makeReceiver :: () -> FamilyBatch
makeReceiver () = familyInput
HS
(cd ../provider && "${ghc[@]}" -c ReceiverProducer.hs)
cp ../provider/ReceiverProducer.hi .

cat >TypedReceiverClient.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module TypedReceiverClient (main) where

import IndexedMap (getCells, mapIndexed)
import ReceiverProducer (makeReceiver)

main :: IO ()
main = print (getCells (mapIndexed (fmap not) (makeReceiver ())))
HS

cat >WrongProducer.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module WrongProducer where

import IndexedMap (getCells, mapIndexed)
import ReceiverProducer (makeReceiver)

wrong :: IO ()
wrong = print (getCells (mapIndexed (fmap not) (makeReceiver True)))
HS

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

    "${ghc[@]}" "-X$mode" -fforce-recomp -c TypedReceiverClient.hs -main-is TypedReceiverClient
    "${ghc[@]}" -main-is TypedReceiverClient TypedReceiverClient.o \
        ../provider/{IndexedMap,ReceiverProducer}.o -o typed-receiver
    [[ $(./typed-receiver) == '[Just True,Nothing]' ]]

    if "${ghc[@]}" "-X$mode" -c WrongProducer.hs >producer.log 2>&1 ||
       ! grep -Fq '[GHC-83865]' producer.log ||
       ! grep -Fq "namely \`True'" producer.log; then
        cat producer.log >&2
        exit 1
    fi

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
