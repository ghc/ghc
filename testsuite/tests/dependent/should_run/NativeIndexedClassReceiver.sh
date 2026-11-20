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

# Hide the field application behind a family equation. The instance's
# functional dependency determines the receiver after the result is rewritten.
cat >../provider/LateReceiver.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE Modifiers, MultiParamTypeClasses, PolyKinds, StandaloneKindSignatures #-}
{-# LANGUAGE TypeAbstractions, TypeFamilies, UndecidableInstances #-}
module LateReceiver (fromKey, hiddenContents) where

import Data.Kind (Constraint, Type)
import GHC.Matchability (Matchability, U)
import IndexedMap (Batch(..), Pick)

type Observe :: forall (m :: Matchability). (Type %m -> Type) -> Type
type family Observe @m f where
    Observe @m f = f Bool

hiddenContents :: Batch @m f -> [Observe @m f]
hiddenContents (Batch xs) = xs

type Choose :: forall (m :: Matchability). Type -> (Type %m -> Type) -> Constraint
class Choose @m key f | key -> m f where
    fromKey :: key -> Batch @m f

instance (f ~ Pick 'True) => Choose @U Bool f where
    fromKey _ = Batch [Just True]
HS
(cd ../provider && "${ghc[@]}" -c LateReceiver.hs)
cp ../provider/LateReceiver.hi .

cat >LateReceiverClient.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module LateReceiverClient (main) where

import LateReceiver (fromKey, hiddenContents)

cells :: [Maybe Bool]
cells = hiddenContents (fromKey False)

main :: IO ()
main = print cells
HS

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

cat >WrongResult.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module WrongResult where

import IndexedMap (familyInput, getCells, mapIndexed)

wrong :: [Maybe Char]
wrong = getCells (mapIndexed (fmap not) familyInput)
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
    if "${ghc[@]}" "-X$mode" -c WrongResult.hs >result.log 2>&1 ||
       ! grep -Fq '[GHC-83865]' result.log ||
       ! grep -Fq '[Maybe Char]' result.log; then
        cat result.log >&2
        exit 1
    fi
    if "${ghc[@]}" "-X$mode" -c ExplicitMatchable.hs >wrong.log 2>&1 ||
       ! grep -Fq 'familyInput' wrong.log ||
       ! grep -Eq '\[GHC-(83865|18872)\]' wrong.log; then
        cat wrong.log >&2
        exit 1
    fi

    "${ghc[@]}" "-X$mode" -fforce-recomp -c LateReceiverClient.hs -main-is LateReceiverClient
    "${ghc[@]}" -main-is LateReceiverClient LateReceiverClient.o \
        ../provider/{IndexedMap,LateReceiver}.o -o late-receiver
    [[ $(./late-receiver) == '[Just True]' ]]
done
