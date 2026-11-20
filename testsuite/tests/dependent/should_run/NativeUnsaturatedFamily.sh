#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -O0 -i -i. -package-env - -dcore-lint -dstg-lint
     -fdiagnostics-color=never)
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
cp "$src"/NativeUnsaturated{Eta,Inverse,Client}.hs .
mkdir producer client

"${ghc[@]}" -c NativeUnsaturatedEta.hs
if "${ghc[@]}" -c NativeUnsaturatedInverse.hs >inverse.log 2>&1 ||
   ! grep -Fq '[GHC-25897]' inverse.log; then
    cat inverse.log
    exit 1
fi

(
    cd producer
    cp "$src/NativeUnsaturatedProvider.hs" .
    "${ghc[@]}" -c NativeUnsaturatedProvider.hs
)
cp producer/NativeUnsaturatedProvider.hi NativeUnsaturatedClient.hs client/
(cd client && "${ghc[@]}" -c NativeUnsaturatedClient.hs)

# Two different prefixes are not known apart solely because their
# unsaturated applications cannot be decomposed.
sed -e 's/module NativeUnsaturatedClient/module Stuck/' \
    -e "s/Same (Alias 'True) (Alias 'True) :~: 'True/Same (Alias 'True) (Alias 'False) :~: 'False/" \
    NativeUnsaturatedClient.hs >client/Stuck.hs
if (cd client && "${ghc[@]}" -c Stuck.hs) >stuck.log 2>&1 ||
   ! grep -Fq "Couldn't match type" stuck.log ||
   ! grep -Fq 'reflexive = Refl' stuck.log; then
    cat stuck.log
    exit 1
fi

# Change only the provider equation. The client is unchanged and sees only
# the new interface. Compile without -fforce-recomp to exercise invalidation.
(
    cd producer
    sed "s/^type instance Select a 'True = a$/type instance Select a 'True = 'False/" \
        "$src/NativeUnsaturatedProvider.hs" >NativeUnsaturatedProvider.hs
    "${ghc[@]}" -c NativeUnsaturatedProvider.hs
)
cp producer/NativeUnsaturatedProvider.hi client/
if (cd client && "${ghc[@]}" -c NativeUnsaturatedClient.hs) >changed.log 2>&1 ||
   ! grep -Fq "Couldn't match type" changed.log ||
   ! grep -Fq "saturated = Refl" changed.log; then
    cat changed.log
    exit 1
fi

# A validated alias can cross into an ordinary module. A synonym prefix
# still cannot establish that its supplied argument is injective.
cd "$out/producer"
cp "$src/NativePartialSynonym.hs" .
"${ghc[@]}" -c NativePartialSynonym.hs
cd "$out/client"
cp ../producer/NativePartialSynonym.hi .
cat >OrdinarySynonym.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module OrdinarySynonym where
import NativePartialSynonym (Saved)
use :: Saved Bool -> Saved Bool
use = id
HS
"${ghc[@]}" -c OrdinarySynonym.hs
cat >BadInjectivity.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TypeFamilyDependencies #-}
module BadInjectivity where
import NativePartialSynonym (Drop, Slot)
type family Claim a = r | r -> a where
    Claim a = Slot (Drop a)
HS
if "${ghc[@]}" -c BadInjectivity.hs >injectivity.log 2>&1 ||
   ! grep -Fq '[GHC-05175]' injectivity.log ||
   ! grep -Fq 'Claim a = Slot (Drop a)' injectivity.log; then
    cat injectivity.log
    exit 1
fi

# The omitted variable must be absent from the function that remains.
cat >BadForward.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TypeOperators #-}
module BadForward where
import Data.Type.Equality
import NativePartialSynonym (Slot)
type Pair e a = (e, a)
type Copy a = Pair a a
wrong :: Slot Copy :~: Slot (Pair Bool)
wrong = Refl
HS
if "${ghc[@]}" -c BadForward.hs >forward.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' forward.log; then
    cat forward.log
    exit 1
fi
