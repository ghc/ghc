#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -O0 -i -i. -package-env - -dcore-lint -dstg-lint
     -fdiagnostics-color=never)
stripped=(-fomit-interface-pragmas -fno-write-if-simplified-core)
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
    "${ghc[@]}" "${stripped[@]}" -c NativeUnsaturatedProvider.hs
)
cp producer/NativeUnsaturatedProvider.hi NativeUnsaturatedClient.hs client/
(
    cd client
    for mode in NoImpredicativeTypes ImpredicativeTypes; do
        "${ghc[@]}" "-X$mode" -fforce-recomp -c NativeUnsaturatedClient.hs
    done
)

# A direct partial family needs the extension when the named binder accepts U.
cat >client/DirectCallback.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, TypeApplications, TypeOperators #-}
module DirectCallback where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (M, U)
import NativeUnsaturatedProvider (NamedMap, Select)

checked :: NamedMap @Bool @U @Bool (Select 'True) '[ 'True] :~: '[Select 'True 'True]
checked = Refl
HS
if (cd client && "${ghc[@]}" -XNoExperimentalDependentTypes -c DirectCallback.hs) >direct.log 2>&1 ||
   ! grep -Fq 'DirectCallback.hs:8:37: error: [GHC-27346]' direct.log ||
   ! grep -Fq 'ExperimentalDependentTypes' direct.log; then
    cat direct.log
    exit 1
fi
(cd client && "${ghc[@]}" -XExperimentalDependentTypes -fforce-recomp -c DirectCallback.hs)

# A fixed M binder rejects the U family prefix while checking its kind.
sed -e 's/module DirectCallback/module DirectMismatch/' -e 's/@U/@M/' \
    client/DirectCallback.hs >client/DirectMismatch.hs
if (cd client && "${ghc[@]}" -XNoExperimentalDependentTypes -c DirectMismatch.hs) >direct-m.log 2>&1 ||
   ! grep -Fq 'DirectMismatch.hs:8:37: error: [GHC-83865]' direct-m.log ||
   grep -Fq 'ExperimentalDependentTypes' direct-m.log; then
    cat direct-m.log
    exit 1
fi

# A written higher-order arrow keeps its M default.
cat >client/WrittenArrow.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module WrittenArrow where

import NativeUnsaturatedProvider (Callback, WrittenMap)

type Bad = WrittenMap (Callback 'True) '[ 'True]
HS
if (cd client && "${ghc[@]}" -c WrittenArrow.hs) >written.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' written.log ||
   ! grep -Fq 'Unmatchable' written.log ||
   ! grep -Fq 'Matchable' written.log; then
    cat written.log
    exit 1
fi

# NoPolyKinds retains usable M kinds for both application and forwarding.
cat >client/NoPolyFamily.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, NoPolyKinds #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
module NoPolyFamily where

import Data.Type.Equality ((:~:)(Refl))
import NativeUnsaturatedProvider (ForwardMap)

type family ApplyOne f x where
  ApplyOne f x = f x

type family ForwardOne f xs where
  ForwardOne f (x ': y ': xs) = ForwardMap f xs

app :: ApplyOne Maybe Bool :~: Maybe Bool
app = Refl

forward :: ForwardOne Maybe '[Int, Bool, Char] :~: '[Maybe Char]
forward = Refl
HS
(cd client && "${ghc[@]}" -c NoPolyFamily.hs)

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
    "${ghc[@]}" "${stripped[@]}" -c NativeUnsaturatedProvider.hs
)
cp producer/NativeUnsaturatedProvider.hi client/
if (cd client && "${ghc[@]}" -c NativeUnsaturatedClient.hs) >changed.log 2>&1 ||
   ! grep -Fq "Couldn't match type" changed.log ||
   ! grep -Fq "saturated = Refl" changed.log; then
    cat changed.log
    exit 1
fi
