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
cp "$src/AnchorReceiver.hs" provider/
(cd provider && "${ghc[@]}" -c AnchorReceiver.hs)
cp provider/AnchorReceiver.hi "$src/LaterMatchability.hs" client/
cd client
"${ghc[@]}" -c LaterMatchability.hs
"${ghc[@]}" -fprint-explicit-kinds -dppr-cols=250 --show-iface LaterMatchability.hi >client.iface
grep -Eq 'Receiver @.*Matchable .*Select @.*Matchable ' client.iface
grep -Eq 'Receiver @.*Unmatchable .*Select @.*Unmatchable ' client.iface
grep -Eq 'type DefaultPrefix = AnchorReceiver.Select @.*Unmatchable ' client.iface
grep -Eq 'type FamilyBox :: forall \{.*:: .*Matchability\}' client.iface

# An ordinary client checks the result using only interfaces.
rm LaterMatchability.hs
mv LaterMatchability.o ../provider/
cp "$src/LaterMatchabilityClient.hs" .
"${ghc[@]}" -c LaterMatchabilityClient.hs

cat >Main.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import LaterMatchabilityClient (rankedMap)

main :: IO ()
main = print rankedMap
HS
"${ghc[@]}" -c Main.hs
"${ghc[@]}" Main.o LaterMatchabilityClient.o ../provider/{AnchorReceiver,LaterMatchability}.o -o ranked-map
test "$(./ranked-map)" = '(Just True,[False])'

cat >RankMismatch.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures, Modifiers, TypeFamilies #-}
module RankMismatch where

import Data.Kind (Type)
import GHC.Matchability (M)
import LaterMatchability (Recipe)

type family MOnly (f :: Type %M -> Type) (xs :: [Type]) :: [Type]
type Bad = Recipe MOnly
HS
if "${ghc[@]}" -c RankMismatch.hs >rank-mismatch.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' rank-mismatch.log || ! grep -Fq 'Recipe MOnly' rank-mismatch.log; then
    cat rank-mismatch.log >&2
    exit 1
fi

cat >RankTelescope.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, PolyKinds, RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures, TypeFamilies #-}
module RankTelescope where

import Data.Kind (Type)
import GHC.Matchability (M, U)
import LaterMatchability (Map)

type ShortMapper = forall {b} {a}. @U (a %M -> b) %U -> [a] %U -> [b]
type Short :: ShortMapper -> Type
data Short f = Short

type Specialized = Short Map
HS
"${ghc[@]}" -c RankTelescope.hs
cat >>RankTelescope.hs <<'HS'

type WrongMapper = forall (flag :: Bool) {b} {a}. @U (a %M -> b) %U -> [a] %U -> [b]
type Wrong :: WrongMapper -> Type
data Wrong f = Wrong

type BadKind = Wrong Map
HS
if "${ghc[@]}" -c RankTelescope.hs >rank-telescope.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' rank-telescope.log || ! grep -Fq 'Wrong Map' rank-telescope.log; then
    cat rank-telescope.log >&2
    exit 1
fi

cat >RankOptIn.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module RankOptIn where

import LaterMatchability (Map, Recipe)

type Bad = Recipe Map
HS
if "${ghc[@]}" -c RankOptIn.hs >rank-opt-in.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' rank-opt-in.log || ! grep -Fq 'Recipe Map' rank-opt-in.log; then
    cat rank-opt-in.log >&2
    exit 1
fi

cat >ScopeControls.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures, TypeAbstractions, TypeApplications, TypeFamilies #-}
module ScopeControls where

import AnchorReceiver
import Data.Kind (Type)
import GHC.Matchability (M, U, Matchability)

type family Forget (m :: Matchability) :: Matchability where
    Forget M = U
    Forget U = U

type HiddenReceiver :: forall (m :: Matchability).
    ((Type %m -> Type) %U -> Type) -> Anchor (Forget m) -> Type
type family HiddenReceiver @m f anchor where
    HiddenReceiver @M f 'Unmatch = f Maybe
    HiddenReceiver @U f 'Unmatch = f Identity

type UnrelatedReceiver :: forall (m :: Matchability).
    ((Type %m -> Type) %U -> Type) -> Bool -> Type
type family UnrelatedReceiver @m f flag where
    UnrelatedReceiver @M f 'True = f Maybe
    UnrelatedReceiver @U f 'True = f Identity

hidden :: HiddenReceiver (Select 'True) 'Unmatch
hidden = True

hiddenExplicit :: HiddenReceiver @M (Select @M 'True) 'Unmatch
hiddenExplicit = Just True

unrelated :: UnrelatedReceiver (Select 'True) 'True
unrelated = True

unrelatedExplicit :: UnrelatedReceiver @M (Select @M 'True) 'True
unrelatedExplicit = Just True
HS
"${ghc[@]}" -c ScopeControls.hs

cat >ExplicitMismatch.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeApplications #-}
module ExplicitMismatch where

import AnchorReceiver
import GHC.Matchability (M, U)

type FixedReceiver = Receiver @U (Select 'True) 'Match
type FixedSelect = Receiver (Select @U 'True) 'Match
type WrongAnchor = Receiver @M (Select @M 'True) 'Unmatch
HS
if "${ghc[@]}" -c ExplicitMismatch.hs >mismatch.log 2>&1 ||
   [[ $(grep -Fc '[GHC-83865]' mismatch.log) != 3 ]]; then
    cat mismatch.log >&2
    exit 1
fi

cat >Ambiguous.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures, TypeAbstractions, TypeFamilies #-}
module Ambiguous where

import AnchorReceiver
import Data.Kind (Type)
import GHC.Matchability (Matchability, U)

type family Selector (a :: Type) :: Matchability
type Unselected :: forall a. @U Anchor (Selector a)
type family Unselected @a

type Ambiguous = Receiver (Select 'True) Unselected
HS
if "${ghc[@]}" -c Ambiguous.hs >ambiguous.log 2>&1 ||
   ! grep -Fq '[GHC-16220]' ambiguous.log ||
   ! grep -Fq 'Uninferrable type variable' ambiguous.log; then
    cat ambiguous.log >&2
    exit 1
fi

cat >BoxShapes.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures, PolyKinds, TypeFamilies #-}
module BoxShapes where

import AnchorReceiver (WrapOpen)
import LaterMatchability (FamilyBox)

data Recursive f = Recursive (f Bool) (Maybe (Recursive f))

sample :: Recursive Maybe
sample = Recursive (Just True) Nothing

open :: Recursive (WrapOpen 'True)
open = Recursive [False] Nothing

type OpenCallback = WrapOpen 'True
data UsesBoxKind (x :: FamilyBox OpenCallback) = UsesBoxKind
HS
"${ghc[@]}" -c BoxShapes.hs

cat >BoxMismatch.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures, TypeFamilies #-}
module BoxMismatch where

import AnchorReceiver (WrapOpen)
import Data.Kind (Type)

data FixedBox (f :: Type -> Type) = FixedBox (f Bool)

type Bad = FixedBox (WrapOpen 'True)
HS
if "${ghc[@]}" -c BoxMismatch.hs >box-mismatch.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' box-mismatch.log ||
   ! grep -Fq 'WrapOpen' box-mismatch.log; then
    cat box-mismatch.log >&2
    exit 1
fi

cat >BoxArity.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeFamilies #-}
module BoxArity where

import AnchorReceiver (WrapOpen)
import LaterMatchability (ForwardBox)

type Bad = ForwardBox (WrapOpen 'True)
HS
if "${ghc[@]}" -c BoxArity.hs >box-arity.log 2>&1 ||
   ! grep -Fq '[GHC-27346]' box-arity.log ||
   ! grep -Fq 'should have 2 arguments, but has been given 1' box-arity.log; then
    cat box-arity.log >&2
    exit 1
fi
