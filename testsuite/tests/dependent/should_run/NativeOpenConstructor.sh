#!/usr/bin/env bash
set -euo pipefail

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -O0 -v0 -package-env - -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{worker,exporter,client,stripped}
cd "$out/worker"
cp "$src/OpenConstructorWorker.hs" .
"${ghc[@]}" -c OpenConstructorWorker.hs
"${ghc[@]}" --show-iface OpenConstructorWorker.hi > worker.iface
awk '/^  routeCached ::/{body=1;next} body && /^[0-9a-f]+$/{exit} body{print}' \
    worker.iface > route-cached.iface
if [[ $(grep -c 'cachedPick' route-cached.iface) -lt 2 ]]; then
    cat route-cached.iface >&2
    printf '%s\n' 'The cache control needs two calls to cachedPick in its unfolding.' >&2
    exit 1
fi
cp OpenConstructorWorker.hi ../exporter/
cd ../exporter
cp "$src/OpenConstructorSource.hs" .
"${ghc[@]}" -c OpenConstructorSource.hs
cp OpenConstructorSource.hi OpenConstructorWorker.hi ../client/

expect_rejection() {
    local mod=$1 line=$2 code=$3 message=$4
    if "${ghc[@]}" -c "$mod.hs" > reject.log 2>&1 ||
       ! grep -Eq "^$mod\\.hs:$line:[0-9]+(-[0-9]+)?: error:" reject.log ||
       ! grep -Eq "$code" reject.log ||
       ! grep -Fq "$message" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

cat > Blocked.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
module Blocked where
import Data.Kind (Type)
import Data.Proxy (Proxy)
import qualified OpenConstructorWorker as W
blocked :: forall (p :: W.Choice Type Type). Proxy p -> W.demand p
blocked _ = undefined
SOURCE
expect_rejection Blocked 6 '\[GHC-70566\]' 'rigid argument p'

cat > Strict.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module Strict where
import qualified OpenConstructorWorker as W
strict :: forall a b. a -> b -> W.demand ('W.LeftFields a b)
strict _ y = y
SOURCE
expect_rejection Strict 4 '\[GHC-70566\]' 'rigid argument a'

cat > WrongField.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module WrongField where
import qualified OpenConstructorWorker as W
wrong :: forall a b. a -> b -> W.pick ('W.RightFields b a)
wrong x _ = x
SOURCE
expect_rejection WrongField 5 '\[GHC-(25897|83865)\]' 'x'

cat > DisjointRule.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-inline-rule-shadowing #-}
module DisjointRule where
import qualified OpenConstructorWorker as W
direct :: forall a b. a -> b -> W.pick ('W.LeftFields a b)
direct _ y = y
throughHelper :: forall a b. a -> b -> W.route ('W.RightFields b a)
throughHelper _ y = y
{-# RULES "constructor/disjoint" forall (shell :: W.Choice Bool Char).
    W.pick shell = 'z' #-}
SOURCE
"${ghc[@]}" -c DisjointRule.hs

cat > OpenRule.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-inline-rule-shadowing #-}
module OpenRule where
import Data.Kind (Type)
import qualified OpenConstructorWorker as W
pick :: forall a b. a -> b -> W.routeCached ('W.LeftFields a b)
pick _ y = y
{-# RULES "constructor/matching" forall (shell :: W.Choice Type Type).
    W.cachedPick shell = undefined #-}
SOURCE
expect_rejection OpenRule 8 '\[GHC-83605\]' 'rewrite rule can change the result'

cat > MatchingSumRule.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-inline-rule-shadowing #-}
module MatchingSumRule where
import Data.Kind (Type)
import qualified OpenConstructorWorker as W
pick :: forall a b. a -> b -> W.pick ('W.LeftFields a b)
pick _ y = y
{-# RULES "sum/matching" forall (shell :: W.Choice Type Type).
    W.pick shell = undefined #-}
SOURCE
expect_rejection MatchingSumRule 7 '\[GHC-83605\]' 'rewrite rule can change the result'

# Check the residual equations with only stripped interfaces. This route
# cannot observe the worker's ordinary unfolding or run its object code.
cd ../stripped
sed '/^{-# OPTIONS_GHC/d' "$src/OpenConstructorWorker.hs" > OpenConstructorWorker.hs
"${ghc[@]}" -fomit-interface-pragmas -c OpenConstructorWorker.hs
rm OpenConstructorWorker.hs OpenConstructorWorker.o
"${ghc[@]}" --show-iface OpenConstructorWorker.hi > stripped.iface
grep -Fq 'T:pick ' stripped.iface
grep -Fq 'T:pickNested ' stripped.iface
if grep -Fq 'T:unwrap ' stripped.iface; then
    echo 'The interface published a projection for a datatype with hidden constructors.' >&2
    exit 1
fi

cat > Residual.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module Residual (Pick, Reordered) where
import Data.Kind (Type)
import qualified OpenConstructorWorker as W

type Pick :: W.Choice Type Type -> Type
type Pick p = W.pick p

type Reordered :: W.Choice Bool Char -> Char
type Reordered p = W.reordered @Char @Bool p
SOURCE
"${ghc[@]}" -XTypeApplications -c Residual.hs

cat > ResidualClient.hs <<'SOURCE'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module ResidualClient where
import Data.Type.Equality ((:~:)(Refl))
import qualified OpenConstructorWorker as W
import Residual

left :: Pick ('W.LeftFields Int Bool) :~: Bool
left = Refl
right :: Pick ('W.RightFields Char Int) :~: Char
right = Refl
reordered :: Reordered ('W.LeftFields 'True 'x') :~: 'x'
reordered = Refl
SOURCE
"${ghc[@]}" -c ResidualClient.hs

cat > Abstract.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
module Abstract where
import Data.Kind (Type)
import qualified OpenConstructorWorker as W
type Bad (p :: W.Envelope Type) = W.unwrap p
SOURCE
expect_rejection Abstract 5 '\[GHC-70566\]' 'Cannot reduce the open call'

cat > NestedResidual.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ExplicitForAll #-}
{-# LANGUAGE PolyKinds, StandaloneKindSignatures #-}
module NestedResidual (PickNested) where

import qualified OpenConstructorWorker as W

type PickNested :: forall a. W.Nested a -> a -> a
type PickNested p fallback = W.pickNested p fallback
SOURCE
"${ghc[@]}" -c NestedResidual.hs

cat > NestedClient.hs <<'SOURCE'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables, TypeOperators #-}
module NestedClient where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NestedResidual (PickNested)
import qualified OpenConstructorWorker as W

just :: forall (x :: Bool) (fallback :: Bool).
        Proxy x -> Proxy fallback -> PickNested ('W.Nested ('Just x)) fallback :~: x
just _ _ = Refl

nothing :: forall (fallback :: Char).
           Proxy fallback -> PickNested ('W.Nested 'Nothing) fallback :~: fallback
nothing _ = Refl
SOURCE
"${ghc[@]}" -c NestedClient.hs

cat > NestedDemand.hs <<'SOURCE'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables, TypeOperators #-}
module NestedDemand where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NestedResidual (PickNested)
import qualified OpenConstructorWorker as W

-- Both defined Maybe () constructors yield '(). The worker still demands m.
bad :: forall (m :: Maybe ()). Proxy m -> PickNested ('W.Nested m) '() :~: '()
bad _ = Refl
SOURCE
expect_rejection NestedDemand 11 '\[GHC-(25897|83865|18872)\]' "Couldn't match"

cat > OuterDemand.hs <<'SOURCE'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables, TypeOperators #-}
module OuterDemand where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NestedResidual (PickNested)
import qualified OpenConstructorWorker as W

-- A rigid outer constructor cannot be assumed either.
bad :: forall (p :: W.Nested ()). Proxy p -> PickNested p '() :~: '()
bad _ = Refl
SOURCE
expect_rejection OuterDemand 11 '\[GHC-(25897|83865|18872)\]' "Couldn't match"

cat > LargerWorker.hs <<'SOURCE'
module LargerWorker (Larger(..), choose, Product(..), project, Marker(..), retain, Deep(..), innermost) where
data Larger a = Larger (Maybe a) a | Empty
choose :: Larger a -> a -> a
choose (Larger (Just x) _) _ = x
choose (Larger Nothing _) x = x
choose Empty x = x

data Product a = Product a
project :: Product a -> a
project (Product x) = x

data Marker a = Marker
retain :: Marker a -> a -> a
retain Marker x = x

data Deep a = Deep (Maybe (Maybe a))
innermost :: Deep a -> a -> a
innermost (Deep outer) fallback = case outer of
    Nothing -> fallback
    Just inner -> case inner of
        Nothing -> fallback
        Just x -> x
SOURCE
"${ghc[@]}" -fomit-interface-pragmas -c LargerWorker.hs
rm LargerWorker.hs LargerWorker.o
cat > LargerClient.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module LargerClient where
import Data.Type.Equality ((:~:)(Refl))
import qualified LargerWorker as L
proof :: L.choose ('L.Larger ('Just 'True) 'False) 'False :~: 'True
proof = Refl
empty :: L.choose 'L.Empty 'False :~: 'False
empty = Refl
productField :: L.project ('L.Product 'True) :~: 'True
productField = Refl
phantomFallback :: L.retain 'L.Marker 'False :~: 'False
phantomFallback = Refl
deep :: L.innermost ('L.Deep ('Just ('Just 'x'))) 'y' :~: 'x'
deep = Refl
innerEmpty :: L.innermost ('L.Deep ('Just 'Nothing)) 'y' :~: 'y'
innerEmpty = Refl
SOURCE
"${ghc[@]}" -c LargerClient.hs

cat > WrongMarkerKind.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module WrongMarkerKind where
import Data.Type.Equality ((:~:)(Refl))
import qualified LargerWorker as L
bad :: L.retain ('L.Marker :: L.Marker 'True) 'False :~: 'False
bad = Refl
SOURCE
expect_rejection WrongMarkerKind 5 '\[GHC-(83865|18872)\]' 'kind'

cat > NestedRule.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-inline-rule-shadowing #-}
module NestedRule where
import Data.Type.Equality ((:~:)(Refl))
import qualified OpenConstructorWorker as W
proof :: W.pickNested ('W.Nested ('Just 'True)) 'False :~: 'True
proof = Refl
{-# RULES "nested/specialized" forall (p :: W.Nested Char) x.
    W.pickNested p x = x #-}
SOURCE
"${ghc[@]}" -c NestedRule.hs
sed 's/W.Nested Char/W.Nested Bool/' NestedRule.hs > NestedRule.next
mv NestedRule.next NestedRule.hs
expect_rejection NestedRule 7 '\[GHC-83605\]' 'rewrite rule can change the result'

# Force the previously ignored field in the ordinary body. The datatype,
# signature and unchanged interface-only exporter stay the same.
sed -e '/^{-# OPTIONS_GHC/d' \
    -e 's/^pick (LeftFields _ y) = y$/pick (LeftFields x y) = x `seq` y/' \
    -e 's/^pickNested (Nested (Just x)) _ = x$/pickNested (Nested (Just x)) fallback = fallback `seq` x/' \
    "$src/OpenConstructorWorker.hs" > OpenConstructorWorker.hs
"${ghc[@]}" -fomit-interface-pragmas -c OpenConstructorWorker.hs
rm OpenConstructorWorker.hs OpenConstructorWorker.o
"${ghc[@]}" --show-iface OpenConstructorWorker.hi > strict.iface
if grep -Eq 'T:pick(Nested)? ' strict.iface; then
    echo 'The interface kept the projection after the worker began forcing a field.' >&2
    exit 1
fi
expect_rejection Residual 7 '\[GHC-70566\]' 'Cannot reduce the open call'
expect_rejection NestedResidual 8 '\[GHC-(68947|70565|70566|83865)\]' 'pickNested'

cd ../client
cp "$src/NativeOpenConstructor.hs" .
"${ghc[@]}" -c NativeOpenConstructor.hs
"${ghc[@]}" NativeOpenConstructor.o ../exporter/OpenConstructorSource.o ../worker/OpenConstructorWorker.o -o client
./client


# Change only field strictness. The unchanged exporter must recheck even
# though the worker's source body and its ordinary type stay the same.
cd ../worker
"${ghc[@]}" --show-iface OpenConstructorWorker.hi > original.iface
sed 's/LeftFields a b/LeftFields !a b/' "$src/OpenConstructorWorker.hs" > OpenConstructorWorker.hs
"${ghc[@]}" -c OpenConstructorWorker.hs
"${ghc[@]}" --show-iface OpenConstructorWorker.hi > changed.iface
grep -F 'ABI hash:' original.iface > original.hash
grep -F 'ABI hash:' changed.iface > changed.hash
if cmp -s original.hash changed.hash; then
    printf '%s\n' 'Changing field strictness did not change the worker interface ABI hash.' >&2
    exit 1
fi
cp OpenConstructorWorker.hi ../exporter/
cd ../exporter
expect_rejection OpenConstructorSource 9 '\[GHC-70566\]' 'ordinary boxed constructors with lazy fields'
