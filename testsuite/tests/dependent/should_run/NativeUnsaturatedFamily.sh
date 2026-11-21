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
mkdir producer gateway client

"${ghc[@]}" -c NativeUnsaturatedEta.hs
if "${ghc[@]}" -c NativeUnsaturatedInverse.hs >inverse.log 2>&1 ||
   ! grep -Fq '[GHC-25897]' inverse.log; then
    cat inverse.log
    exit 1
fi

cat >producer/PatternMatchability.hs <<'HS'
{-# LANGUAGE DataKinds, ExplicitForAll, FlexibleInstances, Modifiers, MultiParamTypeClasses #-}
{-# LANGUAGE NoExperimentalDependentTypes, PolyKinds, TypeApplications, TypeFamilies #-}
{-# LANGUAGE TypeOperators, UndecidableInstances #-}
module PatternMatchability where

import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (M, Matchability, U)

type family UnApp a where
  UnApp (f x) = x

matchable :: UnApp (Maybe Bool) :~: Bool
matchable = Refl

type family Flip (m :: Matchability) where
  Flip U = M
  Flip M = U

type family Initial (flag :: Bool) :: Matchability where
  Initial flag = U

data Box (flag :: Bool) (f :: Type %(Flip (Initial flag)) -> Type)

class C (flag :: Bool) box t where
  tag :: Proxy flag -> Proxy box -> Proxy t -> Bool

instance forall flag (f :: Type %(Flip (Initial flag)) -> Type) x.
         C flag (Box flag f) (f x) where
  tag _ _ _ = True

local :: Bool
local = tag (Proxy @'True) (Proxy @(Box 'True Maybe)) (Proxy @(Maybe Bool))

type family Loop (flag :: Bool) :: Matchability where
  Loop flag = Loop flag

class Unused t
instance forall (unused :: Type %(Loop 'True) -> Type). Unused Int
HS

cat >client/PatternClient.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeApplications #-}
module Main where

import Data.Proxy (Proxy(Proxy))
import PatternMatchability (Box, local, tag)

main :: IO ()
main = print (local, tag (Proxy @'False) (Proxy @(Box 'False Maybe)) (Proxy @(Maybe Bool)))
HS
for opt in -O0 -O2; do
    (cd producer && "${ghc[@]}" "$opt" "${stripped[@]}" -fforce-recomp -freduction-depth=8 -c PatternMatchability.hs)
    cp producer/PatternMatchability.hi client/
    (
        cd client
        "${ghc[@]}" "$opt" -fforce-recomp -c PatternClient.hs
        "${ghc[@]}" "$opt" PatternClient.o ../producer/PatternMatchability.o -o matchable-pattern
        test "$(./matchable-pattern)" = '(True,True)'
    )
done

cat >UnmatchablePattern.hs <<'HS'
{-# LANGUAGE DataKinds, ExplicitForAll, FlexibleInstances, Modifiers, PolyKinds, TypeFamilies #-}
module UnmatchablePattern where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import GHC.Matchability (M, Matchability, U)

class C t
instance forall (f :: Type %U -> Type) x. C (f x)

type family ToU (m :: Matchability) where
  ToU M = U

class Computed t
instance forall (f :: Type %(ToU M) -> Type) x. Computed (f x)

type family UnApp p q r where
  forall (f :: Type %U -> Type) x. UnApp (Proxy f) (Proxy x) (f x) = x
HS
cat >RigidPattern.hs <<'HS'
{-# LANGUAGE ExplicitForAll, FlexibleInstances, Modifiers, PolyKinds, TypeFamilies #-}
module RigidPattern where

import Data.Kind (Type)
import GHC.Matchability (M, Matchability)

class C t
instance forall (m :: Matchability) (f :: Type %m -> Type) x. (m ~ M) => C (f x)
HS
for flag in -XExperimentalDependentTypes -XNoExperimentalDependentTypes; do
    for defer in -fno-defer-type-errors -fdefer-type-errors; do
        if "${ghc[@]}" "$flag" "$defer" -c UnmatchablePattern.hs >pattern.log 2>&1 ||
           [[ $(grep -Fc 'error: [GHC-83865]' pattern.log) != 3 ]] ||
           ! grep -Fq 'Unmatchable' pattern.log || ! grep -Fq 'Matchable' pattern.log ||
           [[ -e UnmatchablePattern.hi ]]; then
            cat pattern.log >&2
            exit 1
        fi
        if "${ghc[@]}" "$flag" "$defer" -c RigidPattern.hs >pattern.log 2>&1 ||
           ! grep -Fq 'error: [GHC-25897]' pattern.log || [[ -e RigidPattern.hi ]]; then
            cat pattern.log >&2
            exit 1
        fi
    done
done

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

# Compile the ordinary re-export against the provider interface.
cp producer/NativeUnsaturatedProvider.hi gateway/
cat >gateway/NativeUnsaturatedReexport.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module NativeUnsaturatedReexport (module P) where
import NativeUnsaturatedProvider as P
HS
(cd gateway && "${ghc[@]}" -c NativeUnsaturatedReexport.hs)
cp gateway/NativeUnsaturatedReexport.hi client/
sed -e 's/^module NativeUnsaturatedClient/module GatewayClient/' \
    -e 's/^import NativeUnsaturatedProvider /import NativeUnsaturatedReexport /' \
    NativeUnsaturatedClient.hs >client/GatewayClient.hs
(cd client && "${ghc[@]}" -c GatewayClient.hs)

(
    cd client
    for provider in NativeUnsaturatedProvider NativeUnsaturatedReexport; do
        # Use the original family at the same unmatchable input as the alias.
        cat >RawPrefix.hs <<EOF
{-# LANGUAGE DataKinds, TypeApplications, TypeOperators #-}
module RawPrefix where
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (FUN, M, U, Multiplicity(Many))
import $provider (Select)
type Apply :: FUN @U 'Many Bool Bool -> Bool
type Apply f = f 'True
proof :: Apply (Select 'True) :~: 'True
proof = Refl
EOF
        if "${ghc[@]}" -XNoExperimentalDependentTypes -c RawPrefix.hs >raw.log 2>&1 ||
           ! grep -Fq '[GHC-27346]' raw.log ||
           ! grep -Fq 'ExperimentalDependentTypes' raw.log; then
            cat raw.log
            exit 1
        fi
        "${ghc[@]}" -XExperimentalDependentTypes -c RawPrefix.hs
    done

    # Opting in does not let a partial family fill a matchable arrow.
    sed -e 's/^module RawPrefix/module WrongMatchability/' \
        -e 's/FUN @U/FUN @M/' RawPrefix.hs >WrongMatchability.hs
    for mode in NoExperimentalDependentTypes ExperimentalDependentTypes; do
        if "${ghc[@]}" -X"$mode" -c WrongMatchability.hs >bad.log 2>&1 ||
           ! grep -Fq '[GHC-83865]' bad.log ||
           grep -Fq ExperimentalDependentTypes bad.log; then
            cat bad.log
            exit 1
        fi
    done

    # The extension cannot enable a partial built-in family or make a family
    # application legal in an instance head.
    cat >BadArity.hs <<'EOF'
{-# LANGUAGE DataKinds #-}
module BadArity where
import Data.Proxy
import GHC.TypeNats (CmpNat)
bad :: Proxy (CmpNat 1)
bad = Proxy
EOF
    cat >BadPattern.hs <<'EOF'
{-# LANGUAGE DataKinds, FlexibleInstances, TypeApplications #-}
module BadPattern where
import GHC.Exts (FUN, U, Multiplicity(Many))
import NativeUnsaturatedReexport (Select)
class C (f :: FUN @U 'Many Bool Bool)
instance C (Select 'True)
EOF
    for file in BadArity BadPattern; do
        if "${ghc[@]}" -XNoExperimentalDependentTypes -c "$file.hs" >bad.log 2>&1 ||
           ! grep -Fq '[GHC-27346]' bad.log ||
           grep -Fq ExperimentalDependentTypes bad.log; then
            cat bad.log
            exit 1
        fi
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
