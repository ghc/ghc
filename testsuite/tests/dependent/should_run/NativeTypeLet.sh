#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,client}
g=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
   -fdiagnostics-color=never)

cd "$out/provider"
cp "$src/"{PackedVecLibrary,TypeLet}.hs .
"${g[@]}" -c PackedVecLibrary.hs
"${g[@]}" -c TypeLet.hs
cat >SignatureLet.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module SignatureLet (capture) where
capture :: let { saved = a; a = Bool } in saved -> a -> saved
capture saved _ = saved
HS
"${g[@]}" -fomit-interface-pragmas -c SignatureLet.hs
cat >SignedLets.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
module SignedLets (Braced, Layout) where

type Braced (b :: Bool) = let { tag = Bool; tag :: tag; tag = b } in tag

type Layout (b :: Bool) =
  let tag = Bool
      tag :: tag
      tag = b
  in tag
HS
"${g[@]}" -fomit-interface-pragmas -ddump-parsed -c SignedLets.hs >parsed.txt
{
  head -n 1 SignedLets.hs
  sed -n '/^module /,$p' parsed.txt
} >Pretty.hs
test "$(grep -c 'tag :: tag' Pretty.hs)" = 2
"${g[@]}" -fforce-recomp -fno-code -c Pretty.hs
cp TypeLet.hi PackedVecLibrary.hi SignatureLet.hi SignedLets.hi "$out/client/"
cat >PairBindings.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ExplicitNamespaces, KindSignatures, TypeFamilies, TypeOperators #-}
module PairBindings (Count, AliasCount, Held, KnownPrefix, Rebuild, ShadowCount) where

import GHC.TypeNats (Nat, type (+))

type Count (dimensions :: (Nat, Nat)) =
  let (rows, cols) = dimensions
  in rows + cols

type Dimensions = '(2, 3)
type Alias = Dimensions
type AliasCount = let { (rows, _) = Alias; cols = 3 } in rows + cols

type family OpaqueField (n :: Nat) :: Bool
type KnownPrefix (n :: Nat) = let { (rows, _) = '(2, OpaqueField n) } in rows

type Held (dimensions :: (Nat, Nat)) = let { (_, _) = dimensions } in Bool
type Rebuild (dimensions :: (Nat, Nat)) = let { (rows, cols) = dimensions } in '(rows, cols)

type ShadowCount (rows :: Nat) =
  let dimensions = '(rows, 3)
      (rows, cols) = dimensions
  in rows + cols
HS
"${g[@]}" -fomit-interface-pragmas -ddump-parsed -c PairBindings.hs >pair-parsed.txt
{
  head -n 1 PairBindings.hs
  sed -n '/^module /,$p' pair-parsed.txt
} >PrettyPair.hs
"${g[@]}" -fforce-recomp -fno-code -c PrettyPair.hs
cp PairBindings.hi "$out/client/"
cd "$out/client"
cat >OrdinaryLet.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module OrdinaryLet where
import Data.Type.Equality ((:~:)(Refl))
import Data.Proxy (Proxy)
import PairBindings (AliasCount, Count, Held, KnownPrefix, ShadowCount)
import qualified SignatureLet
import SignedLets (Braced, Layout)
import TypeLet

roadmap :: F Bool :~: Ordering
roadmap = Refl

shadow :: Shadow 'False :~: Ordering
shadow = Refl

outer :: Outer 'False :~: Char
outer = Refl

reuse :: Reuse 'True :~: '( 'True, 'True)
reuse = Refl

nested :: Nested 'False :~: '( 'False, 'False)
nested = Refl

capture :: CaptureBeforeShadow 'False :~: '( 'False, 'True)
capture = Refl

forward :: a -> Bool -> a
forward = SignatureLet.capture

braced :: Braced 'True :~: 'True
braced = Refl

layout :: Layout 'False :~: 'False
layout = Refl

knownPair :: Count '(2, 3) :~: 5
knownPair = Refl

aliasPair :: AliasCount :~: 5
aliasPair = Refl

opaqueField :: Proxy n -> KnownPrefix n :~: 2
opaqueField _ = Refl

heldPair :: Proxy dimensions -> Held dimensions :~: Bool
heldPair _ = Refl

shadowPair :: ShadowCount 2 :~: 5
shadowPair = Refl

yes :: CaseResult 'True :~: Either Bool Bool
yes = Refl

no :: CaseResult 'False :~: Either Char Char
no = Refl
HS
"${g[@]}" -c OrdinaryLet.hs

reject() {
    local file=$1 location=$2 code=$3 message=$4 status=0
    "${g[@]}" -c "$file.hs" >error.log 2>&1 || status=$?
    if [[ $status != 1 ]] ||
       ! grep -Fq "$file.hs:$location: error: [GHC-$code]" error.log ||
       ! grep -Fq "$message" error.log; then
        cat error.log >&2
        exit 1
    fi
}

cat >UnusedCase.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies #-}
module UnusedCase where
type Bad = let ignored = case 'True of { True -> Maybe 'True; False -> Int } in Bool
HS
reject UnusedCase 3:56 83865 'In the first argument of'

cat >RigidCase.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module RigidCase where
import Data.Proxy
import Data.Type.Equality
import TypeLet
bad :: Proxy b -> CaseResult b :~: Either Bool Bool
bad _ = Refl
HS
reject RigidCase 7:9 83865 'Refl'

cat >Recursive.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Recursive where
type Bad = let b = b in Maybe b
HS
reject Recursive 3:20 76037 'Not in scope: type variable'

cat >Forward.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Forward where
type Bad = let { a = b; b = Bool } in Maybe a
HS
reject Forward 3:22 76037 'Not in scope: type variable'

cat >Disabled.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Disabled where
type Bad = let b = Bool in Maybe b
HS
reject Disabled 3:12 29822 'requires -XExperimentalDependentTypes.'
grep -Eq '^[[:space:]]*\|[[:space:]]*\^\^\^$' error.log

cat >DisabledGroup.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module DisabledGroup where
type Bad = let { a = Bool; b = Maybe a } in b
HS
reject DisabledGroup 3:12 29822 'requires -XExperimentalDependentTypes.'
grep -Eq '^[[:space:]]*\|[[:space:]]*\^\^\^$' error.log

cat >UnusedInvalid.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module UnusedInvalid where
type Bad (a :: Bool) = let b = Maybe a in Int
HS
reject UnusedInvalid 3:38 83865 'Expected a type'

cat >UnusedGroup.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module UnusedGroup where
type Bad (a :: Bool) = let { b = a; ignored = Maybe b } in Int
HS
reject UnusedGroup 3:53 83865 'Expected a type'

cat >SignedKind.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
module SignedKind where
type Bad = let { width :: Bool; width = 0 } in Int
HS
reject SignedKind 3:41 83865 'Expected kind'

cat >SignedName.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
module SignedName where
type Bad = let { width :: Bool; height = 'True } in height
HS
reject SignedName 3:18 58217 'must immediately precede its binding'

cat >SignedForward.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
module SignedForward where
type Bad = let { width :: later; width = 'True; later = Bool } in width
HS
reject SignedForward 3:27 76037 'Not in scope: type variable'

cat >PairDuplicate.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module PairDuplicate where
type Bad = let (x, x) = '(Bool, Char) in x
HS
reject PairDuplicate 3:20 50547 'twice'

cat >IgnoredNonPair.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module IgnoredNonPair where
type Bad = let (_, _) = Bool in Int
HS
reject IgnoredNonPair 3:25 83865 'Expected kind'

cat >PairEta.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module PairEta where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import PairBindings (Rebuild)

bad :: Proxy dimensions -> Rebuild dimensions :~: dimensions
bad _ = Refl
HS
reject PairEta 8:9 83865 'Refl'

# The infix operator can be a captured type variable.
cd "$out/provider"
cat > Operator.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, LinearTypes, StarIsType, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-star-is-type #-}
module Operator where
import GHC.Exts (Multiplicity)
type Result (f :: * -> * -> *) a b flag =
    let field = a `f` b in case flag of { True -> Maybe field; False -> [field] }
type Prefix f a b flag =
    let field = f a b in case flag of { True -> Maybe field; False -> [field] }
type Handler (m :: Multiplicity) a b flag =
    let callback = a %m -> b in case flag of { True -> Maybe callback; False -> [callback] }
HS
"${g[@]}" -fomit-interface-pragmas -c Operator.hs
cp Operator.hi "$out/client/"
cd "$out/client"
cat > Main.hs <<'HS'
{-# LANGUAGE DataKinds, LinearTypes, NoExperimentalDependentTypes, TypeApplications, TypeOperators #-}
module Main where
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (Multiplicity(..))
import Operator (Handler, Prefix, Result)
import Data.Proxy (Proxy)
import PackedVecLibrary (Nat(..), Vec(..))
import TypeLet (Frame, frameRow, frameToList)

longer :: Proxy n -> Frame n 'True Char :~: Vec Char ('S n)
longer _ = Refl
shorter :: Proxy n -> Frame n 'False Char :~: Vec Char n
shorter _ = Refl
captured :: Result Either Char Bool 'True :~: Maybe (Either Char Bool)
captured = Refl
second :: Result (,) Char Bool 'False :~: [(Char, Bool)]
second = Refl
inferred :: Prefix Either Int Char 'True :~: Maybe (Either Int Char)
inferred = Refl
linear :: Handler 'One Char Bool 'True :~: Maybe (Char %1 -> Bool)
linear = Refl
many :: Handler 'Many Char Bool 'False :~: [Char -> Bool]
many = Refl

main :: IO ()
main = do
    let row = VCons 'x' (VCons 'y' VNil)
        render flag = frameToList @Char flag 2
            (frameRow flag 2 (if flag then ':' else error "unused header") row)
    print (render True, render False)
HS
for opt in -O0 -O2; do
    "${g[@]}" "$opt" -fforce-recomp -c Main.hs
    "${g[@]}" Main.o ../provider/{Operator,TypeLet,PackedVecLibrary}.o -o frame
    ./frame | grep -Fxq '(":xy","xy")'
done
cat >WrongWidth.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module WrongWidth where
import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Nat(..), Vec)
import TypeLet (Frame)
wrong :: Frame 'Z 'True Char :~: Vec Char 'Z
wrong = Refl
HS
if "${g[@]}" -c WrongWidth.hs >error.log 2>&1; then exit 1; fi
grep -Fq '[GHC-83865]' error.log
grep -Fq 'Refl' error.log
