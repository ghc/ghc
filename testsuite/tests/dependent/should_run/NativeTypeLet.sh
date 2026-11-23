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
"${g[@]}" -fno-code "$src/NativeTypeLetContexts.hs"
cat >SignatureLet.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeApplications #-}
module SignatureLet (capture, selected) where
import Data.Proxy (Proxy(..))

capture :: let { saved = a; a = Bool } in saved -> a -> saved
capture saved _ = saved

selected :: Proxy (let seed = 'True
                       first = case seed of { True -> Bool; False -> Char }
                       saved = first
                       result = case 'False of { True -> Int; False -> saved }
                   in result)
selected = Proxy @Bool
HS
"${g[@]}" -fomit-interface-pragmas -c SignatureLet.hs
cat >SignedLets.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
module SignedLets (Braced, Layout, CaseChain) where

type Braced (b :: Bool) = let { tag = Bool; tag :: tag; tag = b } in tag

type Layout (b :: Bool) =
  let tag = Bool
      tag :: tag
      tag = b
  in tag

type CaseChain (b :: Bool) =
  let first = case b of { True -> 'False; False -> 'True }
      second = case first of { True -> 'True; False -> 'False }
  in '(first, second)
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
module PairBindings (AliasCount, Held, KnownPrefix, Rebuild, ShadowCount,
                     NestedCount, NestedRight, NestedHeight,
                     Axis(..), AxisLength, Volume(..), VolumeRows, VolumeFields,
                     Grid(..), GridTotal, GridRows, GridHeld, OpaqueGrid, OpaqueColumn) where

import GHC.TypeNats (Nat, type (+))

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

type NestedCount (width :: Nat) =
  let ((width, _), height) = '( '(width, 'True), 3)
  in width + height

type NestedRight (height :: Nat) =
  let { (width, (_, height)) = '(2, '( 'True, height)) }
  in width + height

type family OpaquePair (n :: Nat) :: (Nat, Bool)
type NestedHeight (n :: Nat) =
  let ((_, _), height) = '(OpaquePair n, 3)
  in height

data Grid = Grid Nat Nat
data Axis = Axis Nat
data Volume = Volume Nat Bool Nat

type AxisLength (axis :: Axis) = let Axis len = axis in len
type VolumeRows (volume :: Volume) = let Volume rows _ _ = volume in rows
type VolumeFields (volume :: Volume) =
  let Volume rows solid depth = volume
  in '(rows, solid, depth)

type GridTotal (dimensions :: Grid) =
  let Grid rows cols = dimensions
  in rows + cols

type GridRows (dimensions :: Grid) =
  let Grid rows _ = dimensions
  in rows

type family OpaqueGrid :: Grid
type family OpaqueColumn :: Nat
type GridHeld = let Grid _ _ = OpaqueGrid in Bool
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
import Data.Proxy (Proxy(..))
import PairBindings (AliasCount, Held, KnownPrefix, ShadowCount,
                     NestedCount, NestedRight, NestedHeight,
                     Axis(..), AxisLength, Volume(..), VolumeRows, VolumeFields,
                     Grid(..), GridTotal, GridRows, GridHeld, OpaqueGrid, OpaqueColumn)
import qualified SignatureLet
import SignedLets (Braced, Layout, CaseChain)
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

signatureCase :: Proxy Bool
signatureCase = SignatureLet.selected

braced :: Braced 'True :~: 'True
braced = Refl

layout :: Layout 'False :~: 'False
layout = Refl

caseChain :: CaseChain 'True :~: '( 'False, 'False)
caseChain = Refl

knownPair :: DimensionSum '(2, 3) :~: 5
knownPair = Refl

chosenWidth :: DimensionWithOverride 'False 'Nothing :~: 2
chosenWidth = Refl

inlineDefaultWidth :: Proxy 3
inlineDefaultWidth = defaultWidth

explicitWidth :: Proxy width -> DimensionWithOverride width ('Just 7) :~: 7
explicitWidth _ = Refl

knownProduct :: GridTotal ('Grid 2 3) :~: 5
knownProduct = Refl

oneField :: AxisLength ('Axis 2) :~: 2
oneField = Refl

threeFields :: VolumeRows ('Volume 2 'False OpaqueColumn) :~: 2
threeFields = Refl

allFields :: VolumeFields ('Volume 2 'True 4) :~: '(2, 'True, 4)
allFields = Refl

lazyProduct :: GridRows ('Grid 2 OpaqueColumn) :~: 2
lazyProduct = Refl

unknownProduct :: Proxy (GridRows OpaqueGrid)
unknownProduct = Proxy

unusedProduct :: GridHeld :~: Bool
unusedProduct = Refl

nestedLeftPair :: NestedCount 7 :~: 10
nestedLeftPair = Refl

nestedRightPair :: NestedRight 3 :~: 5
nestedRightPair = Refl

nestedUnusedPair :: Proxy n -> NestedHeight n :~: 3
nestedUnusedPair _ = Refl

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

cat >UnknownWidth.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module UnknownWidth where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import TypeLet (DimensionWithOverride)
unknown :: Proxy width -> DimensionWithOverride width 'Nothing :~: 2
unknown _ = Refl
HS

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

reject UnknownWidth 7:13 83865 'Refl'

cat >RefutableProduct.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module RefutableProduct where
import GHC.TypeNats (Nat)
data Choice = Some Nat Nat | None
type Bad = let Some _ _ = 'Some 2 3 in Bool
HS
status=0
"${g[@]}" -c RefutableProduct.hs >error.log 2>&1 || status=$?
test "$status" = 1
grep -Fq 'must be the sole constructor of a regular datatype' error.log

cat >WrongProductArity.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module WrongProductArity where
import PairBindings (Grid(..))
type Bad = let Grid _ = 'Grid 2 3 in Bool
HS
status=0
"${g[@]}" -c WrongProductArity.hs >error.log 2>&1 || status=$?
test "$status" = 1
grep -Fq 'Expected 2, found 1.' error.log

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

cat >NestedPairDuplicate.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module NestedPairDuplicate where
type Bad = let ((x, _), x) = '( '(Bool, Char), Int) in x
HS
reject NestedPairDuplicate 3:25 50547 'twice'

cat >IgnoredNonPair.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module IgnoredNonPair where
type Bad = let (_, _) = Bool in Int
HS
reject IgnoredNonPair 3:25 83865 "but \`Bool' has kind"
grep -Eq '^[[:space:]]*\|[[:space:]]*\^{4}$' error.log
grep -Fq "In the type \`let (_, _) = Bool in Int'" error.log

cat >BadPairRhs.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module BadPairRhs where
type Bad = let (value, _) = '(Maybe 'True, 'False) in value
HS
reject BadPairRhs 3:37 83865 "In the first argument of \`Maybe', namely 'True"
grep -Fq "In the type \`let (value, _) =" error.log

cat >BadPairOperand.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module BadPairOperand where
type Bad = let (flag, _) = '( 'True, 'False) in Maybe flag
HS
reject BadPairOperand 3:55 83865 "In the first argument of \`Maybe', namely \`flag'"
grep -Fq "In the type \`let (flag, _) =" error.log

cat >NestedWrongKind.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
module NestedWrongKind where
import GHC.TypeNats (Nat)

type Bad (dims :: (Bool, Nat)) =
  let ((_, _), height) = dims
  in height
HS
reject NestedWrongKind 6:8 83865 'Expected kind'
grep -Fq "has kind \`Bool'" error.log
grep -Eq '^[[:space:]]*\|[[:space:]]*\^{6}$' error.log
grep -Fq "In the type \`let ((_, _), height) = dims in height'" error.log

cat >PairEta.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module PairEta where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import PairBindings (Rebuild)

bad :: Proxy dimensions -> Rebuild dimensions :~: dimensions
bad _ = Refl
HS
reject PairEta 8:9 25897 'Refl'

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
