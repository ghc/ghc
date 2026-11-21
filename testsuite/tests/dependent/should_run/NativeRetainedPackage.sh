#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,projection,client}
g=("$1" "-B$2" -v0 -package-env - -O0 -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
g+=("$@")
cd "$out/provider"
cp "$src"/{PackedVecLibrary,RetainedNat,PackedLength,RetainedWitness,DependentPair}.hs .
"${g[@]}" -fomit-interface-pragmas -c PackedVecLibrary.hs RetainedNat.hs PackedLength.hs RetainedWitness.hs
"${g[@]}" -O1 -fomit-interface-pragmas -fno-write-if-simplified-core -c DependentPair.hs
cp DependentPair.hi "$out/client/"
cat >IndexedCapture.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, NoStandaloneKindSignatures, PolyKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
module IndexedCapture (captured) where
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import PackedVecLibrary (Add, Nat(..))

type Keep (n :: Nat) (m :: Nat) (x :: Proxy (Add n m)) =
    some (tag :: Bool) | (x :~: x)

captured :: Keep 'Z ('S 'Z) 'Proxy
captured = Pair True Refl
HS
"${g[@]}" -fomit-interface-pragmas -Werror=unused-foralls -c IndexedCapture.hs
cp IndexedCapture.hi "$out/client/"
cp {PackedVecLibrary,PackedLength,RetainedNat,RetainedWitness,DependentPair}.hi "$src"/{PackedProjection,RetainedProjection,DependentProjection}.hs "$out/projection/"
cd "$out/projection"
"${g[@]}" -fomit-interface-pragmas -c PackedProjection.hs RetainedProjection.hs DependentProjection.hs
cp DependentProjection.hi "$out/client/"
cat >Mixed.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies #-}
module Mixed (witness) where
import Data.Proxy
import PackedProjection
import PackedVecLibrary
type family Payload (n :: Nat) :: Vec Bool n
witness :: foreach (n :: Nat) -> Proxy (Length ('PackVec n (Payload n)))
witness _ = Proxy
HS
"${g[@]}" -fomit-interface-pragmas -c Mixed.hs
cp {PackedVecLibrary,RetainedNat,PackedLength,RetainedWitness,PackedProjection,RetainedProjection,Mixed}.hi "$out/client/"
cd "$out/client"
cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module Main where
import Data.Functor.Const (Const(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import IndexedCapture (captured)
import PackedVecLibrary
import RetainedNat
import RetainedProjection (project)

main :: IO ()
main = do
  print (withRetained (constantPackage (error "unused witness") (7 :: Int))
                  (\_ box -> getConst box),
              withRetained (constantPackage Z (error "unused payload" :: Int))
                  (\n _ -> natToInt n))
  let q = constantPackage (error "unused projected witness") (7 :: Int)
  print (getConst (project q))
  print (withRetained q (\_ box -> case box of Const x -> x))
  case captured of
    Pair True Refl -> pure ()
    Pair False _ -> fail "unexpected captured witness"
HS
"${g[@]}" -c Main.hs
"${g[@]}" Main.o ../projection/RetainedProjection.o ../provider/{IndexedCapture,PackedVecLibrary,RetainedNat,RetainedWitness}.o -o run
./run
cat >General.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds #-}
module General where
import Data.Functor.Const
import DependentPair
import DependentProjection

data Flag b where
  Yes :: Flag 'True
  No  :: Flag 'False

pack :: a -> r -> DPair a (Const r)
pack x value = Pair x (Const value)

readInt :: DPair Int (Const Int) -> Int
readInt (Pair _ (Const value)) = value

readFlag :: DPair Bool Flag -> Bool
readFlag q = case second q of
  Yes -> True
  No  -> False

type AsWritten t = t

-- Name a retained consumer through an ordinary synonym.
readTag :: AsWritten (Consumer a (Const Char) Char)
readTag _ c = getConst c

readAction :: Consumer (IO Int) (Const Char) Char
readAction action tag = readTag action tag

value :: DPair a (Const r) -> r
value q = getConst (second q)

main :: IO ()
main = print (readInt (pack (error "unused integer witness") 7),
              withPair (pack (error "unused IO" :: IO Int) 'i') readAction,
              value (pack (error "unused function" :: Int -> Int) 'f'),
              first (pack (12 :: Int) (error "unused payload" :: Char)),
              readFlag (Pair True Yes), readFlag (Pair False No))
HS
"${g[@]}" -main-is General -c General.hs
"${g[@]}" -main-is General General.o ../provider/DependentPair.o ../projection/DependentProjection.o -o general
./general
cat >AmbiguousConsumer.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, TypeFamilies #-}
module AmbiguousConsumer where
import Data.Functor.Const (Const(..))
import Data.Kind (Type)
import DependentPair (Consumer)
type family Hidden a :: Type
bad :: Consumer (Hidden a) (Const Bool) Bool
bad _ (Const b) = b
HS
if "${g[@]}" -c AmbiguousConsumer.hs >bad.log 2>&1 ||
   ! grep -Fq "In the ambiguity check for" bad.log; then
    cat bad.log >&2
    exit 1
fi
# An inferred result must be independent of the runtime argument.
cat >EscapeCallback.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, PolyKinds #-}
module EscapeCallback where
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
type Body (a :: Type) r = foreach (x :: a) -> Proxy x -> r
type Indexed (a :: Type) = foreach (x :: a) -> Proxy x -> Proxy x
indexed :: Indexed (IO Int)
indexed _ value = value
takeBody :: Body (IO Int) r -> Proxy r
takeBody _ = Proxy
leak = takeBody indexed
HS
if "${g[@]}" -c EscapeCallback.hs >escape-callback.log 2>&1 ||
   ! grep -Fq 'EscapeCallback.hs:11:17: error: [GHC-83865]' escape-callback.log; then
    cat escape-callback.log >&2
    exit 1
fi
cat >StrictRetainedField.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module StrictRetainedField where

import Data.Functor.Const (Const)
import DependentPair (DPair)

data Holder = Holder !(DPair Bool (Const Int))
HS
"${g[@]}" -O1 -c StrictRetainedField.hs
(
  cd "$out/provider"
  cat >SelectorProvider.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module SelectorProvider where

data Flag (b :: Bool) where
  Yes :: Int -> Flag 'True
  No :: Flag 'False

type Package = some (b :: Bool) | Flag b
data Aliased = Aliased { getAlias :: Package }
data Direct = Direct { getDirect :: some (b :: Bool) | Flag b }
HS
  "${g[@]}" -O1 -c SelectorProvider.hs
  cp SelectorProvider.hi "$out/client/"
)
cat >SelectorClient.hs <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module SelectorClient where

import GHC.Sigma.Projection (second)
import SelectorProvider

readFlag :: Flag b -> Int
readFlag (Yes n) = n
readFlag No = 0

viaAlias :: Aliased -> Int
viaAlias box = let pair = getAlias box in readFlag (second pair)

viaDirect :: Direct -> Int
viaDirect box = let pair = getDirect box in readFlag (second pair)
HS
"${g[@]}" -O1 -c SelectorClient.hs
cat >WrongPair.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module WrongPair where
import DependentPair
import General (Flag(..))
wrong :: DPair Bool Flag
wrong = Pair True No
HS
if "${g[@]}" -c WrongPair.hs >bad.log 2>&1 || ! grep -Fq 'WrongPair.hs:6:19: error: [GHC-83865]' bad.log; then
    cat bad.log >&2
    exit 1
fi
cat >EscapePair.hs <<'HS'
{-# LANGUAGE PolyKinds, NoExperimentalDependentTypes #-}
module EscapePair where
import DependentPair
escape :: DPair a p -> p w
escape (Pair _ value) = value
HS
if "${g[@]}" -c EscapePair.hs >bad.log 2>&1 || ! grep -Fq 'EscapePair.hs:5:25: error:' bad.log; then
    cat bad.log >&2
    exit 1
fi
# A stricter first projection cannot supply the equation used by second.
(
    mkdir strict-first
    cd strict-first
    sed 's/first (Pair x _) = x/first (Pair x !_) = x/' "$src/DependentPair.hs" >DependentPair.hs
    "${g[@]}" -XBangPatterns -fomit-interface-pragmas -c DependentPair.hs
    cp "$src/DependentProjection.hs" .
    if "${g[@]}" -c DependentProjection.hs >strict.log 2>&1 ||
       ! grep -Fq '[GHC-05617]' strict.log || ! grep -Fq 'first (Pair' strict.log; then
        cat strict.log >&2
        exit 1
    fi
)
cat >Escape.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module Escape where
import PackedVecLibrary
import RetainedNat

newtype Box (n :: Nat) = Box { contents :: Vec Int n }
escape :: Retained Box -> Vec Int m
escape p = withRetained p (\_ box -> contents box)
HS
if "${g[@]}" -c Escape.hs >bad.log 2>&1 || ! grep -Fq 'Escape.hs:8:' bad.log ||
   ! grep -Fq 'Vec Int' bad.log; then
    cat bad.log >&2
    exit 1
fi

sed 's/contents box/case box of Box xs -> xs/' Escape.hs >EscapePattern.hs
if "${g[@]}" -c EscapePattern.hs >bad.log 2>&1 ||
   ! grep -Fq 'EscapePattern.hs:8:' bad.log || ! grep -Fq 'Vec Int' bad.log; then
    cat bad.log >&2
    exit 1
fi

cat >Projection.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeOperators #-}
module Projection where
import Data.Type.Equality
import PackedProjection
import PackedVecLibrary

one :: Length One :~: 'S 'Z
one = Refl
zero :: Length ('PackVec 'Z ('VNil :: Vec Bool 'Z)) :~: 'Z
zero = Refl
HS
"${g[@]}" -c Projection.hs
cat >WrongIndex.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module WrongIndex where
import PackedVecLibrary
type Bad = 'PackVec 'Z ('VCons 'True 'VNil)
HS
if "${g[@]}" -c WrongIndex.hs >bad.log 2>&1 || ! grep -Fq 'WrongIndex.hs:4:' bad.log ||
   ! grep -Fq '[GHC-83865]' bad.log; then
    cat bad.log >&2
    exit 1
fi

cat >MixedClient.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module MixedClient where
import Data.Proxy
import Mixed
import PackedVecLibrary
readWitness :: foreach (n :: Nat) -> Proxy n
readWitness n = witness n
HS
"${g[@]}" -c MixedClient.hs

cat >RuntimeProjection.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module Main where
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isPrefixOf)
import PackedProjection
import PackedVecLibrary

consume :: PackedVec a -> [a]
consume p = toList (payload p)

fields :: Nat -> PackedVec Int
fields n = PackVec n (error "payload")

main :: IO ()
main = do
    case consume (packList [7 :: Int]) of
      [7] -> pure ()
      _ -> fail "wrong vector"
    outcome <- try (evaluate (consume (fields (error "witness"))))
    case outcome of
      Left e | "payload" `isPrefixOf` displayException (e :: ErrorCall) ->
        putStrLn "the projected payload leaves the witness lazy"
      _ -> fail "wrong field evaluated"
HS
"${g[@]}" -c RuntimeProjection.hs
"${g[@]}" RuntimeProjection.o ../projection/PackedProjection.o ../provider/{PackedLength,PackedVecLibrary}.o -o projection
./projection

cat >WrongProjection.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module WrongProjection where
import PackedProjection
import PackedVecLibrary
wrong :: foreach (p :: PackedVec a) -> Vec a ('S (Length p))
wrong (PackVec _ xs) = xs
HS
if "${g[@]}" -c WrongProjection.hs >bad.log 2>&1 ||
   ! grep -Fq 'WrongProjection.hs:6:24: error: [GHC-05617]' bad.log; then
    cat bad.log >&2
    exit 1
fi

cat >AtFamily.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module AtFamily where
import PackedProjection
import PackedVecLibrary
atFamily :: foreach (p :: PackedVec a) -> Vec a (Length p)
atFamily (PackVec _ xs) = xs
HS
"${g[@]}" -c AtFamily.hs

cp "$src/NativePackageOperand.hs" .
"${g[@]}" -c NativePackageOperand.hs
line=$(awk '/^wrong n _ xs/ {print NR}' NativePackageOperand.hs)
if "${g[@]}" -fforce-recomp -DWRONG_WITNESS -c NativePackageOperand.hs >bad.log 2>&1 ||
   ! grep -Fq "NativePackageOperand.hs:$line:" bad.log ||
   ! grep -Fq '[GHC-83865]' bad.log; then
    cat bad.log >&2
    exit 1
fi

reject_family() {
    local flag=$1 anchor=$2 expected=$3
    local line
    line=$(awk -v anchor="$anchor" 'index($0,anchor) == 1 {print NR}' NativePackageOperand.hs)
    if "${g[@]}" -fforce-recomp "-D$flag" -c NativePackageOperand.hs >bad.log 2>&1 ||
       ! grep -Fq "NativePackageOperand.hs:$line:" bad.log ||
       ! grep -Fq "$expected" bad.log; then
        cat bad.log >&2
        exit 1
    fi
}
reject_family WRONG_FAMILY_RESULT 'wrongFamilyResult (Retain' '[GHC-83865]'
reject_family WRONG_FAMILY_KIND 'type WrongFamily =' '[GHC-83865]'
reject_family SHIFTED_WITNESS "    ShiftWitness ('Shift" '[GHC-83605]'

# Keep the dependent source unchanged while the ordinary worker changes.
(
    cd "$out/provider"
    sed 's/witness (Retain n _) = n/witness (Retain n _) = S n/' \
        "$src/RetainedWitness.hs" >RetainedWitness.hs
    "${g[@]}" -fomit-interface-pragmas -c RetainedWitness.hs
    cp RetainedWitness.hi "$out/projection/"
)
(
    cd "$out/projection"
    if "${g[@]}" -fforce-recomp -c RetainedProjection.hs >wrong-worker.log 2>&1 ||
       ! grep -Fq '[GHC-05617]' wrong-worker.log ||
       ! grep -Fq 'project (Retain _ value) = value' wrong-worker.log; then
        cat wrong-worker.log >&2
        exit 1
    fi
)

cd "$out/provider"
cp "$src/"{StoredVector,ConservedPartition}.hs .
"${g[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core -c StoredVector.hs ConservedPartition.hs
cat >VectorIdentity.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds #-}
module VectorIdentity (vectorRefl, forwardVector, zeroRefl, matchZero, reverseZero) where

import Data.Type.Equality ((:~:)(Refl), (:~~:)(HRefl))
import PackedVecLibrary (Nat(..), Vec(..))

vectorRefl :: foreach (xs :: Vec a n) -> xs :~: xs
vectorRefl _ = Refl

forwardVector :: foreach (xs :: Vec a n) -> xs :~: xs
forwardVector xs = vectorRefl xs

zeroRefl :: foreach (xs :: Vec Bool 'Z) -> xs :~: ('VNil :: Vec Bool 'Z)
zeroRefl xs = case xs of
    VNil -> Refl

matchZero :: foreach (xs :: Vec Bool n) -> Maybe (xs :~~: ('VNil :: Vec Bool 'Z))
matchZero xs = case xs of
    VNil -> Just HRefl
    VCons _ _ -> Nothing

#ifdef WRONG_VECTOR
wrongVector :: foreach (xs :: Vec a n) -> foreach (ys :: Vec a n) -> xs :~: ys
wrongVector _ _ = Refl

wrongZero :: foreach (xs :: Vec Bool 'Z) -> foreach (ys :: Vec Bool 'Z) ->
             (xs :~: ('VNil :: Vec Bool 'Z), ys :~: ('VNil :: Vec Bool 'Z))
wrongZero xs _ = case xs of
    VNil -> (Refl, Refl)
#endif

reverseZero :: foreach (xs :: Vec a n) -> a :~: Bool ->
    xs :~~: ('VNil :: Vec a 'Z) -> ('VNil :: Vec Bool 'Z) :~~: xs
reverseZero _ Refl HRefl = HRefl

#ifdef WRONG_VECTOR
crossed :: foreach (xs :: Vec Bool n) -> foreach (ys :: Vec Bool n) ->
    xs :~~: ('VNil :: Vec Bool 'Z) -> ('VNil :: Vec Bool 'Z) :~~: ys
crossed _ _ HRefl = HRefl
#endif
HS
"${g[@]}" -O2 -Werror=inaccessible-code -Werror=overlapping-patterns \
    -fno-omit-interface-pragmas -fno-write-if-simplified-core -c VectorIdentity.hs
"${g[@]}" --show-iface VectorIdentity.hi >vector-identity.iface
grep -Fq 'indexed-constructor-case' vector-identity.iface
if "${g[@]}" -ferror-spans -fforce-recomp -DWRONG_VECTOR -c VectorIdentity.hs >wrong-vector.log 2>&1 ||
   ! grep -Fq 'VectorIdentity.hs:24:19-22: error: [GHC-83865]' wrong-vector.log ||
   ! grep -Fq 'VectorIdentity.hs:29:20-23: error: [GHC-83865]' wrong-vector.log ||
   ! grep -Fq 'VectorIdentity.hs:39:21-25: error: [GHC-83865]' wrong-vector.log; then
    cat wrong-vector.log >&2
    exit 1
fi
cp {StoredVector,ConservedPartition}.hi "$src/ListVectorClient.hs" "$out/projection/"
cd "$out/projection"
"${g[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core -c ListVectorClient.hs
cp {StoredVector,ConservedPartition,ListVectorClient}.hi "$out/client/"
cp "$out/provider/VectorIdentity.hi" "$out/client/"
cd "$out/client"
# A successor-length vector cannot be VNil.
cat >Impossible.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Impossible where
import Data.Type.Equality ((:~~:)(HRefl))
import PackedVecLibrary (Nat(..), Vec(..))
impossible :: foreach (xs :: Vec Bool ('S n)) -> Maybe (xs :~~: ('VNil :: Vec Bool 'Z)) -> Bool
impossible _ proof = case proof of
    Just HRefl -> True
    Nothing -> False
HS
if "${g[@]}" -Werror=inaccessible-code -c Impossible.hs >impossible.log 2>&1 ||
   ! grep -Fq '[GHC-40564]' impossible.log; then
    cat impossible.log >&2
    exit 1
fi
# Allow desugaring and Core Lint to run on the same source.
"${g[@]}" -Wno-inaccessible-code -Wno-overlapping-patterns -c Impossible.hs

cat >ListVectorMain.hs <<'HS'
{-# LANGUAGE DataKinds, ExplicitNamespaces, GADTs, NoExperimentalDependentTypes, RequiredTypeArguments, ScopedTypeVariables, TypeApplications #-}
module Main where
import Control.Exception (ErrorCall, evaluate, try)
import ConservedPartition (Split, branchContents, partitionVec)
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.Maybe (isJust, isNothing)
import Data.Type.Equality ((:~:)(Refl), (:~~:)(HRefl))
import GHC.Sigma (Sigma(Pair))
import ListVectorClient
import PackedVecLibrary (Add, CheckedSplit, Nat(..), Vec(..), appendNative, appendVec, dropVec, replicateNative,
                         splitAtVec, splitAtVecChecked, takeVec, toList)
import StoredVector (ListVector)
import VectorIdentity (forwardVector, matchZero, reverseZero, vectorRefl, zeroRefl)
observe :: ListVector Char -> (Int, String)
observe = contents
empty :: Split Int 'Z
empty = partitionVec (error "unused empty predicate") (VNil :: Vec Int 'Z)
onlyKept, onlyDropped :: Split Int ('S 'Z)
onlyKept = Pair (S Z) (Pair Z (VCons 7 VNil, error "unused dropped partition", error "unused sum proof"))
onlyDropped = Pair Z (Pair (S Z) (error "unused kept partition", VCons 9 VNil, Refl))
-- | Matching the proof refines the original vector's unknown length.
onlyEmpty :: forall n. Vec Bool n -> Maybe (Vec Bool 'Z)
onlyEmpty xs = case matchZero @n xs of
    Just HRefl -> Just xs
    Nothing -> Nothing

vectorProofs :: Vec Int n -> (Bool, Bool)
vectorProofs xs =
    (case vectorRefl xs of Refl -> True, case forwardVector xs of Refl -> True)
rejoin :: Vec a cut -> Vec a suffix -> (Add cut suffix :~: total) -> Vec a total
rejoin left right Refl = appendVec left right
deferProof :: CheckedSplit Bool ('S 'Z) total -> Vec Bool total -> Maybe [Bool]
deferProof (Pair Z ~(_, _, proof)) later = case proof of
    Refl -> case later of
        (next :: Vec Bool ('S 'Z)) -> Just (toList next)
deferProof _ _ = Nothing
ignoreTuple :: CheckedSplit Bool ('S 'Z) total -> Vec Bool total -> Bool
ignoreTuple (Pair _ ~(_, _, _)) _ = True
main :: IO ()
main = do
    print (observe (fromList ""), observe (fromList "ab"),
           take 1 (snd (observe (fromList ('x' : error "unused tail")))),
           fst (observe (fromList [error "unused element" :: Char])),
           fromList (error "unused list" :: String) `seq` True)
    let mixed = regroupObservation (> (2 :: Int)) [3, 1, 4, 2]
        lazy = length (snd (regroupObservation (const True) [error "unused partition element" :: Int]))
        selected :: [(Int, [Int])]
        selected =
            [ choosePartition (type 'Z) True empty branchContents
            , choosePartition (type 'Z) False empty branchContents
            , choosePartition (type ('S 'Z)) True onlyKept branchContents
            , choosePartition (type ('S 'Z)) False onlyDropped branchContents
            ]
        split = case splitAtVec (S Z) (VCons (7 :: Int) (VCons 9 VNil)) of
            (left, right) -> (toList left, toList right)
        zeroVector = splitAtVec Z (error "unused vector" :: Vec Int ('S 'Z))
        zeroElement = splitAtVec Z (VCons (error "unused split element" :: Int) VNil)
        one = splitAtVec (S Z) (VCons (error "unused prefix element" :: Int)
                                     (error "unused suffix" :: Vec Int ('S 'Z)))
        demand = (length (toList (fst zeroVector)), length (toList (snd zeroElement)),
                  length (toList (fst one)))
    let proofs = (vectorProofs (VCons 7 VNil),
                  vectorProofs (error "unused indexed vector" :: Vec Int 'Z))
    let zero = VNil :: Vec Bool 'Z
        nonzero = VCons (error "unused head") (error "unused tail" :: Vec Bool 'Z)
        knownZero = case zeroRefl zero of Refl -> True
        openZero = case matchZero zero of
            Just proof -> case reverseZero zero Refl proof of HRefl -> True
            Nothing -> False
        lazyCons = case matchZero nonzero of
            Nothing -> True
            Just _ -> False
        bottom = error "zero case demands the scrutinee" :: Vec Bool 'Z
    forced <- try (evaluate (case zeroRefl bottom of Refl -> True)) :: IO (Either ErrorCall Bool)
    let emptyResults = isJust (onlyEmpty zero) && isNothing (onlyEmpty nonzero)
        zeroChecks = knownZero && openZero && lazyCons && emptyResults
                  && either (const True) (const False) forced
    case (mixed, lazy, selected, split, demand, proofs, zeroChecks) of
        ((4, [3, 4, 1, 2]), 1, [(0, []), (0, []), (1, [7]), (1, [9])],
         ([7], [9]), (0, 1, 1), ((True, True), (True, True)), True) -> pure ()
        other -> fail (show other)
    cut <- newIORef (S (S Z)) >>= readIORef
    let joined = appendNative @Int cut (S Z) (replicateNative cut 7) (VCons 9 VNil)
        prefix = takeVec @('S 'Z) cut joined
        suffix = dropVec @('S 'Z) cut joined
        takeZero = takeVec Z (error "unused vector at zero cut" :: Vec Int ('S 'Z))
        dropZero = dropVec Z (VCons (9 :: Int) VNil)
        firstCell = take 1 (toList (takeVec @'Z (S (S Z))
                          (VCons 7 (error "unused rest of the prefix"))))
        onlyPrefix = takeVec (S Z) (VCons (7 :: Int) (error "unused suffix" :: Vec Int ('S 'Z)))
        onlySuffix = dropVec (S Z) (VCons (error "unused dropped element") (VCons (9 :: Int) VNil))
    case (toList prefix, toList suffix, toList takeZero, toList dropZero,
          firstCell, toList onlyPrefix, toList onlySuffix) of
        ([7, 7], [9], [], [9], [7], [7], [9]) -> pure ()
        other -> fail (show other)
    cuts <- newIORef [Z, S Z, S (S Z), S (S (S Z))] >>= readIORef
    input <- newIORef [7 :: Int, 9] >>= readIORef
    case (checkedSplitObservation Z ([] :: [Int]), map (`checkedSplitObservation` input) cuts) of
        (Just (0, [], []), [Just (2, [], [7, 9]), Just (1, [7], [9]), Just (0, [7, 9], []), Nothing]) -> pure ()
        other -> fail (show other)
    case fromList input of
        Pair total cells -> case splitAtVecChecked (S Z) total cells of
            Just (Pair _ (left, right, proof))
                | toList (rejoin left right proof) == input -> pure ()
            _ -> fail "checked split did not rejoin at the input length"
    bottomTotal <- newIORef (error "unused total for zero cut" :: Nat) >>= readIORef
    partialTotal <- newIORef (S (error "unused remainder length")) >>= readIORef
    excess <- newIORef (S (error "unused excessive cut tail")) >>= readIORef
    let zero = isJust (splitAtVecChecked Z bottomTotal (replicateNative bottomTotal (7 :: Int)))
        success = isJust (splitAtVecChecked (S Z) partialTotal (replicateNative partialTotal (7 :: Int)))
        unusedVector = isJust (splitAtVecChecked (S Z) (S Z) (error "unused accepted vector" :: Vec Int ('S 'Z)))
        failure = isNothing (splitAtVecChecked excess Z (error "unused rejected vector" :: Vec Int 'Z))
        leftOnly = case splitAtVecChecked (S Z) (S (S Z)) (VCons (7 :: Int) (error "unused suffix")) of
            Just (Pair _ (left, _, _)) -> toList left
            Nothing -> []
        rightOnly = case splitAtVecChecked (S Z) (S (S Z)) (VCons (error "unused prefix") (VCons (9 :: Int) VNil)) of
            Just (Pair _ (_, right, _)) -> toList right
            Nothing -> []
    case (zero, success, unusedVector, failure, leftOnly, rightOnly) of
        (True, True, True, True, [7], [9]) -> pure ()
        other -> fail (show other)
    cell <- newIORef (VCons True VNil)
    let obtainSplit = splitAtVecChecked (S Z) (S Z) <$> readIORef cell
        later = VCons False VNil
    actual <- checkedSplitIO obtainSplit later
    delayed <- fmap (>>= \pair -> deferProof pair later) obtainSplit
    case (actual, delayed) of
        (Just ([True], [False]), Just [False]) -> pure ()
        other -> fail (show other)
    let badTuple = Pair Z (error "split tuple forced") :: CheckedSplit Bool ('S 'Z) ('S 'Z)
        badProof = Pair Z (VCons True VNil, VNil, error "split proof forced")
                   :: CheckedSplit Bool ('S 'Z) ('S 'Z)
        checkBottom message action = do
            result <- try @ErrorCall action
            case result of
                Left err | takeWhile (/= '\n') (displayException err) == message -> pure ()
                _ -> fail ("expected " ++ message)
    if ignoreTuple badTuple later then pure () else fail "lazy tuple did not match"
    checkBottom "split tuple forced" (checkedSplitIO (pure (Just badTuple)) later >>= evaluate)
    checkBottom "split proof forced" (evaluate (deferProof badProof later))
HS
"${g[@]}" -c ListVectorMain.hs
"${g[@]}" ListVectorMain.o ../projection/ListVectorClient.o \
    ../provider/{StoredVector,ConservedPartition,PackedVecLibrary,VectorIdentity}.o -o list-vector
./list-vector
cat >SplitProofScope.hs <<'HS'
{-# LANGUAGE BangPatterns, DataKinds, GADTs, NoExperimentalDependentTypes, ScopedTypeVariables #-}
module SplitProofScope where
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import PackedVecLibrary (CheckedSplit, Nat(..), Vec)

lazyProof :: CheckedSplit Bool ('S 'Z) total -> Vec Bool total -> Bool
lazyProof (Pair Z ~(_, _, Refl)) (_ :: Vec Bool ('S 'Z)) = True
lazyProof _ _ = False

reversed :: Vec Bool total -> CheckedSplit Bool ('S 'Z) total -> Bool
reversed (_ :: Vec Bool ('S 'Z)) (Pair Z !(_, _, Refl)) = True
reversed _ _ = False

escape :: CheckedSplit Bool ('S 'Z) ('S 'Z) -> Vec Bool suffix
escape (Pair _ (_, right, Refl)) = right
HS
if "${g[@]}" -c SplitProofScope.hs >split-proof-scope.log 2>&1 ||
   ! grep -Fq 'SplitProofScope.hs:8:35: error: [GHC-25897]' split-proof-scope.log ||
   ! grep -Fq 'SplitProofScope.hs:12:11: error: [GHC-25897]' split-proof-scope.log ||
   ! grep -Fq 'SplitProofScope.hs:16:36: error: [GHC-25897]' split-proof-scope.log; then
    cat split-proof-scope.log >&2
    exit 1
fi
cat >WrongPartition.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeOperators #-}
module WrongPartition where
import ConservedPartition (Split, partitionVec)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import PackedVecLibrary (Add, Nat(..), Vec(..), appendVec, dropVec, splitAtVec, takeVec)

keepOnly :: Vec Int n -> Vec Int n
keepOnly xs = case partitionVec even xs of
    Pair _ (Pair _ (kept, _, Refl)) -> kept

wrongTotal :: Split Int ('S 'Z)
wrongTotal = Pair Z (Pair Z (VNil, VNil, Refl))

badCut :: (Vec Int ('S ('S 'Z)), Vec Int 'Z)
badCut = splitAtVec (S (S Z)) (VCons (7 :: Int) VNil)

badSuffix :: (Vec Int ('S 'Z), Vec Int ('S ('S 'Z)))
badSuffix = splitAtVec (S Z) (VCons (7 :: Int) (VCons 9 VNil))

badTake :: Vec Int 'Z
badTake = takeVec (S Z) (VCons 7 VNil)

badDrop :: Vec Int 'Z
badDrop = dropVec (S Z) (VCons 7 (VCons 9 VNil))

omitProof :: Vec a cut -> Vec a suffix -> (Add cut suffix :~: total) -> Vec a total
omitProof left right _ = appendVec left right
HS
if "${g[@]}" -ferror-spans -c WrongPartition.hs >split-errors.log 2>&1 ||
   ! grep -Fq 'WrongPartition.hs:10:40-43: error: [GHC-25897]' split-errors.log ||
   ! grep -Fq 'WrongPartition.hs:13:29-46: error: [GHC-83865]' split-errors.log ||
   ! grep -Fq 'WrongPartition.hs:16:32-52: error: [GHC-83865]' split-errors.log ||
   ! grep -Fq 'WrongPartition.hs:19:31-61: error: [GHC-83865]' split-errors.log ||
   ! grep -Fq 'WrongPartition.hs:22:11-38: error: [GHC-83865]' split-errors.log ||
   ! grep -Fq 'WrongPartition.hs:25:26-47: error: [GHC-83865]' split-errors.log ||
   ! grep -Fq 'WrongPartition.hs:28:26-45: error: [GHC-25897]' split-errors.log; then
    cat split-errors.log >&2
    exit 1
fi

cat >AmbiguousChoice.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, RankNTypes #-}
module AmbiguousChoice where
import ConservedPartition (Branch, Split)

chooseAmbiguous :: Split a total -> Branch a r -> r
chooseAmbiguous = undefined
HS
if "${g[@]}" -ferror-spans -c AmbiguousChoice.hs >ambiguous-choice.log 2>&1 ||
   ! grep -Fq 'AmbiguousChoice.hs:5:20-51: error: [GHC-83865]' ambiguous-choice.log; then
    cat ambiguous-choice.log >&2
    exit 1
fi

cat >WrongList.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module WrongList where
import GHC.Sigma (Sigma(Pair))
import PackedVecLibrary (sourceLength, sourceVector)
import StoredVector (ListVector)
wrong :: [a] -> [a] -> ListVector a
wrong xs ys = Pair (sourceLength xs) (sourceVector ys)
HS
if "${g[@]}" -c WrongList.hs >bad.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' bad.log || ! grep -Fq 'sourceVector ys' bad.log; then
    cat bad.log >&2
    exit 1
fi
