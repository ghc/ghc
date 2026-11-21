#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint)
for opt in -O0 -O1 -O2; do
    out="$tmp/$opt"
    mkdir -p "$out"/{indices,proof,client}
    cd "$out/indices"
    cp "$src"/{PackedVecLibrary,ParameterizedIndexLibrary}.hs .
    "${ghc[@]}" "$opt" -fwrite-source-equations -c PackedVecLibrary.hs
    "${ghc[@]}" "$opt" -Werror=incomplete-patterns -c ParameterizedIndexLibrary.hs
    cd ../proof
    cp "$src/"{FinitePosition,NativeIndexedFinGate}.hs ../indices/*.hi .
    "${ghc[@]}" "$opt" -Werror=incomplete-patterns -c FinitePosition.hs NativeIndexedFinGate.hs
    cd ../client
    cp "$src/FinitePositionClient.hs" ../proof/*.hi .
    "${ghc[@]}" "$opt" -Werror=incomplete-patterns -c FinitePositionClient.hs
    cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module Main where
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.Maybe (isJust)
import FinitePositionClient (lookupVecViaView, observeConstructedViews, observeEmptyIndex, observeFinitePosition, observeListIndices)
import NativeIndexedFinGate (Gate(..), Ticket(..), grant)
import PackedVecLibrary (Nat(..), Vec(..))
import ParameterizedIndexLibrary (indexVec, lookupVecChecked, mkFinVec)

main :: IO ()
main = do
    observeFinitePosition >>= print
    observeListIndices >>= print
    empty <- observeEmptyIndex
    gate <- newIORef (Shut (error "gate field")) >>= readIORef
    denied <- try (evaluate (grant gate (error "ticket"))) :: IO (Either ErrorCall Bool)
    case denied of
        Left err | empty && grant (Open True) Granted
                 , takeWhile (/= '\n') (displayException err) == "ticket" -> pure ()
        _ -> fail "wrong demand for an omitted retained equation"
    views <- observeConstructedViews
    if views == (Just 1, True) then pure () else fail (show views)
    positions <- newIORef [minBound, -1, 0, 1, 2, 3, maxBound] >>= readIORef
    let cells = VCons (7 :: Int) (VCons 9 (VCons 13 VNil))
        other = VCons (11 :: Int) (VCons 19 (VCons 23 VNil))
        expected = [Nothing, Nothing, Just 7, Just 9, Just 13, Nothing, Nothing]
    case (map (lookupVecChecked cells) positions, map (lookupVecViaView cells) positions,
          fmap (\i -> indexVec i other) (mkFinVec cells 2)) of
        (direct, viaView, Just 23) | direct == expected && viaView == expected -> pure ()
        actual -> fail (show actual)
    let absent = error "forced vector for negative position" :: Vec Int 'Z
        unusedTail = error "forced suffix past checked position" :: Vec Int ('S 'Z)
        prefix = VCons (error "forced skipped element") (VCons (29 :: Int) unusedTail)
        selectedBottom = VCons (7 :: Int) (VCons (error "forced selected element") VNil)
    case (lookupVecViaView absent minBound, lookupVecViaView (VNil :: Vec Int 'Z) 0,
          lookupVecViaView prefix 1, isJust (lookupVecViaView selectedBottom 1)) of
        (Nothing, Nothing, Just 29, True) -> pure ()
        actual -> fail (show actual)
HS
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" Main.o FinitePositionClient.o ../proof/*.o ../indices/*.o -o run
    ./run
done
cd "$out/client"
cat >MissingRows.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies #-}
module MissingRows where
import NativeIndexedFinGate

type family Unknown (g :: Gate) :: Bool where
    Unknown ('Open b) = 'True

inhabited :: foreach (g :: Gate) -> Ticket (Always g) -> Bool
inhabited (Open b) Granted = b

stuck :: foreach (g :: Gate) -> Ticket (Unknown g) -> Bool
stuck (Open b) Granted = b

noSplit :: foreach (g :: Gate) -> Ticket (Ready g) -> Bool
noSplit (Open b) _ = b

lazySplit :: foreach (g :: Gate) -> Ticket (Ready g) -> Bool
lazySplit (Open b) ~Granted = b

type family Always (g :: Gate) :: Bool where
    Always ('Open b) = 'True
    Always ('Shut b) = 'True
HS
if "${ghc[@]}" -O0 -fdiagnostics-color=never -c MissingRows.hs >missing.log 2>&1; then
    echo 'Accepted an uncovered retained equation'
    exit 1
fi
for line in 9 12 15 18; do
    grep -Eq "^MissingRows.hs:$line:[0-9]+: error: .GHC-83605." missing.log
done
test "$(grep -c 'Missing equation for the retained constructor .*Shut' missing.log)" -eq 4
cat >MissingSuccessor.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables #-}
module MissingSuccessor where
import PackedVecLibrary (sourceLength)
import ParameterizedIndexLibrary (Fin(..))
index :: forall a. foreach (xs :: [a]) -> Fin (sourceLength xs) -> a
index (x : _) FZ = x
HS
if "${ghc[@]}" -O0 -Werror=incomplete-patterns -c MissingSuccessor.hs >successor.log 2>&1; then
    echo 'Accepted an uncovered successor position'
    exit 1
fi
grep -Fq 'MissingSuccessor.hs:6:1: error: [GHC-62161]' successor.log
grep -Fq 'FS' successor.log
cat >InvalidListIndex.hs <<'HS'
{-# LANGUAGE DataKinds, LinearTypes, NoExperimentalDependentTypes, ScopedTypeVariables, TypeApplications #-}
module InvalidListIndex where
import ParameterizedIndexLibrary (Fin(FZ), indexGeneric, mkFinGeneric)

empty :: Char
empty = indexGeneric @Char [] FZ

linear :: [Char] %1 -> Bool
linear xs = case mkFinGeneric xs 0 of
    Just _ -> True
    Nothing -> False

rigid :: forall a. a -> Bool
rigid xs = case mkFinGeneric @Char xs 0 of
    Just _ -> True
    Nothing -> False
HS
if "${ghc[@]}" -O0 -fdiagnostics-color=never -c InvalidListIndex.hs >invalid.log 2>&1; then
    echo 'Accepted an invalid source-length index'
    exit 1
fi
grep -Fq 'InvalidListIndex.hs:6:31: error: [GHC-83865]' invalid.log
grep -Fq 'InvalidListIndex.hs:9:31: error: [GHC-83605]' invalid.log
grep -Fq 'InvalidListIndex.hs:14:36: error: [GHC-25897]' invalid.log
cat >DifferentPredecessor.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module DifferentPredecessor where
import Data.Type.Equality ((:~:)(Refl))
import ParameterizedIndexLibrary (Fin(..))
wrong :: forall n. foreach (p :: Fin n) -> foreach (q :: Fin n) -> 'FS q :~: 'FS p
wrong _ _ = Refl
HS
if "${ghc[@]}" -O0 -ferror-spans -c DifferentPredecessor.hs >reject.log 2>&1; then
    echo 'Accepted an independent predecessor'
    exit 1
fi
grep -F 'DifferentPredecessor.hs:6:13-16: error:' reject.log >/dev/null

# A previous reader can still read a vector operand with the original shape.
cp "$src"/{NativeGeneralGadtOperandProvider,NativeIndexedOperandWire}.hs .
"${ghc[@]}" -O2 -c NativeGeneralGadtOperandProvider.hs NativeIndexedOperandWire.hs
"${ghc[@]}" --show-iface NativeGeneralGadtOperandProvider.hi > new-wire.log
grep -Fq 'general-indexed-constructor-operand' new-wire.log
"${ghc[@]}" --show-iface NativeIndexedOperandWire.hi > vector-wire.log
grep -Fq 'indexed-constructor-operand' vector-wire.log
if grep -Fq 'general-indexed-constructor-operand' vector-wire.log; then
    echo 'Wrote the extended operand header for a vector'
    exit 1
fi
if [[ $# -ge 3 ]]; then
    "$3" "-B$2" --show-iface NativeIndexedOperandWire.hi > old-vector.log
    if "$3" "-B$2" --show-iface NativeGeneralGadtOperandProvider.hi > old-fields.log 2>&1; then
        echo 'Previous reader accepted the new operand header'
        exit 1
    fi
    grep -Fq 'mismatched interface file versions' old-fields.log
    grep -Fq 'general-indexed-constructor-operand' old-fields.log
fi

mkdir "$tmp/multi-index-client"
cp NativeGeneralGadtOperandProvider.hi "$tmp/multi-index-client/"
cd "$tmp/multi-index-client"
cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module Main where

import Data.Proxy (Proxy(..))
import NativeGeneralGadtOperandProvider (Both(..), make)

fixed :: (Both (Maybe Bool) [Ordering], Proxy ('Mk2 'LT 'False))
fixed = make False LT

lazy :: Bool -> Char -> Bool
lazy x y = case fst (make x y) of Mk2 _ _ -> True

main :: IO ()
main = case (fixed, lazy undefined undefined) of
    ((Mk2 LT False, Proxy), True) -> pure ()
    _ -> fail "wrong indexed constructor fields"
HS
"${ghc[@]}" -O2 -ddump-inlinings -dsuppress-module-prefixes -c Main.hs > inlinings.log
grep -Fxq 'Inlining done: make' inlinings.log
"${ghc[@]}" Main.o "$out/client/NativeGeneralGadtOperandProvider.o" -o run
./run
