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
    cp "$src/"{FinitePosition,ListCursor,AlignedList,NativeIndexedFinGate,NativeLinearIndexColumns}.hs ../indices/*.hi .
    "${ghc[@]}" "$opt" -Werror=incomplete-patterns -c FinitePosition.hs AlignedList.hs NativeIndexedFinGate.hs
    lint=()
    if [[ $opt == -O0 ]]; then lint=(-dlinear-core-lint); fi
    "${ghc[@]}" "$opt" "${lint[@]}" -Werror=incomplete-patterns -c ListCursor.hs NativeLinearIndexColumns.hs
    cd ../client
    cp "$src/"{FinitePositionClient,CursorClient}.hs ../proof/*.hi .
    "${ghc[@]}" "$opt" -Werror=incomplete-patterns -c FinitePositionClient.hs CursorClient.hs
    cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module Main where
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.Maybe (isJust)
import CursorClient (observeBoth, observeZip, observeAligned, observeCursors, observeIndexedCallback)
import NativeLinearIndexColumns (choose)
import FinitePositionClient (lookupVecViaView, observeConstructedViews, observeEmptyIndex, observeFinitePosition, observeListIndices)
import NativeIndexedFinGate (Gate(..), Ticket(..), grant)
import PackedVecLibrary (Nat(..), Vec(..))
import ParameterizedIndexLibrary (indexVec, lookupVecChecked, mkFinGeneric, mkFinVec)

main :: IO ()
main = do
    observeFinitePosition >>= print
    observeListIndices >>= print
    both <- observeBoth
    if both == [Just ('b', 10), Just ('b', 20), Nothing] then pure () else fail (show both)
    zipped <- observeZip
    if zipped == (Just [('a', 10), ('b', 20)], Nothing, Just 30, [('x', 'x')]) then pure () else fail (show zipped)
    aligned <- observeAligned
    if aligned == [Just ('b', 20), Nothing, Nothing] then pure () else fail (show aligned)
    callbacks <- observeIndexedCallback
    if callbacks == (Just "cc", Nothing, True) then pure () else fail (show callbacks)
    word <- newIORef "xz" >>= readIORef
    case mkFinGeneric word 1 of
        Nothing -> fail "missing position"
        Just pos -> do
            let selected = choose word pos True (error "forced explicitly lazy argument")
                fallback = choose word pos False '!'
            if (selected, fallback) == ('z', '!') then pure () else fail "wrong branch"
    (ordinary, linear) <- observeCursors
    let expectedCursors = [Just 'c', Nothing, Nothing, Just 'b', Just 'x', Just 'y']
    if ordinary == expectedCursors && linear == expectedCursors then pure ()
        else fail (show (ordinary, linear))
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
    "${ghc[@]}" Main.o FinitePositionClient.o CursorClient.o ../proof/*.o ../indices/*.o -o run
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
cat >IndependentLengths.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, LinearTypes, ScopedTypeVariables #-}
module IndependentLengths where
import Data.Proxy (Proxy)
import PackedVecLibrary (sourceLength)
import ParameterizedIndexLibrary (Fin)

data Positions a b where
    Positions :: foreach (xs :: [a]) -> Fin (sourceLength xs) %1
              -> foreach (ys :: [b]) -> Fin (sourceLength ys) %1 -> Positions a b

ordinary :: Proxy (xs :: [a]) -> Proxy (ys :: [b])
         -> Proxy (sourceLength xs) -> Proxy (sourceLength ys)
         -> (Proxy (sourceLength xs), Proxy (sourceLength ys))
ordinary _ _ p q = (p, q)
#ifdef BAD_INDEX
data Wrong where
    Wrong :: Proxy (sourceLength True) -> Wrong
#endif
HS
for opt in -O0 -O2; do
    "${ghc[@]}" "$opt" -fforce-recomp -c IndependentLengths.hs
done
if "${ghc[@]}" -O0 -fforce-recomp -DBAD_INDEX -fdiagnostics-color=never -c IndependentLengths.hs >wrong-length.log 2>&1; then
    echo 'Generalized a certified source function over a wrong argument kind'
    exit 1
fi
grep -Eq '^IndependentLengths.hs:17:[0-9]+: error: .GHC-83865.' wrong-length.log
cat >ScopedLength.hs <<'HS'
{-# LANGUAGE CPP, EmptyCase, ExperimentalDependentTypes, GADTs, LinearTypes, ScopedTypeVariables #-}
module ScopedLength where
import PackedVecLibrary (sourceLength)
import ParameterizedIndexLibrary (Fin(..))
#ifdef UNRELATED
lookupAt :: forall n m a. foreach (xs :: [a]) -> (m ~ sourceLength xs) => Fin n %1 -> a
#else
lookupAt :: forall n m a. foreach (xs :: [a]) -> (m ~ sourceLength xs, n ~ m) => Fin n %1 -> a
#endif
#ifndef OMIT
lookupAt [] i = case i of {}
#endif
lookupAt (x : _) FZ = x
lookupAt (_ : xs) (FS i) = lookupAt xs i
HS
for clause in explicit omitted; do
    flags=()
    if [[ $clause == omitted ]]; then flags=(-DOMIT); fi
    "${ghc[@]}" -O0 -fforce-recomp -dlinear-core-lint -Werror=incomplete-patterns "${flags[@]}" -c ScopedLength.hs
done
if "${ghc[@]}" -O0 -fforce-recomp -DUNRELATED -DOMIT -fdefer-type-errors -fdiagnostics-color=never -c ScopedLength.hs >unrelated.log 2>&1; then
    echo 'Used an unrelated list length to omit a retained equation'
    exit 1
fi
grep -Fq 'ScopedLength.hs:13:1: error: [GHC-83605]' unrelated.log
cat >MatrixLength.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeFamilies #-}
module MatrixLength where
import AlignedList (zipWithExact)
import PackedVecLibrary (Nat(..), Vec(..), sourceLength, toList)

#ifdef OMIT
onlyNil :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
        -> (sourceLength xs ~ sourceLength ys) => Vec (a, b) (sourceLength xs)
onlyNil [] [] = VNil
#elif defined(WRONG)
wrong :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
      -> (sourceLength xs ~ sourceLength ys) => Vec (a, b) (sourceLength xs)
wrong [] [] = VNil
wrong (x : xs) (y : ys) = VNil
#else
useFallback :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
            -> (sourceLength xs ~ sourceLength ys) => [(a, b)]
useFallback [] [] = []
useFallback xs ys = contents
  where contents = toList (zipWithExact xs ys (,))

oneSided :: foreach (n :: Nat) -> (n ~ 'Z) => foreach (m :: Nat) -> Vec Bool m
oneSided Z Z = VNil
oneSided Z (S k) = VCons False (oneSided Z k)
#endif
HS
"${ghc[@]}" -O0 -Werror=incomplete-patterns -c MatrixLength.hs
if "${ghc[@]}" -O0 -DOMIT -fforce-recomp -fdefer-type-errors -fdiagnostics-color=never -c MatrixLength.hs >pair.log 2>&1; then
    echo 'Accepted an omitted cons/cons equation'
    exit 1
fi
grep -Fq 'MatrixLength.hs:9:1: error: [GHC-83605]' pair.log
grep -Fq 'Missing equation for the retained constructor pair (:, :).' pair.log
if "${ghc[@]}" -O0 -DWRONG -fforce-recomp -fdiagnostics-color=never -c MatrixLength.hs >result.log 2>&1; then
    echo 'Accepted an empty result for two nonempty lists'
    exit 1
fi
grep -Fq 'MatrixLength.hs:14:27: error: [GHC-83865]' result.log
if grep -Fq '$d~' result.log; then
    echo 'Reported an implicit dictionary as a source binding'
    exit 1
fi
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
for variant in DISCARD DUPLICATE; do
    cp "$src/NativeLinearIndexColumns.hs" .
    if "${ghc[@]}" -O0 -fforce-recomp "-D$variant" -fdiagnostics-color=never -c NativeLinearIndexColumns.hs >bad-columns.log 2>&1; then
        echo "Accepted $variant of a linear callback"
        exit 1
    fi
    grep -Fq '[GHC-18872]' bad-columns.log
    test "$(grep -Fc 'In an equation for' bad-columns.log)" = 1
    grep -Fq "multiplicity of" bad-columns.log
done

cat >BadCursor.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LinearTypes, ScopedTypeVariables, TypeApplications #-}
module BadCursor where
import GHC.Types (Multiplicity)
import PackedVecLibrary (sourceLength)
import ParameterizedIndexLibrary (Fin, indexGeneric, mkFinGeneric)

onlyOnce :: forall a. foreach (xs :: [a]) -> Fin (sourceLength xs) %1 -> a
onlyOnce xs i = indexGeneric xs i

fixed :: [Char] -> Int -> Maybe Char
fixed xs n = fmap (onlyOnce xs) (mkFinGeneric xs n)

independent :: [Char] -> [Char] -> Int -> Maybe Char
independent xs ys n = fmap (indexGeneric xs) (mkFinGeneric ys n)

rigid :: forall (p :: Multiplicity) a b. (a %p -> b) -> Maybe a -> Maybe b
rigid f = fmap f
HS
if "${ghc[@]}" -O0 -fdiagnostics-color=never -c BadCursor.hs >bad-cursor.log 2>&1; then
    echo 'Accepted a mismatched callback or an independent position'
    exit 1
fi
grep -Fq 'BadCursor.hs:11:19: error: [GHC-83865]' bad-cursor.log
grep -Fq 'BadCursor.hs:14:28: error: [GHC-18872]' bad-cursor.log
grep -Fq 'sourceLength xs' bad-cursor.log
grep -Fq 'sourceLength ys' bad-cursor.log
grep -Fq 'BadCursor.hs:17:16: error: [GHC-25897]' bad-cursor.log
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
