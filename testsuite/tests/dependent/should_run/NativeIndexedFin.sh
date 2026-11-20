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
    cp "$src/FinitePosition.hs" ../indices/*.hi .
    "${ghc[@]}" "$opt" -c FinitePosition.hs
    cd ../client
    cp "$src/FinitePositionClient.hs" ../proof/*.hi .
    "${ghc[@]}" "$opt" -Werror=incomplete-patterns -c FinitePositionClient.hs
    cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module Main where
import Data.IORef (newIORef, readIORef)
import Data.Maybe (isJust)
import FinitePositionClient (lookupVecViaView, observeFinitePosition)
import PackedVecLibrary (Nat(..), Vec(..))
import ParameterizedIndexLibrary (indexVec, lookupVecChecked, mkFinVec)

main :: IO ()
main = do
    observeFinitePosition >>= print
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
    "${ghc[@]}" Main.o FinitePositionClient.o ../proof/FinitePosition.o ../indices/*.o -o run
    ./run
done
cd "$out/client"
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
