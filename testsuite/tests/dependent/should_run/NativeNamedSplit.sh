#!/usr/bin/env bash
set -euo pipefail
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
g=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
cd "$out"
for opt in -O0 -O2; do
    mkdir -p "./$opt"/{provider,client}
    pushd "./$opt/provider" >/dev/null
    cp "$src/PackedVecLibrary.hs" .
    "${g[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c PackedVecLibrary.hs
    cat >Opaque.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TypeFamilies, UndecidableInstances #-}
module Opaque (Opaque, Token) where
import PackedVecLibrary (Nat)
type family Token (n :: Nat)
type Opaque = some (n :: Nat) | Token n
HS
    "${g[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c Opaque.hs
    cp {PackedVecLibrary,Opaque}.hi "$src/NamedSplitClient.hs" ../client/
    cd ../client
    "${g[@]}" "$opt" -c NamedSplitClient.hs
    cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, ScopedTypeVariables, TypeApplications #-}
module Main where
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import NamedSplitClient (observeNamedSplitIO)
import PackedVecLibrary (CheckedSplit, Nat(..), Vec(..), splitAtVecChecked, toList)

keepLazy :: CheckedSplit Bool ('S 'Z) total -> Vec Bool total -> Bool
keepLazy (Pair _ ~(_, _ :: Vec Bool suffix, _)) _ = True

defer :: CheckedSplit Bool ('S 'Z) total -> Vec Bool total -> [Bool]
defer (Pair _ ~(_, _ :: Vec Bool suffix, proof)) second =
    case proof of
        Refl -> case second of
            (xs :: Vec Bool ('S suffix)) -> toList @Bool @('S suffix) xs

main :: IO ()
main = do
    ref <- newIORef (VCons True (VCons False VNil))
    let obtain = splitAtVecChecked (S Z) (S (S Z)) <$> readIORef ref
        later = VCons False (VCons True VNil)
        bottomTuple = Pair (S Z) (error "unused tuple") :: CheckedSplit Bool ('S 'Z) ('S ('S 'Z))
        badProof = Pair (S Z) (VCons True VNil, VCons False VNil, error "split proof forced")
                   :: CheckedSplit Bool ('S 'Z) ('S ('S 'Z))
    result <- observeNamedSplitIO obtain later
    deferred <- fmap (fmap (`defer` later)) obtain
    failed <- try @ErrorCall (evaluate (defer badProof later))
    case (result, deferred, keepLazy bottomTuple later, failed) of
        (Just (1, [True, False], [False, True]), Just [False, True], True, Left err)
            | takeWhile (/= '\n') (displayException err) == "split proof forced" -> pure ()
        _ -> fail "checked split lost its suffix or changed when the lazy patterns demand it"
HS
    "${g[@]}" "$opt" -c Main.hs
    "${g[@]}" "$opt" Main.o NamedSplitClient.o ../provider/PackedVecLibrary.o -o run
    ./run

    cat >Scope.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, ScopedTypeVariables #-}
module Scope where
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import Opaque (Opaque, Token)
import PackedVecLibrary (CheckedSplit, Nat(..), Vec)

independent :: CheckedSplit Bool ('S 'Z) ('S 'Z) -> CheckedSplit Bool ('S 'Z) ('S 'Z) -> Bool
independent (Pair _ (_, _ :: Vec Bool same, _)) (Pair _ (_, _ :: Vec Bool same, _)) = True

escape :: forall outside. CheckedSplit Bool ('S 'Z) ('S 'Z) -> Vec Bool outside
escape (Pair _ (_, rhs :: Vec Bool suffix, _)) = rhs

lazyProof :: CheckedSplit Bool ('S 'Z) total -> Vec Bool total -> Bool
lazyProof (Pair _ ~(_, _ :: Vec Bool suffix, Refl)) (_ :: Vec Bool ('S suffix)) = True

nonInjective :: Opaque -> Bool
nonInjective (Pair _ (_ :: Token suffix)) = True
HS
    if "${g[@]}" "$opt" -c Scope.hs >scope.log 2>&1 ||
       ! grep -Eq '^Scope.hs:9:[0-9]+: error: \[GHC-83865\]' scope.log ||
       ! grep -Eq '^Scope.hs:12:[0-9]+: error: \[GHC-25897\]' scope.log ||
       ! grep -Eq '^Scope.hs:15:[0-9]+: error: \[GHC-25897\]' scope.log ||
       ! grep -Eq '^Scope.hs:18:[0-9]+: error: \[GHC-83865\]' scope.log; then
        cat scope.log >&2
        exit 1
    fi
    popd >/dev/null
done
