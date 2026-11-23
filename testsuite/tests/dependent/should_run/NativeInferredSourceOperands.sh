#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fno-enable-rewrite-rules -fdiagnostics-color=never
     -fomit-interface-pragmas -fno-write-if-simplified-core)

for opt in -O0 -O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/PackedVecLibrary.hs" .
    "${ghc[@]}" "$opt" -fwrite-source-equations -c PackedVecLibrary.hs
    "${ghc[@]}" --show-iface PackedVecLibrary.hi >provider.iface
    grep -Fq 'FinalCoreSourceTable T:appendVec' provider.iface
    cp PackedVecLibrary.hi ../client/

    cd ../client
    cat >AppendOperands.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs #-}
module AppendOperands (Observed(..), appendObserved) where

import qualified PackedVecLibrary as P

type One = 'P.S 'P.Z
data Observed (xs :: P.Vec Bool One) = Observed Bool

observe :: foreach (xs :: P.Vec Bool One) -> Observed xs
observe (P.VCons x _) = Observed x

#if defined(CONFLICT)
appendObserved :: foreach (xs :: P.Vec Bool 'P.Z) -> foreach (ys :: P.Vec Char One) -> Bool
appendObserved xs ys = case observe (P.appendVec xs ys) of Observed b -> b
#elif defined(UNDERCONSTRAINED)
appendObserved :: foreach (ys :: P.Vec Bool One) -> Bool
appendObserved ys = case observe (P.appendVec P.VNil ys) of Observed b -> b
#else
appendObserved :: foreach (xs :: P.Vec Bool 'P.Z) -> foreach (ys :: P.Vec Bool One)
               -> Observed (P.appendVec xs ys)
appendObserved xs ys = observe (P.appendVec xs ys)
#endif
HS
    "${ghc[@]}" "$opt" -c AppendOperands.hs
    mv AppendOperands.hs ../provider/
    cat >Main.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where
import AppendOperands (Observed(..), appendObserved)
import qualified PackedVecLibrary as P

main :: IO ()
main = case appendObserved P.VNil (P.VCons True P.VNil) of Observed b -> print b
HS
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" Main.o AppendOperands.o ../provider/PackedVecLibrary.o -o observe
    ./observe >actual
    grep -Fxq True actual

    mv ../provider/AppendOperands.hs .
    for mode in CONFLICT UNDERCONSTRAINED; do
        if "${ghc[@]}" "$opt" -fforce-recomp "-D$mode" -c AppendOperands.hs >rejected.log 2>&1 ||
           ! grep -Fq '[GHC-83605]' rejected.log || ! grep -Fq 'P.appendVec' rejected.log; then
            cat rejected.log >&2
            exit 1
        fi
    done
done
