#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -O0 -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fno-enable-rewrite-rules -fdiagnostics-color=never
     -fomit-interface-pragmas -fno-write-if-simplified-core)

mkdir -p "$out"/{provider,client}
cd "$out/provider"
cp "$src/PackedVecLibrary.hs" .
"${ghc[@]}" -fwrite-source-equations -c PackedVecLibrary.hs
"${ghc[@]}" --show-iface PackedVecLibrary.hi >provider.iface
grep -Fq 'FinalCoreSourceTable T:appendVec' provider.iface
cp PackedVecLibrary.hi ../client/

cd ../client
cat >AppendOperands.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, TypeApplications, TypeOperators #-}
module AppendOperands (Observed(..), appendObserved) where

import qualified PackedVecLibrary as P

type One = 'P.S 'P.Z
type Result = 'P.Z P.+ One
data Observed (xs :: P.Vec Bool Result) = Observed

observe :: foreach (xs :: P.Vec Bool Result) -> Observed xs
observe _ = Observed

#if defined(CONFLICT)
appendObserved :: foreach (xs :: P.Vec Bool 'P.Z) -> foreach (ys :: P.Vec Char One) -> Bool
appendObserved xs ys = case observe (P.appendVec xs ys) of Observed -> True
#elif defined(UNDERCONSTRAINED)
appendObserved :: foreach (ys :: P.Vec Bool One) -> Bool
appendObserved ys = case observe (P.appendVec P.VNil ys) of Observed -> True
#elif defined(EXPLICIT_EMPTY)
appendObserved :: foreach (ys :: P.Vec Bool One) -> Bool
appendObserved ys = case observe (P.appendVec @Bool @'P.Z @One P.VNil ys) of Observed -> True
#else
appendObserved :: foreach (xs :: P.Vec Bool 'P.Z) -> foreach (ys :: P.Vec Bool One)
               -> Observed (P.appendVec xs ys)
#ifdef EXPLICIT
appendObserved xs ys = observe (P.appendVec @Bool @'P.Z @One xs ys)
#else
appendObserved xs ys = observe (P.appendVec xs ys)
#endif
#endif
HS
"${ghc[@]}" -c AppendOperands.hs
mv AppendOperands.hs ../provider/
cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module Main where
import AppendOperands (Observed(..), appendObserved)
import qualified PackedVecLibrary as P

readObserved :: P.Vec Bool 'P.Z -> P.Vec Bool ('P.S 'P.Z) -> Bool
readObserved xs ys = case appendObserved xs ys of Observed -> True

main :: IO ()
main = print (readObserved P.VNil (P.VCons True P.VNil))
HS
"${ghc[@]}" -c Main.hs
"${ghc[@]}" Main.o AppendOperands.o ../provider/PackedVecLibrary.o -o observe
./observe >actual
grep -Fxq True actual

mv ../provider/AppendOperands.hs .
for mode in EXPLICIT EXPLICIT_EMPTY; do
    "${ghc[@]}" -fforce-recomp "-D$mode" -c AppendOperands.hs
done
for mode in CONFLICT UNDERCONSTRAINED; do
    if "${ghc[@]}" -fforce-recomp "-D$mode" -c AppendOperands.hs >rejected.log 2>&1 ||
       ! grep -Fq '[GHC-83605]' rejected.log || ! grep -Fq 'P.appendVec' rejected.log; then
        cat rejected.log >&2
        exit 1
    fi
done
