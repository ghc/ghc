#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

cat >"$work/Count.hs" <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Count (Count(..), same) where

data Count = Z | S Count

same :: Count -> Count -> Bool
same Z Z = True
same Z (S _) = False
same (S _) Z = False
same (S m) (S n) = same m n
HS
cat >"$work/Main.hs" <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Main where

import Count (Count(..), same)
import Data.Type.Equality ((:~:)(Refl))

sameRefl :: foreach (n :: Count) -> same n n :~: 'True
sameRefl Z = Refl
sameRefl (S n) = sameRefl n

#ifdef WRONG
bad :: foreach (n :: Count) -> same 'Z ('S n) :~: 'True
bad _ = Refl
#elif defined(UNKNOWN_FIRST)
bad :: foreach (n :: Count) -> same n 'Z :~: 'False
bad _ = Refl
#endif

main :: IO ()
main = case sameRefl (S (S Z)) of
  Refl -> print (same Z (S (error "forced predecessor")))
HS

for opt in -O0 -O2; do
    base="$work/${opt#-}"
    mkdir -p "$base/provider" "$base/client"
    cd "$base/provider"
    cp "$work/Count.hs" .
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -c Count.hs
    cp Count.{hi,o} ../client/
    cd ../client
    cp "$work/Main.hs" .
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" "$opt" Main.o Count.o -o test
    ./test >result
    printf 'False\n' | cmp - result

    for mode in WRONG UNKNOWN_FIRST; do
        if "${ghc[@]}" "$opt" "-D$mode" -fforce-recomp -c Main.hs >"$mode.log" 2>&1 ||
           ! grep -Fq 'error: [GHC-83865]' "$mode.log"; then
            cat "$mode.log" >&2
            exit 1
        fi
    done
done
