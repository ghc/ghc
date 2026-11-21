#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fpedantic-bottoms)
cd "$tmp"
cat >Provider.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies #-}
module Provider (advance, select) where

type family Payload (flag :: Bool) where
    Payload True = Int
    Payload False = Bool

{-# INLINABLE advance #-}
advance :: Int -> Int
advance n =
    let unused :: foreach (flag :: Bool) -> Payload flag
        unused flag = error "forced unused advance binding"
    in n + 1

{-# INLINE select #-}
select :: Bool -> Int -> Int
select flag =
    let unused :: foreach (flag :: Bool) -> Payload flag
        unused other = error "forced unused select binding"
    in case flag of
        True -> (+ 1)
        False -> \_ -> 0
HS
cat >Main.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import Provider (advance, select)

main :: IO ()
main = do
    forced <- try (evaluate (select (error "forced flag"))) :: IO (Either ErrorCall (Int -> Int))
    let demanded = either (const True) (const False) forced
    print (advance 41, select True 41, select False (error "forced operand"), demanded)
HS
expected='(42,42,0,True)'
for opt in -O1 -O2; do
    out="$tmp/$opt"
    mkdir -p "$out"/{provider,client,make}
    cp Provider.hs "$out/provider/"
    (
        cd "$out/provider"
        "${ghc[@]}" "$opt" -c Provider.hs
        "${ghc[@]}" --show-iface Provider.hi > provider.iface
        sed -n '/^  advance ::/,/^  select ::/p' provider.iface | grep 'Unfolding: Core: Stable' >/dev/null
        sed -n '/^  select ::/,/^Warnings:/p' provider.iface | grep 'Unfolding: Core: StableUser' >/dev/null
    )
    cp Main.hs "$out/client/"
    cp "$out/provider/Provider.hi" "$out/client/"
    (
        cd "$out/client"
        "${ghc[@]}" "$opt" -c Main.hs
        "${ghc[@]}" Main.o ../provider/Provider.o -o run
        test "$(./run)" = "$expected"
    )
    cp Provider.hs Main.hs "$out/make/"
    (
        cd "$out/make"
        "${ghc[@]}" "$opt" --make Main.hs -o run
        test "$(./run)" = "$expected"
    )
done
"${ghc[@]}" --interactive -ignore-dot-ghci -fexternal-interpreter Main.hs > ghci.log <<'GHCi'
main
print (advance 41)
GHCi
printf '%s\n42\n' "$expected" | cmp - ghci.log

cat >Residual.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies #-}
module Residual where

type family Payload (flag :: Bool) where
    Payload True = Int
    Payload False = Bool

{-# NOINLINE choose #-}
choose :: foreach (flag :: Bool) -> Payload flag
choose True = 42
choose False = False

ordinary :: Int
ordinary = choose True
HS
"${ghc[@]}" -O1 -c Residual.hs
for pragma in INLINE INLINABLE; do
    printf '{-# %s ordinary #-}\n' "$pragma" >> Residual.hs
    if "${ghc[@]}" -O1 -fdiagnostics-color=never -fforce-recomp -c Residual.hs > residual.log 2>&1; then
        echo "Accepted residual retained Core under $pragma"
        exit 1
    fi
    grep -qFx 'Residual.hs:14:1: error: [GHC-92256]' residual.log
    grep -q "The $pragma unfolding of .*ordinary.* still contains retained Core." residual.log
    sed -i '$d' Residual.hs
done
