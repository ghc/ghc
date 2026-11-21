#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fpedantic-bottoms)
cd "$tmp"
cat >Work.hs <<'HS'
{-# LANGUAGE BangPatterns, DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies #-}
module Work (tally, constant) where

type family Payload (flag :: Bool) where
    Payload True = Int
    Payload False = ()

{-# NOINLINE bump #-}
bump :: foreach (flag :: Bool) -> Int -> Payload flag
bump True n = n + 2
bump False _ = ()

{-# INLINABLE tally #-}
tally :: Num a => (Int, Int) -> a -> a
tally (n, c) v
    | n <= 0 = v
    | otherwise = tally (n - 1, c) $! v + fromIntegral (bump True c + n)

{-# INLINABLE constant #-}
constant :: Int
constant = bump True 40
HS
cat >Main.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, TypeApplications #-}
module Main (main) where
import Control.Exception (ErrorCall, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Work (constant, tally)

threw :: Int -> IO Bool
threw x = do
    value <- try (evaluate x) :: IO (Either ErrorCall Int)
    pure (either (const True) (const False) value)

main :: IO ()
main = do
    if constant == 42 then pure () else fail "wrong cast worker result"
    p <- newIORef (2, 0) >>= readIORef
    strictCount <- threw (tally @Int (error "pair") 0)
    strictInitial <- threw (tally @Int p (error "initial"))
    untouched <- evaluate (tally @Int (0, error "unused offset") 35)
    print (tally @Int p 35, tally @Integer p 35, tally @Int (0, 0) 35, strictCount, strictInitial, untouched)
HS
expected='(42,42,35,True,True,35)'
for opt in -O1 -O2; do
    out="$tmp/$opt"
    mkdir -p "$out"/{provider,client}
    cp Work.hs "$out/provider/"
    cp Main.hs "$out/client/"
    (
        cd "$out/provider"
        "${ghc[@]}" "$opt" -c Work.hs
        "${ghc[@]}" --show-iface Work.hi > work.iface
        grep -Fq 'retained-user-unfolding' work.iface
        for bndr in '$wtally' tally constant; do
            grep -A 10 -F "  $bndr ::" work.iface | grep -F 'Core: StableRetainedUser' >/dev/null
        done
    )
    (
        cd "$out/client"
        cp ../provider/Work.hi .
        "${ghc[@]}" "$opt" -ddump-rule-firings -c Main.hs > client.log
        for ty in Int Integer; do
            grep -Fq "Rule fired: SPEC/Main tally @$ty (Main)" client.log
            grep -Fq "Rule fired: SPEC/Main \$wtally @$ty (Main)" client.log
        done
        "${ghc[@]}" Main.o ../provider/Work.o -o run
        test "$(./run)" = "$expected"
    )
done
cd "$tmp/-O2/client"
printf '{-# SPECIALISE tally :: (Int, Int) -> Int -> Int #-}\n' >> Main.hs
"${ghc[@]}" -O2 -fforce-recomp -ddump-rule-firings -c Main.hs > explicit.log
grep -Fq 'Rule fired: USPEC tally @Int (Main)' explicit.log
"${ghc[@]}" --show-iface Main.hi > explicit.iface
grep -Fq '"USPEC tally @Int"' explicit.iface
"${ghc[@]}" Main.o ../provider/Work.o -o run-explicit
test "$(./run-explicit)" = "$expected"

# The marker is readable even when the inactive template needs a missing interface.
out="$tmp/lazy"
mkdir -p "$out"/{provider,client}
cd "$out/provider"
cat >HiddenIndex.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies #-}
module HiddenIndex (bump) where

{-# NOINLINE bump #-}
bump :: foreach (flag :: Bool) -> ((flag ~ flag) => Int) -> Int
bump flag n = n + 2
HS
cat >Work.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module Work (value) where
import HiddenIndex (bump)

{-# INLINABLE [0] value #-}
value :: Int
value = bump True 40
HS
"${ghc[@]}" -O2 -fforce-recomp -c HiddenIndex.hs Work.hs
"${ghc[@]}" --show-iface Work.hi > work.iface
grep -Fq 'StableRetainedUser' work.iface
grep -Fq 'HiddenIndex.bump' work.iface
cp Work.hi ../client/
cd ../client/
cat >Main.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main (main) where
import Work (value)

main :: IO ()
main = print value
HS
# Read the tag with optimization disabled. The body needs HiddenIndex.hi.
test ! -e HiddenIndex.hi
"${ghc[@]}" -O0 -fno-ignore-interface-pragmas -fomit-interface-pragmas -fforce-recomp -c Main.hs
"${ghc[@]}" Main.o ../provider/{HiddenIndex,Work}.o -o run
test "$(./run)" = 42
