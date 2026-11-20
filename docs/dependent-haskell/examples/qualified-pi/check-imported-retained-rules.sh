#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
ghc=$(realpath "$1")
lib=$(realpath "$2")
old=${3:-}
if [[ -n $old ]]; then old=$(realpath "$old"); fi
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
cat >Owner.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module Owner (Box(..), keep) where
data Box (b :: Bool) = Box Int
{-# NOINLINE keep #-}
keep :: foreach (b :: Bool) -> Box b -> Box b
keep _ x = x
HS
cat >Orphan.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Orphan () where
import Owner
{-# RULES "keep/false-orphan" forall x. keep False x = x #-}
HS
cat >Anchored.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables #-}
module Anchored (anchor) where
import Owner
{-# NOINLINE anchor #-}
anchor :: Box b -> Box b
anchor x = x
{-# RULES "keep/anchored" forall (b :: Bool) (x :: Box b). keep b (anchor x) = anchor x #-}
HS
cat >Use.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Use where
import Owner
import Orphan ()
import Anchored (anchor)
closed :: Int -> Int
closed x = case keep False (Box x) of Box n -> n
open :: Bool -> Int -> Int
open b x = case keep b (anchor (Box x)) of Box n -> n
HS
cat >Main.hs <<'HS'
module Main where
import Use
import Data.IORef
main :: IO ()
main = do
    b <- newIORef True >>= readIORef
    print (closed 11, open b 17, open (error "unused index") 19)
HS
cat >Plain.hs <<'HS'
module Plain where
import Owner
plain :: Int -> Int
plain x = case keep False (Box x) of Box n -> n
HS

levels=(2)
if [[ -n $old ]]; then levels=(1 2 3); fi
for compression in "${levels[@]}"; do
    cd "$out"
    mkdir -p "$compression"/{owner,rules,client}
    flags=("-B$lib" -package-env - -v0 -i -i. -O2 -fforce-recomp -dcore-lint -dstg-lint
           "-fwrite-if-compression=$compression" -fdiagnostics-color=never)
    cd "$compression/owner"
    cp "$out/Owner.hs" .
    "${old:-$ghc}" "${flags[@]}" -c Owner.hs
    cd ../rules
    cp ../owner/Owner.hi "$out"/{Orphan,Anchored}.hs .
    "$ghc" "${flags[@]}" -c Orphan.hs
    "$ghc" "${flags[@]}" -Werror=orphans -c Anchored.hs
    cd ../client
    cp ../rules/*.hi "$out"/{Use,Main,Plain}.hs .
    if [[ -n $old ]]; then
        if "$old" "${flags[@]}" -c Use.hs >old.log 2>&1; then exit 1; fi
        grep -Fq 'imported-retained-rule' old.log || { cat old.log; exit 1; }
    fi
    "$ghc" "${flags[@]}" -ddump-rule-firings -c Use.hs >firings
    grep -Fq 'Rule fired: keep/false-orphan (Orphan)' firings || { cat firings; exit 1; }
    grep -Fq 'Rule fired: keep/anchored (Anchored)' firings || { cat firings; exit 1; }
    "$ghc" "${flags[@]}" -c Main.hs
    "$ghc" "${flags[@]}" Main.o Use.o ../rules/{Orphan,Anchored}.o ../owner/Owner.o -o run
    [[ $(./run) == '(11,17,19)' ]]
    "$ghc" "${flags[@]}" -ddump-rule-firings -c Plain.hs >plain.log
    if grep -Eq 'keep/(false-orphan|anchored)' plain.log; then cat plain.log; exit 1; fi
done
