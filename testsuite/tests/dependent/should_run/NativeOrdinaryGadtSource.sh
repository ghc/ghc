#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

cat >"$work/Tree.hs" <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Tree (Tree(..), focus, known) where

import Data.Type.Equality ((:~:)(Refl))

data Tree where
    Empty, Other :: Tree
    Arrow, Join  :: Tree -> Tree -> Tree

focus :: Tree -> Tree
focus Empty = Empty
focus Other = Other
focus (Arrow x _) = x
focus (Join l r) = Join (focus l) (focus r)

known :: focus ('Join ('Arrow 'Empty 'Other) 'Other) :~: 'Join 'Empty 'Other
known = Refl

swapSum :: Either a b -> Either b a
swapSum (Left x) = Right x
swapSum (Right y) = Left y

sumRoundTrip :: foreach (s :: Either a b) -> swapSum (swapSum s) :~: s
sumRoundTrip (Left _) = Refl
sumRoundTrip (Right _) = Refl

swapPair :: (a, b) -> (b, a)
swapPair (x, y) = (y, x)

pairRoundTrip :: foreach (p :: (a, b)) -> swapPair (swapPair p) :~: p
pairRoundTrip p = case p of
    (,) _ _ -> Refl

-- Keep the existing open-evaluator route for a local projection.
first :: (a, b) -> a
first (x, _) = x

firstLaw :: forall a b. first '(a, b) :~: a
firstLaw = Refl
HS
cat >"$work/Main.hs" <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import Tree

open :: foreach (x :: Tree) -> focus ('Join ('Arrow x 'Empty) 'Other) :~: 'Join x 'Other
open _ = Refl

#ifdef WRONG
wrong :: foreach (x :: Tree) -> focus ('Arrow x 'Empty) :~: 'Join x 'Other
wrong _ = Refl
#endif

main :: IO ()
main = case open (Arrow Empty Other) of Refl -> putStrLn "Refl"
HS

for opt in -O0 -O2; do
    mkdir -p "$work/$opt"/{provider,reader}
    cd "$work/$opt/provider"
    cp "$work/Tree.hs" .
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c Tree.hs
    cp Tree.{hi,o} ../reader/
    cd ../reader
    cp "$work/Main.hs" .
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" "$opt" Main.o Tree.o -o check
    ./check >actual
    grep -Fxq Refl actual
    if "${ghc[@]}" "$opt" -DWRONG -fforce-recomp -c Main.hs >wrong.log 2>&1 ||
       ! grep -Fq 'error: [GHC-83865]' wrong.log; then
        cat wrong.log >&2
        exit 1
    fi
done
