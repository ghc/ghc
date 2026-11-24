#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

cat >"$work/Tree.hs" <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Tree (Tree(..), focus, known, pairRoundTrip, swapPair, later) where

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
pairRoundTrip (x, y) = Refl

pairRoundTripCase :: foreach (p :: (a, b)) -> swapPair (swapPair p) :~: p
pairRoundTripCase p = case p of
    (_, _) -> Refl

-- Keep the existing open-evaluator route for a local projection.
first :: (a, b) -> a
first (x, _) = x

firstLaw :: forall a b. first '(a, b) :~: a
firstLaw = Refl

later :: Bool -> Bool -> foreach (t :: Tree) -> (Bool, t :~: t)
later False _ _ = (False, Refl)
later True flag (Join _ _) | flag = (False, Refl)
later True _ (Join _ _) = (True, Refl)
later _ _ _ = (True, Refl)

laterCallback :: foreach (f :: Tree -> Bool) -> foreach (t :: Tree) -> f t :~: f t
laterCallback _ (Join _ _) = Refl
laterCallback _ _ = Refl
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
main = case open (Arrow Empty Other) of
    Refl -> case pairRoundTrip (False, EQ) of
        Refl -> do
            let bottom = error "forced the second argument" :: Tree
                node = Join bottom bottom
            if not (fst (later False False bottom)) && fst (later True False node)
                then putStrLn "Refl"
                else fail "unexpected result from the later column"
HS
cat >"$work/Mixed.hs" <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Mixed where

import Data.Type.Equality ((:~:)(Refl))
import Tree (swapPair)

mixed :: foreach (b :: Bool) -> foreach (p :: (Bool, Ordering))
      -> '(b, swapPair (swapPair p)) :~: '(b, p)
mixed False _ = error "no proof for False"
mixed True (_, _) = Refl
HS

for opt in -O0 -O2; do
    mkdir -p "$work/$opt"/{provider,reader}
    cd "$work/$opt/provider"
    cp "$work/Tree.hs" .
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c Tree.hs
    cp Tree.{hi,o} ../reader/
    cd ../reader
    cp "$work/Main.hs" "$work/Mixed.hs" .
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" "$opt" Main.o Tree.o -o check
    ./check >actual
    grep -Fxq Refl actual
    "${ghc[@]}" "$opt" -c Mixed.hs
    if "${ghc[@]}" "$opt" -firrefutable-tuples -fforce-recomp -c Mixed.hs >lazy.log 2>&1 ||
       ! grep -Fq 'Dependent case refinement requires one unguarded match' lazy.log; then
        cat lazy.log >&2
        exit 1
    fi
    if "${ghc[@]}" "$opt" -DWRONG -fforce-recomp -c Main.hs >wrong.log 2>&1 ||
       ! grep -Fq 'error: [GHC-83865]' wrong.log; then
        cat wrong.log >&2
        exit 1
    fi
done
