#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

cat >"$work/Worker.hs" <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Worker (Term(..), Label(..), decorate) where

data Term = Empty | Atom Label | Next Term
data Label = Here | There Label

decorate :: Term -> Label -> Label -> Term
decorate Empty left right = Empty
decorate (Atom _) left right = Next (Atom right)
decorate (Next term) left right = Next (decorate term left right)
HS
cat >"$work/Main.hs" <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import Worker

next :: foreach (t :: Term) -> foreach (l :: Label) -> foreach (r :: Label)
     -> decorate ('Next t) l r :~: 'Next (decorate t l r)
next _ _ _ = Refl

#ifdef WRONG
wrong :: foreach (l :: Label) -> foreach (r :: Label) -> decorate 'Empty l r :~: 'Atom r
wrong _ _ = Refl
#endif

main :: IO ()
main = case decorate Empty (error "left label") (error "right label") of
    Empty -> print True
    _ -> print False
HS

for opt in -O0 -O2; do
    base="$work/${opt#-}"
    mkdir -p "$base/provider" "$base/client"
    cd "$base/provider"
    cp "$work/Worker.hs" .
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -c Worker.hs
    cp Worker.{hi,o} ../client/
    cd ../client
    cp "$work/Main.hs" .
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" "$opt" Main.o Worker.o -o check
    ./check >actual
    printf 'True\n' | cmp - actual

    if "${ghc[@]}" "$opt" -DWRONG -fforce-recomp -c Main.hs >wrong.log 2>&1 ||
       ! grep -Fq 'error: [GHC-83865]' wrong.log; then
        cat wrong.log >&2
        exit 1
    fi
done
