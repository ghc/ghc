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

{-# NOINLINE matches #-}
matches :: Label -> Label -> Bool
matches Here Here = True
matches Here (There _) = False
matches (There _) Here = False
matches (There _) (There _) = True

decorate :: Term -> Label -> Term -> Term
decorate Empty _ _ = Empty
decorate (Atom x) label value = case matches x label of
    True -> value
    False -> Atom x
decorate (Next t) label value = Next (decorate t label value)
HS
cat >"$work/Main.hs" <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Type.Equality ((:~:)(Refl))
import Worker

next :: foreach (t :: Term) -> foreach (l :: Label) -> foreach (v :: Term)
     -> decorate ('Next t) l v :~: 'Next (decorate t l v)
next _ _ _ = Refl

#ifdef UNKNOWN
unknown :: foreach (x :: Label) -> foreach (v :: Term) -> decorate ('Atom x) x v :~: v
unknown _ _ = Refl
#endif

main :: IO ()
main = do
    case next Empty Here Empty of Refl -> putStrLn "Refl"
    case decorate Empty (error "label") (error "replacement") of
        Empty -> pure ()
        _ -> error "expected Empty"
    case decorate (Next (error "field")) (error "label") (error "replacement") of
        Next _ -> pure ()
        _ -> error "expected Next"
    result <- try (evaluate (decorate (Atom (error "label")) Here Empty)) :: IO (Either ErrorCall Term)
    case result of
        Left _ -> putStrLn "strict guard"
        Right _ -> error "expected the guard to fail"
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
    printf 'Refl\nstrict guard\n' | cmp - actual

    if "${ghc[@]}" "$opt" -DUNKNOWN -fforce-recomp -c Main.hs >unknown.log 2>&1 ||
       ! grep -Fq 'error: [GHC-83865]' unknown.log; then
        cat unknown.log >&2
        exit 1
    fi
done
