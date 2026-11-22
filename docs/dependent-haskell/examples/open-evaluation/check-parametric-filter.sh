#!/usr/bin/env bash
set -euo pipefail

if [[ $# != 2 ]]; then
    echo "Usage: bash $0 GHC LIBDIR" >&2
    exit 2
fi

area=$(cd -- "$(dirname -- "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
export LC_ALL=C
ghc=("$(realpath "$1")" "-B$(realpath "$2")" -v0 -package-env - -i -i.
     -dcore-lint -dstg-lint -fdiagnostics-color=never)

for opt in 0 2; do
    mkdir "$work/O$opt"
    (
        cd "$work/O$opt"
        cp "$area"/ParametricFilter{,Adapters}.hs .
        # Keep dynamic objects for closed evaluation after hiding the sources.
        "${ghc[@]}" "-O$opt" -dynamic-too -fomit-interface-pragmas -fno-write-if-simplified-core \
            -c ParametricFilterAdapters.hs ParametricFilter.hs
        mv ParametricFilterAdapters.hs adapters.source
        mv ParametricFilter.hs proof.source

        cat > Main.hs <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes, RankNTypes, TypeApplications #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import ParametricFilter (filterNaturality)
import ParametricFilterAdapters (boolToOrdering, isGT)

type FilterLike = forall a. (a -> Bool) -> [a] -> [a]

dropFirst :: FilterLike
dropFirst p [] = p `seq` []
dropFirst p (_ : xs) = p `seq` xs

forcePredicate :: FilterLike
forcePredicate p _ = p `seq` []

sides :: FilterLike -> (b -> Bool) -> (a -> b) -> [a] -> ([b], [b])
sides r p h xs = (r p (map h xs), map h (r (p . h) xs))

proved :: [Bool] -> Bool
proved xs = case filterNaturality xs of Refl -> True

main :: IO ()
main = do
    xs <- newIORef [True, False, True] >>= readIORef
    print (proved [] && proved xs)
    print (sides filter isGT boolToOrdering xs)
    print (sides dropFirst isGT boolToOrdering xs)
    let (left, right) = sides forcePredicate
            (error "predicate bottom" :: Bool -> Bool) id []
    result <- try @ErrorCall (evaluate (null left))
    case result of
        Left _ | null right -> putStrLn "bottom predicate: bottom, empty"
        _ -> fail "expected a difference in demand"
HS
        "${ghc[@]}" "-O$opt" -c Main.hs
        "${ghc[@]}" Main.o ParametricFilter.o ParametricFilterAdapters.o -o run
        ./run > actual
        cat > expected <<'OUT'
True
([GT,GT],[GT,GT])
([LT,GT],[LT,GT])
bottom predicate: bottom, empty
OUT
        diff -u expected actual

        cp "$area/ParametricFilterByType.hs" .
        "${ghc[@]}" "-O$opt" -c ParametricFilterByType.hs
        if "${ghc[@]}" "-O$opt" -DWRONG -fforce-recomp -c ParametricFilterByType.hs > by-type.log 2>&1; then
            exit 1
        fi
        grep -Fq 'error: [GHC-83865]' by-type.log
        grep -Fq "Couldn't match type: '[]" by-type.log
        grep -Fq "with: '[GT]" by-type.log
        grep -Fq 'In the expression: Refl' by-type.log

        cat > NoDerivation.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, ExperimentalDependentTypes, RankNTypes #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NoDerivation where
import Data.Type.Equality ((:~:)(Refl))
import ParametricFilter (FilterNaturality)

claimed :: FilterNaturality
claimed _ = Refl
HS
        if "${ghc[@]}" "-O$opt" -c NoDerivation.hs > rigid.log 2>&1; then exit 1; fi
        grep -Fq 'NoDerivation.hs:8:13: error: [GHC-83865]' rigid.log
        grep -Fq 'In the expression: Refl' rigid.log

        cat > RuntimeBinder.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, RankNTypes #-}
module RuntimeBinder where

type Retained = foreach (r :: forall a. (a -> Bool) -> [a] -> [a]) -> ()
HS
        if "${ghc[@]}" "-O$opt" -c RuntimeBinder.hs > runtime-binder.log 2>&1; then exit 1; fi
        grep -Fq 'error: [GHC-83605]' runtime-binder.log
        grep -Fq 'unsupported function domain' runtime-binder.log
    )
done
