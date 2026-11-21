#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

if [[ $# != 2 ]]; then
    echo "Usage: $0 GHC LIBDIR" >&2
    exit 2
fi
compiler=$(realpath "$1")
libdir=$(realpath "$2")
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd "$work"

cat >IntField.source <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, MagicHash, NegativeLiterals, TypeFamilies #-}
module IntField where

import GHC.Exts (Int(I#), Int#)

type family MatchInt (i :: Int) :: () where
    MatchInt ('I# x) = '()

type family MatchWildcard (i :: Int) :: () where
    MatchWildcard ('I# _) = '()

type family MatchAnnotation (i :: Int) :: () where
    MatchAnnotation ('I# (x :: Int#)) = '()

type family IsMinusThree (i :: Int) :: Bool where
    IsMinusThree ('I# (-3#)) = 'True
    IsMinusThree ('I# x) = 'False

type Three = 'I# 3#
type MinusThree = 'I# (-3#)
type Wrap64 = 'I# 18446744073709551613#
type Wrap32 = 'I# 4294967293#
HS

cat >Client.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
#include "MachDeps.h"
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import IntField

named :: MatchInt Three :~: '()
named = Refl

wildcard :: MatchWildcard Three :~: '()
wildcard = Refl

annotation :: MatchAnnotation Three :~: '()
annotation = Refl

wrap64 :: MinusThree :~: Wrap64
wrap64 = Refl

#if WORD_SIZE_IN_BITS == 64
wrap32 :: IsMinusThree Wrap32 :~: 'False
#elif WORD_SIZE_IN_BITS == 32
wrap32 :: IsMinusThree Wrap32 :~: 'True
#else
#error Unexpected target Int# width
#endif
wrap32 = Refl

main :: IO ()
main = case (named, wildcard, annotation, wrap64, wrap32) of
    (Refl, Refl, Refl, Refl, Refl) -> print WORD_SIZE_IN_BITS
HS

cat >Neutral.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module Neutral where

import Data.Type.Equality ((:~:)(Refl))
import IntField (MatchInt)

neutral :: forall (i :: Int). MatchInt i :~: '()
neutral = Refl
HS

cat >ForallControl.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, MagicHash, ExplicitForAll #-}
module ForallControl where

import GHC.Exts (Int#)

ordinary :: forall (i :: Int#). ()
ordinary = ()
HS

sed 's/ExperimentalDependentTypes/NoExperimentalDependentTypes/' IntField.source >Disabled.hs
ghc=("$compiler" "-B$libdir" -XGHC2024 -v0 -package-env - -fforce-recomp -fhide-source-paths -dcore-lint -dstg-lint)
width=$("$compiler" "-B$libdir" --info | sed -n 's/.*("target word size in bits","\([0-9]*\)").*/\1/p')
for opt in -O0 -O2; do
    mkdir "$work/$opt"
    (
        cd "$work/$opt"
        cp ../IntField.source IntField.hs
        flags=("${ghc[@]}" "$opt")
        if [[ $opt == -O0 ]]; then
            flags+=(-fomit-interface-pragmas -fno-write-if-simplified-core)
        fi
        "${flags[@]}" -c IntField.hs
        rm IntField.hs
        "${flags[@]}" -c ../Client.hs -ohi Client.hi -o Client.o
        "${flags[@]}" Client.o IntField.o -o client
        test "$(./client)" = "$width"
        "$compiler" "-B$libdir" --show-iface IntField.hi >IntField.iface
        grep -Fq 'experimental-int-prim-index' IntField.iface

        if "${flags[@]}" -fno-code -c ../Neutral.hs >neutral.log 2>&1 ||
           ! grep -Fq "Expected: MatchInt i :~:" neutral.log; then
            cat neutral.log >&2
            exit 1
        fi
        printf 'Int# family interface client and %s-bit target width passed at %s\n' "$width" "$opt"
    )
done
if "${ghc[@]}" -fno-code Disabled.hs >disabled.log 2>&1 ||
   ! grep -Fq 'GHC-18872' disabled.log ||
   ! grep -Fq 'GHC-51819' disabled.log; then
    cat disabled.log >&2
    exit 1
fi
if "${ghc[@]}" -fno-code ForallControl.hs >forall.log 2>&1 ||
   ! grep -Fq 'GHC-83865' forall.log; then
    cat forall.log >&2
    exit 1
fi
