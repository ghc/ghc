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

cat >WordField.source <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, MagicHash, TypeFamilies #-}
#include "MachDeps.h"
module WordField where

import GHC.Exts (Word(W#), Word#)

type family MatchWord (w :: Word) :: () where
    MatchWord ('W# x) = '()

type family MatchWildcard (w :: Word) :: () where
    MatchWildcard ('W# _) = '()

type family MatchAnnotation (w :: Word) :: () where
    MatchAnnotation ('W# (x :: Word#)) = '()

type family IsFullWidth (w :: Word) :: Bool where
#if WORD_SIZE_IN_BITS == 64
    IsFullWidth ('W# 18446744073709551615##) = 'True
#elif WORD_SIZE_IN_BITS == 32
    IsFullWidth ('W# 4294967295##) = 'True
#else
#error unexpected machine word size
#endif
    IsFullWidth ('W# _) = 'False

type Zero = 'W# 0##
#if WORD_SIZE_IN_BITS == 64
type FullWidth = 'W# 18446744073709551615##
type OneBelow = 'W# 18446744073709551614##
type Wrapped = 'W# 18446744073709551616##
#else
type FullWidth = 'W# 4294967295##
type OneBelow = 'W# 4294967294##
type Wrapped = 'W# 4294967296##
#endif
HS

cat >Client.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
#include "MachDeps.h"
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import WordField

named :: MatchWord FullWidth :~: '()
named = Refl

wildcard :: MatchWildcard FullWidth :~: '()
wildcard = Refl

annotation :: MatchAnnotation FullWidth :~: '()
annotation = Refl

maximum :: IsFullWidth FullWidth :~: 'True
maximum = Refl

oneBelow :: IsFullWidth OneBelow :~: 'False
oneBelow = Refl

wrapped :: Wrapped :~: Zero
wrapped = Refl

main :: IO ()
main = case (named, wildcard, annotation, Main.maximum, oneBelow, wrapped) of
    (Refl, Refl, Refl, Refl, Refl, Refl) -> print WORD_SIZE_IN_BITS
HS

cat >Unknown.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module Unknown where

import Data.Type.Equality ((:~:)(Refl))
import WordField (IsFullWidth)

unknown :: forall (w :: Word). IsFullWidth w :~: 'False
unknown = Refl
HS

cat >WrongWord.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, CPP, DataKinds, ExperimentalDependentTypes, MagicHash, TypeFamilies #-}
#include "MachDeps.h"
module WrongWord where

import GHC.Exts (Word(W#), Word#)

type WrongSigned = 'W# 3#
HS

cat >WrongInt.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, MagicHash #-}
module WrongInt where

import GHC.Exts (Int(I#))

type Wrong = 'I# 3##
HS

cat >ForallControl.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, CPP, DataKinds, ExperimentalDependentTypes, MagicHash, TypeFamilies #-}
#include "MachDeps.h"
module ForallControl where

import GHC.Exts (Word(W#), Word#)

ordinary :: forall (w :: Word#). ()
ordinary = ()
HS

cat >Escapes.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, MagicHash, TypeFamilies #-}
module Escapes where

import GHC.Exts (Word(W#))

type family Escapes (w :: Word) :: Word where
    Escapes ('W# x) = 'W# x
HS

sed 's/ExperimentalDependentTypes/NoExperimentalDependentTypes/' WordField.source >Disabled.hs

ghc=("$compiler" "-B$libdir" -XGHC2024 -v0 -package-env - -i -i. -fforce-recomp
     -fomit-interface-pragmas -fno-write-if-simplified-core -dcore-lint -dstg-lint)
width=$("$compiler" "-B$libdir" --info | sed -n 's/.*("target word size in bits","\([0-9]*\)").*/\1/p')
test "$width" = 64 || test "$width" = 32

reject() {
    local source=$1 diagnostic=$2 message=$3
    if "${ghc[@]}" -fno-code -c "$source" >reject.log 2>&1 ||
       ! grep -Fq "[$diagnostic]" reject.log ||
       ! grep -Fq "$message" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

for opt in -O0 -O2; do
    mkdir -- "$opt"
    (
        cd -- "$opt"
        ghc+=("$opt")
        cp ../WordField.source WordField.hs
        "${ghc[@]}" -c WordField.hs
        rm WordField.hs
        "${ghc[@]}" -c ../Client.hs -ohi Client.hi -o Client.o
        "${ghc[@]}" Client.o WordField.o -o client
        test "$(./client)" = "$width"
        "$compiler" "-B$libdir" --show-iface WordField.hi >WordField.iface
        grep -Fq 'experimental-word-prim-index' WordField.iface

        reject ../Unknown.hs GHC-83865 'IsFullWidth w'
        reject ../WrongWord.hs GHC-83865 'Expected kind `Word#'
        reject ../WrongInt.hs GHC-83865 'has kind `GHC.Internal.Prim.Word#'
        reject ../ForallControl.hs GHC-83865 'Expected a lifted type'
        reject ../Escapes.hs GHC-95274 'primitive field binder'
        printf 'Word# family interface client and %s-bit target width passed at %s\n' "$width" "$opt"
    )
done
reject Disabled.hs GHC-51819 'cannot be promoted'
