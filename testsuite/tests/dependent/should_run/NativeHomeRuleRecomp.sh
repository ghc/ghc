#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -O2 -v1 -package-env - -dcore-lint -dstg-lint -fdiagnostics-color=never)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"

# Produce a final Core table even when the caller only writes an interface.
cat > FoldProvider.hs <<'HS'
{-# OPTIONS_GHC -fobject-code -fwrite-source-equations #-}
module FoldProvider (fold) where

{-# NOINLINE fold #-}
fold :: (a -> b -> b) -> b -> [a] -> b
fold _ seed [] = seed
fold step seed (x : xs) = step x (fold step seed xs)
HS
cat > RuleHelper.hs <<'HS'
{-# LANGUAGE Trustworthy #-}
module RuleHelper (value, delayed) where

import FoldProvider (fold)

value :: Int
value = 7

{-# NOINLINE private #-}
private :: (Int, Int)
private = (1, 2)

{-# NOINLINE lateStep #-}
lateStep :: Bool -> Bool -> Bool
lateStep _ result = result

{-# INLINE delayed #-}
delayed :: Bool -> [Bool] -> Bool
delayed seed xs = fold lateStep seed xs
HS
cat > RuleBridge.hs <<'HS'
{-# LANGUAGE Safe #-}
module RuleBridge (value) where
import RuleHelper (value)
HS
cat > SafeConsumer.hs <<'HS'
{-# LANGUAGE Safe #-}
module SafeConsumer (value) where
import RuleBridge (value)
HS
cat > RuleProof.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
{-# LANGUAGE LiberalTypeSynonyms, Modifiers, TypeOperators #-}
module RuleProof (Callback, proof, observed) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import FoldProvider (fold)
import GHC.Exts (Matchability(Matchable), U)
import RuleBridge (value)

type Callback (f :: Type %('Matchable) -> Type) = f Bool

type Fold :: (a %U -> b %U -> b) -> b -> [a] -> b
type Fold step seed xs = fold step seed xs

proof :: Proxy step -> Proxy seed -> Fold step seed '[] :~: seed
proof _ _ = Refl

observed :: Int
observed = value
HS

build() {
    local log=$1
    shift
    if ! "${ghc[@]}" --make "$@" RuleProof.hs SafeConsumer.hs >"$log" 2>&1; then
        cat "$log" >&2
        exit 1
    fi
}
no_compile() {
    if grep -Fq 'Compiling ' "$1"; then cat "$1" >&2; exit 1; fi
}
require() {
    if ! grep -Eq "$1" "$2"; then cat "$2" >&2; exit 1; fi
}
for mode in code nocode; do
    mkdir "$mode"
    cp ./*.hs "$mode/"
    (
        cd "$mode"
        flags=(-no-link)
        if [[ $mode == nocode ]]; then flags=(-fno-code -fwrite-interface); fi
        build initial.log "${flags[@]}"
        build noop.log "${flags[@]}"
        no_compile noop.log
        "${ghc[@]}" --show-iface RuleProof.hi > proof-before.iface
        "${ghc[@]}" --show-iface RuleHelper.hi > helper-before.iface
        require 'liberal-synonym-callback-v1' proof-before.iface
        require 'home-rule-closures-reviewed-v1' proof-before.iface
        require 'homeRuleInterface RuleBridge ' proof-before.iface
        require 'homeRuleInterface FoldProvider ' proof-before.iface
        before=$(sed -n 's/^ *interface hash: //p' helper-before.iface)
        require "homeRuleInterface RuleHelper $before" proof-before.iface
        cp SafeConsumer.hi safe-before.hi

        sed 's/NOINLINE private/INLINE private/' RuleHelper.hs > helper.hs
        mv helper.hs RuleHelper.hs
        build changed.log "${flags[@]}"
        require 'Compiling RuleHelper .*Source file changed' changed.log
        require 'Compiling RuleProof .*RuleHelper changed \(raw\)' changed.log
        if grep -Eq 'Compiling (RuleBridge|SafeConsumer|FoldProvider)' changed.log; then
            cat changed.log >&2
            exit 1
        fi
        "${ghc[@]}" --show-iface RuleHelper.hi > helper-after.iface
        "${ghc[@]}" --show-iface RuleProof.hi > proof-after.iface
        after=$(sed -n 's/^ *interface hash: //p' helper-after.iface)
        [[ -n "$before" && -n "$after" && "$before" != "$after" ]]
        [[ $(grep -F 'ABI hash:' helper-before.iface) == "$(grep -F 'ABI hash:' helper-after.iface)" ]]
        require 'liberal-synonym-callback-v1' proof-after.iface
        require 'home-rule-closures-reviewed-v1' proof-after.iface
        require "homeRuleInterface RuleHelper $after" proof-after.iface
        cmp safe-before.hi SafeConsumer.hi
        if [[ $mode == nocode ]]; then test ! -e RuleProof.o; fi
        build final-noop.log "${flags[@]}"
        no_compile final-noop.log

        if [[ $mode == code ]]; then
            cat >> RuleHelper.hs <<'HS'

{-# RULES "late/home-fold-empty" [0] fold lateStep False [] = True #-}
HS
            if "${ghc[@]}" --make "${flags[@]}" RuleProof.hs SafeConsumer.hs > rule.log 2>&1; then
                cat rule.log >&2
                exit 1
            fi
            require 'Compiling RuleProof ' rule.log
            require 'RuleProof.hs:.*error: \[GHC-83605\]' rule.log
        fi
    )
done
