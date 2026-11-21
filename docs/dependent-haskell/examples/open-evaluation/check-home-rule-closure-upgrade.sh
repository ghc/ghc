#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
if (( $# != 4 )); then
    echo "Usage: $0 OLD_GHC OLD_LIB NEW_GHC NEW_LIB" >&2
    exit 2
fi
old=$(realpath "$1")
old_lib=$(realpath "$2")
new=$(realpath "$3")
new_lib=$(realpath "$4")
flags=(-O2 -v1 -package-env - -dcore-lint -dstg-lint -fdiagnostics-color=never)
reason='Compiler now tracks home rule dependencies for final Core equations'
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd "$work"

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


cat > Ordinary.hs <<'HS'
module Ordinary (runtime) where
import FoldProvider (fold)

runtime :: [Int] -> Int
runtime = fold (+) 0
HS

build() {
    local compiler=$1 lib=$2 log=$3
    shift 3
    "$compiler" "-B$lib" "${flags[@]}" --make -no-link "$@" >"$log" 2>&1
}

check() {
    if ! grep -Fq "$1" "$2"; then cat "$2" >&2; exit 1; fi
}

no_compile() {
    if grep -Fq 'Compiling ' "$1"; then cat "$1" >&2; exit 1; fi
}

for mode in code nocode; do
    mkdir "$mode"
    cp ./*.hs "$mode/"
    (
        cd "$mode"
        options=()
        if [[ $mode == nocode ]]; then options=(-fno-code -fwrite-interface); fi
        build "$old" "$old_lib" old.log "${options[@]}" RuleProof.hs SafeConsumer.hs
        cat >> RuleHelper.hs <<'HS'

{-# RULES "late/home-fold-empty" [0] fold lateStep False [] = True #-}
HS
        build "$old" "$old_lib" stale.log "${options[@]}" RuleProof.hs SafeConsumer.hs
        check 'Compiling RuleHelper' stale.log
        if grep -Fq 'Compiling RuleProof' stale.log; then cat stale.log >&2; exit 1; fi
        if build "$new" "$new_lib" upgrade.log "${options[@]}" RuleProof.hs SafeConsumer.hs; then
            cat upgrade.log >&2
            exit 1
        fi
        check "$reason" upgrade.log
        check '[GHC-83605]' upgrade.log
        printf 'Old %s home proof rechecked and rejected.\n' "$mode"
    )
done

mkdir rules-off ordinary
cp ./*.hs rules-off/
cp ./*.hs ordinary/
(
    cd rules-off
    build "$old" "$old_lib" old.log -fno-enable-rewrite-rules RuleProof.hs SafeConsumer.hs
    sha256sum ./*.{hi,o} > warm.sha
    build "$new" "$new_lib" upgrade.log -fno-enable-rewrite-rules RuleProof.hs SafeConsumer.hs
    no_compile upgrade.log
    sha256sum --quiet -c warm.sha
    printf '%s\n' 'Rules-off interfaces and objects reused.'
)
(
    cd ordinary
    build "$old" "$old_lib" old.log Ordinary.hs
    build "$new" "$new_lib" upgrade.log Ordinary.hs
    check "$reason" upgrade.log
    "$new" "-B$new_lib" --show-iface Ordinary.hi > ordinary.iface
    check 'home-rule-closures-reviewed-v1' ordinary.iface
    sha256sum Ordinary.{hi,o} > warm.sha
    build "$new" "$new_lib" warm.log Ordinary.hs
    no_compile warm.log
    sha256sum --quiet -c warm.sha
    printf '%s\n' 'Ordinary use rechecked once and reused on the next build.'
)
