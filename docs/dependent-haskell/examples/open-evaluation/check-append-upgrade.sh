#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
if [[ $# != 3 ]]; then
    echo "Usage: $0 OLD_GHC NEW_GHC COMMON_LIBDIR" >&2
    exit 2
fi
old=$(realpath "$1")
new=$(realpath "$2")
lib=$(realpath "$3")
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd "$work"

cat >Append.hs <<'HS'
module Append (append) where

type ListOf a = [a]
type Appender a = ListOf a -> ListOf a -> ListOf a

append :: Appender a
append [] ys = ys
append (x : xs) ys = x : append xs ys
HS
cat >StrictAppend.hs <<'HS'
module StrictAppend (append) where

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = ys `seq` (x : append xs ys)
HS
cat >Control.hs <<'HS'
module Control (lengthsEqual) where

lengthsEqual :: [a] -> [a] -> Bool
lengthsEqual xs ys = length xs == length ys
HS
cat >Alias.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module Alias (Appended) where

import qualified Append

type Appended :: forall a. [a] -> [a] -> [a]
type Appended xs ys = Append.append xs ys
HS
cat >Proof.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, PolyKinds, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Proof (cons) where

import Alias
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))

cons :: Proxy x -> Proxy xs -> Proxy ys
     -> Appended (x ': xs) ys :~: (x ': Appended xs ys)
cons _ _ _ = Refl
HS

reject_certificate() {
    "$new" "-B$lib" --show-iface "$1.hi" >"$1.iface"
    if grep -Eq 'SourceTable T:append|type family T:append' "$1.iface"; then
        cat "$1.iface" >&2
        exit 1
    fi
}

snapshot() {
    for stem in "$@"; do
        sha256sum "$stem".{hi,o}
        stat -c '%n %y' "$stem".{hi,o}
    done
}

sources=(Append.hs StrictAppend.hs Control.hs)
targets=(Proof.hs StrictAppend.hs Control.hs)
common=("-B$lib" -package-env - -dcore-lint -dstg-lint -v1 -fhide-source-paths)
for optimization in -O0 -O1; do
    mkdir "$work/$optimization"
    cd "$work/$optimization"
    cp "$work/"*.hs .
    flags=("${common[@]}" "$optimization" -fomit-interface-pragmas -fno-write-if-simplified-core)

    "$old" "${flags[@]}" --make -no-link "${sources[@]}" >old.log 2>&1
    reject_certificate Append
    if "$old" "${flags[@]}" -c Alias.hs >old-proof.log 2>&1 ||
       ! grep -Fq 'GHC-70566' old-proof.log; then
        cat old-proof.log >&2
        exit 1
    fi
    snapshot Control >control.before

    "$new" "${flags[@]}" --make -no-link "${targets[@]}" >upgrade.log 2>&1 || { cat upgrade.log >&2; exit 1; }
    for stem in Append StrictAppend; do
        grep -E "Compiling $stem .*Compiler now checks list append equations" upgrade.log
    done
    "$new" "-B$lib" --show-iface Append.hi >Append.iface
    [[ $(grep -Fc '  FinalCoreSourceTable T:append' Append.iface) == 1 ]]
    reject_certificate StrictAppend
    cmp control.before <(snapshot Control)

    snapshot Append StrictAppend Control Alias Proof >warm.before
    "$new" "${flags[@]}" --make -no-link "${targets[@]}" >warm.log 2>&1 || { cat warm.log >&2; exit 1; }
    cmp warm.before <(snapshot Append StrictAppend Control Alias Proof)

    mkdir no-code
    cp Append.hs no-code/
    cd no-code
    "$new" "${flags[@]}" -fno-code -fwrite-interface -c Append.hs
    reject_certificate Append
    "$new" "${flags[@]}" --make -no-link Append.hs
    "$new" "-B$lib" --show-iface Append.hi >Append.iface
    [[ $(grep -Fc '  FinalCoreSourceTable T:append' Append.iface) == 1 ]]

    printf 'Append upgrade and warm reuse passed at %s\n' "$optimization"
done
