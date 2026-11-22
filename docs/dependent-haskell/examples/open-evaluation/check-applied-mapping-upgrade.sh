#!/usr/bin/env bash
set -euo pipefail
if [[ $# != 4 ]]; then
    echo "Usage: $0 OLD_GHC OLD_LIBDIR NEW_GHC NEW_LIBDIR" >&2
    exit 2
fi
old=("$(realpath "$1")" "-B$(realpath "$2")")
new=("$(realpath "$3")" "-B$(realpath "$4")")
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
flags=(-O0 -v0 -package-env - -i -i. -fomit-interface-pragmas -fno-write-if-simplified-core)

for mode in polymorphic ground; do
    mkdir -p "$out/$mode"/{provider,reader}
    cd "$out/$mode/provider"
    if [[ $mode == polymorphic ]]; then
        cp "$src/These.hs" .
    else
        cat >These.hs <<'HS'
{-# LANGUAGE FlexibleInstances #-}
module These where
import Data.Kind (Type)
data These e a = This e | That a | These e a
instance Functor (These Type) where
    fmap _ (This e) = This e
    fmap f (That x) = That (f x)
    fmap f (These e x) = These e (f x)
HS
    fi
    "${old[@]}" "${flags[@]}" --make -no-link These.hs
    "${old[@]}" --show-iface These.hi >old.iface
    if grep -q 'DFunId SourceMethods.*fmap' old.iface; then
        echo 'Use a comparison compiler from before general mapping workers.' >&2
        exit 1
    fi
    if [[ $mode == polymorphic ]]; then
        grep -q 'checked-polymorphic-container-mapping-v1' old.iface
    fi
    "${new[@]}" "${flags[@]}" -v1 --make -no-link These.hs >review.log 2>&1
    grep -Fq 'Compiler now checks mapping methods for applied receivers' review.log
    "${new[@]}" --show-iface These.hi >new.iface
    grep -q 'DFunId SourceMethods.*fmap' new.iface
    cp These.hi reviewed.hi
    "${new[@]}" "${flags[@]}" -v1 --make -no-link These.hs >again.log 2>&1
    if grep -Fq 'Compiling These' again.log; then exit 1; fi
    cmp reviewed.hi These.hi
    cd ../reader
    cp ../provider/These.hi .
    cat >Ordinary.hs <<'HS'
module Ordinary where
import These
value :: These e Bool
value = That True
HS
    "${old[@]}" "${flags[@]}" -fno-code -c Ordinary.hs
    if [[ $mode == polymorphic ]]; then
        "${new[@]}" "${flags[@]}" -fexternal-interpreter -c "$src/TheseMapping.hs" -outputdir .
    fi
done
