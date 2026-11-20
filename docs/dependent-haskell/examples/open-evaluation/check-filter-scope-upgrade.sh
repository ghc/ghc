#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
if [[ $# != 4 ]]; then
    echo "Usage: $0 SCOPE_GHC FILTER_GHC COMBINED_GHC COMMON_LIBDIR" >&2
    exit 2
fi
scoped=$(realpath "$1")
filtered=$(realpath "$2")
new=$(realpath "$3")
lib=$(realpath "$4")
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd "$work"
cat >filter-body <<'HS'
select :: (a -> Bool) -> [a] -> [a]
select _ [] = []
select p (x:xs) = if p x then x : select p xs else select p xs
HS
cat >cases <<'HS'
data Gate k where
    Plain :: Bool -> Gate k
    Other :: Gate k
type Pick (k :: Type) (d :: Maybe (Gate k)) = case d of
    Just (Plain True) -> 'Nothing
    Just _ -> 'Nothing
    Nothing -> 'Nothing
HS
printf 'module Control where\ncontrol :: Bool\ncontrol = True\n' >Control.hs
for mod in Mixed Cases Filter; do
    {
      printf '{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, TypeFamilies #-}\nmodule %s where\nimport Data.Kind (Type)\n' "$mod"
      if [[ "$mod" != Cases ]]; then cat filter-body; fi
      if [[ "$mod" != Filter ]]; then cat cases; fi
    } >"$mod.hs"
done
opts=("-B$lib" -O1 -i -i. -package-env - -dcore-lint -dstg-lint -v1 -fhide-source-paths --make -no-link)
for kind in scope filter; do
    mkdir "$work/$kind"
    cd "$work/$kind"
    cp "$work"/*.hs .
    old=$scoped
    expected='Compiler now checks guarded list filters'
    missing=Filter
    already=Cases
    if [[ $kind == filter ]]; then
        old=$filtered
        expected='Compiler now checks dependent type case captures'
        missing=Cases
        already=Filter
    fi
    "$old" "${opts[@]}" Control.hs Filter.hs Cases.hs Mixed.hs >initial.log 2>&1 || { cat initial.log; exit 1; }
    "$new" "${opts[@]}" Control.hs Filter.hs Cases.hs Mixed.hs >upgrade.log 2>&1 || { cat upgrade.log; exit 1; }
    for module in "$missing" Mixed; do
        grep -Fq "Compiling $module [$expected]" upgrade.log || { cat upgrade.log; exit 1; }
    done
    if grep -Eq "Compiling ($already|Control) " upgrade.log; then cat upgrade.log; exit 1; fi
    "$new" "${opts[@]}" Control.hs Filter.hs Cases.hs Mixed.hs >warm.log 2>&1 || { cat warm.log; exit 1; }
    if grep -Fq 'Compiling ' warm.log; then cat warm.log; exit 1; fi
    echo "$kind precursor: rebuilt $missing and Mixed once; reused $already and Control."
done
