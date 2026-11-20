#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

if [[ $# != 3 ]]; then
    echo "Usage: $0 PRE_LEXICAL_RULE_GHC NEW_GHC COMMON_LIBDIR" >&2
    exit 2
fi
old=$(realpath "$1")
new=$(realpath "$2")
lib=$(realpath "$3")
src=$(cd "$(dirname "$0")" && pwd)
tests="$src/../../../../testsuite/tests/dependent/should_run"
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
opts=("-B$lib" -v0 -O2 -fforce-recomp -i -i. -package-env - -dcore-lint -dstg-lint)
for level in 1 2 3; do
    dir="$work/$level"
    mkdir -p "$dir"/{provider,client}
    cd "$dir/provider"
    cp "$src"/{PackageRules,LexicalPackageRules}.hs .
    "$new" "${opts[@]}" "-fwrite-if-compression=$level" -c PackageRules.hs LexicalPackageRules.hs

    cd "$dir/client"
    cp ../provider/*.hi "$tests/NativePackageRuleClient.hs" .
    if "$old" "${opts[@]}" -c NativePackageRuleClient.hs >diagnostic 2>&1; then
        echo "The old compiler accepted lexical package rules at compression level $level." >&2
        exit 1
    fi
    grep -Fq '[GHC-47808]' diagnostic && grep -Fq 'mismatched interface file versions' diagnostic &&
        grep -Fq 'lexical-rule' diagnostic || { cat diagnostic >&2; exit 1; }
    "$new" "${opts[@]}" -c NativePackageRuleClient.hs
    "$new" "-B$lib" -v0 NativePackageRuleClient.o ../provider/{PackageRules,LexicalPackageRules}.o -o run-new
    ./run-new >actual
    grep -Fxq '((4,3,6),5,1,7,5,6,True)' actual

    cat >Main.hs <<'HS'
module Main where
import PackageRules (hide, open)
main :: IO ()
main = print (open (hide 5))
HS
    "$old" "${opts[@]}" -ddump-rule-firings -c Main.hs >firings
    grep -Fq 'Rule fired: open/hide (PackageRules)' firings
    "$old" "-B$lib" -v0 Main.o ../provider/PackageRules.o -o run-old
    test "$(./run-old)" = 5
    echo "Package rule interfaces passed at compression level $level."
done
