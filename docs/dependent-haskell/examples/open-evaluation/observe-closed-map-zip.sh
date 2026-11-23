#!/usr/bin/env bash
set -euo pipefail
g=("$1" "-B$2" -v0 -package-env - -dcore-lint -dstg-lint)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
mkdir support
cd support
cat >Support.hs <<'HS'
module Support (label, replacement, report) where

{-# OPAQUE label #-}
label :: (Bool, ()) -> Char
label _ = error "empty traversal forced its callback"

{-# OPAQUE replacement #-}
replacement :: String
replacement = "X"

{-# OPAQUE report #-}
report :: String -> IO ()
report = print
HS
"${g[@]}" -O2 -fno-enable-rewrite-rules -c Support.hs
cd ..
cat >Main.hs <<'HS'
{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables #-}
module Main where
import Support (label, replacement, report)
#ifdef REPLACE_BUILDER
import GHC.Exts (build)
{-# RULES "closed/buildChar" [~1]
    forall (g :: forall r. (Char -> r -> r) -> r -> r).
    build g = replacement #-}
#endif

{-# OPAQUE observe #-}
observe :: String
observe = map label (zip ([] :: [Bool]) ([] :: [()]))

main :: IO ()
main = report observe
HS
for mode in stock disabled helper helper-disabled helper-disabled-no-specialise; do
    opts=()
    case "$mode" in helper*) opts+=(-DREPLACE_BUILDER);; esac
    case "$mode" in *disabled*) opts+=(-fno-enable-rewrite-rules);; esac
    case "$mode" in *no-specialise) opts+=(-fno-specialise);; esac
    mkdir "$mode"
    cp support/Support.hi Main.hs "$mode/"
    (
        cd "$mode"
        "${g[@]}" -i -i. -O2 "${opts[@]}" -ddump-rule-firings \
            -Wno-inline-rule-shadowing -Wno-orphans -c Main.hs >rules
        "${g[@]}" Main.o ../support/Support.o -o observe
        ./observe >actual
    )
    printf '%s\t%s\n' "$mode" "$(cat "$mode/actual")"
done
for rule in zip map fold/build closed/buildChar; do
    grep -Fq "Rule fired: $rule (" helper/rules
done
