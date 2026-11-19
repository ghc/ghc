#!/usr/bin/env bash
set -euo pipefail
g=("$1" "-B$2" -v0 -package-env - -dcore-lint -dstg-lint)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
cat >Worker.hs <<'HS'
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Worker (negateAll) where

#ifdef REWRITE_BUILD
import GHC.Exts (build)
import Sentinel (sentinel)
#endif

{-# NOINLINE negateAll #-}
negateAll :: [Bool] -> [Bool]
negateAll xs = map not xs

#ifdef REWRITE_BUILD
{-# RULES "replace Bool list builder" [~1]
    forall (g :: forall r. (Bool -> r -> r) -> r -> r).
    build g = sentinel #-}
#endif
HS
cat >Sentinel.hs <<'HS'
module Sentinel (sentinel) where

{-# OPAQUE sentinel #-}
sentinel :: [Bool]
sentinel = True : []
HS
cat >Main.hs <<'HS'
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import Worker (negateAll)

main :: IO ()
main = do
    print (negateAll [])
    case negateAll (True : error "tail") of
      x : _ -> print x
      [] -> fail "expected a cons"
    print (take 3 (negateAll (cycle [True, False])))
    spine <- try (evaluate (null (negateAll (error "spine")))) :: IO (Either ErrorCall Bool)
    print (either (const Nothing) Just spine)
    print (null (negateAll (error "element" : [])))
HS
"${g[@]}" -O0 -c Sentinel.hs
for mode in normal plain helper; do
    opts=()
    case $mode in
      plain) opts=(-O0);;
      helper) opts=(-DREWRITE_BUILD);;
    esac
    "${g[@]}" -O1 -cpp -fforce-recomp -Wno-inline-rule-shadowing "${opts[@]}" -ddump-rule-firings -c Worker.hs >"$mode.rules"
    "${g[@]}" -O0 -fforce-recomp -c Main.hs
    "${g[@]}" Main.o Worker.o Sentinel.o -o "$mode.run"
    "./$mode.run" >"$mode.out"
    echo "$mode"
    cat "$mode.out"
    if [[ $mode != plain ]]; then grep -F 'Rule fired: map (' "$mode.rules"; fi
done
printf '%s\n' '[]' False '[False,True,False]' Nothing False >normal.expected
diff -u normal.expected normal.out
cmp normal.out plain.out
grep -F 'replace Bool list builder' helper.rules
printf '%s\n' '[True]' True '[True]' 'Just False' False >helper.expected
diff -u helper.expected helper.out
