#!/usr/bin/env bash
set -euo pipefail

work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cp NativeTypeLetQuote.hs "$work"
cd "$work"
cat > Main.hs <<'HS'
{-# LANGUAGE Haskell2010, NoExperimentalDependentTypes #-}
module Main where
import NativeTypeLetQuote (result)
main :: IO ()
main = print result
HS
cat > Disabled.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, TemplateHaskell #-}
module Disabled where
import Language.Haskell.TH
type Bad = $(do b <- newName "b"; letT b (conT ''Int) (varT b))
HS
cat > BadRhs.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TemplateHaskell #-}
module BadRhs where
import Language.Haskell.TH
type Bad = $(do b <- newName "b"; letT b (appT (conT ''Int) (conT ''Bool)) (conT ''Int))
HS
ghc=("$1" "-B$2" -v0 -package-env - -dcore-lint -dstg-lint)
for opt in -O0 -O2; do
    for mode in -fno-external-interpreter -fexternal-interpreter; do
        out="$work/$opt$mode"
        mkdir -p "$out"
        "${ghc[@]}" "$opt" "$mode" -fomit-interface-pragmas -fforce-recomp -odir "$out" -hidir "$out" -c NativeTypeLetQuote.hs
        cp Main.hs "$out"
        (
            cd "$out"
            "${ghc[@]}" -i -i. -c Main.hs
            "${ghc[@]}" Main.o NativeTypeLetQuote.o -o observe
            test "$(./observe)" = "(Right (Just True),Just 'q',(Just 3,Nothing))"
        )
    done
done
for source in Disabled BadRhs; do
    if "${ghc[@]}" -fexternal-interpreter -fno-code "$source.hs" > "$source.log" 2>&1; then exit 1; fi
done
grep -q 'GHC-29822' Disabled.log
grep -q "Expected kind" BadRhs.log
