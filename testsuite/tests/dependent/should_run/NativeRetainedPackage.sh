#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,client}
g=("$1" "-B$2" -v0 -package-env - -O0 -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
g+=("$@")
cd "$out/provider"
cp "$src"/{RetainedNat,PackedVecLibrary}.hs .
"${g[@]}" -fomit-interface-pragmas -c PackedVecLibrary.hs RetainedNat.hs
cp PackedVecLibrary.hi RetainedNat.hi "$out/client/"
cd "$out/client"
cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module Main where
import Data.Functor.Const (getConst)
import PackedVecLibrary
import RetainedNat

main :: IO ()
main = print (withRetained (constantPackage (error "unused witness") (7 :: Int))
                  (\_ box -> getConst box),
              withRetained (constantPackage Z (error "unused payload" :: Int))
                  (\n _ -> natToInt n))
HS
"${g[@]}" -c Main.hs
"${g[@]}" Main.o ../provider/{PackedVecLibrary,RetainedNat}.o -o run
./run
cat >Escape.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module Escape where
import PackedVecLibrary
import RetainedNat

newtype Box (n :: Nat) = Box { contents :: Vec Int n }
escape :: Retained Box -> Vec Int m
escape p = withRetained p (\_ box -> contents box)
HS
if "${g[@]}" -c Escape.hs >bad.log 2>&1 || ! grep -Fq 'Escape.hs:8:' bad.log ||
   ! grep -Fq 'Vec Int' bad.log; then
    cat bad.log >&2
    exit 1
fi
