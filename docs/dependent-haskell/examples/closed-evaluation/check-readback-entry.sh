#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
g=("$1" "-B$2" -v0 -dynamic -package-env - -dcore-lint -dstg-lint)
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{plugin,throwing,returning,client}
cd "$out/plugin"
cp "$src/PairReadbackProbe.hs" .
"${g[@]}" -package ghc -c PairReadbackProbe.hs
cd ../throwing
cat >Worker.hs <<'HS'
{-# LANGUAGE ScopedTypeVariables #-}
module Worker (pair, observe) where
import Numeric.Natural (Natural)

pair :: Natural -> (Natural, Char)
pair n = (n + 1, errorWithoutStackTrace "unread")
-- The experiment replaces this entry in final Core.
{-# OPAQUE observe #-}
observe :: Natural -> (Bool, (Natural, Char))
observe n = (False, pair n)
HS
cp Worker.hs ../returning/
cat >>../returning/Worker.hs <<'HS'
{-# RULES "error/returns" forall (s :: String). errorWithoutStackTrace s = 'x' #-}
HS
for dir in ../throwing ../returning; do
  (
    cd "$dir"
    "${g[@]}" -O1 -fno-worker-wrapper -fforce-recomp -i. -i../plugin -package ghc \
      -fplugin=PairReadbackProbe -fomit-interface-pragmas -fno-write-if-simplified-core \
      -Wno-inline-rule-shadowing -c Worker.hs
  )
done
cd ../client
cp ../throwing/Worker.hi .
cat >Observe.hs <<'HS'
module Observe (readResult, Answer(..)) where
import Numeric.Natural (Natural)
data Answer = Partial Natural | Whole Natural Char deriving Show
readResult :: (Bool, (Natural, Char)) -> Answer
readResult (dead, (n,c)) | dead = Partial n
                         | otherwise = Whole n c
HS
cat >Main.hs <<'HS'
module Main where
import Observe
import Worker (observe)
main :: IO ()
main = print (readResult (observe 7))
HS
"${g[@]}" -O0 -c Observe.hs Main.hs
for dir in ../throwing ../returning; do
  "${g[@]}" Main.o Observe.o "$dir/Worker.o" -o run-observer
  ./run-observer >out
  cat out
  cp "$dir/Worker.o" .
  cat >Proof.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Proof where
import qualified Observe as O
import qualified Worker as W
import Data.Type.Equality ((:~:)(Refl))
HS
  if [[ $dir == ../throwing ]]; then
    printf "%s\n" "proof :: O.readResult (W.observe 7) :~: 'O.Partial 8" >>Proof.hs
  else
    if "${g[@]}" -fforce-recomp -c Before.hs >old.log 2>&1 || ! grep -Fq "Couldn't match" old.log; then cat old.log; exit 1; fi
    printf "%s\n" "proof :: O.readResult (W.observe 7) :~: 'O.Whole 8 'x'" >>Proof.hs
  fi
  echo 'proof = Refl' >>Proof.hs
  "${g[@]}" -fforce-recomp -c Proof.hs
  sed 's/^module Proof /module Before /' Proof.hs >Before.hs
  cmp ../throwing/Worker.hi Worker.hi
  if [[ $dir == ../throwing ]]; then grep -Fqx 'Partial 8' out; else grep -Fqx "Whole 8 'x'" out; fi
done
