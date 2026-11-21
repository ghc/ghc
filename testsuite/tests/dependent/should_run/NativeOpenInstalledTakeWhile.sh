#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{producer,client,strict}
cd "$out/producer"
stripped=(-fomit-interface-pragmas -fno-write-if-simplified-core)
cp "$src"/Prelude{MapLength{Size,Source},Filter{Source,LengthProof,Runtime},TakeWhile}.hs .
"${ghc[@]}" -O2 "${stripped[@]}" -c Prelude{MapLength{Size,Source},Filter{Source,LengthProof,Runtime},TakeWhile}.hs

# The same runtime gate can accept or reject. Neither result demands the tail.
cd "$out/client"
cp ../producer/*.hi .
cat >Main.hs <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Main where

import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import PreludeTakeWhile (PrefixStep(..), RuntimePrefix(..), observePrefix)

first :: RuntimePrefix -> String
first (RuntimePrefix gate x xs step) = case step of
    Stops Refl -> case takeWhile (gate &&) (x : xs) of
        [] -> "stop"
        _ -> "unexpected cons"
    Keeps Refl -> case takeWhile (gate &&) (x : xs) of
        True : _ -> "keep"
        _ -> "unexpected result"

main :: IO ()
main = do
    off <- newIORef False >>= readIORef
    on <- newIORef True >>= readIORef
    print ( first (observePrefix off undefined undefined)
          , first (observePrefix on False undefined)
          , first (observePrefix on True undefined) )
HS
for level in -O0 -O2; do
    "${ghc[@]}" "$level" -fforce-recomp -c Main.hs
    "${ghc[@]}" Main.o ../producer/*.o -o run
    [[ $(./run) == '("stop","stop","keep")' ]]
done

# The installed fusion rules remain outside the checked equations.
sed '/^{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}$/d' Main.hs >Rules.hs
line=$(awk '/^    Stops Refl ->/ { print NR }' Rules.hs)
if "${ghc[@]}" -O2 -fenable-rewrite-rules -c Rules.hs >rules.log 2>&1 ||
   ! grep -Fq "Rules.hs:$line:" rules.log ||
   ! grep -Fq '[GHC-83605]' rules.log ||
   ! grep -Fq 'with the current rewrite rules' rules.log; then
    cat rules.log >&2
    exit 1
fi

# An observation of another input cannot choose the branch at the current head.
sed 's/booleanResult (gate \&\& x)/booleanResult (gate \&\& False)/' "$src/PreludeTakeWhile.hs" >WrongHead.hs
line=$(awk '/^    IsFalse -> Stops Refl/ { print NR }' WrongHead.hs)
if "${ghc[@]}" -O0 -c WrongHead.hs >wrong-head.log 2>&1 ||
   ! grep -Fq "WrongHead.hs:$line:" wrong-head.log ||
   ! grep -Fq '[GHC-05617]' wrong-head.log ||
   ! grep -Fq '#guard:takeWhile' wrong-head.log; then
    cat wrong-head.log >&2
    exit 1
fi

# A producer that forces the tail on rejection cannot publish the prefix equation.
cd "$out/strict"
cat >Producer.hs <<'HS'
{-# OPTIONS_GHC -fwrite-source-equations #-}
module Producer (prefix) where

prefix :: (a -> Bool) -> [a] -> [a]
prefix _ [] = []
prefix p (x : xs)
    | p x = x : prefix p xs
    | otherwise = []
HS
"${ghc[@]}" -O2 "${stripped[@]}" -c Producer.hs
"${ghc[@]}" --show-iface Producer.hi >lazy.iface
grep -Fq 'FinalCoreSourceTable T:prefix' lazy.iface
sed 's/otherwise = \[\]/otherwise = xs `seq` []/' Producer.hs >Strict.hs
mv Strict.hs Producer.hs
"${ghc[@]}" -O2 "${stripped[@]}" -fforce-recomp -c Producer.hs
"${ghc[@]}" --show-iface Producer.hi >strict.iface
if grep -Eq 'T:(#guard:)?prefix' strict.iface; then
    cat strict.iface >&2
    exit 1
fi
