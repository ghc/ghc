#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint)
cd "$out"
cp "$src"/{DefaultVector,PackedVecLibrary}.hs .
"${ghc[@]}" -O1 -c PackedVecLibrary.hs DefaultVector.hs
mkdir client
cp DefaultVector.hi PackedVecLibrary.hi client/
cd client
cat >Main.hs <<'HS'
{-# LANGUAGE TypeApplications #-}
module Main where
import DefaultVector
import PackedVecLibrary

instance DefaultElement Char where
  defaultElement = 'x'

main :: IO ()
main = print (toList (filled @Char 3))
HS
for mode in -O1 -fignore-interface-pragmas -fno-ignore-interface-pragmas; do
    "${ghc[@]}" -O0 "$mode" -fforce-recomp -ddump-simpl -c Main.hs >client.core
    if grep -Fq '$dmfilled' client.core; then
        echo "The default method was not expanded at $mode" >&2
        exit 1
    fi
    "${ghc[@]}" Main.o ../DefaultVector.o ../PackedVecLibrary.o -o run
    ./run
done
cat >BadHead.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, TypeApplications #-}
module BadHead where
import DefaultVector
import Main ()
import PackedVecLibrary

safeHead :: Vec a ('S n) -> a
safeHead (VCons x _) = x

bad :: Char
bad = safeHead (filled @Char 0)
HS
if "${ghc[@]}" -O0 -fdiagnostics-color=never -c BadHead.hs >bad.log 2>&1; then
    echo "Accepted the head of a vector with zero cells" >&2
    exit 1
fi
grep -Fq 'GHC-83865' bad.log && grep -Eq "Actual: Vec Char '?Z" bad.log || { cat bad.log >&2; exit 1; }
