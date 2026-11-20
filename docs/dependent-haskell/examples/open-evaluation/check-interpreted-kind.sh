#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

if [[ $# -ne 2 ]]; then
    printf 'Usage: bash %s GHC LIBDIR\n' "$0" >&2
    exit 2
fi
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$(realpath "$1")" "-B$(realpath "$2")" -v0 -O0 -package-env - -fexternal-interpreter
     -dcore-lint -dstg-lint -fomit-interface-pragmas -fno-write-if-simplified-core
     -fdiagnostics-color=never)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out/provider" "$out/client"
cp "$src"/{InterpretedKind,InterpretedKindCallback}.hs "$out/provider/"
cp "$src/InterpretedKindUseSite.hs" "$out/client/"

(
    cd "$out/provider"
    "${ghc[@]}" -c InterpretedKind.hs
    "${ghc[@]}" -c InterpretedKindCallback.hs
)
cp "$out/provider/"*.hi "$out/client/"
cd "$out/client"
"${ghc[@]}" -c InterpretedKindUseSite.hs
sed -e 's/^module InterpretedKindUseSite/module WrongSelector/' \
    -e 's/^defaultAtResult = Just 3$/defaultAtResult = Right 3/' \
    InterpretedKindUseSite.hs >WrongSelector.hs
if "${ghc[@]}" -fprint-explicit-kinds -fprint-explicit-matchabilities \
    -c WrongSelector.hs >wrong.log 2>&1 ||
    ! grep -Fq '[GHC-83865]' wrong.log ||
    ! grep -Fq '@{GHC.Internal.Types.Matchable}' wrong.log ||
    ! grep -Fq 'In the expression: Right 3' wrong.log; then
    cat wrong.log >&2
    exit 1
fi

cat >Main.hs <<'HS'
module Main where

import InterpretedKindUseSite (atM, atU, defaultAtResult)

main :: IO ()
main = print (atM, atU, defaultAtResult)
HS
"${ghc[@]}" -c Main.hs
"${ghc[@]}" Main.o InterpretedKindUseSite.o "$out/provider/"*.o -o run
./run >actual.stdout
diff -u <(printf '%s\n' '(Just 3,Right 3,Just 3)') actual.stdout
cat actual.stdout
