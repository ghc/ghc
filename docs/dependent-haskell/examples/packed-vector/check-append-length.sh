#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
ghc=$1
libdir=$2
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

for opt in -O0 -O2; do
    mkdir -p "$out/$opt/"{provider,client}
    cd "$out/$opt/provider"
    cp "$src/PackedVecLibrary.hs" .
    flags=("-B$libdir" -v0 -package-env - "$opt" -i -i. -dcore-lint -dstg-lint)
    if [[ $opt == -O0 ]]; then
        flags+=(-fomit-interface-pragmas -fno-write-if-simplified-core)
    fi
    "$ghc" "${flags[@]}" -c PackedVecLibrary.hs
    cp PackedVecLibrary.hi "$src/AppendLengthClient.hs" ../client/
    cd ../client
    cat > Main.hs <<'HS'
module Main where
import AppendLengthClient (appendTwoAndOne)
main :: IO ()
main = print appendTwoAndOne
HS
    "$ghc" "${flags[@]}" -c AppendLengthClient.hs Main.hs
    "$ghc" "-B$libdir" -v0 Main.o AppendLengthClient.o ../provider/PackedVecLibrary.o -o run
    ./run | tee actual
    printf '%s\n' '("aab",'"'a'"',"")' | cmp - actual

    sed 's/Length (2 N.+ 1)/Length (2 N.+ 2)/' AppendLengthClient.hs > WrongLength.hs
    sed 's/headVec joined/headVec empty/' AppendLengthClient.hs > EmptyHead.hs
    if "$ghc" "${flags[@]}" -c WrongLength.hs > wrong.log 2>&1; then exit 1; fi
    grep -Fq '[GHC-83865]' wrong.log
    grep -Fq 'appendNative 2 1 two one' wrong.log
    if "$ghc" "${flags[@]}" -c EmptyHead.hs > empty.log 2>&1; then exit 1; fi
    grep -Fq '[GHC-83865]' empty.log
    grep -Fq 'headVec empty' empty.log
done
