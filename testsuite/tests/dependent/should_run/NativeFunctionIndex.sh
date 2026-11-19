#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
area=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cd "$tmp"
g=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
flags=(-fomit-interface-pragmas -fno-write-if-simplified-core)
mkdir provider consumer client
cp "$area/FunctionFieldLibrary.hs" provider/
cd provider
"${g[@]}" -O0 "${flags[@]}" -ddump-simpl-iterations -c FunctionFieldLibrary.hs >retained.log 2>&1 || { cat retained.log; exit 1; }
grep -Fq 'Simplifier iteration=1' retained.log || { cat retained.log; exit 1; }
status=0
"${g[@]}" -O1 "${flags[@]}" -fforce-recomp -c FunctionFieldLibrary.hs >optimized.log 2>&1 || status=$?
if [[ $status != 1 ]] || ! grep -Fq 'Native dependent Core cannot enter optimized module compilation' optimized.log; then
    cat optimized.log
    exit 1
fi
cp FunctionFieldLibrary.hi "$area/FunctionFieldClient.hs" ../consumer/
cd ../consumer
"${g[@]}" -O0 "${flags[@]}" -c FunctionFieldClient.hs
cp FunctionField{Library,Client}.hi "$area/NativeFunctionIndexClient.hs" ../client/
cd ../client
"${g[@]}" -O0 "${flags[@]}" -c NativeFunctionIndexClient.hs
"${g[@]}" NativeFunctionIndexClient.o ../provider/FunctionFieldLibrary.o ../consumer/FunctionFieldClient.o -o run
./run
