#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
area=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cd "$tmp"
g=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
flags=(-fomit-interface-pragmas -fno-write-if-simplified-core)
cp "$area/NativeSimplifierProvider.hs" .
"${g[@]}" -O0 "${flags[@]}" -ddump-simpl-iterations -c NativeSimplifierProvider.hs >retained.log 2>&1 || { cat retained.log; exit 1; }
grep -Fq 'Simplifier iteration=1' retained.log || { cat retained.log; exit 1; }
status=0
"${g[@]}" -O1 "${flags[@]}" -fforce-recomp -c NativeSimplifierProvider.hs >optimized.log 2>&1 || status=$?
if [[ $status != 1 ]] || ! grep -Fq 'Native dependent Core cannot enter optimized module compilation' optimized.log; then
  cat optimized.log
  exit 1
fi
mkdir client
cp "$area/NativeSimplifierClient.hs" NativeSimplifierProvider.hi client/
cd client
"${g[@]}" -O0 "${flags[@]}" -c NativeSimplifierClient.hs
"${g[@]}" NativeSimplifierClient.o ../NativeSimplifierProvider.o -o run
./run
