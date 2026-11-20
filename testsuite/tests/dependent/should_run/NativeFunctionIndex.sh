#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
area=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cd "$tmp"
g=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
flags=(-fomit-interface-pragmas -fno-write-if-simplified-core)
for level in 0 1; do
    mkdir "O$level"
    (
        cd "O$level"
        mkdir provider consumer client
        cp "$area/FunctionFieldLibrary.hs" provider/
        cd provider
        "${g[@]}" "-O$level" "${flags[@]}" -ddump-simpl-iterations -ddump-worker-wrapper -c FunctionFieldLibrary.hs >retained.log 2>&1 || { cat retained.log; exit 1; }
        grep -Fq 'Simplifier iteration=1' retained.log || { cat retained.log; exit 1; }
        if [[ "$level" == 1 ]]; then
            grep -Fq 'Worker Wrapper binds' retained.log || { cat retained.log; exit 1; }
        fi
        cp FunctionFieldLibrary.hi "$area/FunctionFieldClient.hs" ../consumer/
        cd ../consumer
        "${g[@]}" -O0 "${flags[@]}" -c FunctionFieldClient.hs
        cp FunctionField{Library,Client}.hi "$area/NativeFunctionIndexClient.hs" ../client/
        cd ../client
        "${g[@]}" -O0 "${flags[@]}" -c NativeFunctionIndexClient.hs
        "${g[@]}" NativeFunctionIndexClient.o ../provider/FunctionFieldLibrary.o ../consumer/FunctionFieldClient.o -o run
        ./run >observed
    )
done
diff -u O0/client/observed O1/client/observed
cat O1/client/observed
