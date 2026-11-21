#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd "$(dirname "$0")/retained-constructor-context" && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -fforce-recomp -dcore-lint -dstg-lint
     -Werror=incomplete-patterns)

for opt in -O0 -O2; do
    provider="$work/$opt/provider"
    mkdir -p "$provider"
    "${ghc[@]}" "$opt" -c -dynamic-too -fomit-interface-pragmas \
        -fno-write-if-simplified-core -outputdir "$provider" "$src/Provider.hs"
    "${ghc[@]}" --show-iface "$provider/Provider.hi" >"$provider/interface"
    grep -Fq 'retained-constructor-context' "$provider/interface"

    for client in Main ReifiedMain QuotedMain; do
        out="$work/$opt/$client"
        mkdir -p "$out"
        "${ghc[@]}" "$opt" -c -i "-i$provider" -o "$out/Main.o" \
            -ohi "$out/Main.hi" "$src/$client.hs"
        "${ghc[@]}" "$out/Main.o" "$provider/Provider.o" -o "$out/run"
        test "$("$out/run")" = '("71","r")'
    done
done
