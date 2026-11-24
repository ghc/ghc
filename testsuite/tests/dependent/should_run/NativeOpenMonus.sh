#!/usr/bin/env bash
set -euo pipefail
ghc=("$1" "-B$2" -v0 -package-env - -i -dcore-lint -dstg-lint
     -fomit-interface-pragmas -fno-write-if-simplified-core -fdiagnostics-color=never)
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

for opt in 0 2; do
    providers="$out/$opt/providers"
    interfaces="$out/$opt/interfaces"
    reader="$out/$opt/reader"
    mkdir -p "$providers" "$interfaces" "$reader"
    cd "$providers"
    cp "$src/"{PreludeMapLengthSize,NativeOpenMonusSource}.hs .
    "${ghc[@]}" -O"$opt" -i. --make -no-link -outputdir "$interfaces" NativeOpenMonusSource.hs
    "${ghc[@]}" --show-iface "$interfaces/NativeOpenMonusSource.hi" >source.iface
    grep -Fq 'FinalCoreSourceTable T:monus' source.iface

    cd "$reader"
    cp "$src/NativeOpenMonus.hs" .
    "${ghc[@]}" -O"$opt" "-i$interfaces" -c NativeOpenMonus.hs
    "${ghc[@]}" NativeOpenMonus.o "$interfaces/"{NativeOpenMonusSource,PreludeMapLengthSize}.o -o check
    [[ $(./check) == '(True,True,True,True)' ]]
    for error in WRONG_SUCCESSORS UNKNOWN_RIGHT; do
        if "${ghc[@]}" -O"$opt" -fforce-recomp "-i$interfaces" -D"$error" -c NativeOpenMonus.hs >error.log 2>&1 ||
           ! grep -Fq 'error: [GHC-83865]' error.log; then
            cat error.log >&2
            exit 1
        fi
    done
done
