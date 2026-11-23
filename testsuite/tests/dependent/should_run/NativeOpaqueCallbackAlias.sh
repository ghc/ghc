#!/usr/bin/env bash
set -euo pipefail

src=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never -ferror-spans)
fixture=NativeOpaqueCallbackAlias

for opt in 0 2; do
    mkdir -p "$work/$opt"/{provider,reader}
    cd "$work/$opt/provider"
    for module in "${fixture}Proof" "${fixture}Provider"; do
        cp "$src/$module.hs" .
        "${ghc[@]}" "-O$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c "$module.hs"
        cp "$module".{hi,o} ../reader/
    done
    cp "$src/${fixture}Bridge.hs" .
    bridge_pragmas=-fomit-interface-pragmas
    if [[ $opt == 2 ]]; then bridge_pragmas=-fno-omit-interface-pragmas; fi
    "${ghc[@]}" "-O$opt" "$bridge_pragmas" -c "${fixture}Bridge.hs"
    cp "${fixture}Bridge".{hi,o} ../reader/

    cd ../reader
    if [[ $opt == 2 ]]; then
        "${ghc[@]}" --show-iface "${fixture}Bridge.hi" > bridge.iface
        grep -q 'StableRetainedUser' bridge.iface
    fi
    cp "$src/${fixture}Client.hs" Main.hs
    flags=("-O$opt" -fomit-interface-pragmas -fno-write-if-simplified-core)
    "${ghc[@]}" "${flags[@]}" -c Main.hs
    "${ghc[@]}" ./*.o -o run
    ./run > result
    printf "(True,True)\nTrue\n('l','r')\n" > expected
    diff -u expected result

    reject() {
        local code=$1
        shift
        if "${ghc[@]}" "${flags[@]}" -fforce-recomp -c Main.hs "$@" > reject.log 2>&1 ||
           ! grep -Fq "[$code]" reject.log; then
            cat reject.log >&2
            exit 1
        fi
    }
    reject GHC-83605 -DDIRECT_LOCAL
    reject GHC-83865 -DDIFFERENT_FUNCTION
    reject GHC-83865 -DDIFFERENT_ALIAS
    reject GHC-83865 -DGUESSED_REDUCTION
done
