#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

src=$(cd -- "$(dirname -- "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -ferror-spans -fdiagnostics-color=never -fomit-interface-pragmas -fno-write-if-simplified-core)
provider=NativeKnownPairProvider
client=NativeKnownPairClient

for opt in O0 O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/$provider.hs" .
    "${ghc[@]}" "-$opt" -c "$provider.hs"
    cp "$provider.hi" "$src/$client.hs" ../client/
    cd ../client
    if "${ghc[@]}" "-$opt" -XImpredicativeTypes -DBAD_CONSTRUCTION -c "$client.hs" >reject.log 2>&1 ||
       ! grep -Fq "$client.hs:25:20-23: error: [GHC-83865]" reject.log ||
       ! grep -Fq "$client.hs:28:15-18: error: [GHC-83605]" reject.log ||
       [[ $(grep -Fc 'error:' reject.log) != 2 ]]; then
        cat reject.log >&2
        exit 1
    fi
    for impred in NoImpredicativeTypes ImpredicativeTypes; do
        for package in open named; do
            args=()
            if [[ $package == named ]]; then args=(-DNAMED); fi
            "${ghc[@]}" "-$opt" "-X$impred" "${args[@]}" -fforce-recomp -c "$client.hs"
            "${ghc[@]}" "$client.o" "../provider/$provider.o" -o run
            ./run
        done
    done
done
