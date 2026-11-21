#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -ferror-spans -fdiagnostics-color=never)
provider=NativeClosedFamilySumProvider
client=NativeClosedFamilySumClient

reject() {
    local expected=$1
    shift
    if "$@" > reject.log 2>&1 || ! grep -Fq "$expected" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

for opt in O0 O2; do
    work="$out/$opt"
    mkdir -p "$work"/{provider,client}
    cd "$work/provider"
    cp "$src/$provider.hs" .
    reject "$provider.hs:15:51-56: error: [GHC-76037]" \
        "${ghc[@]}" "-$opt" -DBAD_SCOPE -c "$provider.hs"
    "${ghc[@]}" "-$opt" -c "$provider.hs"
    "${ghc[@]}" --show-iface "$provider.hi" > iface.txt
    test "$(grep -Fc 'sum codomain of $lambda:Pack:' iface.txt)" = 2
    cp "$provider.hi" "$src/$client.hs" ../client/

    cd ../client
    reject "$client.hs:15:24-34: error: [GHC-83865]" \
        "${ghc[@]}" "-$opt" -DBAD_TAG -c "$client.hs"
    reject "$client.hs:37:15-18: error: [GHC-83605]" \
        "${ghc[@]}" "-$opt" -DBAD_PATTERN -c "$client.hs"
    test "$(grep -Fc 'error: [GHC-83605]' reject.log)" = 3
    "${ghc[@]}" "-$opt" -c "$client.hs"
    "${ghc[@]}" "$client.o" "../provider/$provider.o" -o run
    ./run
done
