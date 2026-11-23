#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -package-env - -v0 -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never -ferror-spans)
provider=NativeAssociatedPayloadSumProvider
client=NativeAssociatedPayloadSumClient

reject() {
    local code=$1 message=$2
    shift 2
    if "${ghc[@]}" "$@" > reject.log 2>&1 ||
       ! grep -Fq "$code" reject.log ||
       ! grep -Fq "$message" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

for opt in -O0 -O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/$provider.hs" .
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c "$provider.hs"
    "${ghc[@]}" --show-iface "$provider.hi" > provider.iface
    grep -Fq 'scoped-sum-domain-v1' provider.iface
    cp "$provider.hi" "$src/$client.hs" ../client/

    cd ../client
    "${ghc[@]}" "$opt" -c "$client.hs"
    "${ghc[@]}" "$client.o" "../provider/$provider.o" -o observe
    timeout 15 ./observe > actual
    grep -Fxq "((True,\"x\"),True,((False,\"43\"),False))" actual

    reject 'GHC-83865' "In the second argument of ‘Pair’, namely ‘'w'’" \
        "$opt" -DWRONG_PAYLOAD -fforce-recomp -c "$client.hs"
    reject 'GHC-83605' 'Quick Look inference' \
        "$opt" -DMISSING_DOMAIN -fforce-recomp -fdefer-type-errors -c "$client.hs"
    reject 'GHC-39999' 'Allowed Int' \
        "$opt" -DMISSING_TYPE_DOMAIN -fforce-recomp -fdefer-type-errors -c "$client.hs"

    cd ../provider
    reject 'GHC-90647' 'from its owning class constraint' \
        "$opt" -DWRONG_OWNER -fforce-recomp -fdefer-type-errors -c "$provider.hs"
done
