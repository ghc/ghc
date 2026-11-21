#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
name=NativeLinearRetainedPackage
provider=${name}Provider
client=${name}Client
compiler=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -Werror=incomplete-patterns)

reject() {
    local source=$1
    shift
    if "${ghc[@]}" -fno-diagnostics-show-caret -c "$source.hs" >reject.log 2>&1; then
        echo "Accepted $source" >&2
        exit 1
    fi
    for expected in "$@"; do
        grep -Eq "^$source.hs:$expected" reject.log || { cat reject.log >&2; exit 1; }
    done
}

for opt in 0 2; do
    ghc=("${compiler[@]}" "-O$opt")
    if [[ $opt == 0 ]]; then ghc+=(-dlinear-core-lint); fi
    work=$out/$opt
    mkdir -p "$work"/{provider,client}
    cd "$work/provider"
    cp "$src/$provider.hs" .
    "${ghc[@]}" -c "$provider.hs"
    cp "$provider.hi" "$src/$client.hs" \
       "$src/$name"{Reject,EvidenceReject,FallthroughReject}.hs ../client/
    cd ../client
    "${ghc[@]}" -c "$client.hs"
    "${ghc[@]}" "$client.o" "../provider/$provider.o" -o run
    ./run
    reject "${name}Reject" '6:[0-9]+: error: \[GHC-18872\]' '8:[0-9]+: error: \[GHC-18872\]'
    reject "${name}FallthroughReject" '6:[0-9]+: error: \[GHC-47619\]'
    reject "${name}EvidenceReject" '7:[0-9]+: error: \[GHC-05617\]' '11:[0-9]+: error: \[GHC-83865\]' \
           '15:[0-9]+: error: \[GHC-05617\]'
done
