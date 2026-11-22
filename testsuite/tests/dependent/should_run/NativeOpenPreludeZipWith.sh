#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,client}

cd "$out/provider"
"${ghc[@]}" -fno-enable-rewrite-rules -fomit-interface-pragmas \
    -fno-write-if-simplified-core -c "$src/PreludeZipWithSource.hs" -outputdir .

cd "$out/client"
cp ../provider/PreludeZipWithSource.hi .
proof="$src/PreludeZipWithProof.hs"
for opt in -O0 -O2; do
    "${ghc[@]}" "$opt" -fno-enable-rewrite-rules -fforce-recomp -c "$proof" -outputdir .
done

reject() {
    local code=$1
    shift
    if "${ghc[@]}" "$@" -fforce-recomp -c "$proof" -outputdir . >rejected.log 2>&1; then
        echo "Unexpected success: $*" >&2
        exit 1
    fi
    grep -Fq "$code" rejected.log || { cat rejected.log >&2; exit 1; }
}
for mode in UNKNOWN_LEFT DROP_CONS; do
    reject '[GHC-83865]' -fno-enable-rewrite-rules "-D$mode"
done
reject '[GHC-83605]' -O1 -fenable-rewrite-rules -DRULES_ON
