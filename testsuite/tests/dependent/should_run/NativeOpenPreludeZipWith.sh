#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,client}

cd "$out/provider"
strip=(-fno-enable-rewrite-rules -fomit-interface-pragmas -fno-write-if-simplified-core)
for module in PreludeMapLengthSize PreludeMapLengthSource PreludeMapLengthProof PreludeZipWithSource; do
    "${ghc[@]}" "${strip[@]}" -c "$src/$module.hs" -outputdir .
done

cd "$out/client"
cp ../provider/*.hi .
proof="$src/PreludeZipWithProof.hs"
length_proof="$src/PreludeZipWithLengthProof.hs"
for opt in -O0 -O2; do
    "${ghc[@]}" "$opt" -fno-enable-rewrite-rules -fforce-recomp -c "$proof" "$length_proof" -outputdir .
done

reject() {
    local code=$1 file=$2
    shift 2
    if "${ghc[@]}" "$@" -fforce-recomp -c "$file" -outputdir . >rejected.log 2>&1; then
        echo "Unexpected success: $*" >&2
        exit 1
    fi
    grep -Fq "$code" rejected.log || { cat rejected.log >&2; exit 1; }
}
for mode in UNKNOWN_LEFT DROP_CONS; do
    reject '[GHC-83865]' "$proof" -fno-enable-rewrite-rules "-D$mode"
done
for mode in UNKNOWN_LEFT_LENGTH DROP_SUCCESSOR; do
    reject '[GHC-83865]' "$length_proof" "-D$mode"
done
reject '[GHC-83605]' "$proof" -O1 -fenable-rewrite-rules -DRULES_ON
