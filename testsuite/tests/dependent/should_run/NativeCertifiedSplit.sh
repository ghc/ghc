#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint)
for opt in -O0 -O1 -O2; do
    out="$tmp/$opt"
    mkdir -p "$out"/{provider,proof,client}
    cd "$out/provider"
    cp "$src/PackedVecLibrary.hs" .
    "${ghc[@]}" "$opt" -fwrite-source-equations -c PackedVecLibrary.hs
    cd ../proof
    cp "$src/CertifiedSplit.hs" ../provider/PackedVecLibrary.hi .
    "${ghc[@]}" "$opt" -c CertifiedSplit.hs
    cd ../client
    cp "$src/CertifiedSplitClient.hs" ../proof/*.hi .
    "${ghc[@]}" "$opt" -c CertifiedSplitClient.hs
    cat >Main.hs <<'HS'
module Main where
import CertifiedSplitClient
main :: IO ()
main = do
    splitObservation >>= print
    checkDemand >>= print
HS
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" Main.o CertifiedSplitClient.o ../proof/CertifiedSplit.o ../provider/PackedVecLibrary.o -o run
    ./run
done
cd "$out/proof"
sed 's/stepProof _ _ _ _ HRefl = HRefl/stepProof _ _ _ _ _ = HRefl/' CertifiedSplit.hs >MissingPremise.hs
if "${ghc[@]}" -O0 -ferror-spans -fforce-recomp -c MissingPremise.hs >reject.log 2>&1; then
    echo 'Accepted reassembly without the induction premise'
    exit 1
fi
rule=$(grep -n '^stepProof _ _ _ _ _ = HRefl' MissingPremise.hs | cut -d: -f1)
grep -F "MissingPremise.hs:$rule:23-27: error: [GHC-83865]" reject.log >/dev/null
