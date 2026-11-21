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
    checkedObservation >>= print
    print checkBounds
HS
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" Main.o CertifiedSplitClient.o ../proof/CertifiedSplit.o ../provider/PackedVecLibrary.o -o run
    ./run
done
cd "$out/proof"
sed 's/(case premise of Refl -> Refl)/Refl/' CertifiedSplit.hs >MissingPremise.hs
if "${ghc[@]}" -O0 -ferror-spans -fforce-recomp -c MissingPremise.hs >reject.log 2>&1; then
    echo 'Accepted reassembly without the induction premise'
    exit 1
fi
location=$(awk '/Pair \(P.VCons x left\) \(Pair right Refl\)/ {
    col = index($0, "Refl")
    print NR ":" col "-" col + 3
}' MissingPremise.hs)
grep -Fq "MissingPremise.hs:$location: error: [GHC-05617]" reject.log
grep -Fq 'P.appendVec left right ~ xs' reject.log
if grep -Fq '$lambda:' reject.log; then
    cat reject.log >&2
    exit 1
fi
# The width proof is available for both vectors. Only the original
# vector has the identity named by the expected split type.
sed -e 's/foreach (v :: P.Vec a total) -> Maybe/foreach (v :: P.Vec a total) -> foreach (other :: P.Vec a total) -> Maybe/' \
    -e 's/^checkedSplitAtValue cut total v =/checkedSplitAtValue cut total v other =/' \
    -e 's/splitAtValue @a @suffix cut v)))/splitAtValue @a @suffix cut other)))/' \
    CertifiedSplit.hs >OtherVector.hs
if "${ghc[@]}" -O0 -fforce-recomp -c OtherVector.hs >other.log 2>&1 ||
   ! grep -Eq '^OtherVector.hs:.*error: \[GHC-83865\]' other.log ||
   ! grep -Fq 'splitAtValue @a @suffix cut other' other.log; then
    cat other.log >&2
    exit 1
fi
