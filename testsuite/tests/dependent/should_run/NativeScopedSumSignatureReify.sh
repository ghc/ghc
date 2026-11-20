#!/usr/bin/env bash
set -euo pipefail

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint)
shift 2

mkdir "$out"/{provider,client}
cd "$out/provider"
cp "$src/NativeScopedSumSignatureReifyProvider.hs" .
"${ghc[@]}" "$@" -c NativeScopedSumSignatureReifyProvider.hs

cp ./*.hi "$src/NativeScopedSumSignatureReifyClient.hs" ../client/
cd ../client
"${ghc[@]}" "$@" -c NativeScopedSumSignatureReifyClient.hs
