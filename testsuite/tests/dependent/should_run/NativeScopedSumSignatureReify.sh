#!/usr/bin/env bash
set -euo pipefail

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fomit-interface-pragmas -fno-write-if-simplified-core)
shift 2

mkdir "$out"/{provider,client,hidden}
cd "$out/provider"
cp "$src/NativeScopedSumSignatureReifyProvider.hs" .
"${ghc[@]}" "$@" -c NativeScopedSumSignatureReifyProvider.hs

mv NativeScopedSumSignatureReifyProvider.hs ../hidden/
cp ./*.hi "$src/NativeScopedSumSignatureReifyClient.hs" ../client/
cd ../client
main=NativeScopedSumSignatureReifyClient
"${ghc[@]}" "$@" -main-is "$main" -c "$main.hs"
"${ghc[@]}" "$@" -main-is "$main" "$main.o" ../provider/NativeScopedSumSignatureReifyProvider.o -o run
./run
