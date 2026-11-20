#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd "$(dirname "$0")" && pwd)
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -O1 -dcore-lint -dstg-lint)
shift 2
ghc+=("$@")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
cp "$src"/{NativeDefaultO1InterfaceProducer,DependentPair,DependentProjection}.hs .
"${ghc[@]}" -c NativeDefaultO1InterfaceProducer.hs DependentPair.hs DependentProjection.hs
"${ghc[@]}" --show-iface NativeDefaultO1InterfaceProducer.hi >producer.iface
grep -Fq '$windexedWorker' producer.iface
grep -Fq '$wordinaryWorker' producer.iface

# Compile the ordinary client with the interface alone on the import path.
mkdir client
cp {NativeDefaultO1InterfaceProducer,DependentPair,DependentProjection}.hi "$src/NativeDefaultO1Interface.hs" client/
cd client
"${ghc[@]}" -ddump-inlinings -ddump-simpl -c NativeDefaultO1Interface.hs >client.log 2>&1 || {
    cat client.log
    exit 1
}
grep -Fq 'Inlining done: NativeDefaultO1InterfaceProducer.ordinaryWorker' client.log
grep -Fq '$wordinaryWorker' client.log
"${ghc[@]}" NativeDefaultO1Interface.o ../{NativeDefaultO1InterfaceProducer,DependentPair,DependentProjection}.o -o run
./run 5
