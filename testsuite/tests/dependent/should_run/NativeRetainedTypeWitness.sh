#!/usr/bin/env bash
set -euo pipefail
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -v0 -package-env - -dcore-lint -dstg-lint)
shift 2
ghc+=("$@")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"

provider=NativeRetainedTypeWitnessLib
client=NativeRetainedTypeWitness
for opt in -O0 -O2; do
    mkdir -p "./$opt"/{provider,client,lib}
    (
        cd "./$opt/provider"
        cp "$src/$provider.hs" .
        "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c "$provider.hs"
        cp "$provider.hi" "$provider.o" ../lib/
        "${ghc[@]}" --show-iface "$provider.hi" > interface
        grep -Fq 'forall {a}.' interface
        grep -Fq 'TypeRep a -> a -> a' interface
        grep -Fxq 'trusted: safe' interface
    )
    (
        cd "./$opt/client"
        cp "$src/$client.hs" .
        "${ghc[@]}" "$opt" -i -i../lib -c "$client.hs"
        "${ghc[@]}" "$client.o" "../lib/$provider.o" -o "$client"
        "./$client" > result
        cp "$src/NativeRetainedTypeOperand.hs" .
        "${ghc[@]}" "$opt" -i -i../lib -fwrite-ide-info -c NativeRetainedTypeOperand.hs
        "${ghc[@]}" NativeRetainedTypeOperand.o "../lib/$provider.o" -o operand
        ./operand >> result
    )
done
cmp ./-O0/client/result ./-O2/client/result
cat ./-O0/client/result
