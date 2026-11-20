#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
lib=$2
ghc=("$1" "-B$lib" -O1 -v0 -package-env - -i -i. -dcore-lint -dstg-lint)
shift 2
ghc+=("$@")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
cp "$src"/NativeDepFunCo{Provider,Mutate}.hs .
"${ghc[@]}" -package ghc NativeDepFunCoMutate.hs -o mutate
for compression in 1 2; do
    mkdir "$compression"
    (
        cd "$compression"
        cp ../NativeDepFunCoProvider.hs .
        "${ghc[@]}" "-fwrite-if-compression=$compression" -c NativeDepFunCoProvider.hs
        "${ghc[@]}" --show-iface NativeDepFunCoProvider.hi >provider.log
        grep -Fq 'retained-depfunco-v1' provider.log
        mkdir client
        cp NativeDepFunCoProvider.hi "$src"/NativeDepFunCo{Client,Wire}.hs client/
        cd client
        "${ghc[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core \
            -ddump-inlinings -c NativeDepFunCoClient.hs >client.log 2>&1 || {
            cat client.log
            exit 1
        }
        grep -Fq 'Inlining done: NativeDepFunCoProvider.transport' client.log
        "${ghc[@]}" -c NativeDepFunCoWire.hs
        "${ghc[@]}" NativeDepFunCoClient.o NativeDepFunCoWire.o ../NativeDepFunCoProvider.o -o run
        test "$(./run)" = 9
    )
done
for mutation in role scope; do
    mkdir "$mutation"
    (
        cd "$mutation"
        ../mutate "$lib" "$mutation" ../2/NativeDepFunCoProvider.hi NativeDepFunCoProvider.hi
        cp "$src"/NativeDepFunCoClient.hs .
        if "${ghc[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core \
                -c NativeDepFunCoClient.hs >client.log 2>&1; then
            exit 1
        fi
        case "$mutation" in
            role) grep -Fq 'Expected nominal evidence.' client.log ;;
            scope) grep -Fq 'Iface id out of scope:' client.log ;;
        esac
    )
done
