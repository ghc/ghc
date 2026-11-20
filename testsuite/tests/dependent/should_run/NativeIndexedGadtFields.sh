#!/usr/bin/env bash
set -euo pipefail
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -Werror=inaccessible-code -Werror=incomplete-patterns -fdiagnostics-color=never)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
src=$PWD

# Compile the provider and read its unfolding without its source in scope.
for opt in O0 O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/"{PackedVecLibrary,NativeIndexedGadtFieldsProvider}.hs .
    "${ghc[@]}" "-$opt" -c PackedVecLibrary.hs NativeIndexedGadtFieldsProvider.hs
    if [[ $opt = O2 ]]; then
        "${ghc[@]}" --show-iface NativeIndexedGadtFieldsProvider.hi > provider.iface
        grep -Fq 'indexed-constructor-case+indexed-constructor-fields' provider.iface
    fi
    cp ./*.hi "$src/NativeIndexedGadtFields.hs" ../client/
    cd ../client
    "${ghc[@]}" "-$opt" -c NativeIndexedGadtFields.hs
    "${ghc[@]}" "-$opt" NativeIndexedGadtFields.o ../provider/{PackedVecLibrary,NativeIndexedGadtFieldsProvider}.o -o run
    test "$(./run)" = '(True,True,False,True)'
done

cd "$out/O0/provider"
for wrong in WRONG_HEAD WRONG_SCRUTINEE; do
    if "${ghc[@]}" -O0 -fforce-recomp "-D$wrong" -c NativeIndexedGadtFieldsProvider.hs > negative.log 2>&1 ||
       ! grep -Eq '\[GHC-(05617|83865)\]' negative.log ||
       ! grep -Fq 'ConsSeen @' negative.log; then
        cat negative.log >&2
        exit 1
    fi
done
