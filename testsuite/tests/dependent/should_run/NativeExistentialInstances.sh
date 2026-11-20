#!/usr/bin/env bash
set -euo pipefail
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

for mode in O0 O2; do
    work="$out/$mode"
    mkdir -p "$work"/{provider,instances,client}
    cd "$work/provider"
    cp "$src/ErasedStream.hs" .
    "${ghc[@]}" "-$mode" -c ErasedStream.hs
    cd ../instances
    cp ../provider/ErasedStream.hi "$src/StreamPreview.hs" .
    "${ghc[@]}" "-$mode" -c StreamPreview.hs
    cd ../client
    cp ../instances/*.hi "$src/NativeExistentialInstances.hs" .
    "${ghc[@]}" "-$mode" -c NativeExistentialInstances.hs
    "${ghc[@]}" "-$mode" NativeExistentialInstances.o ../instances/StreamPreview.o ../provider/ErasedStream.o -o client
    test "$(./client)" = '("GH","[True]","x",(Refl,Refl))'
done

reject() {
    local option=$1 lineno diagnostic=$2 binding=$3
    lineno=$(awk -v source="$binding" '$0 == source {print NR; exit}' NativeExistentialInstances.hs)
    test -n "$lineno"
    if "${ghc[@]}" -fforce-recomp "-D$option" -c NativeExistentialInstances.hs >reject.log 2>&1; then
        echo "Unexpected acceptance of $option" >&2
        exit 1
    fi
    grep -Eq "^NativeExistentialInstances.hs:$lineno:[0-9]+: error: \\[$diagnostic\\]" reject.log || { cat reject.log >&2; exit 1; }
}
reject STUCK GHC-83865 'wrong = Refl'
reject OPEN GHC-83865 'wrong _ = Refl'
reject INVERSE GHC-25897 'wrong Refl = Refl'
reject REPEATED GHC-22979 'instance Sample (a,a) => Sample (Maybe (Stream a))'
reject DUPLICATE GHC-59692 'instance Sample First'
