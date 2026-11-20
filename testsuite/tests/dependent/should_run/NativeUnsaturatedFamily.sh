#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -O0 -i -i. -package-env - -dcore-lint -dstg-lint
     -fdiagnostics-color=never)
stripped=(-fomit-interface-pragmas -fno-write-if-simplified-core)
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
cp "$src"/NativeUnsaturated{Eta,Inverse,Client}.hs .
mkdir producer client

"${ghc[@]}" -c NativeUnsaturatedEta.hs
if "${ghc[@]}" -c NativeUnsaturatedInverse.hs >inverse.log 2>&1 ||
   ! grep -Fq '[GHC-25897]' inverse.log; then
    cat inverse.log
    exit 1
fi

(
    cd producer
    cp "$src/NativeUnsaturatedProvider.hs" .
    "${ghc[@]}" "${stripped[@]}" -c NativeUnsaturatedProvider.hs
)
cp producer/NativeUnsaturatedProvider.hi NativeUnsaturatedClient.hs client/
(
    cd client
    for mode in NoImpredicativeTypes ImpredicativeTypes; do
        "${ghc[@]}" "-X$mode" -fforce-recomp -c NativeUnsaturatedClient.hs
    done
)

# Two different prefixes are not known apart solely because their
# unsaturated applications cannot be decomposed.
sed -e 's/module NativeUnsaturatedClient/module Stuck/' \
    -e "s/Same (Alias 'True) (Alias 'True) :~: 'True/Same (Alias 'True) (Alias 'False) :~: 'False/" \
    NativeUnsaturatedClient.hs >client/Stuck.hs
if (cd client && "${ghc[@]}" -c Stuck.hs) >stuck.log 2>&1 ||
   ! grep -Fq "Couldn't match type" stuck.log ||
   ! grep -Fq 'reflexive = Refl' stuck.log; then
    cat stuck.log
    exit 1
fi

# Change only the provider equation. The client is unchanged and sees only
# the new interface. Compile without -fforce-recomp to exercise invalidation.
(
    cd producer
    sed "s/^type instance Select a 'True = a$/type instance Select a 'True = 'False/" \
        "$src/NativeUnsaturatedProvider.hs" >NativeUnsaturatedProvider.hs
    "${ghc[@]}" "${stripped[@]}" -c NativeUnsaturatedProvider.hs
)
cp producer/NativeUnsaturatedProvider.hi client/
if (cd client && "${ghc[@]}" -c NativeUnsaturatedClient.hs) >changed.log 2>&1 ||
   ! grep -Fq "Couldn't match type" changed.log ||
   ! grep -Fq "saturated = Refl" changed.log; then
    cat changed.log
    exit 1
fi
