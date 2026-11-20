#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint)
provider=NativeAlternatingBindersProvider
client=NativeAlternatingBindersClient

for opt in O0 O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/$provider.hs" .
    "${ghc[@]}" "-$opt" -c "$provider.hs"
    mv "$provider.hs" ../
    cp "$provider.hi" "$src/$client.hs" ../client/
    cd ../client
    "${ghc[@]}" "-$opt" -XNoExperimentalDependentTypes -c "$client.hs"
    "${ghc[@]}" "-$opt" "$client.o" "../provider/$provider.o" -o run
    ./run
done

cat >WrongKind.hs <<'HS'
{-# LANGUAGE DataKinds, TypeApplications #-}
module WrongKind where
import NativeAlternatingBindersProvider
wrong = forwardKind True @'EQ Marker
HS
if "${ghc[@]}" -XNoExperimentalDependentTypes -c WrongKind.hs >reject.log 2>&1; then
    echo 'Accepted the wrong kind for an erased argument' >&2
    exit 1
fi
grep -Fq "Couldn't match kind ‘Ordering’ with ‘Bool’" reject.log || { cat reject.log >&2; exit 1; }

cat >WrongIndex.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, TypeAbstractions #-}
module WrongIndex where
import NativeAlternatingBindersProvider
wrong :: foreach (left :: Bool) -> foreach (right :: Bool) ->
         forall a. foreach (m :: Maybe a) -> Reading left m -> Reading right m
wrong l r @a m x = keepAfter r @a m x
HS
if "${ghc[@]}" -c WrongIndex.hs >reject.log 2>&1; then
    echo 'Accepted two different runtime indices' >&2
    exit 1
fi
grep -Fq "Couldn't match type ‘l’ with ‘r’" reject.log || { cat reject.log >&2; exit 1; }
