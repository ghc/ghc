#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

src=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
provider=NativeLiftedCallbackProvider
client=NativeLiftedCallbackClient
expected=$'(False,False,True)\nTrue\nTrue\nTrue'

for opt in O0 O2; do
    flags=("-$opt")
    if [[ $opt == O0 ]]; then
        flags+=(-fomit-interface-pragmas -fno-write-if-simplified-core)
    fi
    mkdir -p "$work/$opt"/{provider,client}
    cd "$work/$opt/provider"
    cp "$src/$provider.hs" .
    "${ghc[@]}" "${flags[@]}" -c "$provider.hs"
    mv "$provider.hs" ../
    cp "$provider.hi" "$src/$client.hs" ../client/
    cd ../client
    "${ghc[@]}" "-$opt" -c "$client.hs"
    "${ghc[@]}" "$client.o" "../provider/$provider.o" -o run
    actual=$(./run True)
    [[ $actual == "$expected" ]] || { printf '%s\n' "$actual"; exit 1; }
done

cat > WrongEndpoint.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, GADTs, Modifiers #-}
module WrongEndpoint where

import Data.Kind (Type)
import NativeLiftedCallbackProvider (Decision, decide)

wrongHead :: forall (a :: Type). foreach (f :: a -> Bool) -> foreach (g :: a -> Bool)
          -> foreach (x :: a) -> Decision (f x)
wrongHead f g x = decide @a g x

wrongArg :: forall (a :: Type). foreach (f :: a -> Bool) -> foreach (x :: a)
         -> foreach (y :: a) -> Decision (f x)
wrongArg f x y = decide @a f y
HS

if "${ghc[@]}" -c WrongEndpoint.hs >reject.log 2>&1 ||
   ! grep -Eq '^WrongEndpoint\.hs:9:.*\[GHC-83865\]' reject.log ||
   ! grep -Eq '^WrongEndpoint\.hs:13:.*\[GHC-83865\]' reject.log ||
   ! grep -Eq 'Actual: Decision \(g[0-9]* x[0-9]*\)' reject.log ||
   ! grep -Eq 'Actual: Decision \(f[0-9]* y[0-9]*\)' reject.log; then
    cat reject.log >&2
    exit 1
fi
