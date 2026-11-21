#!/usr/bin/env bash
set -euo pipefail
src=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint)
mkdir -p "$tmp"/{lib,provider,client}
cd "$tmp/lib"
cp "$src/NativeMatchableProjectionLib.hs" .
"${ghc[@]}" -O2 -c NativeMatchableProjectionLib.hs
"${ghc[@]}" --show-iface NativeMatchableProjectionLib.hi >lib.iface
if grep -Eq '^ *got .*matchable-projection' lib.iface; then
    echo 'A projection with two statically matchable heads requested the scoped rule.'
    exit 1
fi
cd ../provider
cp ../lib/*.hi "$src/NativeMatchableProjection.hs" .
for opt in -O0 -O2; do
    "${ghc[@]}" "$opt" -fforce-recomp -fexpose-all-unfoldings -funfolding-creation-threshold=900 -c NativeMatchableProjection.hs
done
"${ghc[@]}" --show-iface NativeMatchableProjection.hi >provider.iface
grep -Eq '^ *got .*matchable-projection' provider.iface
cd ../client
cp ../provider/*.hi .
cat >Main.hs <<'HSCLIENT'
{-# LANGUAGE DataKinds, Modifiers, NoExperimentalDependentTypes, TypeApplications #-}
module Main where
import Data.Proxy (Proxy(..))
import GHC.Matchability (M)
import NativeMatchableProjection
main :: IO ()
main = do
    let input = Applied @M @Maybe Proxy KnownM 'q' (fmap odd)
        (c, run) = viewResult input
    print (c, run (Just 3), betaResult input (Just 4))
HSCLIENT
"${ghc[@]}" -O2 -c Main.hs
"${ghc[@]}" Main.o ../provider/NativeMatchableProjection.o ../lib/NativeMatchableProjectionLib.o -o check
./check
cat >Unproved.hs <<'HSCONTROL'
{-# LANGUAGE DataKinds, GADTs, Modifiers, NoExperimentalDependentTypes, PolyKinds, TypeOperators #-}
module Unproved where
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (Matchability)
cancel :: forall (m :: Matchability) (f :: Type %m -> Type) x y.
          Proxy f -> f x :~: f y -> x :~: y
cancel _ Refl = Refl
HSCONTROL
if "${ghc[@]}" -O2 -fdiagnostics-color=never -c Unproved.hs >unproved.log 2>&1; then
    echo 'Decomposed an application with unknown matchability.'
    exit 1
fi
grep -Fq 'Unproved.hs:9:17: error: [GHC-25897]' unproved.log
