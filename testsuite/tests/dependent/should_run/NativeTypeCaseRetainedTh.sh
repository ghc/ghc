#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
area=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
base=("$1" "-B$2" -v0 -package-env - -i -i. -fexternal-interpreter -dcore-lint -dstg-lint)
cd "$work"
cp "$area"/NativeRetainedTH.hs "$area"/NativeTypeCaseRetainedTh{,Wire}.hs .
cat >NativeTypeCaseRetainedThProvider.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeTypeCaseRetainedThProvider where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import NativeRetainedTH (Select, quoted)

shadowed :: forall (b :: Type). Proxy b -> foreach (b :: Bool) -> Select b
shadowed _ = quoted
HS
cat >NativeTypeCaseRetainedThClient.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, TypeApplications #-}
module Main where

import Data.Proxy (Proxy(..))
import Data.IORef (newIORef, readIORef, writeIORef)
import NativeRetainedTH (quoted, render)
import NativeTypeCaseRetainedTh (fromQuote, fromReify)

main :: IO ()
main = do
    print (quoted True, quoted False,
           fromQuote (Proxy @Int) True, fromQuote (Proxy @Int) False,
           fromReify (Proxy @Int) True, fromReify (Proxy @Int) False)
    flag <- newIORef True
    first <- observe <$> readIORef flag
    writeIORef flag False
    second <- observe <$> readIORef flag
    print (first, second)

observe :: Bool -> String
observe b = render b (quoted b)
HS
cat >NativeTypeCaseRetainedThWrong.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}
module NativeTypeCaseRetainedThWrong where
import Language.Haskell.TH
import NativeRetainedTH (Select)

wrong :: $(do b <- newName "b"
              pure (ForEachT BndrReq b (ConT ''Bool) (AppT (ConT ''Select) (VarT b))))
wrong = undefined
HS

"${base[@]}" -package ghci -package template-haskell -package binary -package bytestring \
    NativeTypeCaseRetainedThWire.hs -o wire
./wire

for opt in -O0 -O2; do
    ghc=("${base[@]}" "$opt" -fforce-recomp)
    "${ghc[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core -c NativeRetainedTH.hs
    "${ghc[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core -c NativeTypeCaseRetainedThProvider.hs
    mv NativeTypeCaseRetainedThProvider.hs provider.source
    "${ghc[@]}" -c NativeTypeCaseRetainedTh.hs
    "${ghc[@]}" -c NativeTypeCaseRetainedThClient.hs
    "${ghc[@]}" NativeTypeCaseRetainedThClient.o NativeTypeCaseRetainedTh.o \
        NativeTypeCaseRetainedThProvider.o NativeRetainedTH.o -o run
    ./run > actual
    printf "(11,'x',11,'x',11,'x')\n(\"11\",\"x\")\n" | diff -u - actual
    mv provider.source NativeTypeCaseRetainedThProvider.hs
done

status=0
"${base[@]}" -c NativeTypeCaseRetainedThWrong.hs > wrong.log 2>&1 || status=$?
if [[ $status != 1 ]] || ! grep -Fq 'Malformed type VarT' wrong.log; then
    cat wrong.log >&2
    exit 1
fi
