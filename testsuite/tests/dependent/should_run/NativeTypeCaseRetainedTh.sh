#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
area=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
base=("$1" "-B$2" -v0 -package-env - -i -i. -fexternal-interpreter -dcore-lint -dstg-lint)
cd "$work"
cp "$area"/NativeRetainedTH.hs "$area"/NativeTypeCaseRetainedTh{,Raw,Wire}.hs .
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
import NativeTypeCaseRetainedThRaw (chooseInline, chooseDirect, observeInline, caseData)

main :: IO ()
main = do
    print (quoted True, quoted False,
           fromQuote (Proxy @Int) True, fromQuote (Proxy @Int) False,
           fromReify (Proxy @Int) True, fromReify (Proxy @Int) False)
    flag <- newIORef True
    first <- observe <$> readIORef flag
    firstInline <- observeInline <$> readIORef flag
    writeIORef flag False
    second <- observe <$> readIORef flag
    secondInline <- observeInline <$> readIORef flag
    print (first, second)
    print (firstInline, secondInline)
    print (chooseInline True :: Int, chooseInline False :: Char)
    print (chooseDirect True () :: Int, chooseDirect False () :: Char)
    print caseData

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
cat >NativeTypeCaseRetainedThEscape.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}
module NativeTypeCaseRetainedThEscape where
import Data.Proxy (Proxy)
import Language.Haskell.TH (Q, Type)

escape :: Q Type
escape = [t| foreach (b :: Maybe Bool) ->
             case b of { Just field -> Proxy field; Nothing -> Proxy field } |]
HS
cat >NativeTypeCaseRetainedThWrongBranch.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module NativeTypeCaseRetainedThWrongBranch where
import NativeTypeCaseRetainedThRaw (chooseInline)

wrong :: Int
wrong = chooseInline False
HS
cat >NativeTypeCaseRetainedThSeparate.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}
module NativeTypeCaseRetainedThSeparate where

choose :: $([t| foreach (b :: Bool) -> case b of { True -> Int; False -> Char } |])
choose True = 13
choose False = 'q'

render :: $([t| foreach (b :: Bool) -> (case b of { True -> Int; False -> Char }) -> String |])
render True i = show i
render False c = [c]

separate :: foreach (b :: Bool) -> String
separate b = render b (choose b)
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
    : > case-effects
    "${ghc[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core -c NativeTypeCaseRetainedThRaw.hs
    printf 'after\ncase\nbefore\nfinalizer\n' | diff -u - case-effects
    mv NativeTypeCaseRetainedThRaw.hs raw.source
    "${ghc[@]}" -c NativeTypeCaseRetainedThClient.hs
    "${ghc[@]}" NativeTypeCaseRetainedThClient.o NativeTypeCaseRetainedTh.o \
        NativeTypeCaseRetainedThProvider.o NativeTypeCaseRetainedThRaw.o NativeRetainedTH.o -o run
    ./run > actual
    printf "(11,'x',11,'x',11,'x')\n(\"11\",\"x\")\n(\"13\",\"q\")\n(13,'q')\n(19,'c')\nTrue\n" | diff -u - actual
    status=0
    "${ghc[@]}" -c NativeTypeCaseRetainedThWrongBranch.hs > wrong-branch.log 2>&1 || status=$?
    if [[ $status != 1 ]] || ! grep -Fq "Couldn't match type" wrong-branch.log ||
       ! grep -Fq 'chooseInline False' wrong-branch.log; then
        cat wrong-branch.log >&2
        exit 1
    fi
    mv raw.source NativeTypeCaseRetainedThRaw.hs
    mv provider.source NativeTypeCaseRetainedThProvider.hs
done

status=0
"${base[@]}" -c NativeTypeCaseRetainedThWrong.hs > wrong.log 2>&1 || status=$?
if [[ $status != 1 ]] || ! grep -Fq 'Malformed type VarT' wrong.log; then
    cat wrong.log >&2
    exit 1
fi
status=0
"${base[@]}" -c NativeTypeCaseRetainedThEscape.hs > escape.log 2>&1 || status=$?
if [[ $status != 1 ]] || ! grep -Eq 'Not in scope: .*field' escape.log; then
    cat escape.log >&2
    exit 1
fi
status=0
"${base[@]}" -c NativeTypeCaseRetainedThSeparate.hs > separate.log 2>&1 || status=$?
if [[ $status != 1 ]] || ! grep -Fq 'GHC-83865' separate.log ||
   ! grep -Fq 'render b (choose b)' separate.log; then
    cat separate.log >&2
    exit 1
fi
