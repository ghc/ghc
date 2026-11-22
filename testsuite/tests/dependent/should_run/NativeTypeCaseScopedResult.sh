#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd "$work"

cat >Domain.hs <<'HS'
module Domain (Choice(..)) where

data Choice = Neither | Both Bool Bool
HS
cat >Provide.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Provide (field, capture, nested) where

import Data.Proxy (Proxy(..))
import Domain (Choice(..))

field :: foreach (d :: Choice) ->
    (case d of { Neither -> Proxy 'False; Both l r -> Proxy l })
field Neither = Proxy
field (Both l _) = Proxy @l

capture :: foreach (a :: Bool) -> foreach (b :: Bool) -> foreach (d :: Choice) ->
    (case d of { Neither -> Proxy a; Both l r -> Proxy b })
capture a b = \cases
    Neither -> Proxy @a
    (Both _ _) -> Proxy @b

nested :: foreach (d :: Choice) ->
    Maybe (case d of { Neither -> Char; Both True r -> Int; Both False r -> Int })
nested _ = Nothing
HS
cat >Consume.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Consume (sameField, otherField, sameCapture, otherCapture, nested) where

import Data.Proxy (Proxy(..))
import Domain (Choice(..))

sameField :: foreach (x :: Choice) ->
    (case x of { Both first second -> Proxy first; Neither -> Proxy 'False }) -> String
sameField Neither Proxy = "neither"
sameField (Both _ _) Proxy = "both"

otherField :: foreach (x :: Choice) ->
    (case x of { Both first second -> Proxy second; Neither -> Proxy 'False }) -> String
otherField _ _ = "wrong field"

sameCapture :: foreach (x :: Bool) -> foreach (y :: Bool) -> foreach (d :: Choice) ->
    (case d of { Both l r -> Proxy y; Neither -> Proxy x }) -> String
sameCapture _ _ = \cases
    Neither Proxy -> "neither"
    (Both _ _) Proxy -> "both"

otherCapture :: foreach (x :: Bool) -> foreach (y :: Bool) -> foreach (d :: Choice) ->
    (case d of { Both l r -> Proxy x; Neither -> Proxy y }) -> String
otherCapture _ _ _ _ = "wrong capture"

nested :: foreach (d :: Choice) ->
    Maybe (case d of { Both False r -> Int; Neither -> Char; Both True r -> Int }) -> String
nested _ _ = "nested"
HS
cat >Main.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import Data.IORef (newIORef, readIORef, writeIORef)
import Domain (Choice(..))
import qualified Provide as P
import qualified Consume as C

observe :: Bool -> Bool -> Choice -> (String, String)
observe a b d = (C.sameField d (P.field d), C.sameCapture a b d (P.capture a b d))

main :: IO ()
main = do
    cell <- newIORef Neither
    let check = observe (error "forced first capture") (error "forced second capture")
    before <- check <$> readIORef cell
    writeIORef cell (Both (error "forced first field") (error "forced second field"))
    after <- check <$> readIORef cell
    print (before, after)
HS
cat >Reject.hs <<'HS'
{-# LANGUAGE CPP, NoExperimentalDependentTypes #-}
module Reject where

import Domain (Choice)
import qualified Provide as P
import qualified Consume as C

#if defined(FIELD)
bad :: Choice -> String
bad d = C.otherField d (P.field d)
#elif defined(CAPTURE)
bad :: Bool -> Bool -> Choice -> String
bad a b d = C.otherCapture a b d (P.capture a b d)
#elif defined(CAPTURE_ARG)
bad :: Bool -> Bool -> Choice -> String
bad a b d = C.sameCapture a b d (P.capture b a d)
#elif defined(INDEX)
bad :: Choice -> Choice -> String
bad d e = C.sameField d (P.field e)
#else
bad :: Choice -> String
bad d = C.nested d (P.nested d)
#endif
HS

for opt in -O0 -O2; do
    mkdir "${opt#-}"
    cd "${opt#-}"
    cp ../{Domain,Provide,Consume,Main,Reject}.hs .
    for module in Domain Provide Consume; do
        "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c "$module.hs"
        mv "$module.hs" "$module.source"
    done
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" "$opt" Main.o Domain.o Provide.o Consume.o -o run
    ./run > actual
    printf '(("neither","neither"),("both","both"))\n' | diff -u - actual

    for control in FIELD CAPTURE CAPTURE_ARG INDEX NESTED; do
        if "${ghc[@]}" "$opt" "-D$control" -c Reject.hs > "$control.log" 2>&1 ||
           ! grep -Fq 'error: [GHC-83865]' "$control.log"; then
            cat "$control.log" >&2
            exit 1
        fi
    done
    cd ..
done
