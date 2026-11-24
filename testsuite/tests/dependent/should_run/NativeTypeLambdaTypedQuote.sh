#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
g=("$1" "-B$2" -v0 -package-env - -i -i. -fexternal-interpreter
   -dcore-lint -dstg-lint -fdiagnostics-color=never)

cat >"$work/TypedCallbacks.hs" <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, Modifiers, StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeApplications #-}
module TypedCallbacks (named, before, after, annotated, captured) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Matchability (U)
import Language.Haskell.TH.Syntax (Code, Q)

type WithError :: Type %U -> Type %U -> Type
type WithError = \err cell -> Either err cell

atInt :: forall (f :: Type %U -> Type). Proxy f -> f Int -> f Int
atInt _ x = x

named :: Code Q (Either String Int)
named = [|| atInt @(WithError String) Proxy (Right 42) ||]

before :: Code Q (Either String Int)
before = [|| atInt @(\cell -> Either String cell) Proxy (Right 42) ||]

$(pure [])

after :: Code Q (Either Char Int)
after = [|| atInt @((\err cell -> Either err cell) Char) Proxy (Left 'x') ||]

annotated :: Code Q (Either String Int)
annotated = [|| Right 43 :: (\err cell -> Either err cell) String Int ||]

captured :: Code Q (String -> Either String Int)
captured = [|| (\msg -> id @((\cell -> Either err cell) Int) (Left msg))
                :: forall err. err -> Either err Int ||]
HS

cat >"$work/Main.hs" <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, TemplateHaskell #-}
module Main where
import TypedCallbacks (after, annotated, before, captured, named)
main :: IO ()
main = print ($$(named), $$(before), $$(after), $$(annotated), $$(captured) "checked")
HS

cat >"$work/BadCallback.hs" <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, TemplateHaskell, TypeApplications #-}
module BadCallback where
import Language.Haskell.TH.Syntax (Code, Q)
bad :: Code Q (Either String Int)
#ifdef SCOPE
bad = [|| id @((\cell -> Either missing cell) Int) (Right 42) ||]
#else
bad = [|| id @((\cell -> Either 'True cell) Int) (Right 42) ||]
#endif
HS

cat >"$work/Stage.hs" <<'HS'
{-# LANGUAGE TemplateHaskell #-}
module Stage where
import Language.Haskell.TH.Syntax (Code, Q)
local :: Code Q Int
local = [|| 42 ||]
bad :: Int
bad = $$(local)
HS

for opt in O0 O2; do
    mkdir -p "$work/$opt/provider" "$work/$opt/consumer"
    cd "$work/$opt/provider"
    cp "$work/TypedCallbacks.hs" .
    "${g[@]}" "-$opt" -fomit-interface-pragmas -c TypedCallbacks.hs
    cp TypedCallbacks.{hi,o} ../consumer/
    cd ../consumer
    cp "$work/Main.hs" .
    "${g[@]}" "-$opt" -c Main.hs
    "${g[@]}" "-$opt" Main.o TypedCallbacks.o -o test
    ./test >result
    printf '%s\n' '(Right 42,Right 42,Left '"'"'x'"'"',Right 43,Left "checked")' | cmp - result
done

cp "$work"/{BadCallback,Stage}.hs .
if "${g[@]}" -DSCOPE -c BadCallback.hs >scope.log 2>&1 ||
   ! grep -Fq 'BadCallback.hs:6:33: error: [GHC-76037]' scope.log ||
   ! grep -Eq "Not in scope: type variable .missing." scope.log; then
    cat scope.log >&2
    exit 1
fi
if "${g[@]}" -c BadCallback.hs >kind.log 2>&1 ||
   ! grep -Fq 'BadCallback.hs:8:33: error: [GHC-83865]' kind.log ||
   ! grep -Fq 'In the type lambda in a visible type argument' kind.log; then
    cat kind.log >&2
    exit 1
fi
if "${g[@]}" -c Stage.hs >stage.log 2>&1 ||
   ! grep -Fq 'Stage.hs:7:10: error: [GHC-28914]' stage.log; then
    cat stage.log >&2
    exit 1
fi
