#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -fexternal-interpreter
     -dcore-lint -dstg-lint -fdiagnostics-color=never)

cd "$work"
mkdir provider client
cd provider
cp "$src"/{TypeLambda,TypeLambdaArgument}.hs .
"${ghc[@]}" -fomit-interface-pragmas -c TypeLambda.hs TypeLambdaArgument.hs
cp ./*.hi ../client/

cd ../client
cat >Calls.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables, TemplateHaskell, TypeApplications #-}
module Calls (shadowed, spliced, ordinaryWildcard) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import TypeLambda (Fields)
import TypeLambdaArgument (rowFor)

shadowed :: forall (cell :: Type) (e :: Type). Proxy cell -> e -> Fields '[Either e Int, Either e Bool]
shadowed _ msg = rowFor @(\cell -> Either e cell) Proxy (Right 42) (Left msg)

spliced :: Fields '[Either String Int, Either String Bool]
spliced = $([| rowFor @(\cell -> Either String cell) Proxy (Right 42) (Left "quoted flag") |])

ordinaryWildcard :: Int
ordinaryWildcard = id @_ 17
HS
"${ghc[@]}" -c Calls.hs

cat >SplicedTypeApp.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, TypeApplications #-}
module SplicedTypeApp (ordinarySplice, splicedMulti) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Language.Haskell.TH.Syntax (addModFinalizer, runIO)
import TypeLambda (Fields)
import TypeLambdaArgument (rowFor)

ordinarySplice :: Int
ordinarySplice = id @($([t| Int |])) 18

splicedMulti :: forall (e :: Type). e -> Fields '[Either e Int, Either e Bool]
#ifdef QUOTED
splicedMulti msg =
    rowFor @($(do
        runIO (appendFile "splice-events" "expanded\n")
        addModFinalizer (runIO (appendFile "splice-events" "finalized\n"))
        [t| \err cell -> Either err cell |]) e)
        Proxy (Right 42) (Left msg)
#else
splicedMulti msg =
    rowFor @((\err cell -> Either err cell) e) Proxy (Right 42) (Left msg)
#endif
HS
"${ghc[@]}" -DQUOTED -c SplicedTypeApp.hs
printf 'expanded\nfinalized\n' >expected-events
cmp expected-events splice-events

cat >BadSplicedArgument.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, TemplateHaskell, TypeApplications #-}
module BadSplicedArgument where
import Data.Proxy (Proxy(..))
import TypeLambdaArgument (rowFor)
#ifdef SCOPE
bad = rowFor @($([t| \err cell -> Either missing cell |]) String) Proxy (Right 42) (Left "flag")
#else
bad = rowFor @($([t| \err cell -> Either 'True cell |]) String) Proxy (Right 42) (Left "flag")
#endif
HS
if "${ghc[@]}" -DSCOPE -c BadSplicedArgument.hs >splice-scope.log 2>&1 ||
   ! grep -Eq '^BadSplicedArgument.hs:6:[0-9]+: error: \[GHC-76037\]' splice-scope.log ||
   ! grep -Eq "Not in scope: type variable .missing." splice-scope.log; then
    cat splice-scope.log >&2
    exit 1
fi
if "${ghc[@]}" -c BadSplicedArgument.hs >splice-kind.log 2>&1 ||
   ! grep -Eq '^BadSplicedArgument.hs:8:[0-9]+: error: \[GHC-83865\]' splice-kind.log ||
   ! grep -Fq 'type lambda in a visible type argument' splice-kind.log; then
    cat splice-kind.log >&2
    exit 1
fi

cat >BadSplicedCapture.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, TypeApplications #-}
module BadSplicedCapture where

import Data.Proxy (Proxy(..))
import TypeLambda (Fields)
import TypeLambdaArgument (rowFor)
bad :: forall err. Proxy err -> Fields '[Either String Int, Either String Bool]
bad _ = rowFor @($([t| \tag cell -> Either tag cell |]) err) Proxy (Right 42) (Right True)
HS
if "${ghc[@]}" -c BadSplicedCapture.hs >splice-capture.log 2>&1; then
    exit 1
fi
sed '/^[[:space:]]*|/,$d' splice-capture.log >splice-capture.body
if ! grep -Fq 'BadSplicedCapture.hs:9:57: error: [GHC-25897]' splice-capture.body ||
   ! grep -Eq ".k. is a rigid type variable" splice-capture.body ||
   ! grep -F 'In the type' splice-capture.body |
       grep -Fq '($([t| \tag cell -> Either tag cell |]) err)' ||
   grep -Fq '$signature-lambda:' splice-capture.body; then
    cat splice-capture.log >&2
    exit 1
fi

cat >BadArgument.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, NamedWildCards, TypeApplications #-}
module BadArgument where
import Data.Proxy (Proxy(..))
import TypeLambdaArgument (rowFor)

#if defined(FREE)
bad = rowFor @missing Proxy (Right 42) (Left "flag")
#elif defined(FREE_LAMBDA)
bad = rowFor @(\cell -> Either missing cell) Proxy (Right 42) (Left "flag")
#elif defined(ANONYMOUS)
bad = rowFor @(\cell -> Either _ cell) Proxy (Right 42) (Left "flag")
#elif defined(NAMED)
bad = rowFor @(\cell -> Either _problem cell) Proxy (Right 42) (Left "flag")
#else
bad = rowFor @(\cell -> Either 'True cell) Proxy (Right 42) (Left "flag")
#endif
HS
reject() {
    local mode=$1 code=$2 message=$3
    if "${ghc[@]}" "-D$mode" -c BadArgument.hs >"$mode.log" 2>&1 ||
       ! grep -Eq "error: \\[GHC-($code)\\]" "$mode.log" || ! grep -Fq "$message" "$mode.log"; then
        cat "$mode.log" >&2
        exit 1
    fi
}
reject FREE 76037 'Not in scope: type variable'
reject FREE_LAMBDA 27185 'A type lambda needs a supported position'
reject ANONYMOUS 27185 'A type lambda needs a supported position'
reject NAMED 27185 'A type lambda needs a supported position'
reject KIND '83865|25897' 'type lambda in a visible type argument'

mv Calls.hs SplicedTypeApp.hs ../provider/
cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeApplications #-}
module Main where
import Data.Proxy (Proxy(..))
import Calls (ordinaryWildcard, shadowed, spliced)
import SplicedTypeApp (ordinarySplice, splicedMulti)
import TypeLambda (Fields(..))
import TypeLambdaArgument (messageCells)

printCells :: Fields '[Either String Int, Either String Bool] -> IO ()
printCells (Field count (Field flag End)) = print (count, flag)

main :: IO ()
main = do
    printCells (messageCells "missing flag")
    printCells (shadowed (Proxy @Ordering) "shadowed flag")
    printCells spliced
    printCells (splicedMulti "type-spliced flag")
    print ordinaryWildcard
    print ordinarySplice
HS
"${ghc[@]}" -c Main.hs
"${ghc[@]}" Main.o Calls.o SplicedTypeApp.o ../provider/{TypeLambda,TypeLambdaArgument}.o -o run
./run >actual
cat >expected <<'OUT'
(Right 42,Left "missing flag")
(Right 42,Left "shadowed flag")
(Right 42,Left "quoted flag")
(Right 42,Left "type-spliced flag")
17
18
OUT
cmp expected actual
