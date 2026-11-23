#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -fexternal-interpreter
     -dcore-lint -dstg-lint -fdiagnostics-color=never)

cd "$work"
mkdir provider capture client
cd provider
cp "$src"/TypeLambda.hs .
cat >RowBuilder.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers #-}
module RowBuilder (rowFor) where
import Data.Kind (Type)
import Data.Proxy (Proxy)
import GHC.Matchability (U)
import TypeLambda (Fields(..))

rowFor :: forall (f :: Type %U -> Type). Proxy f -> f Int -> f Bool -> Fields '[f Int, f Bool]
rowFor _ count flag = Field count (Field flag End)
HS
"${ghc[@]}" -fomit-interface-pragmas -c TypeLambda.hs RowBuilder.hs
cp ./*.hi ../capture/

cd ../capture
cat >Captured.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, ScopedTypeVariables, TemplateHaskell, TypeApplications #-}
module Captured (captured) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import RowBuilder (rowFor)
import TypeLambda (Fields)

#ifdef CONFLICT
captured :: forall (err :: Type) (cell :: Type). Proxy err -> Proxy cell -> Fields '[Either String Int, Either String Bool]
#else
captured :: forall (err :: Type) (cell :: Type). Proxy err -> Proxy cell -> Fields '[Either err Int, Either err Bool]
#endif
captured _ _ = $([| rowFor @(\cell -> Either err cell) Proxy (Right 42) (Right True) |])
HS

status=0
if ! "${ghc[@]}" -c Captured.hs >positive.log 2>&1; then
    cat positive.log >&2
    status=1
fi
# Only the result type supplies the conflicting String index.
mkdir conflict
cp Captured.hs RowBuilder.hi TypeLambda.hi conflict/
if (cd conflict && "${ghc[@]}" -DCONFLICT -c Captured.hs) >conflict.log 2>&1 ||
   ! grep -Fq 'Captured.hs:14:17: error: [GHC-25897]' conflict.log ||
   ! grep -Eq ".err. is a rigid type variable" conflict.log; then
    cat conflict.log >&2
    status=1
fi
test "$status" = 0
cp Captured.hi TypeLambda.hi ../client/

cd ../client
cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeApplications #-}
module Main where
import Captured (captured)
import Data.Proxy (Proxy(..))
import TypeLambda (Fields(..))

cells :: Fields '[Either e Int, Either e Bool] -> (Either e Int, Either e Bool)
cells (Field count (Field flag End)) = (count, flag)

main :: IO ()
main = do
    print (cells (captured (Proxy @Bool) (Proxy @Ordering)))
    print (cells (captured (Proxy @Char) (Proxy @())))
HS
"${ghc[@]}" -c Main.hs
"${ghc[@]}" Main.o ../capture/Captured.o ../provider/{TypeLambda,RowBuilder}.o -o run
./run >actual
printf '(Right 42,Right True)\n(Right 42,Right True)\n' >expected
cmp expected actual
