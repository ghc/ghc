#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
g=("$1" "-B$2" -v0 -package-env - -i -i. -fexternal-interpreter
   -dcore-lint -dstg-lint -fdiagnostics-color=never)

cd "$work"
mkdir provider client
cd provider
cp "$src"/{TypeLambda,TypeLambdaQuote}.hs .
"${g[@]}" -c TypeLambda.hs
"${g[@]}" -c TypeLambdaQuote.hs
cp ./*.{hi,o} ../client/

cat >Names.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TemplateHaskell #-}
module Names where
import Language.Haskell.TH (Type(..))
$(do shadowed <- [t| \a -> \a -> a |]
     captured <- [t| \a -> \b -> Either a b |]
     case (shadowed, captured) of
       (LamT [outer] (LamT [inner] (VarT use)),
        LamT [a] (LamT [b] (AppT (AppT (ConT _) (VarT a')) (VarT b'))))
         | outer /= inner, inner == use, a == a', b == b' -> pure []
       _ -> fail "A quoted lambda changed the scope of a binder")
HS
"${g[@]}" -c Names.hs

cat >QuotedShadow.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, ScopedTypeVariables, TemplateHaskell #-}
module QuotedShadow (shadowed) where
import Language.Haskell.TH
$(do outer <- newName "label"
     inner <- newName "label"
     cell <- newName "cell"
     let top = mkName "shadowed"
         work = mkName "work"
         row = mkName "row"
         x = mkName "x"
         y = mkName "y"
         arrow arg result = AppT (AppT ArrowT arg) result
         triple a b c = AppT (AppT (AppT (TupleT 3) a) b) c
         quant name = ForallT [KindedTV name SpecifiedSpec StarT] []
         bool = ConT ''Bool
         topType = quant outer (arrow (VarT outer) (triple (VarT outer) (ConT ''Ordering) bool))
         workType = quant inner (arrow (VarT inner) (triple (VarT outer) (VarT inner) bool))
         callback = LamT [cell] (triple (VarT outer) (VarT (mkName "label")) (VarT cell))
         rowType = AppT callback bool
         rowValue = TupE (map Just [VarE x, VarE y, ConE 'True])
         rowDecls = [SigD row rowType, ValD (VarP row) (NormalB rowValue) []]
         workDecls = [SigD work workType, FunD work [Clause [VarP y] (NormalB (VarE row)) rowDecls]]
     pure [SigD top topType,
           FunD top [Clause [VarP x] (NormalB (AppE (VarE work) (ConE 'GT))) workDecls]])
HS
"${g[@]}" -c QuotedShadow.hs
cp QuotedShadow.{hi,o} ../client/

cat >UnavailableKind.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables #-}
module UnavailableKind where
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
bad :: forall (f :: Bool -> Type). Proxy f -> Bool
bad _ = const True witness
  where
    witness :: Proxy ((\cell -> (f 'True, cell)) Bool)
    witness = Proxy
HS
if "${g[@]}" -c UnavailableKind.hs >kind.log 2>&1 ||
   ! grep -Fq 'UnavailableKind.hs:8:24: error: [GHC-27185]' kind.log; then
    cat kind.log >&2
    exit 1
fi

cat >WrongKind.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}
module WrongKind where
import Data.Proxy (Proxy(..))
wrong = $([| Proxy :: Proxy ((\a -> Either 'True a) Bool) |])
HS
if "${g[@]}" -c WrongKind.hs >wrong.log 2>&1 ||
   ! grep -Fq 'error: [GHC-25897]' wrong.log ||
   ! grep -Fq 'has kind' wrong.log; then
    cat wrong.log >&2
    exit 1
fi

cat >Scope.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TemplateHaskell #-}
module Scope where
import Language.Haskell.TH (Q, Type)
quoted :: Q Type
quoted = [t| ((\field -> field) Bool, field) |]
HS
if "${g[@]}" -c Scope.hs >scope.log 2>&1 ||
   ! grep -Fq 'error: [GHC-76037]' scope.log ||
   ! grep -Eq "Not in scope: type variable .field." scope.log; then
    cat scope.log >&2
    exit 1
fi

cd ../client
cat >Main.hs <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module Main where
import TypeLambda (Fields(..))
import TypeLambdaQuote (checkedColumns, labelledColumn)
import QuotedShadow (shadowed)
main :: IO ()
main = case (checkedColumns, labelledColumn 'a') of
  (Field count (Field flag End), Field label End) ->
    print (count, flag, label, shadowed 'b')
HS
"${g[@]}" Main.hs TypeLambda.o TypeLambdaQuote.o QuotedShadow.o -o run
printf '%s\n' "(Right 42,Left \"missing flag\",('a',True),('b',GT,True))" >expected
./run >result
cmp expected result
