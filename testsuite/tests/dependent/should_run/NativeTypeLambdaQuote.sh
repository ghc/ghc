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

cat >WildcardQuote.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TemplateHaskell, TypeFamilies, TypeOperators #-}
module WildcardQuote where

import Data.Type.Equality ((:~:)(Refl))

$([d|
  type Direct = case 'Just 'True of { Nothing -> Char; Just _ -> Bool }
  type Nested = (\tag -> case tag of { Nothing -> Char; Just _ -> Bool }) ('Just 'True)
  type CatchAll = case ('Nothing :: Maybe Bool) of { _ -> Int }
  type family Family (tag :: Maybe Bool) where
    Family ('Just _) = Bool
    Family _ = Char
  |])

evidence :: (Direct :~: Bool, Nested :~: Bool, CatchAll :~: Int, Family 'Nothing :~: Char)
evidence = (Refl, Refl, Refl, Refl)
HS
"${g[@]}" -c WildcardQuote.hs

cat >WildcardRhs.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, TemplateHaskell, TypeFamilies #-}
module WildcardRhs where
import Data.Proxy (Proxy)
import Language.Haskell.TH (Dec, Q)

bad :: Q [Dec]
bad = [d|
#ifdef FAMILY
  type family Bad (tag :: Maybe Bool) where
    Bad ('Just _) = Proxy _
#else
  type Bad = case ('Nothing :: Maybe Bool) of { _ -> Proxy _ }
#endif
  |]
HS
for mode in CASE FAMILY; do
    if "${g[@]}" "-D$mode" -c WildcardRhs.hs >wildcard-rhs.log 2>&1 ||
       [[ $(grep -c 'error: \[GHC-65507\]' wildcard-rhs.log) != 1 ]] ||
       ! grep -Fq 'Proxy _' wildcard-rhs.log; then
        cat wildcard-rhs.log >&2
        exit 1
    fi
done

cat >WildcardScope.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}
module WildcardScope where
import Data.Proxy (Proxy)
import Language.Haskell.TH (Dec, Q)

bad :: Q [Dec]
bad = [d|
  type Bad = \tag -> case tag of { Just field -> Proxy field; _ -> Proxy field }
  |]
HS
if "${g[@]}" -c WildcardScope.hs >wildcard-scope.log 2>&1 ||
   ! grep -Fq 'error: [GHC-76037]' wildcard-scope.log ||
   ! grep -Eq "Not in scope: type variable .field." wildcard-scope.log ||
   grep -Fq 'error: [GHC-65507]' wildcard-scope.log; then
    cat wildcard-scope.log >&2
    exit 1
fi

cat >WrongKind.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}
module WrongKind where
import Data.Proxy (Proxy(..))
wrong = $([| Proxy :: Proxy ((\a -> Either 'True a) Bool) |])
HS
if "${g[@]}" -c WrongKind.hs >wrong.log 2>&1 ||
   ! grep -Fq 'WrongKind.hs:4:10: error: [GHC-83865]' wrong.log ||
   ! grep -Fq 'has kind' wrong.log ||
   ! grep -Fq 'In the type lambda in an expression annotation' wrong.log; then
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
"${g[@]}" -c Main.hs
"${g[@]}" Main.o TypeLambda.o TypeLambdaQuote.o QuotedShadow.o -o run
printf '%s\n' "(Right 42,Left \"missing flag\",('a',True),('b',GT,True))" >expected
./run >result
cmp expected result
