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
import TypeLambdaQuote (checkedColumns)
main :: IO ()
main = case checkedColumns of
  Field count (Field flag End) -> print (count, flag)
HS
"${g[@]}" Main.hs TypeLambda.o TypeLambdaQuote.o -o run
printf '%s\n' '(Right 42,Left "missing flag")' >expected
./run >result
cmp expected result
