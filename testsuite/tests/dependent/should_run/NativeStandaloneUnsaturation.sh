#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

ghc=("$1" "-B$2" -v0 -XHaskell2010 -O0 -package-env - -i -i.
     -fforce-recomp -fomit-interface-pragmas -dcore-lint -dstg-lint
     -fdiagnostics-color=never)
standalone=(-XUnsaturatedTypeFamilies -XNoExperimentalDependentTypes -XNoLiberalTypeSynonyms)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
mkdir provider client

cat >provider/Provider.hs <<'HS'
module Provider (Apply, Box(Box), Identity, Plain, Result) where

type family Identity a where
    Identity a = a

type Apply f a = f a
type Plain a = Maybe a
type Result = Apply Identity Bool

data Box f = Box (f Bool)
HS
(cd provider && "${ghc[@]}" "${standalone[@]}" -XPolyKinds -c Provider.hs)
cp provider/Provider.hi client/
cd client
cat >Client.hs <<'HS'
module Client where

import Provider

ordinary :: Apply Maybe Bool
ordinary = Just True

family :: Apply Identity Bool
family = True

synonym :: Box Plain
synonym = Box (Just True)

boxedFamily :: Box Identity
boxedFamily = Box True
HS
cat >OrdinaryClient.hs <<'HS'
module OrdinaryClient where

import Provider

ordinary :: Apply Maybe Bool
ordinary = Just True

saturatedAlias :: Result
saturatedAlias = True
HS

reject() {
    local code=$1 message=$2
    shift 2
    if "${ghc[@]}" "$@" >reject.log 2>&1 ||
       ! grep -Fq "[GHC-$code]" reject.log || ! grep -Fq "$message" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

"${ghc[@]}" "${standalone[@]}" -c Client.hs
"${ghc[@]}" -XNoExperimentalDependentTypes -XNoLiberalTypeSynonyms -c OrdinaryClient.hs
reject 27346 'UnsaturatedTypeFamilies' "${standalone[@]}" -XNoUnsaturatedTypeFamilies -c Client.hs
"${ghc[@]}" -XNoUnsaturatedTypeFamilies "${standalone[@]}" -c Client.hs

# Keep the umbrella's original admission and independent flag implications.
"${ghc[@]}" -XExperimentalDependentTypes -XNoUnsaturatedTypeFamilies -c Client.hs
cat >LocalGeneralisation.hs <<'HS'
module LocalGeneralisation where

both :: a -> (Bool, Char)
both x =
    let select y = const y x
    in (select True, select 'x')
HS
"${ghc[@]}" -XExperimentalDependentTypes -c LocalGeneralisation.hs
reject 83865 'select' "${standalone[@]}" -c LocalGeneralisation.hs
"${ghc[@]}" "${standalone[@]}" -XNoMonoLocalBinds -c LocalGeneralisation.hs

cat >ForeachOnly.hs <<'HS'
{-# LANGUAGE DataKinds #-}
module ForeachOnly where

retain :: foreach (b :: Bool) -> Bool
retain b = b
HS
cat >SomeOnly.hs <<'HS'
{-# LANGUAGE DataKinds, KindSignatures #-}
module SomeOnly where

import Data.Proxy (Proxy)

type Packet = some (b :: Bool) | Proxy b
HS
cat >PromoteOnly.hs <<'HS'
{-# LANGUAGE DataKinds #-}
module PromoteOnly where

import qualified Prelude as P

type Promoted = P.not 'P.False
HS
for module in ForeachOnly SomeOnly PromoteOnly; do
    "${ghc[@]}" -XExperimentalDependentTypes -c "$module.hs"
done
reject 83865 'foreach b' "${standalone[@]}" -c ForeachOnly.hs
reject 58481 'parse error' "${standalone[@]}" -c SomeOnly.hs
reject 37479 'term-level binding' "${standalone[@]}" -c PromoteOnly.hs
