#!/usr/bin/env bash
set -euo pipefail
compiler=$1
lib=$2
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
mkdir provider client
cat >provider/Cert.hs <<'HS'
{-# LANGUAGE BangPatterns #-}
module Cert where

flipBit :: Bool -> Bool
flipBit False = True
flipBit True = False

choose :: Bool -> Bool -> Bool
choose False b = b
choose True _ = False

bothDemanded :: Bool -> Bool -> Bool
bothDemanded False b = b
bothDemanded True b = case b of
    True -> False
    False -> True

secondFirst :: Bool -> Bool -> Bool
secondFirst a False = a
secondFirst _a True = False

explicitStrict :: Bool -> Bool -> Bool
explicitStrict False b = b
explicitStrict True !_b = False

three :: Bool -> Bool -> Bool -> Bool
three False b _c = b
three True _b _c = False

{-# NOINLINE guarded #-}
guarded :: Bool -> Bool -> Bool
guarded False b = b
guarded True _ = False

{-# RULES "guarded/true" forall b. guarded True b = True #-}
HS
cat >provider/ImplicitStrict.hs <<'HS'
{-# LANGUAGE Strict #-}
module ImplicitStrict where

choose :: Bool -> Bool -> Bool
choose False b = b
choose True _b = False
HS
flags=("-B$lib" -v0 -O0 -package-env - -fexternal-interpreter -dcore-lint -dstg-lint
       -fomit-interface-pragmas -fno-write-if-simplified-core -fdiagnostics-color=never)
(
    cd provider
    "$compiler" "${flags[@]}" -XNoExperimentalDependentTypes -c Cert.hs
    "$compiler" "${flags[@]}" --show-iface Cert.hi >cert.iface
    grep -Fq 'SourceTable T:flipBit' cert.iface
    grep -Fq 'ExecutableSourceTable T:choose' cert.iface
    for worker in secondFirst three guarded; do
        if grep -Fq "SourceTable T:$worker" cert.iface; then exit 1; fi
    done
    # Only the False branch of these workers has a checked Core equation.
    for worker in bothDemanded explicitStrict; do
        if grep -Fq "ExecutableSourceTable T:$worker" cert.iface; then exit 1; fi
    done
    "$compiler" "${flags[@]}" -XNoExperimentalDependentTypes -c ImplicitStrict.hs
    "$compiler" "${flags[@]}" --show-iface ImplicitStrict.hi >strict.iface
    if grep -Fq 'SourceTable T:choose' strict.iface; then exit 1; fi
)
cp provider/Cert.hi client
cd client
cat >Prefix.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, TypeFamilies #-}
module Prefix where

import GHC.Matchability (U)
import qualified Cert as C

type family Finish (f :: Bool %U -> Bool) (b :: Bool) where
    Finish f b = f b

type Skipped b = Finish (C.choose 'True) b
type Carried b = Finish (C.choose 'False) b
type Unary = Finish C.flipBit 'False
type BothFalse b = C.bothDemanded 'False b
type StrictFalse b = C.explicitStrict 'False b
type StrictTrue b = C.explicitStrict 'True b
HS
"$compiler" "${flags[@]}" -c Prefix.hs
rm Prefix.hs
cat >Proof.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module Proof where

import Data.Type.Equality ((:~:)(Refl))
import Prefix

skipped :: Skipped b :~: 'False
skipped = Refl

carried :: Carried b :~: b
carried = Refl

unary :: Unary :~: 'True
unary = Refl

bothFalse :: BothFalse b :~: b
bothFalse = Refl

strictFalse :: StrictFalse b :~: b
strictFalse = Refl
HS
"$compiler" "${flags[@]}" -c Proof.hs
cat >Demand.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module Demand where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import Prefix

bad :: Proxy b -> StrictTrue b :~: 'False
bad _ = Refl
HS
if "$compiler" "${flags[@]}" -c Demand.hs >demand.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' demand.log || ! grep -Fq 'bad _ = Refl' demand.log; then
    cat demand.log >&2
    exit 1
fi
cat >Ruled.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module Ruled where

import Data.Type.Equality ((:~:)(Refl))
import qualified Cert as C
import Prefix

{-# RULES "redirect" forall b. C.choose True b = True #-}

wrong :: Skipped b :~: 'False
wrong = Refl
HS
if "$compiler" "${flags[@]}" -fenable-rewrite-rules -Wno-inline-rule-shadowing -c Ruled.hs >rule.log 2>&1 ||
   ! grep -Fq '[GHC-83605]' rule.log || ! grep -Fq 'wrong = Refl' rule.log; then
    cat rule.log >&2
    exit 1
fi
cat >Main.hs <<'HS'
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isPrefixOf)
import Cert (choose, flipBit)

throws :: String -> Bool -> IO Bool
throws prefix value = do
    result <- try (evaluate value) :: IO (Either ErrorCall Bool)
    pure (either (isPrefixOf prefix . displayException) (const False) result)

main :: IO ()
main = do
    pivot <- throws "first" (choose (error "first") (error "second"))
    carried <- throws "second" (choose False (error "second"))
    unary <- throws "unary" (flipBit (error "unary"))
    print (choose True (error "skipped"), pivot, carried, unary)
HS
"$compiler" "${flags[@]}" -c Main.hs
"$compiler" "${flags[@]}" Main.o ../provider/Cert.o -o run
test "$(./run)" = '(False,True,True,True)'

cat >Saturated.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Saturated where

import Data.Type.Equality ((:~:)(Refl))
import qualified Cert as C

proof :: C.choose 'False 'False :~: 'False
proof = Refl
HS
cp ../provider/Cert.o .
"$compiler" "${flags[@]}" -c Saturated.hs
"$compiler" "${flags[@]}" --show-iface Saturated.hi >saturated.iface
grep -Eq 'addDependentFile.*Cert[.]o' saturated.iface
sha256sum Cert.hi >provider.sha
sed 's/^choose False b = b$/choose False _b = True/' ../provider/Cert.hs >Cert.hs
"$compiler" "${flags[@]}" -XNoExperimentalDependentTypes -c Cert.hs -o Cert.o -ohi changed.hi
rm Cert.hs
sha256sum -c --quiet provider.sha
if "$compiler" "${flags[@]}" -c Saturated.hs >changed.log 2>&1 ||
   ! grep -Fq 'proof = Refl' changed.log; then
    cat changed.log >&2
    exit 1
fi


# A reducing use also depends on the provider interface through the saved alias.
rm Cert.o
mv changed.hi Cert.hi
sha256sum Prefix.hi >prefix.sha
if "$compiler" "${flags[@]}" -c Proof.hs >nominal-changed.log 2>&1 ||
   ! grep -Fq 'carried = Refl' nominal-changed.log; then
    cat nominal-changed.log >&2
    exit 1
fi
sha256sum -c --quiet prefix.sha
