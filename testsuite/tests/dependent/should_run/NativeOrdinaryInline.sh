#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fpedantic-bottoms)
cd "$tmp"
cat >Provider.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies #-}
module Provider (advance, select) where

type family Payload (flag :: Bool) where
    Payload True = Int
    Payload False = Bool

{-# INLINABLE advance #-}
advance :: Int -> Int
advance n =
    let unused :: foreach (flag :: Bool) -> Payload flag
        unused flag = error "forced unused advance binding"
    in n + 1

{-# INLINE select #-}
select :: Bool -> Int -> Int
select flag =
    let unused :: foreach (flag :: Bool) -> Payload flag
        unused other = error "forced unused select binding"
    in case flag of
        True -> (+ 1)
        False -> \_ -> 0
HS
cat >Main.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import Provider (advance, select)

main :: IO ()
main = do
    forced <- try (evaluate (select (error "forced flag"))) :: IO (Either ErrorCall (Int -> Int))
    let demanded = either (const True) (const False) forced
    print (advance 41, select True 41, select False (error "forced operand"), demanded)
HS
expected='(42,42,0,True)'
for opt in -O1 -O2; do
    out="$tmp/$opt"
    mkdir -p "$out"/{provider,client,make}
    cp Provider.hs "$out/provider/"
    (
        cd "$out/provider"
        "${ghc[@]}" "$opt" -c Provider.hs
        "${ghc[@]}" --show-iface Provider.hi > provider.iface
        sed -n '/^  advance ::/,/^  select ::/p' provider.iface | grep 'Unfolding: Core: Stable' >/dev/null
        sed -n '/^  select ::/,/^Warnings:/p' provider.iface | grep 'Unfolding: Core: StableUser' >/dev/null
    )
    cp Main.hs "$out/client/"
    cp "$out/provider/Provider.hi" "$out/client/"
    (
        cd "$out/client"
        "${ghc[@]}" "$opt" -c Main.hs
        "${ghc[@]}" Main.o ../provider/Provider.o -o run
        test "$(./run)" = "$expected"
    )
    cp Provider.hs Main.hs "$out/make/"
    (
        cd "$out/make"
        "${ghc[@]}" "$opt" --make Main.hs -o run
        test "$(./run)" = "$expected"
    )
done
"${ghc[@]}" --interactive -ignore-dot-ghci -fexternal-interpreter Main.hs > ghci.log <<'GHCi'
main
print (advance 41)
GHCi
printf '%s\n42\n' "$expected" | cmp - ghci.log

cat >LocalProducer.hs <<'HS'
{-# LANGUAGE BangPatterns, DataKinds, ExperimentalDependentTypes, ScopedTypeVariables, TypeApplications #-}
module LocalProducer (inlineCapture, inlinableCapture, forwardLocal, nested, erasedCapture) where

import Data.Proxy (Proxy(..))

data Key (tag :: Bool) = Key

{-# NOINLINE choose #-}
choose :: foreach (tag :: Bool) -> Key tag -> Int -> Int -> Int
choose False _ _ n = n
choose True _ !capture n = capture + n

{-# NOINLINE inlineCapture #-}
inlineCapture :: Int -> Int -> Int
inlineCapture free =
    let {-# INLINE [0] useInline #-}
        useInline :: Int -> Int
        useInline n = choose True Key free n
    in useInline

{-# NOINLINE inlinableCapture #-}
inlinableCapture :: Int -> Int -> Int
inlinableCapture free =
    let {-# INLINABLE useInlinable #-}
        useInlinable :: Int -> Int
        useInlinable n = choose True Key free n
    in useInlinable

{-# NOINLINE withIndex #-}
withIndex :: foreach (tag :: Bool) -> Key tag -> Int -> Int -> Int
withIndex tag key free =
    let {-# INLINE useIndex #-}
        useIndex :: Int -> Int
        useIndex n = choose tag key free n
    in useIndex

{-# NOINLINE forwardLocal #-}
forwardLocal :: Int -> Int -> Int
forwardLocal = withIndex False Key

{-# INLINE [1] nested #-}
nested :: Int -> Int -> Int
nested free n =
    let {-# INLINE [0] inNested #-}
        inNested :: Int -> Int
        inNested x = choose True Key free x
    in inNested n + inNested n

{-# NOINLINE fromType #-}
fromType :: forall a. Proxy a -> Int -> Int
fromType _ n = n

{-# NOINLINE erasedCapture #-}
erasedCapture :: forall a. Proxy a -> (Int, Int)
erasedCapture =
    let {-# INLINABLE useErased #-}
        useErased :: Int
        useErased = choose True Key (fromType (Proxy @a) 17) 1
    in \_ -> (useErased, useErased)
HS
cat >LocalClient.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, TypeApplications #-}
module Main where
import Control.Exception (evaluate, SomeException, try)
import Data.IORef (newIORef, readIORef)
import Data.Proxy (Proxy(..))
import LocalProducer

throws :: IO a -> IO Bool
throws act = either (const True) (const False) <$> try @SomeException act

main :: IO ()
main = do
    bad <- readIORef =<< newIORef (error "captured")
    inlinePartial <- throws (evaluate (inlineCapture bad `seq` ()))
    inlineFull <- throws (evaluate (inlineCapture bad 41))
    inlinablePartial <- throws (evaluate (inlinableCapture bad `seq` ()))
    inlinableFull <- throws (evaluate (inlinableCapture bad 41))
    print (inlineCapture 1 41, inlinableCapture 1 41, forwardLocal bad 41, nested 1 41)
    print (erasedCapture (Proxy @Bool))
    print (inlinePartial, inlineFull, inlinablePartial, inlinableFull)
HS

for opt in -O0 -O1 -O2; do
    out="$tmp/local/$opt"
    mkdir -p "$out"/{provider,client}
    cp LocalProducer.hs "$out/provider/"
    cp LocalClient.hs "$out/client/Main.hs"
    (
        cd "$out/provider"
        "${ghc[@]}" "$opt" -ddump-ds -ddump-simpl -dsuppress-uniques -c LocalProducer.hs > producer.core
        for binder in useInline useInlinable useIndex useErased; do
            grep -m1 -A7 "$binder \[InlPrag=" producer.core | grep -F 'Src=StableRetainedUser' >/dev/null
        done
        if [[ "$opt" != -O0 ]]; then
            sed -n '/==== Tidy Core/,$p' producer.core > tidy.core
            grep -m1 -A7 'useErased \[InlPrag=INLINABLE\] :: Int' tidy.core | grep -F 'Src=StableRetainedUser' >/dev/null
            "${ghc[@]}" --show-iface LocalProducer.hi > producer.iface
            grep -m1 -A6 'inNested ::' producer.iface | grep -F 'Unfolding: Core: StableRetainedUser' >/dev/null
        fi
        cp LocalProducer.hi ../client/
    )
    (
        cd "$out/client"
        "${ghc[@]}" "$opt" -ddump-inlinings -c Main.hs > client.core
        if [[ "$opt" != -O0 ]]; then
            grep -F 'Inlining done: LocalProducer.nested' client.core >/dev/null
        fi
        "${ghc[@]}" Main.o ../provider/LocalProducer.o -o run
        printf '%s\n' '(42,42,41,84)' '(18,18)' '(False,True,False,True)' | cmp - <(./run)
    )
done

cat >LocalPackage.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module LocalPackage (packed, opened, unopened) where

import Data.Kind (Type)

type Hidden = exists (a :: Type). (Maybe a, a -> Int)

{-# NOINLINE inspect #-}
inspect :: Hidden -> Int
inspect (value, render) = maybe 0 render value

{-# NOINLINE hide #-}
hide :: a -> (a -> Int) -> Hidden
hide value render = (Just value, render)

packed :: Int -> Bool -> Int -> Int
packed offset keep n =
    let {-# INLINE packLocal #-}
        packLocal :: Int -> Int
        packLocal n = inspect ((if keep then Just n else Nothing, (+ offset)) :: Hidden)
    in packLocal n + 1

opened :: Int -> Int -> Int
opened offset n =
    let {-# INLINABLE openLocal #-}
        openLocal :: Int -> Int
        openLocal n = case hide n (+ offset) of
            (value, render) -> maybe 0 render value
    in openLocal n + openLocal 2

unopened :: Int -> Int
unopened offset =
    let {-# INLINE unusedLocal #-}
        unusedLocal :: Int -> Int
        unusedLocal _ = case (error "forced unused package" :: Hidden) of
            (value, render) -> maybe 0 render value
    in offset
HS

cat >PackageClient.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import LocalPackage (packed, opened, unopened)

main :: IO ()
main = do
    forced <- try (evaluate (packed 10 True (error "forced retained payload"))) :: IO (Either ErrorCall Int)
    let demanded = either (const True) (const False) forced
    print (packed 10 True 7, packed (error "forced unused offset") False (error "forced unused payload"),
           opened 10 7, unopened 42, demanded)
HS

for opt in -O0 -O1 -O2; do
    out="$tmp/package/$opt"
    mkdir -p "$out"/{provider,client}
    cp LocalPackage.hs "$out/provider/"
    cp PackageClient.hs "$out/client/Main.hs"
    (
        cd "$out/provider"
        # Keep the INLINABLE opener so Tidy must discard its template.
        "${ghc[@]}" "$opt" -ddump-ds -ddump-simpl -ddump-inlinings -dverbose-core2core \
            -funfolding-creation-threshold=0 -dsuppress-uniques \
            -c LocalPackage.hs > provider.core 2> provider.log
        for binder in packLocal openLocal; do
            grep -m1 -A7 "$binder \[InlPrag=" provider.core | grep -F 'Src=StableRetainedUser' >/dev/null
        done
        if [[ "$opt" != -O0 ]]; then
            grep -Fx 'Inlining done: packLocal' provider.core >/dev/null
        fi
        sed -n '/==== Tidy Core/,$p' provider.core > tidy.core
        grep -F 'openLocal [InlPrag=INLINABLE' tidy.core >/dev/null
        if grep -Fq 'Src=StableRetainedUser' tidy.core; then
            echo 'Tidy retained a local template with an erased package operation'
            exit 1
        fi
        "${ghc[@]}" --show-iface LocalPackage.hi > provider.iface
        if grep -Fq 'Unfolding: Core: StableRetainedUser' provider.iface; then
            echo 'Exported a local template with an erased package operation'
            exit 1
        fi
        cp LocalPackage.hi ../client/
    )
    (
        cd "$out/client"
        "${ghc[@]}" "$opt" -c Main.hs
        "${ghc[@]}" Main.o ../provider/LocalPackage.o -o run
        printf '%s\n' '(18,1,29,42,True)' | cmp - <(./run)
    )
done

cat >ExportedPackage.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module ExportedPackage (packed, opened) where

import Data.Kind (Type)

type Hidden = exists (a :: Type). (a, a -> Int)

{-# NOINLINE consume #-}
consume :: Hidden -> Int
consume (value, render) = render value

{-# INLINE packed #-}
packed :: Int -> Bool -> Int -> Int
packed offset keep n =
    consume ((n, \n -> if keep then offset + n else 0) :: Hidden)

{-# NOINLINE hide #-}
hide :: Int -> Hidden
hide n = (n, (+ 1))

{-# INLINABLE opened #-}
opened :: Int -> Int
opened n = case hide n of
    (value, render) -> render value
HS

cat >ExportedPackageClient.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import ExportedPackage (opened, packed)

main :: IO ()
main = do
    required <- try (evaluate (packed 10 True (error "demanded field"))) :: IO (Either ErrorCall Int)
    let demanded = either (const True) (const False) required
    print (packed 10 True 7 + 1,
           packed (error "unused offset") False (error "unused field"),
           opened 22 + 1, demanded)
HS

for opt in -O1 -O2; do
    out="$tmp/exported-package/$opt"
    mkdir -p "$out"/{provider,client}
    cp ExportedPackage.hs "$out/provider/"
    cp ExportedPackageClient.hs "$out/client/Main.hs"
    (
        cd "$out/provider"
        "${ghc[@]}" "$opt" -c ExportedPackage.hs
        "${ghc[@]}" --show-iface ExportedPackage.hi > provider.iface
        grep -Fq 'retained-features-v1-erased+implicit+retained-user-unfolding' provider.iface
        for binder in packed opened; do
            grep -A8 "^  $binder ::" provider.iface | grep -F 'Unfolding: Core: StableRetainedUser' >/dev/null
        done
        cp ExportedPackage.hi ../client/
    )
    (
        cd "$out/client"
        "${ghc[@]}" "$opt" -ddump-inlinings -c Main.hs > client.core
        for binder in packed opened; do
            grep -Fx "Inlining done: ExportedPackage.$binder" client.core >/dev/null
        done
        "${ghc[@]}" Main.o ../provider/ExportedPackage.o -o run
        printf '%s\n' '(18,0,24,True)' | cmp - <(./run)
    )
done

cat >TopRecursive.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module TopRecursive (entry) where

import Data.Kind (Type)

type Hidden = exists (a :: Type). Maybe a

{-# NOINLINE consume #-}
consume :: Hidden -> Int
consume _ = 42

{-# INLINABLE top #-}
top n = if n == 0 then consume (Just n :: Hidden) else peer (n - 1)
peer n = top n

entry :: Int -> Int
entry = top
HS
if "${ghc[@]}" -O1 -fdiagnostics-color=never -fforce-recomp -c TopRecursive.hs > residual.log 2>&1; then
    echo 'Accepted a package template for an internal worker of a top-level recursive group'
    exit 1
fi
grep -qFx 'TopRecursive.hs:13:1: error: [GHC-92256]' residual.log
grep -q 'The INLINABLE unfolding of .*top.* contains unsupported retained Core.' residual.log

cp "$src"/{FinitePosition,ParameterizedIndexLibrary,PackedVecLibrary}.hs .
cat >FinConstruction.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications #-}
module FinConstruction (zero) where
import FinitePosition
import ParameterizedIndexLibrary (Fin(..))
import PackedVecLibrary (Nat(..))

{-# NOINLINE inspect #-}
inspect :: forall n. foreach (i :: Fin n) -> FinView i
inspect i = viewFin i

{-# INLINE zero #-}
zero :: forall n. FinView ('FZ @n)
zero = inspect @('S n) FZ
HS
mkdir -p fin
cat >fin/Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeApplications #-}
module Main where
import FinConstruction (zero)
import FinitePosition (FinView(AtZero))
import PackedVecLibrary (Nat(Z))

main :: IO ()
main = print (case zero @'Z of AtZero -> True)
HS

for opt in -O1 -O2; do
    out="$tmp/fin/$opt"
    mkdir -p "$out"/{provider,client,make}
    cp {FinitePosition,FinConstruction,ParameterizedIndexLibrary,PackedVecLibrary}.hs "$out/provider/"
    (
        cd "$out/provider"
        "${ghc[@]}" "$opt" -fwrite-source-equations -c PackedVecLibrary.hs
        "${ghc[@]}" "$opt" -c ParameterizedIndexLibrary.hs FinitePosition.hs FinConstruction.hs
        "${ghc[@]}" --show-iface FinConstruction.hi > producer.iface
        grep -Fq 'retained-user-unfolding' producer.iface
        grep -Fq 'Unfolding: Core: StableRetainedUser' producer.iface
    )
    cp fin/Main.hs "$out/client/"
    cp "$out/provider/"*.hi "$out/client/"
    (
        cd "$out/client"
        "${ghc[@]}" "$opt" -ddump-inlinings -c Main.hs > client.log
        grep -Fq 'Inlining done: FinConstruction.zero' client.log
        "${ghc[@]}" Main.o ../provider/*.o -o run
        test "$(./run)" = True
    )
    cp "$out/provider/"*.hs fin/Main.hs "$out/make/"
    (
        cd "$out/make"
        "${ghc[@]}" "$opt" -fwrite-source-equations --make -ddump-inlinings Main.hs -o run > make.log
        grep -Fq 'Inlining done: FinConstruction.zero' make.log
        test "$(./run)" = True
        if [[ $opt == -O1 ]]; then
            "${ghc[@]}" --interactive -fobject-code -fexternal-interpreter -ignore-dot-ghci "$opt" Main.hs > ghci.log <<'GHCi'
:set -XDataKinds -XGADTs -XTypeApplications -XNoExperimentalDependentTypes
import FinConstruction
import FinitePosition
import PackedVecLibrary
print (case zero @'Z of AtZero -> True)
GHCi
            printf 'True\n' | cmp - ghci.log
        fi
    )
done

# The wrapper emits its ordinary template without seeing the marker on zero.
cd "$out/client"
cat >Wrapper.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeApplications #-}
module Wrapper (deferred) where
import FinConstruction (zero)
import FinitePosition (FinView)
import GHC.Exts (inline)
import PackedVecLibrary (Nat(Z))
import ParameterizedIndexLibrary (Fin(FZ))

{-# INLINE deferred #-}
deferred :: Applicative f => f (FinView ('FZ @'Z))
deferred = pure (inline (zero @'Z))
HS
"${ghc[@]}" -O2 -fignore-interface-pragmas -c Wrapper.hs
cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeApplications #-}
module Main where
import FinitePosition (FinView(AtZero))
import Wrapper (deferred)

main :: IO ()
main = print (case deferred @Maybe of Just AtZero -> True; Nothing -> False)
HS
"${ghc[@]}" -O2 -ddump-inlinings -ddump-rule-firings -c Main.hs > deferred.log
grep -Fq 'Inlining done: Wrapper.deferred' deferred.log
if grep -Fq 'Rule fired: Inline (BUILTIN)' deferred.log; then
    echo 'The ordinary optimizer unfolded the marked call through GHC.Exts.inline'
    exit 1
fi
"${ghc[@]}" Main.o Wrapper.o ../provider/*.o -o run
test "$(./run)" = True
