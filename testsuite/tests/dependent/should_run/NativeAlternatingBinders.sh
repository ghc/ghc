#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint)
provider=NativeAlternatingBindersProvider
client=NativeAlternatingBindersClient

for opt in O0 O2; do
    flags=("-$opt")
    if [[ $opt == O0 ]]; then
        flags+=(-fomit-interface-pragmas -fno-write-if-simplified-core)
    fi
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/$provider.hs" .
    "${ghc[@]}" "${flags[@]}" -c "$provider.hs"
    mv "$provider.hs" ../
    cp "$provider.hi" "$src/$client.hs" ../client/
    cd ../client
    "${ghc[@]}" "-$opt" -XNoExperimentalDependentTypes -ddump-rule-firings -c "$client.hs" >firings
    if [[ $opt == O2 ]]; then
        grep -Fq "Rule fired: keepCons/Bool ($provider)" firings || { cat firings; exit 1; }
    fi
    "${ghc[@]}" "-$opt" "$client.o" "../provider/$provider.o" -o run
    ./run
done

cat >WrongKind.hs <<'HS'
{-# LANGUAGE DataKinds, TypeApplications #-}
module WrongKind where
import NativeAlternatingBindersProvider
wrong = forwardKind True @'EQ Marker
HS
if "${ghc[@]}" -XNoExperimentalDependentTypes -c WrongKind.hs >reject.log 2>&1; then
    echo 'Accepted the wrong kind for an erased argument' >&2
    exit 1
fi
grep -Fq "Couldn't match kind ‘Ordering’ with ‘Bool’" reject.log || { cat reject.log >&2; exit 1; }

sed -e 's/TypeApplications/ExplicitNamespaces, RequiredTypeArguments/' \
    -e "s/forwardKind True @'EQ/requiredKind True (type 'EQ)/" WrongKind.hs >WrongRequiredKind.hs
if "${ghc[@]}" -XNoExperimentalDependentTypes -c WrongRequiredKind.hs >reject.log 2>&1 ||
   ! grep -Fq "Couldn't match kind ‘Ordering’ with ‘Bool’" reject.log; then
    cat reject.log >&2
    exit 1
fi

cat >WrongIndex.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, TypeAbstractions #-}
module WrongIndex where
import NativeAlternatingBindersProvider
wrong :: foreach (left :: Bool) -> foreach (right :: Bool) ->
         forall a. foreach (m :: Maybe a) -> Reading left m -> Reading right m
wrong l r @a m x = keepAfter r m x
HS
if "${ghc[@]}" -c WrongIndex.hs >reject.log 2>&1; then
    echo 'Accepted two different runtime indices' >&2
    exit 1
fi
grep -Fq "Couldn't match type ‘l’ with ‘r’" reject.log || { cat reject.log >&2; exit 1; }

cat >MissingShow.hs <<'HS'
module MissingShow where
import NativeAlternatingBindersProvider
data NoShow = NoShow
wrong :: Maybe NoShow -> (String, Int)
wrong index = showAfter True index NoShow (Reading 51)
HS
if "${ghc[@]}" -XNoExperimentalDependentTypes -c MissingShow.hs >reject.log 2>&1; then
    echo 'Accepted a call without the required Show instance' >&2
    exit 1
fi
grep -Fq 'No instance for ‘Show NoShow’' reject.log || { cat reject.log >&2; exit 1; }

cat >WrongCons.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables, TypeAbstractions, TypeOperators #-}
module WrongCons where
import Data.Type.Equality ((:~:))
import NativeAlternatingBindersProvider

wrongProof :: foreach (phase :: Phase) -> forall a.
              foreach (xs :: [Maybe a]) -> foreach (ys :: [Maybe a]) ->
              ('Nothing ': ys) :~: ('Nothing ': ys) ->
              ConsReading phase ('Nothing ': xs) -> ConsReading phase ('Nothing ': xs)
wrongProof phase @a xs ys p x = keepCons phase @a xs p x

wrongPhase :: foreach (phase :: Phase) -> foreach (other :: Phase) -> forall a.
              foreach (xs :: [Maybe a]) -> ('Nothing ': xs) :~: ('Nothing ': xs) ->
              ConsReading phase ('Nothing ': xs) -> ConsReading phase ('Nothing ': xs)
wrongPhase phase other @a xs p x = keepCons other @a xs p x
HS
if "${ghc[@]}" -c WrongCons.hs >reject.log 2>&1 ||
   ! grep -Fq "Couldn't match type ‘ys’ with ‘xs’" reject.log ||
   ! grep -Fq "Couldn't match type ‘other’ with ‘phase’" reject.log; then
    cat reject.log >&2
    exit 1
fi

cat >LambdaArrow.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, CPP, DataKinds, ExperimentalDependentTypes, ExplicitNamespaces, Modifiers #-}
{-# LANGUAGE PolyKinds, RankNTypes, RequiredTypeArguments #-}
module LambdaArrow where
import GHC.Exts (M, Matchability, U)

data Payload (tag :: Bool) (m :: Maybe a) = Payload Int

#if FORM == 1
keep :: foreach (tag :: Bool) -> Payload tag (Nothing :: Maybe Bool) %MA -> Payload tag (Nothing :: Maybe Bool)
keep _ x = x
#elif FORM == 2
keep :: foreach (tag :: Bool) -> forall a -> foreach (m :: Maybe a) -> Payload tag m %MA -> Payload tag m
keep _ (type a) _ x = x
#elif FORM == 3
keep :: foreach (tag :: Bool) -> Payload tag (Nothing :: Maybe Bool) %MA -> Payload tag (Nothing :: Maybe Bool)
keep False x = x
keep True x = x
#elif FORM == 4
keep :: (foreach (tag :: Bool) -> forall a -> foreach (m :: Maybe a) -> Payload tag m %M -> Payload tag m)
     -> foreach (tag :: Bool) -> forall a -> foreach (m :: Maybe a) -> Payload tag m %M -> Payload tag m
keep existing = existing
#else
ignore :: forall (ma :: Matchability).
          (foreach (tag :: Bool) -> forall a -> foreach (m :: Maybe a) -> Payload tag m %ma -> Payload tag m) -> Int
ignore _ = 61

infer :: Int
infer = ignore (\_ (type a) _ x -> x)
#endif
HS
for opt in O0 O2; do
    "${ghc[@]}" "-$opt" -DFORM=4 -fforce-recomp -c LambdaArrow.hs
    "${ghc[@]}" "-$opt" -DFORM=5 -fforce-recomp -c LambdaArrow.hs
    for form in 1 2 3; do
        "${ghc[@]}" "-$opt" "-DFORM=$form" -DMA=U -fforce-recomp -c LambdaArrow.hs
        if "${ghc[@]}" "-$opt" "-DFORM=$form" -DMA=M -fforce-recomp -c LambdaArrow.hs >reject.log 2>&1 ||
           ! grep -Fq '[GHC-83865]' reject.log || ! grep -Fq 'Unmatchable' reject.log ||
           ! grep -Fq 'Expected: M' reject.log || grep -Fq 'Core Lint errors' reject.log; then
            cat reject.log >&2
            exit 1
        fi
    done
done

cat >RankProbe.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, PolyKinds, RankNTypes #-}
{-# LANGUAGE TypeAbstractions, TypeApplications #-}
#ifdef DEEP
{-# LANGUAGE DeepSubsumption #-}
#endif
module RankProbe (probe) where
import Data.Kind (Type)
import NativeAlternatingBindersProvider

#if FORM == 2
-- Ordinary arrows still permit a wrapper around an exact retained suffix.
type Worker = () -> forall a. Show a => foreach (m :: Maybe a) -> Reading True m -> Reading True m
#elif FORM == 3 || FORM == 4
-- The analogous dictionary wrapper is now below the first retained argument.
type Worker = foreach (b :: Bool) -> forall a. Show a => foreach (m :: Maybe a) -> Reading b m -> Reading b m
#else
type Worker = foreach (b :: Bool) -> forall (a :: Type). foreach (m :: Maybe a) -> Reading b m -> Reading b m
#endif

probe :: Bool -> Maybe Bool -> Maybe Char -> (Int, Int)
probe tag ib ic =
    let {-# INLINABLE applyBoth #-}
        applyBoth :: Worker -> (Int, Int)
#if FORM == 2
        applyBoth f = (readInt (f () @Bool ib (Reading 41)), readInt (f () @Char ic (Reading 42)))
    in applyBoth plainFirst
#else
        applyBoth f = (readInt (f tag @Bool ib (Reading 41)), readInt (f False @Char ic (Reading 42)))
#if FORM == 4
    in applyBoth (\b @a -> keepAfter b @a)
#else
    in applyBoth keepAfter
#endif
#endif
  where
    readInt (Reading n) = n
HS

cat >OrdinaryRank.hs <<'HS'
{-# LANGUAGE DeepSubsumption, NoExperimentalDependentTypes, RankNTypes, TypeApplications #-}
module OrdinaryRank (pair) where

{-# NOINLINE worker #-}
worker :: Bool -> forall a. Maybe a -> Int -> Int
worker _ _ x = x

pair :: Maybe Bool -> Maybe Char -> (Int, Int)
pair ib ic =
    let {-# INLINABLE applyBoth #-}
        applyBoth :: (Bool -> forall a. Show a => Maybe a -> Int -> Int) -> (Int, Int)
        applyBoth f = (f True @Bool ib 41, f False @Char ic 42)
    in applyBoth worker
HS

cat >HigherRankMain.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where
import ForwardDict
import RankProbe
import qualified OrdinaryRank

main :: IO ()
main = do
    print (probe tag first second, OrdinaryRank.pair first second)
    print (forward tag first second)
  where
    tag = error "retained tag"
    first = error "first index"
    second = error "second index"
HS

cat >ForwardDict.hs <<'HS'
{-# LANGUAGE DataKinds, DeepSubsumption, ExperimentalDependentTypes, PolyKinds, RankNTypes, TypeApplications #-}
module ForwardDict (forward) where
import NativeAlternatingBindersProvider (Reading(..), showAfter)

class Show a => Display a
instance Display Bool
instance Display Char

forward :: Bool -> Maybe Bool -> Maybe Char -> ((String, Int), (String, Int))
forward tag ib ic =
    let apply :: (foreach (b :: Bool) -> forall a. Display a => foreach (m :: Maybe a) ->
                  a -> Reading b m -> (String, Int)) -> ((String, Int), (String, Int))
        apply worker = (worker tag @Bool ib True (Reading 41), worker False @Char ic 'x' (Reading 42))
    in apply showAfter
HS

cat >IndependentRank.hs <<'HS'
{-# LANGUAGE DataKinds, DeepSubsumption, ExperimentalDependentTypes, PolyKinds, RankNTypes #-}
module IndependentRank where
import Data.Proxy (Proxy)
import NativeAlternatingBindersProvider (Reading(..))

bad :: foreach (left :: Bool) -> foreach (right :: Bool) ->
       (foreach (b :: Bool) -> forall a. foreach (m :: Maybe a) -> (Reading left m, Proxy b)) ->
       Reading right ('Nothing :: Maybe Bool)
bad left right worker =
    let use :: (foreach (b :: Bool) -> forall a. Show a =>
                  foreach (m :: Maybe a) -> (Reading right m, Proxy b)) ->
               Reading right ('Nothing :: Maybe Bool)
        use _ = Reading 42
    in use worker
HS

for opt in O0 O1 O2; do
    mkdir -p "$out/rank-$opt"/{provider,client}
    cp "$src/$provider.hs" "$out/rank-$opt/provider/"
    cp {RankProbe,OrdinaryRank,HigherRankMain,ForwardDict,IndependentRank}.hs "$out/rank-$opt/client/"
    (
        cd "$out/rank-$opt/provider"
        "${ghc[@]}" "-$opt" -c "$provider.hs"
        cp "$provider.hi" ../client/
        cd ../client
        "${ghc[@]}" "-$opt" -c OrdinaryRank.hs
        "${ghc[@]}" "-$opt" -c ForwardDict.hs
        for form in 1 2 3 4; do
            mode=()
            if [[ "$form" == 2 || "$form" == 3 ]]; then mode=(-DDEEP); fi
            "${ghc[@]}" "-$opt" "${mode[@]}" "-DFORM=$form" -fforce-recomp -c RankProbe.hs
            "${ghc[@]}" "-$opt" -fforce-recomp -c HigherRankMain.hs -o HigherRankMain.o
            "${ghc[@]}" HigherRankMain.o RankProbe.o OrdinaryRank.o ForwardDict.o "../provider/$provider.o" -o run
            printf '%s\n' '((41,42),(41,42))' "((\"True\",41),(\"'x'\",42))" | cmp - <(./run)
        done
        if "${ghc[@]}" "-$opt" -DFORM=2 -fforce-recomp -c RankProbe.hs >reject.log 2>&1 ||
           ! grep -Fq 'error: [GHC-83865]' reject.log ||
           ! grep -Fq 'first argument of ‘applyBoth’, namely ‘plainFirst’' reject.log; then
            cat reject.log >&2
            exit 1
        fi
        if "${ghc[@]}" "-$opt" -DFORM=3 -fforce-recomp -c RankProbe.hs >reject.log 2>&1 ||
           ! grep -Fq 'error: [GHC-83865]' reject.log ||
           ! grep -Fq 'first argument of ‘applyBoth’, namely ‘keepAfter’' reject.log; then
            cat reject.log >&2
            exit 1
        fi
        if "${ghc[@]}" "-$opt" -fforce-recomp -c IndependentRank.hs >reject.log 2>&1 ||
           ! grep -Fq 'error: [GHC-83865]' reject.log ||
           ! grep -Fq 'first argument of ‘use’, namely ‘worker’' reject.log ||
           ! grep -Fq 'Reading left m' reject.log || ! grep -Fq 'Reading right m' reject.log; then
            cat reject.log >&2
            exit 1
        fi
    )
done
