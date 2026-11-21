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

cat >RankDomainProvider.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds #-}
{-# LANGUAGE RankNTypes, TypeApplications, TypeFamilies, TypeOperators #-}
module RankDomainProvider
    ( Reflect, reflect, bottomReflect, wrongResult, fixedDomain
    , AmbiguousReflect, ambiguous
    ) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))

type Reflect a = foreach (x :: Maybe a) -> x :~: x

reflect :: forall a. Show a => Reflect a
reflect _ = Refl

bottomReflect :: forall a. Show a => Reflect a
bottomReflect = error "callback bottom"

wrongResult :: forall a. Show a => foreach (x :: Maybe a) -> Proxy x
wrongResult _ = Proxy

fixedDomain :: Reflect Bool
fixedDomain _ = Refl

type family Opaque a
type BuiltinReflect a = foreach (x :: Maybe (Opaque a)) -> x :~: x

class Extra a
type AmbiguousReflect a = Extra a => BuiltinReflect a

ambiguous :: forall a. AmbiguousReflect a
ambiguous _ = Refl
HS

cat >RankDomainClient.hs <<'HS'
{-# LANGUAGE BangPatterns, DataKinds, GADTs, NoDeepSubsumption #-}
{-# LANGUAGE NoExperimentalDependentTypes, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions, TypeApplications, TypeOperators #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:)(Refl))
import RankDomainProvider (AmbiguousReflect, Reflect, ambiguous, bottomReflect, reflect)

class Show a => Display a
instance Display Bool
instance Display Char

run :: (forall a. Display a => Reflect a) -> Maybe Bool -> Maybe Char -> (Bool, Bool)
run worker b c = (case worker @Bool b of Refl -> True,
                  case worker @Char c of Refl -> True)

ignore :: (forall a. Display a => Reflect a) -> Bool
ignore _ = True

demand :: (forall a. Display a => Reflect a) -> ()
demand worker = case worker @Bool of !_ -> ()

runOpaque :: (forall a. AmbiguousReflect a) -> Bool
runOpaque _ = True

main :: IO ()
main = do
    b <- newIORef (error "Bool index forced") >>= readIORef
    c <- newIORef (error "Char index forced") >>= readIORef
    print (run reflect b c, run (\ @a -> reflect @a) b c, ignore bottomReflect, runOpaque (\ @a -> ambiguous @a))
    forced <- try (evaluate (demand bottomReflect))
    case forced of
      Left e | "callback bottom" `isInfixOf` displayException (e :: ErrorCall) ->
          putStrLn "callback bottom: forced"
      _ -> fail "callback WHNF was not forced"
HS

cat >RankDomainReject.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, NoDeepSubsumption, NoExperimentalDependentTypes #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module RankDomainReject where

import Data.Proxy (Proxy)
import RankDomainProvider

class Show a => Display a

run :: (forall a. Display a => Reflect a) -> Bool
run _ = True

#if FORM == 1
bad :: Bool
bad = run fixedDomain
#elif FORM == 2
bad :: Bool
bad = run wrongResult
#elif FORM == 3
bad :: forall outer. Show outer => Proxy outer -> Bool
bad _ = run (reflect @outer)
#elif FORM == 4
runOpaque :: (forall a. AmbiguousReflect a) -> Bool
runOpaque _ = True
bad :: Bool
bad = runOpaque ambiguous
#elif FORM == 5
class Unrelated a
runUnrelated :: (forall a. Unrelated a => Reflect a) -> Bool
runUnrelated _ = True
bad :: Bool
bad = runUnrelated reflect
#elif FORM == 6
bad = Just reflect
#elif FORM == 7 || FORM == 8
type Callback = forall a. Reflect a
bad :: Maybe Callback
#if FORM == 7
bad = Just @Callback reflect
#else
bad = Just reflect
#endif
#elif FORM == 9
consume _ _ = True
bad = consume (Just reflect) Nothing
#elif FORM == 10
consume :: forall a. Maybe a -> Maybe Bool -> Bool
consume _ _ = True
bad = consume (Just reflect) Nothing
#elif FORM == 11
type Callback = forall a. Reflect a
consume :: Maybe Callback -> Maybe Bool -> Bool
consume _ _ = True
bad = consume (Just reflect) Nothing
#endif
HS

cat >RankDomainScope.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs #-}
{-# LANGUAGE PolyKinds, RankNTypes, TypeApplications, TypeOperators #-}
module RankDomainScope where
import Data.Type.Equality ((:~:)(Refl))
import RankDomainProvider (Reflect, reflect)

run :: (forall a. Show a => Reflect a) ->
       foreach (index :: Maybe Bool) -> index :~: index
run worker index = worker @Bool index

data CaptureValue (tag :: Bool) (index :: Maybe a) = CaptureValue Int
type Captured tag = forall a. Show a => foreach (index :: Maybe a) -> CaptureValue tag index

capture :: foreach (tag :: Bool) -> Maybe (Captured tag)
capture tag = Just worker
  where
    worker :: Captured tag
    worker _ = CaptureValue 42

#ifdef ESCAPE
escape = \index -> run reflect index
#else
within :: Maybe Bool -> Bool
within index = case run reflect index of Refl -> True
#endif
HS

cat >StoredCallback.hs <<'HS'
{-# LANGUAGE CPP, ImpredicativeTypes, NoDeepSubsumption, NoExperimentalDependentTypes #-}
{-# LANGUAGE GADTs, RankNTypes, TypeApplications #-}
module StoredCallback (Callback, boxed, empty, bottomBox, consume, ignoreField) where
import Data.Type.Equality ((:~:)(Refl))
import RankDomainProvider (Reflect, bottomReflect, reflect)

type Callback = forall a. Show a => Reflect a

boxed, empty, bottomBox :: Maybe Callback
#ifdef EXPLICIT
boxed = Just @Callback reflect
empty = Nothing @Callback
bottomBox = Just @Callback bottomReflect
#else
boxed = Just reflect
empty = Nothing
bottomBox = Just bottomReflect
#endif

consume :: Maybe Callback -> Maybe Bool -> Maybe Char -> (Bool, Bool)
consume Nothing _ _ = (False, False)
consume (Just f) b c = (case f @Bool b of Refl -> True,
                       case f @Char c of Refl -> True)

ignoreField :: Maybe Callback -> Bool
ignoreField Nothing = False
ignoreField (Just _) = True
HS

cat >StoredReader.hs <<'HS'
{-# LANGUAGE BangPatterns, CPP, GADTs, NoExperimentalDependentTypes #-}
{-# LANGUAGE NoImpredicativeTypes, RankNTypes, TypeApplications #-}
module Main where

#ifdef ESCAPE
import RankDomainScope (capture)

escape = \tag -> capture tag
main :: IO ()
main = pure ()
#else
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:)(Refl))
import StoredCallback (bottomBox, boxed, empty)

main :: IO ()
main = do
    b <- newIORef (error "Bool index forced") >>= readIORef
    c <- newIORef (error "Char index forced") >>= readIORef
    let withBoth = case boxed of
          Nothing -> (False, False)
          Just worker -> (case worker @Bool b of Refl -> True,
                          case worker @Char c of Refl -> True)
        isEmpty = case empty of Nothing -> True; Just _ -> False
        isLazy = case bottomBox of Just _ -> True; Nothing -> False
        demand = case bottomBox of
          Nothing -> ()
          Just worker -> case worker @Bool of !_ -> ()
    print (withBoth, isEmpty, isLazy)
    forced <- try (evaluate demand)
    case forced of
      Left e | "callback bottom" `isInfixOf` displayException (e :: ErrorCall) ->
          putStrLn "stored callback bottom: forced"
      _ -> fail "unexpected callback demand"
#endif
HS

cat >ContextualClient.hs <<'HS'
{-# LANGUAGE ImpredicativeTypes, NoDeepSubsumption #-}
{-# LANGUAGE NoExperimentalDependentTypes, RankNTypes #-}
module Main where

import Data.IORef (newIORef, readIORef)
import RankDomainProvider (bottomReflect, reflect)
import StoredCallback (Callback, consume, ignoreField)

partial = consume (Just reflect) Nothing

throughRanked :: Callback -> Maybe Bool -> Maybe Char -> (Bool, Bool)
throughRanked f b c = consume (Just f) b c

main :: IO ()
main = do
    b <- newIORef (error "Bool index forced") >>= readIORef
    c <- newIORef (error "Char index forced") >>= readIORef
    print (partial c)
    putStrLn (show (consume (Just reflect) b c))
    putStrLn (show (throughRanked reflect b c))
    print (ignoreField (Just bottomReflect))
HS

cat >OrdinaryImpred.hs <<'HS'
module Main where
ids :: [forall a. a -> a]
ids = [id]

observe :: [forall a. a -> a] -> (Char, Bool)
observe [] = ('?', False)
observe (f : _) = (f 'x', f True)

main :: IO ()
main = print (observe (id : ids))
HS

reject_rank_domain() {
    local diagnostic=$1
    shift
    if "${ghc[@]}" "$@" >reject.log 2>&1 || ! grep -Fq "$diagnostic" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

# Exercise shallow subsumption against an interface in an ordinary client.
for opt in O0 O2; do
    mkdir -p "$out/domain-$opt"/{provider,client}
    cp RankDomainProvider.hs "$out/domain-$opt/provider/"
    cp {RankDomainClient,RankDomainReject,RankDomainScope,OrdinaryImpred}.hs "$out/domain-$opt/client/"
    cp StoredCallback.hs "$out/domain-$opt/provider/"
    mkdir -p "$out/domain-$opt/reader"
    cp {StoredReader,ContextualClient}.hs "$out/domain-$opt/reader/"
    (
        cd "$out/domain-$opt/provider"
        flags=("-$opt")
        if [[ $opt == O0 ]]; then
            flags+=(-fomit-interface-pragmas -fno-write-if-simplified-core)
        fi
        "${ghc[@]}" "${flags[@]}" -c RankDomainProvider.hs
        cp RankDomainProvider.hi ../client/
        cd ../client
        for mode in NoImpredicativeTypes ImpredicativeTypes; do
            "${ghc[@]}" "-$opt" "-X$mode" -fforce-recomp -c RankDomainClient.hs
            "${ghc[@]}" "-$opt" RankDomainClient.o ../provider/RankDomainProvider.o -o run
            ./run >runtime
            printf '%s\n' '((True,True),(True,True),True,True)' 'callback bottom: forced' | cmp - runtime

            form=0
            for diagnostic in 'with ‘Bool’' 'Proxy x' 'with ‘outer’' 'non-injective type family' 'Could not deduce ‘Show a’'; do
                form=$((form + 1))
                reject_rank_domain "$diagnostic" "-$opt" "-X$mode" "-DFORM=$form" -c RankDomainReject.hs
            done
        done
        for form in 6 9 10; do
            reject_rank_domain 'Quick Look inference does not support this retained type yet:' \
                "-$opt" -XImpredicativeTypes "-DFORM=$form" -c RankDomainReject.hs
        done
        for form in 7 8 11; do
            reject_rank_domain 'No instance for ‘Show a’' \
                "-$opt" -XImpredicativeTypes "-DFORM=$form" -c RankDomainReject.hs
        done
        "${ghc[@]}" "-$opt" -XImpredicativeTypes -c RankDomainScope.hs
        reject_rank_domain 'This inferred retained type requires an explicit signature:' \
            "-$opt" -XImpredicativeTypes -DESCAPE -c RankDomainScope.hs
        "${ghc[@]}" "-$opt" -XImpredicativeTypes OrdinaryImpred.hs -o ordinary
        printf '%s\n' "('x',True)" | cmp - <(./ordinary)

        cp RankDomainScope.hi RankDomainProvider.hi ../reader/
        cd ../reader
        reject_rank_domain 'is out of scope here' "-$opt" -DESCAPE -c StoredReader.hs
        for mode in EXPLICIT INFERRED; do
            (
                cd ../provider
                "${ghc[@]}" "${flags[@]}" "-D$mode" -fforce-recomp -c StoredCallback.hs
                cp StoredCallback.hi ../reader/
            )
            "${ghc[@]}" "-$opt" -fforce-recomp -c StoredReader.hs
            "${ghc[@]}" "-$opt" StoredReader.o ../provider/{StoredCallback,RankDomainProvider}.o -o stored
            printf '%s\n' '((True,True),True,True)' 'stored callback bottom: forced' | cmp - <(./stored)
        done
        "${ghc[@]}" "-$opt" -c ContextualClient.hs
        "${ghc[@]}" "-$opt" ContextualClient.o ../provider/{StoredCallback,RankDomainProvider}.o -o contextual
        printf '%s\n' '(True,True)' '(True,True)' '(True,True)' True | cmp - <(./contextual)
    )
done
