#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd "$work"

cat >Domain.hs <<'HS'
module Domain (Pair(..)) where
data Pair = Pair Bool Bool
HS
cat >Provide.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Provide (pick, pickPair, pickEager, pickPartial, Eager, First) where

import Domain (Pair(..))

type Eager (m :: Maybe Bool) = case m of { Nothing -> Char; Just True -> Int; Just _ -> Int }
type First (p :: Pair) = case p of
    Pair False True -> Char
    Pair True True -> Bool
    Pair _ False -> Int

pick :: foreach (m :: Maybe Bool) ->
    Maybe (case m of { Nothing -> Char; Just True -> Int; Just False -> Bool })
pick _ = Nothing

pickPair :: foreach (p :: Pair) ->
    Maybe (case p of { Pair False True -> Char; Pair True True -> Bool; Pair _ False -> Int })
pickPair _ = Nothing

pickEager :: foreach (m :: Maybe Bool) ->
    Maybe (case m of { Nothing -> Char; Just True -> Int; Just _ -> Int })
pickEager _ = Nothing

pickPartial :: foreach (m :: Maybe Bool) ->
    Maybe (case m of { Nothing -> Char; Just True -> Int })
pickPartial _ = Nothing
HS
cat >Consume.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Consume (render, different, renderPair, renderLazy, renderPartial, Lazy, Second) where

import Domain (Pair(..))

type Lazy (m :: Maybe Bool) = case m of { Nothing -> Char; Just _ -> Int }
type Second (p :: Pair) = case p of
    Pair _ False -> Int
    Pair False True -> Char
    Pair True True -> Bool

render :: foreach (m :: Maybe Bool) ->
    Maybe (case m of { Just False -> Bool; Nothing -> Char; Just True -> Int }) -> String
render _ v = case v of { Nothing -> "nothing"; Just _ -> "just" }

different :: foreach (m :: Maybe Bool) ->
    Maybe (case m of { Just False -> Bool; Nothing -> Char; Just True -> Ordering }) -> String
different _ _ = "different"

renderPair :: foreach (p :: Pair) ->
    Maybe (case p of { Pair _ False -> Int; Pair False True -> Char; Pair True True -> Bool }) -> String
renderPair _ _ = "pair"

renderLazy :: foreach (m :: Maybe Bool) ->
    Maybe (case m of { Nothing -> Char; Just _ -> Int }) -> String
renderLazy _ _ = "lazy"

renderPartial :: foreach (m :: Maybe Bool) ->
    Maybe (case m of { Nothing -> Char; Just True -> Int }) -> String
renderPartial _ _ = "partial"
HS
cat >Main.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import Data.IORef (newIORef, readIORef)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import Domain (Pair(..))
import qualified Consume as C
import qualified Provide as P

atUnknown :: Maybe Bool -> String
atUnknown m = C.render m (P.pick m)

atJust :: Bool -> String
atJust b = C.render (Just b) (P.pick (Just b))

unforcedPairField :: forall (b :: Bool). Proxy b -> C.Second ('Pair b 'False) :~: Int
unforcedPairField _ = Refl

unforcedJustField :: forall (b :: Bool). Proxy b -> C.Lazy ('Just b) :~: Int
unforcedJustField _ = Refl

main :: IO ()
main = do
    m <- newIORef (Just (error "forced inner field")) >>= readIORef
    b <- newIORef (error "forced inner field") >>= readIORef
    print (atUnknown m, atJust b)
HS
cat >Reject.hs <<'HS'
{-# LANGUAGE CPP, NoExperimentalDependentTypes #-}
module Reject where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import Domain (Pair(..))
import qualified Consume as C
import qualified Provide as P

#if defined(CHANGED)
bad :: Bool -> String
bad b = C.different (Just b) (P.pick (Just b))
#elif defined(PAIR)
bad :: Pair -> String
bad p = C.renderPair p (P.pickPair p)
#elif defined(JUST)
bad :: Bool -> String
bad b = C.renderLazy (Just b) (P.pickEager (Just b))
#elif defined(PARTIAL)
bad :: Maybe Bool -> String
bad m = C.renderPartial m (P.pickPartial m)
#elif defined(PAIR_RIGID)
bad :: forall (b :: Bool). Proxy b -> P.First ('Pair b 'False) :~: Int
bad _ = Refl
#else
bad :: forall (b :: Bool). Proxy b -> P.Eager ('Just b) :~: Int
bad _ = Refl
#endif
HS

for opt in -O0 -O2; do
    mkdir "${opt#-}"
    cd "${opt#-}"
    cp ../{Domain,Provide,Consume,Main,Reject}.hs .
    for module in Domain Provide Consume; do
        "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c "$module.hs"
        mv "$module.hs" "$module.source"
    done
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" "$opt" Main.o Domain.o Provide.o Consume.o -o run
    ./run > actual
    printf '("nothing","nothing")\n' | diff -u - actual

    for control in CHANGED PAIR JUST PARTIAL PAIR_RIGID JUST_RIGID; do
        if "${ghc[@]}" "$opt" "-D$control" -c Reject.hs > "$control.log" 2>&1 ||
           ! grep -Fq 'error: [GHC-83865]' "$control.log"; then
            cat "$control.log" >&2
            exit 1
        fi
    done
    cd ..
done
