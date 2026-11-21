#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fforce-recomp)
cd "$out"

cat >Ordinary.hs <<'HS'
{-# LANGUAGE CPP, GADTs, Modifiers, NoExperimentalDependentTypes, ScopedTypeVariables #-}
module Ordinary where
import GHC.Exts (M, Matchability, U)

forward :: forall (ma :: Matchability). ma ~ U => Int %ma -> Int
forward x = x

#ifdef BAD
bad :: Int %M -> Int
bad x = x
#endif
HS

cat >Retained.hs <<'HS'
{-# LANGUAGE BangPatterns, CPP, DataKinds, ExperimentalDependentTypes, ExplicitNamespaces #-}
{-# LANGUAGE GADTs, Modifiers, PolyKinds, RequiredTypeArguments, ScopedTypeVariables #-}
module Retained where
import GHC.Exts (M, Matchability, U)

data Reading (tag :: Bool) (m :: Maybe a) = Reading Int

forward :: forall (ma :: Matchability). ma ~ U =>
           foreach (tag :: Bool) -> forall a -> foreach (m :: Maybe a) -> Reading tag m %ma -> Reading tag m
forward _ (type a) _ x = x

{-# NOINLINE strictPrefix #-}
strictPrefix :: forall (ma :: Matchability). ma ~ U =>
                foreach (tag :: Bool) -> forall a -> foreach (m :: Maybe a) -> Reading tag m -> Int -> Int %ma -> Int
strictPrefix _ (type a) _ _ !_ x = x

{-# NOINLINE constructor #-}
constructor :: forall (ma :: Matchability). ma ~ U =>
               foreach (tag :: Bool) -> Eq Int => foreach (m :: Maybe Bool) -> Reading tag m -> Int %ma -> Int
constructor False _ _ x = x
constructor True _ _ x = x

#ifdef BAD
bad :: foreach (tag :: Bool) -> forall a -> foreach (m :: Maybe a) -> Reading tag m %M -> Reading tag m
bad _ (type a) _ x = x

badConstructor :: foreach (tag :: Bool) -> Reading tag ('Nothing :: Maybe Bool) -> Int %M -> Int
badConstructor False _ x = x
badConstructor True _ x = x
#endif
HS

cat >TooLateGiven.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers, ScopedTypeVariables #-}
module TooLateGiven where
import GHC.Exts (Matchability, U)

data Witness (ma :: Matchability) where
    UnmatchableWitness :: Witness U

data Reading (tag :: Bool) = Reading Int

ordinary :: forall (ma :: Matchability). Witness ma %ma -> Int
ordinary UnmatchableWitness = 7

retained :: forall (ma :: Matchability). foreach (tag :: Bool) -> Witness ma %ma -> Reading tag
retained _ UnmatchableWitness = Reading 7

constructor :: forall (ma :: Matchability). foreach (tag :: Bool) -> Witness ma %ma -> Reading tag
constructor False UnmatchableWitness = Reading 7
constructor True UnmatchableWitness = Reading 7
HS

cat >Main.hs <<'HS'
{-# LANGUAGE ExplicitNamespaces, NoExperimentalDependentTypes, RequiredTypeArguments, TypeApplications #-}
module Main where
import Control.Exception (SomeException, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import GHC.Exts (U)
import qualified Ordinary
import qualified Retained

threw :: IO a -> IO Bool
threw action = either (const True) (const False) <$> try @SomeException action

deferred :: IO a -> IO Bool
deferred action = either (isInfixOf "deferred type error" . displayException) (const False) <$> try @SomeException action

main :: IO ()
main = do
    tag <- newIORef (error "tag") >>= readIORef
    index <- newIORef (error "index" :: Maybe Bool) >>= readIORef
    prefix <- newIORef (error "strict prefix") >>= readIORef
    let result = case Retained.forward @U tag (type Bool) index (Retained.Reading 41) of Retained.Reading n -> n
        partial = Retained.strictPrefix @U tag (type Bool) index (Retained.Reading 0) prefix
        constructorPartial = Retained.constructor @U tag index (Retained.Reading 0)
        bad = case Retained.bad tag (type Bool) index (Retained.Reading 41) of Retained.Reading n -> n
    partialForced <- threw (evaluate (partial `seq` ()))
    fullForced <- threw (evaluate (partial 61))
    constructorPartialForced <- threw (evaluate (constructorPartial `seq` ()))
    constructorFullForced <- threw (evaluate (constructorPartial 61))
    ordinaryDeferred <- deferred (evaluate (Ordinary.bad 41))
    retainedDeferred <- deferred (evaluate bad)
    constructorDeferred <- deferred (evaluate (Retained.badConstructor True (Retained.Reading 0) 61))
    print (Ordinary.forward @U 41, result, Retained.constructor @U True index (Retained.Reading 0) 61)
    print (partialForced, fullForced, constructorPartialForced, constructorFullForced)
    print (ordinaryDeferred, retainedDeferred, constructorDeferred)
HS

if "${ghc[@]}" -fno-code TooLateGiven.hs >reject.log 2>&1 ||
   [[ $(grep -Fc 'error: [GHC-25897]' reject.log) != 3 ]]; then
    cat reject.log >&2
    exit 1
fi

for opt in 0 2; do
    mkdir -p "$out/$opt"/{provider,client}
    cp Ordinary.hs Retained.hs "$out/$opt/provider/"
    cp Main.hs "$out/$opt/client/"
    (
        cd "$out/$opt/provider"
        for module in Ordinary Retained; do
            "${ghc[@]}" "-O$opt" -c "$module.hs"
            if "${ghc[@]}" "-O$opt" -DBAD -c "$module.hs" >reject.log 2>&1 ||
               ! grep -Fq 'error: [GHC-83865]' reject.log || ! grep -Fq 'Expected: M' reject.log; then
                cat reject.log >&2
                exit 1
            fi
            "${ghc[@]}" "-O$opt" -DBAD -fdefer-type-errors -c "$module.hs" >defer.log 2>&1
            grep -Fq 'warning: [GHC-83865]' defer.log || { cat defer.log >&2; exit 1; }
            cp "$module.hi" ../client/
        done
        cd ../client
        "${ghc[@]}" "-O$opt" -c Main.hs
        "${ghc[@]}" Main.o ../provider/Ordinary.o ../provider/Retained.o -o run
        ./run >actual
        printf '%s\n' '(41,41,61)' '(False,True,False,True)' '(True,True,True)' | cmp - actual
    )
done
