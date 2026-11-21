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

#ifdef BAD
bad :: foreach (tag :: Bool) -> forall a -> foreach (m :: Maybe a) -> Reading tag m %M -> Reading tag m
bad _ (type a) _ x = x
#endif
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
        bad = case Retained.bad tag (type Bool) index (Retained.Reading 41) of Retained.Reading n -> n
    partialForced <- threw (evaluate (partial `seq` ()))
    fullForced <- threw (evaluate (partial 61))
    ordinaryDeferred <- deferred (evaluate (Ordinary.bad 41))
    retainedDeferred <- deferred (evaluate bad)
    print (Ordinary.forward @U 41, result, partialForced, fullForced, ordinaryDeferred, retainedDeferred)
HS

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
        printf '%s\n' '(41,41,False,True,True,True)' | cmp - actual
    )
done
