{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Refine a runtime call without assuming that the function is injective.
module OpaqueCallClient where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM_, unless)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import PackedVecLibrary

-- | Construct a vector at the computed index and observe its length.
probeCall :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
probeCall f n original =
    case f n of
        Z ->
            let ys :: Vec Int (f n)
                ys = VNil
             in length (toList ys)
        S k ->
            let ys :: Vec Int (f n)
                ys = VCons 42 (replicateNative @Int k 42)
             in length (toList ys)

-- | Transport a captured vector and a local function through the call proof.
probeCapturedCall :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
probeCapturedCall f n original =
    let consume :: Vec Int (f n) -> Int
        consume cached =
            case f n of
                Z -> length (toList cached)
                S k -> length (toList cached)
     in case f n of
            Z ->
                let ys :: Vec Int (f n)
                    ys = VNil
                 in consume ys
            S k ->
                let ys :: Vec Int (f n)
                    ys = VCons 42 (replicateNative @Int k 42)
                 in consume ys

-- | Observe one element without forcing a successor's predecessor.
probeCallHead :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
probeCallHead f n original =
    case f n of
        Z ->
            let ys :: Vec Int (f n)
                ys = VNil
             in length (toList ys)
        S k ->
            let ys :: Vec Int (f n)
                ys = VCons 42 (replicateNative @Int k 42)
             in head (toList ys)

-- | Keep the source case strict before returning a function.
probeCallFunction :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int -> Int
probeCallFunction f n original =
    case f n of
        Z ->
            let ys :: Vec Int (f n)
                ys = VNil
             in \x -> length (toList ys) + x
        S k ->
            let ys :: Vec Int (f n)
                ys = VCons 42 (replicateNative @Int k 42)
             in \x -> length (toList ys) + x

-- | Preserve the ordinary laziness of a wildcard-only case.
wildcardCall :: (Nat -> Nat) -> Nat -> Int
wildcardCall f n = case f n of _ -> 42

-- | Preserve the ordinary laziness of a variable-only case.
variableCall :: (Nat -> Nat) -> Nat -> Int
variableCall f n = case f n of result -> 42

-- | Keep the runtime function opaque at its call sites.
{-# NOINLINE identityNat #-}
identityNat :: Nat -> Nat
identityNat n = n

-- | Return zero without forcing the input.
{-# NOINLINE constantZero #-}
constantZero :: Nat -> Nat
constantZero n = Z

-- | Require a particular runtime exception.
expectError :: String -> IO a -> IO ()
expectError fragment action = do
    outcome <- try (action >> pure ())
    case outcome of
        Left (err :: SomeException) ->
            unless (fragment `isInfixOf` displayException err) $
                fail (displayException err)
        Right () -> fail ("Expected runtime error: " ++ fragment)

-- | Read ordinary runtime indices and check refinement and demand.
main :: IO ()
main = do
    forM_ [Z, S (S Z), S (S (S Z))] $ \seed -> do
        ref <- newIORef seed
        n <- readIORef ref
        let xs :: Vec Int n
            xs = replicateNative @Int n 7
        print (probeCall identityNat n xs, probeCall constantZero n xs, probeCapturedCall identityNat n xs)
    let n :: Nat
        n = error "argument-second"
        f :: Nat -> Nat
        f = error "head-first"
        partial :: Nat
        partial = S (error "predecessor")
        cyclic :: Nat
        cyclic = S cyclic
    print (probeCall constantZero n (error "unused-vector"))
    print (probeCallHead identityNat partial (error "unused-vector"))
    print (probeCallHead identityNat cyclic (error "unused-vector"))
    print (wildcardCall f n, variableCall f n)
    expectError "head-first" (evaluate (probeCall f n (error "unused-vector")))
    expectError "argument-second" (evaluate (probeCall identityNat n (error "unused-vector")))
    expectError "head-first" (evaluate (probeCallFunction f n (error "unused-vector")))
