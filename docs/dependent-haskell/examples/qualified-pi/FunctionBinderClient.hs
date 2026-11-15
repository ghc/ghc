{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Pass local opaque function heads through retained binders without forcing them.
module FunctionBinderClient (main) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM_, unless)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import PackedVecLibrary

-- | Fix a lifted error result before a visible native type argument.
raiseFunctionError :: forall a. String -> a
raiseFunctionError = error

-- | Preserve an opaque identity function behind a signed local variable.
{-# NOINLINE identityHead #-}
identityHead :: Nat -> Nat
identityHead n = n

-- | Preserve noninjectivity of a constant function.
{-# NOINLINE zeroHead #-}
zeroHead :: Nat -> Nat
zeroHead n = Z

-- | Delay an opaque producer until its function value is demanded.
{-# NOINLINE makeHead #-}
makeHead :: Int -> Nat -> Nat
makeHead token = error "producer-head"

-- | Require the original runtime exception.
expectFunctionError :: String -> IO a -> IO ()
expectFunctionError fragment action = do
    outcome <- try (action >> pure ())
    case outcome of
        Left (err :: SomeException) -> unless (fragment `isInfixOf` displayException err) (fail (displayException err))
        Right () -> fail ("Expected runtime error: " ++ fragment)

-- | Read runtime indices and keep every retained function operand a local variable.
main :: IO ()
main = do
    forM_ [Z, S (S Z), S (S (S Z))] $ \seed -> do
        ref <- newIORef seed
        n <- readIORef ref
        let f :: Nat -> Nat
            f = identityHead
        print (observeFunctionCall f n, ignoreCall f n)
    let bottomN :: Nat
        bottomN = error "function-index"
        bottomF :: Nat -> Nat
        bottomF = error "function-head"
        identity :: Nat -> Nat
        identity = identityHead
        constant :: Nat -> Nat
        constant = zeroHead
        localLambda :: Nat -> Nat
        localLambda = \x -> x
        produced :: Nat -> Nat
        produced = makeHead 7
        cyclic :: Nat
        cyclic = S cyclic
        mixed :: Bool -> Nat
        mixed = \b -> case b of False -> Z; True -> S Z
        bottomB :: Bool
        bottomB = error "function-bool"
    print (ignoreCall bottomF bottomN)
    print (ignoreCall produced bottomN)
    print (observeFunctionCall constant Z, observeFunctionCall constant (S Z))
    print (take 1 (observeFunctionCall identity (S bottomN)))
    print (take 4 (observeFunctionCall identity cyclic))
    print (observeFunctionCall localLambda (S Z))
    print (ignoreMixedCall mixed bottomB)
    expectFunctionError "function-payload" (evaluate (demandCallPayload bottomF bottomN))
    putStrLn "identity-payload: function-payload"
    expectFunctionError "function-head" (evaluate (length (observeFunctionCall bottomF bottomN)))
    putStrLn "call-order: function-head"
    expectFunctionError "function-index" (evaluate (length (observeFunctionCall identity bottomN)))
    putStrLn "call-index: function-index"
    expectFunctionError "producer-head" (evaluate (length (observeFunctionCall produced bottomN)))
    putStrLn "producer-call: producer-head"
  where
    -- \| Ignore all three runtime arguments, including the opaque function head.
    keepCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n) -> Int
    keepCall f n payload = 42

    -- \| Return the original payload at the same actual function and scalar indices.
    idCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n) -> Vec Int (f n)
    idCall f n payload = payload

    -- \| Supply an ignored payload without evaluating its index or head.
    ignoreCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Int
    ignoreCall f n =
        let payload :: Vec Int (f n)
            payload = raiseFunctionError @(Vec Int (f n)) "function-payload"
         in keepCall f n payload

    -- \| Demand the original payload through a fully instantiated identity caller.
    demandCallPayload :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Int
    demandCallPayload f n =
        let caller :: Vec Int (f n) -> Vec Int (f n)
            caller = idCall f n
            payload :: Vec Int (f n)
            payload = raiseFunctionError @(Vec Int (f n)) "function-payload"
         in length (toList (caller payload))

    -- \| Transport an already-instantiated caller through a case on its whole index.
    observeFunctionCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> [Int]
    observeFunctionCall f n =
        let caller :: Vec Int (f n) -> Vec Int (f n)
            caller = idCall f n
         in case f n of
                Z -> let payload :: Vec Int (f n); payload = VNil in toList (caller payload)
                S k -> let payload :: Vec Int (f n); payload = VCons 42 (replicateNative @Int k 42) in toList (caller payload)

    -- \| Keep independently closed input and result domains on a function binder.
    keepMixedCall :: foreach (f :: Bool -> Nat) -> foreach (b :: Bool) -> Vec Int (f b) -> Int
    keepMixedCall f b payload = 42

    -- \| Ignore a mixed-domain payload through the exact actual head.
    ignoreMixedCall :: foreach (f :: Bool -> Nat) -> foreach (b :: Bool) -> Int
    ignoreMixedCall f b =
        let payload :: Vec Int (f b)
            payload = raiseFunctionError @(Vec Int (f b)) "mixed-payload"
         in keepMixedCall f b payload
