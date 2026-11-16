{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Source package with an actual runtime function field.
module FunctionPackageLibrary (
    FunctionPack (..),
    buildPack,
    lazyPack,
    repackFunction,
    ignoreFunction,
    inputLength,
    inspectFunction,
    payloadOnly,
    headOnly,
    pairPayloads,
    guardedIgnore,
) where

import PackedVecLibrary

-- | Bind the function and input used by the following vector field.
data FunctionPack a where
    FunctionPack :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec a (f n) -> FunctionPack a

-- | Give a lifted error an explicit native result type.
raisePayload :: forall a. String -> a
raisePayload = error

-- | Build the vector under the checked equality for the exact whole call.
buildPack :: forall a. (Nat -> Nat) -> Nat -> a -> FunctionPack a
buildPack f n value = case f n of
    Z -> let ys :: Vec a (f n); ys = VNil in FunctionPack f n ys
    S k -> let ys :: Vec a (f n); ys = VCons value (replicateNative @a k value) in FunctionPack f n ys

-- | Store all fields without executing the function or inspecting the input.
lazyPack :: forall a. (Nat -> Nat) -> Nat -> FunctionPack a
lazyPack f n =
    let ys :: Vec a (f n)
        ys = raisePayload @(Vec a (f n)) "function-pack-payload"
     in FunctionPack f n ys

-- | Reconstruct the package with the actual selected field identities.
repackFunction :: FunctionPack a -> FunctionPack a
repackFunction (FunctionPack f n xs) = FunctionPack f n xs

-- | Inspect only the outer package constructor.
ignoreFunction :: FunctionPack a -> Int
ignoreFunction (FunctionPack f n xs) = 42

-- | Demand the original input independently from the function and payload.
inputLength :: FunctionPack a -> Int
inputLength (FunctionPack f n xs) = natToInt n

-- | Demand the computed result and payload through the selected function Id.
inspectFunction :: FunctionPack a -> (Int, [a])
inspectFunction (FunctionPack f n xs) = case f n of
    Z -> (0, toList xs)
    S k -> (natToInt (S k), toList xs)

-- | Observe the payload without executing the stored function.
payloadOnly :: FunctionPack a -> [a]
payloadOnly (FunctionPack f n xs) = toList xs

-- | Demand only one payload constructor.
headOnly :: FunctionPack Int -> Int
headOnly (FunctionPack f n VNil) = 0
headOnly (FunctionPack f n (VCons x rest)) = x

-- | Keep both selected function and index identities in separate scopes.
pairPayloads :: FunctionPack a -> FunctionPack b -> ([a], [b])
pairPayloads (FunctionPack f n xs) (FunctionPack g m ys) = (toList xs, toList ys)

-- | Resume the second row outside the first row's field scope.
guardedIgnore :: Bool -> FunctionPack a -> Int
guardedIgnore flag (FunctionPack f n xs) | flag = ignoreFunction (FunctionPack f n xs)
guardedIgnore _ (FunctionPack f n xs) = ignoreFunction (FunctionPack f n xs)
