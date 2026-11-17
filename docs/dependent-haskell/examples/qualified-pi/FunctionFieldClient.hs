{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}

-- | Use the producer interface without the producer source.
module FunctionFieldClient where

import FunctionFieldLibrary

-- | Refine the whole container before a strict call refines its proof.
inspect :: Box -> Int
inspect (Box Nothing b proof) = 0
inspect (Box (Just f) b proof) = case f b of
    False -> case proof of IsFalse -> 1
    True -> case proof of IsTrue -> 2

-- | Transport the family-exposed proof to an imported direct-call signature.
inspectViaCall :: Box -> Int
inspectViaCall (Box Nothing b proof) = 0
inspectViaCall (Box (Just f) b proof) = consumeFlag f b proof

-- | Observe only the outer package constructor.
outer :: Box -> Int
outer (Box x b proof) = 42

-- | Observe the container constructor without demanding its function field.
which :: Box -> Int
which (Box Nothing b proof) = 0
which (Box (Just f) b proof) = 1

-- | Observe the proof without calling the stored function in the client.
proofOnly :: Box -> Bool
proofOnly (Box x b proof) = case proof of
    IsFalse -> False
    IsTrue -> True
