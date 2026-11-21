{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeTakeWhile
    ( Prefix, Enabled, PrefixStep(..), RuntimePrefix(..), observePrefix, prefixObservation ) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import PreludeFilterRuntime (BooleanResult(..), booleanResult)

-- | Use the installed prefix function in a type.
type Prefix :: (a %U -> Bool) %U -> [a] %U -> [a]
type Prefix p xs = takeWhile p xs

-- | Use the installed conjunction with one retained input.
type Enabled :: Bool %U -> Bool %U -> Bool
type Enabled gate = (&&) gate

-- | Record the first result when the predicate tests this head.
data PrefixStep (p :: Bool %U -> Bool) (x :: Bool) (xs :: [Bool]) where
    Stops :: Prefix p (x ': xs) :~: '[] -> PrefixStep p x xs
    Keeps :: Prefix p (x ': xs) :~: (x ': Prefix p xs) -> PrefixStep p x xs

-- | Store the inputs and the first step of the prefix function.
data RuntimePrefix where
    RuntimePrefix :: foreach (gate :: Bool) -> foreach (x :: Bool) -> foreach (xs :: [Bool])
                  -> PrefixStep (Enabled gate) x xs -> RuntimePrefix

-- | Check the first step for values supplied by an ordinary caller.
observePrefix :: Bool -> Bool -> [Bool] -> RuntimePrefix
observePrefix gate x xs = RuntimePrefix gate x xs (step gate x xs)

step :: foreach (gate :: Bool) -> foreach (x :: Bool) -> foreach (xs :: [Bool])
     -> PrefixStep (Enabled gate) x xs
step gate x _ = case booleanResult (gate && x) of
    IsFalse -> Stops Refl
    IsTrue -> Keeps Refl

-- | Match the equality and observe up to one element of the actual prefix.
prefixObservation :: RuntimePrefix -> (Bool, [Bool])
prefixObservation (RuntimePrefix gate x xs result) =
    (kept, take 1 (takeWhile (gate &&) (x : xs)))
  where
    kept = case result of
        Stops Refl -> False
        Keeps Refl -> True
