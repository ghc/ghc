{-# LANGUAGE AllowAmbiguousTypes, NoExperimentalDependentTypes, UndecidableInstances #-}
module NativeClosedSumLib
    ( weighted, containsAny, identity
    , unavailable, Local (..), local, ambiguous, hiddenRequirement
    , unavailablePair, ambiguousPair, countOptional
    ) where

import Numeric.Natural (Natural)
class Missing a

class Local a where
    localWitness :: a -> a

weighted :: Num a => [a] -> a
weighted = foldr (\n acc -> 2 * n + acc) 0

-- The missing dictionary is deliberately unused.
unavailable :: (Num a, Missing a) => [a] -> a
unavailable _ = 0

local :: (Num a, Local a) => [a] -> a
local _ = localWitness 0

containsAny :: Foldable f => f a -> Bool
containsAny = not . null

ambiguous :: (Num a, Num unused) => [a] -> a
ambiguous _ = 0

identity :: a -> a
identity = id

class Hidden a where
    hidden :: a -> a

instance Num b => Hidden Natural where
    hidden = id

hiddenRequirement :: (Num a, Hidden a) => [a] -> a
hiddenRequirement _ = hidden 0

unavailablePair :: Missing a => a -> [Natural] -> Natural
unavailablePair _ _ = 0

ambiguousPair :: (Num a, Num b) => a -> a -> b
ambiguousPair _ _ = 0

-- | Count a present value when the caller selects it.
countOptional :: Num n => Maybe a -> Bool -> n
countOptional (Just _) True = 1
countOptional _ _ = 0
