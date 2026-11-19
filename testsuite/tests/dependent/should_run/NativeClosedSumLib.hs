{-# LANGUAGE AllowAmbiguousTypes, NoExperimentalDependentTypes, UndecidableInstances #-}
module NativeClosedSumLib
    ( weighted, containsAny, identity
    , unavailable, Local (..), local, ambiguous, hiddenRequirement
    , unavailablePair, ambiguousPair, countOptional
    , keepNatural, chooseLeft, chooseRight, ignoreNatural, ignoreSame, bottomSigned, ignoreBool
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

-- | Fix the argument type of a staged conversion.
keepNatural :: Natural -> Natural
keepNatural = id

-- | Return the optional value or the supplied default.
chooseLeft :: Maybe a -> a -> a
chooseLeft = maybe id const

-- | Put the optional argument after its default.
chooseRight :: a -> Maybe a -> a
chooseRight = flip chooseLeft

-- | Ignore a natural-number computation.
ignoreNatural :: Natural -> Bool
ignoreNatural _ = True

-- | Raise an exception if the staged operand is demanded.
bottomSigned :: Natural -> Int
bottomSigned _ = errorWithoutStackTrace "ran a closed operand ignored by its consumer"

-- | Fix an argument to Bool without forcing its value.
ignoreBool :: Bool -> Natural
ignoreBool _ = 0


-- | Ignore two computations with the same inferred type.
ignoreSame :: a -> a -> Bool
ignoreSame _ _ = True
