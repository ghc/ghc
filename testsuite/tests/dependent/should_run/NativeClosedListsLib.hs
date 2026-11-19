{-# LANGUAGE FlexibleInstances, NoExperimentalDependentTypes #-}
module NativeClosedListsLib (characters, flags, empty, badElement, reverseNaturals, first, sameList) where

import Data.Foldable (toList)
import Numeric.Natural (Natural)

-- Ordinary Show may choose this instance. The type evaluator must not.
instance {-# OVERLAPPING #-} Show [Natural] where
    show _ = "[2,3,5]"

characters :: Char -> String
characters c = ['"', '\\', c, '\SO', 'H', 'λ']

flags :: [Natural] -> [Bool]
flags = map odd

empty :: [Natural] -> [Natural]
empty _ = []

badElement :: [Natural] -> [Natural]
badElement _ = [1, errorWithoutStackTrace "list codec forced the second element"]

reverseNaturals :: [Natural] -> [Natural]
reverseNaturals = reverse

first :: [Natural] -> Natural
first [] = 0
first (n:_) = n

sameList :: (Foldable f, Eq a) => f a -> f a -> Bool
sameList lhs rhs = toList lhs == toList rhs
