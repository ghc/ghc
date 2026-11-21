{-# LANGUAGE Haskell2010 #-}
module OrdinaryScopedComparison (readComparison, observations) where

import ScopedComparison (CompareZero(..), Compared(..), compareTagged)

-- | Read an imported comparison under the caller's dictionary.
readComparison :: CompareZero a => a -> Ordering
readComparison x = case compareTagged x of
    Compared order -> order

instance CompareZero Int where
    compareZero x = compare x 0

data AlwaysBelow = AlwaysBelow

instance CompareZero AlwaysBelow where
    compareZero _ = LT

-- | Compare an integer and check that the second instance ignores its input.
observations :: Int -> (Ordering, Ordering)
observations x =
    (readComparison x, readComparison (error "unused argument" :: AlwaysBelow))
