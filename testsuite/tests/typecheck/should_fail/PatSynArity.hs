{-# language PatternSynonyms #-}

module PatSynArity where

pattern P :: Int -> (Int, Int)
pattern P a b = (a, b)
