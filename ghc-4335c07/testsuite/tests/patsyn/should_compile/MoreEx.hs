{-# LANGUAGE GADTs, PatternSynonyms #-}

-- Tests that for unidirectional pattern synonyms
-- the pattern synonym can be more existential
-- (i.e. lose information) wrt the original

module MoreEx where

pattern ExCons :: a -> [a] -> [b]
pattern ExCons x xs <- x : xs

data T where
  MkT1 :: ([a] -> Int) -> [a] -> T
  MkT2 :: (a -> Int) -> a -> T

pattern ExT1 :: b -> (b -> Int) -> T
pattern ExT1 x f <- MkT1 f x

pattern ExT2 :: b -> (c -> Int) -> T
pattern ExT2 x f <- MkT2 f x
