{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}

module PMC008 where

-- complete match, but because of the guard, the information that `x` is not
-- `Just` has to flow through the term oracle.
foo :: Maybe Int -> Int
foo x | Just y <- x = y
foo Nothing = 43
