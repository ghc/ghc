{-# LANGUAGE BangPatterns, UnboxedTuples #-}
module EnforceEpt004 where

x :: Int
x = x

f :: a -> (# Int, a #)
-- Adapted from a TODO in EnforceEpt.
-- f's tag signature should indicate that the second component
-- of its result is properly tagged: TagTuple[TagDunno,TagProper]
f g = case g of !g' -> (# x, g' #)
