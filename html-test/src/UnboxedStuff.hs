{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE UnboxedSums, UnboxedTuples #-}
module UnboxedStuff where

data X
data Y
data Z

-- * Unboxed type constructors

unboxedUnit :: (# #) -> (# #)
unboxedUnit  = undefined

unboxedTuple :: (# X, Y #) -> (# X, Y, Z #)
unboxedTuple = undefined 

unboxedSum :: (# X | Y #) -> (# X | Y | Z #)
unboxedSum = undefined 

