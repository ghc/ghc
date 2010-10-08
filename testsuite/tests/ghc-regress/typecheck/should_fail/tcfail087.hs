{-# LANGUAGE UnboxedTuples #-}

-- !!! Check that unboxed tuples can't be function arguments
module ShouldFail where

data Ex = Ex (# Int,Int #)

f :: (# Int,Int #) -> Int
f x = error "urk"

g (# x,y #) = x


