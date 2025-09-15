{-# LANGUAGE UnboxedTuples #-}

-- !!! Check that unboxed tuples can be function arguments
module Main where

data Ex = Ex (# Int,Int #)

{-# NOINLINE f #-} -- Make it harder to get right
f :: (# Int,Int #) -> Int
f x = error "urk"

{-# NOINLINE g #-} -- Make it harder to get right
g (Ex (# x,y #)) = x


main = print $ g (Ex (# 10, f (# 20, 30 #) #))