{-# LANGUAGE UnboxedTuples #-}

module ShouldFail where

h :: Int -> (# Int, Int #)
h x = (# x,x #)

foo x = case h x of 
	  ~(# p, q #) -> p
