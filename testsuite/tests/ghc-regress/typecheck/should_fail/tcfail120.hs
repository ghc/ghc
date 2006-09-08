{-# OPTIONS -fglasgow-exts #-}


-- Could be ok, because nothing is bound to the unboxed tuple
-- but actually rejected, because a wild card is rather like
-- an unused variable.  Could fix this, but it's really a corner case

module ShouldFail where

type T a = Int -> (# Int, Int #)

f2 :: T a -> T a
f2 t = \x -> case t x of _ -> (# 3,4 #)

