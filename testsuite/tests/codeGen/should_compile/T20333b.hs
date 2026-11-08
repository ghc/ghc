{-# LANGUAGE BangPatterns, MagicHash #-}

-- A function closure over four free variables, a, b, c and d, all used in both
-- arms of a branch before the call to g and all used again after it, with no
-- loop and no allocation between: the closure is saved across the call in
-- their place and they are loaded from it again after the call (#20333).
module T20333b (mk) where

import GHC.Exts

{-# NOINLINE g #-}
g :: Int -> Int
g x = x * 3 + 1

{-# NOINLINE apply #-}
apply :: (Int# -> Int#) -> Int -> Int
apply f n = go 0 0
  where
    go !acc !i | i >= n = acc
               | otherwise = case i of I# i# -> go (acc + I# (f i#)) (i + 1)

{-# NOINLINE mk #-}
mk :: Int -> Int -> Int -> Int -> Int -> Int
mk (I# a) (I# b) (I# c) (I# d) n = apply f n
  where
    f x = case g (I# (if isTrue# (x ># 7#) then (x *# a) +# (b *# c) -# d
                                           else (x *# b) -# (a *# d) +# c)) of
            I# r -> r +# a +# b +# c +# d
