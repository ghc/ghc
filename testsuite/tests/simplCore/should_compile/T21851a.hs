module T21851a where

f :: Num b => b -> (b, b) -- note: recursive to prevent inlining
f x = (x + 1, snd (f x))  -- on such a small example
{-# SPECIALIZE f :: Int -> (Int, Int) #-}
