{-# LANGUAGE RankNTypes #-}
module Lib (g) where

{-# NOINLINE consume #-}
consume :: Int -> Int
consume n = n `seq` 0

{-# RULES "consume/drop" [~1] forall n. consume n = 0 #-}

-- A dead value argument (n) that comes *before* the dictionary we
-- specialise on.  No implicit parameters involved.
{-# INLINE [1] f #-}
f :: forall a. Int -> Show a => a -> String
f n y = show y ++ replicate (consume n) '!'

{-# NOINLINE g #-}
g :: Bool -> String
g b = f 7 b ++ "."
