module Irr2
where

foo :: Bool -> Int -> Bool
foo b n
  | n > 10    = even n
  | otherwise = odd n
  where
    even 0 = b
    even n = odd (n-1)
    {-# NOINLINE even #-}
    odd 0 = b
    odd n = even (n-1)
    {-# NOINLINE odd #-}
