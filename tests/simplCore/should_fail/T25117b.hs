module T25117b where

f :: Num a => a -> a
f = f

-- We don't allow old-form multiple type ascriptions
{-# SPECIALISE forall . f :: Int->Int, Float->Float #-}
