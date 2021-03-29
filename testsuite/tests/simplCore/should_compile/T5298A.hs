module T5298A where

{-# INLINABLE fac #-}
fac :: (Eq a, Num a) => a -> a
fac 0 = 1
fac n = n * fac (n-1)

{-# INLINE f #-}
f :: (Eq a, Num a) => a -> a
f a = fac a
