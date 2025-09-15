module T25117a where

f :: Ord a => a -> a
f = f

{-# SPECIALISE let x = 2 in f x #-}
