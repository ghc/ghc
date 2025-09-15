module T22513c where

f :: Int -> Int 
f a = g a 
  where
    g :: a -> a
    g = id