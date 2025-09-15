module T7797a where

class Size t where
  size :: t -> t -> Int
  burg :: t -> t

instance (Ord a, Num a) => Size [a] where
  {-# SPECIALISE instance Size [Int] #-}
  size (x:xs) (y:ys) | x+y > 4   = size xs ys
                     | otherwise = size xs ys
  size _ _ = 0
  burg = error "urk"
