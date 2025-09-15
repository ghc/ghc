module T18815 where

loop :: Int -> Int -> (Int, ())
loop x y = go x
  where
    go x = if x > y then (x, ()) else go (x*2)
