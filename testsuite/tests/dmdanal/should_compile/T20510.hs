module T20510 where

small :: Int -> Int
small x = go 0 x
  where
    go z 0 = z * x
    go z y = go (z+y) (y-1)
