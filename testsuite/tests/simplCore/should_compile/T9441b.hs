module T9441b where

f1 :: Integer -> Integer
f1 n
    | n <= 1 = 1
    | otherwise = go n 1
  where
    go 0 r = r
    go m r = go (m - 1) (r * m)

f2 :: Integer -> Integer
f2 n = go n 1
  where
    go 0 s = s
    go p s = go (p - 1) (s * p)
