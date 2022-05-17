module Max where

delta :: Int -> Int -> Int
delta m n =
  let (large, small) = if m > n then (m, n) else (n, m)
  in  large - small
