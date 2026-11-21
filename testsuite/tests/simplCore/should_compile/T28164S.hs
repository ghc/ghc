{-# LANGUAGE BangPatterns #-}
module T28164S where

-- step is lazy in k (the x < 0 branch), so its worker takes k boxed.
-- At -O2 liberate-case peels hash's first iteration, which puts a
-- `case k of I# y` in the body of go's join points; go's exit also takes
-- k apart.
{-# NOINLINE step #-}
step :: Int -> Int -> Int
step k x
  | x < 0 = 0
  | otherwise = go (hash 0 0)
  where
    hash !i !acc | i == k = acc
                 | otherwise = hash (i + 1) (acc * 131 + x + i)
    go !j | j < 1000 = j * k
          | otherwise = go (j `quot` 7)
