module M (f) where

f :: Int -> Int
f i = go [ 1, 0 ]
    where
      go :: [Int] -> Int
      go []     = undefined
      go [1]    = undefined
      go (x:xs) | x == i    = 2
                | otherwise = go xs
