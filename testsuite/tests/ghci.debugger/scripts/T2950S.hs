module T2950S where
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)
  where
    insert x [] = [x]
    insert x (y:ys) | x < y     = x:y:ys
                    | otherwise = y:(insert x ys)
