f :: Int -> [Int]
f i = [ j | j <- [i], h <- [j], k <- [h]]
