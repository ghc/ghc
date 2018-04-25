module ListAux where

import Data.List


-- splitting into n parts, and its inverse:

splitIntoN :: Int -> [a] -> [[a]]
splitIntoN n xs = takeIter parts xs
  where l = length xs
        parts = zipWith (+) ((replicate (l `mod` n) 1) ++ repeat 0)
                            (replicate n (l `div` n))
takeIter :: [Int] -> [a] -> [[a]]
takeIter [] [] = []
takeIter [] _  = error "elements left over"
takeIter (t:ts) xs = hs : takeIter ts rest
    where (hs,rest) = splitAt t xs

unSplit :: [[a]] -> [a]
unSplit = concat

-- splitting into parts of same size. Inverse is concat again.

splitAtN :: Int -> [a] -> [[a]]
splitAtN n [] = []
splitAtN n xs = ys : splitAtN n zs
	        where (ys,zs) = splitAt n xs

----------------------------------------

-- splitting round-robin until list runs empty, and its inverse:

unshuffle :: Int -> [a] -> [[a]]
unshuffle n xs = [takeEach n (drop i xs) | i <- [0..n-1]]
 where takeEach n [] = []
       takeEach n (x:xs) = x : takeEach n (drop (n-1) xs)

-- inverse to unshuffle
shuffle :: [[a]] -> [a]
shuffle = concat . transpose

