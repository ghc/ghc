module T22227b (tsum) where

data Tree a = Bin (Tree a) (Tree a)
            | Leaf a

tsum :: Tree Int -> Int -> Int
tsum (Bin l r) acc = tsum l . tsum r $ acc
tsum (Leaf n) acc = n + acc
