module T11545 where

data A = A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A deriving (Eq)

data QuadTree a = QuadTree !Int [a] (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)

foldQuadTree :: (a -> b -> b) -> Int -> b -> QuadTree a -> b
foldQuadTree f maxSize = go
    where
        go z (QuadTree size elems t1 t2 t3 t4)
            | size <= maxSize = foldr f z elems
            | otherwise       = go (go (go (go z t4) t3) t2) t1
