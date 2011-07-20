module TestDataCon where

{-# CONTRACT g1 :: ({x | x>0}, any) -> {r | r>0} #-}
g1 :: (Int, Int) -> Int
g1 (x,y) = x
{-# CONTRACT g2 :: (any, {y | y>0}) -> {r | r>0} #-}
g2 :: (Int, Int) -> Int
g2 (x,y) = y

bad = error "bad!"

t1 = g1 (5, bad)
t2 = g2 (bad, 6)
t3 = g1 (bad, 7)
t4 = g1 (-1, 6) -- seems inlining is done.

{-
data A a = B a | C a Int

{-# CONTRACT g :: B {x | x>0} -> any #-}
g :: B Int -> Int
g (B x) = x


-}
