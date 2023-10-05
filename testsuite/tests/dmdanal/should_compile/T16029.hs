module T16029 where

data S = MkS Int Int

g1 :: S -> Int -> Int
g1 (MkS x y) 0 = 0
g1 (MkS x y) n = g1 (MkS y x) (n-1)

data T = MkT !Int !Int
g2 :: T -> Int -> Int
g2 (MkT x y) 0 = 0
g2 (MkT x y) n = g2 (MkT y x) (n-1)
