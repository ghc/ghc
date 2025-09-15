newtype MkT2 a = MkT2 [Maybe a] deriving Show

f :: t Int -> t Int
f x = x

f2 :: t Int -> t Int -> (t Int, t Int)
f2 x y = (x,y)