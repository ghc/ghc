-- !!! no type variable on a context
-- !!! reported by Sigbjorn Finne

moby :: Num => Int -> a -> Int
moby x y = x+y
