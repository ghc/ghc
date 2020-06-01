{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TupleSections #-}
module LinearConstructors where

data T a b = MkT a b

f1 :: a #-> b #-> T a b
f1 = MkT

f2 :: a #-> b -> T a b
f2 = MkT

f3 :: a -> b #-> T a b
f3 = MkT

f4 :: a -> b -> T a b
f4 = MkT

-- tuple sections
g1 :: a #-> b #-> (a, b, Int)
g1 = (,,0)

g2 :: a #-> b -> (a, b, Int)
g2 = (,,0)

g3 :: a -> b #-> (a, b, Int)
g3 = (,,0)

g4 :: a -> b -> (a, b, Int)
g4 = (,,0)
