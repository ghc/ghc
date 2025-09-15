module T15730b where

(.!) :: (a, a) -> Bool -> a
a .! True = fst a
a .! False = snd a

t :: Bool -> Integer
t x = (5,6) .! {-# SCC a1 #-} {-# SCC a2 #-} x :: Integer
