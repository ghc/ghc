module ShouldSucceed where

class Eq' a where
 doubleeq :: a -> a -> Bool

class (Eq' a) => Ord' a where
 lt :: a -> a -> Bool

instance Eq' Int where
 doubleeq x y = True

instance Ord' Int where
 lt x y = True

f x y | lt x 1 = True
      | otherwise = False
