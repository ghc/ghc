module ShouldSucceed where

class Eq' a where
 doubleeq :: a -> a -> Bool

class (Eq' a) => Ord' a where
 lt :: a -> a -> Bool

instance Eq' Int where
 doubleeq x y = True

instance (Eq' a) => Eq' [a] where
 doubleeq x y = True

instance Ord' Int where
 lt x y = True

{-
class (Ord a) => Ix a where
 range :: (a,a) -> [a]

instance Ix Int where
 range (x,y) = [x,y]
-}






