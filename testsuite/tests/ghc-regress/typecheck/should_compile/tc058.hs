module ShouldSucceed where

class Eq2 a where
 doubleeq :: a -> a -> Bool

class (Eq2 a) => Ord2 a where
 lt :: a -> a -> Bool

instance Eq2 Int where
 doubleeq x y = True

instance Ord2 Int where
 lt x y = True

instance (Eq2 a,Ord2 a) => Eq2 [a] where
 doubleeq xs ys = True

f x y = doubleeq x [1] 
