module ShouldSucceed where

class Eq1 a where
 deq :: a -> a -> Bool

instance (Eq1 a) => Eq1 [a] where
 deq (a:as) (b:bs) = deq a b

instance Eq1 Int where
 deq x y = True

