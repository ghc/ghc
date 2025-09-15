module ShouldSucceed where

class Eq1 a where
 deq :: a -> a -> Bool

instance Eq1 Int where
 deq x y = True

instance (Eq1 a) => Eq1 [a] where
 deq (a:as) (b:bs) = if (deq a b) then (deq as bs) else False

f x (y:ys) = deq x ys
