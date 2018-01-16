module ShouldSucceed where

class Eq' a where
 deq :: a -> a -> Bool

instance Eq' Int where
 deq x y = True

instance (Eq' a) => Eq' [a] where
 deq (a:as) (b:bs) = if (deq a b) then (deq as bs) else False

f x = deq x [1]
