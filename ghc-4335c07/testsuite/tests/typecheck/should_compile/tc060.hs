module ShouldSucceed where

class Eq2 a where
 deq :: a -> a -> Bool

instance (Eq2 a) => Eq2 [a] where
 deq (a:as) (b:bs) = if (deq a b) then (deq as bs) else False


instance Eq2 Int where
 deq x y = True

