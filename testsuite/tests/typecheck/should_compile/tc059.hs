module ShouldSucceed where

class Eq2 a where
 deq :: a -> a -> Bool
 foo :: a -> a

instance Eq2 Int where
 deq x y = True
 foo x = x

instance (Eq2 a) => Eq2 [a] where
 deq (a:as) (b:bs) = if (deq a (foo b)) then (deq as (foo bs)) else False
 foo x = x

f x = deq x [1]
