module ShouldSucceed where

-- See also tcfail060.hs

class Eq' a where
 deq :: a -> a -> Bool

instance Eq' Int where
  deq x y = True

instance (Eq' a) => Eq' [a] where
   deq (a:as) (b:bs) = dand (f a b) (f as bs)

dand True True = True
dand x y = False

f :: Eq' a => a -> a -> Bool
f p q = dand (deq p q) (deq [1::Int] [2::Int])
