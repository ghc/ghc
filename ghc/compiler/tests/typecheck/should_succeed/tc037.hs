module ShouldSucceed where

class Eq' a where
  deq :: a -> a -> Bool

instance (Eq' a) => Eq' [a] where
 deq []     [] = True
 deq (x:xs) (y:ys) = if (x `deq` y) then (deq xs ys) else False
 deq other1 other2 = False
