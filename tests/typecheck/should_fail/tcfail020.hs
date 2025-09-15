module ShouldFail where

class A a where
 p1 :: a -> a
 p2 :: a -> a -> a

class (A b) => B b where
 p3 :: b

instance (A a) => B [a] where
 p3 = []

data X = XC --, causes stack dump

--instance B Bool where
-- p3 = True
