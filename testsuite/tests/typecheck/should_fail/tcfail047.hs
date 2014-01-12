module ShouldFail where

class A a where
 op1 :: a -> a

instance A (a,(b,c)) where
 op1 a = a
