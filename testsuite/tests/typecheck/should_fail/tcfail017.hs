
module ShouldFail where

class C a where
 op1 :: a -> a

class (C a) => B a where
 op2 :: a -> a -> a

instance (B a) => B [a] where
 op2 xs ys = xs


