module ShouldSucceed where

class C a where
 op1 :: a -> a

class (C a) => B a where
 op2 :: a -> a -> a

instance (B a) => B [a] where
 op2 xs ys = xs

instance C [a] where
 op1 xs = xs

{- This was passed by the prototype, but failed hard in the new
typechecker with the message

Fail:No match in theta_class
-}
