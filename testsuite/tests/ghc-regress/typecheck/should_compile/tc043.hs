module ShouldSucceed where

-- !!! another simple test of class and instance code.

class A a where
 op1 :: a

instance A Int where
 op1 = 2

f x = op1

class B b where
 op2 :: b -> Int

instance (B a) => B [a] where
 op2 [] = 0
 op2 (x:xs) = 1 + op2 xs
