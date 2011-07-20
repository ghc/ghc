module ShouldSucceed where

class C a where
 op1 :: a -> a

class (C a) => B a where
 op2 :: a -> a -> a

{- Failed hard in new tc with "No match in theta_class" -}
