-- !!! a very simple test of class and instance declarations

module ShouldSucceed where

class H a where
 op1 :: a -> a -> a

instance H Bool where
 op1 x y = y

f :: Bool -> Int -> Bool
f x y = op1 x x
