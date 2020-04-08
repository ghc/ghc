{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Linear2 where

dup :: a -> (a,a)
dup x = (x,x)

incorrectApp1 :: a ⊸ ((a,Int),(a,Int))
incorrectApp1 x = dup (x,0)

incorrectApp2 :: (a->b) -> a ⊸ b
incorrectApp2 f x = f x

incorrectIf :: Bool -> Int ⊸ Int
incorrectIf x n =
  if x then n else 0
