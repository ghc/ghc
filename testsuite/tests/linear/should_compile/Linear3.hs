{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Linear3 where

correctApp1 :: (a⊸b) ⊸ a ⊸ b
correctApp1 f a = f a

correctApp2 :: (a⊸a) -> a ⊸ a
correctApp2 f a = f (f a)

correctApp3 :: Int ⊸ Int
correctApp3 x = f x
  where
    f :: Int ⊸ Int
    f y = y

correctApp4 :: Int ⊸ Int
correctApp4 x = f (f x)
  where
    f :: Int ⊸ Int
    f y = y

correctIf :: Bool ⊸ a ⊸ a
correctIf x n =
   if x then n else n
