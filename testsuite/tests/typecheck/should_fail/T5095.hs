{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Test where

instance {-# OVERLAPPABLE #-} Show a => Eq a where
   x == y =  length (show x) == length (show y)

f :: Show a => a -> a -> Bool
f x y = x == y

p = f (3 :: Int) 4
