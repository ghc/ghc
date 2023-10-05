{-# LANGUAGE PartialTypeSignatures #-}
module ExtraConstraints1 where

arbitCs1 :: _ => a -> String
arbitCs1 x = show (succ x) ++ show (x == x)

arbitCs2 :: (Show a, _) => a -> String
arbitCs2 x = arbitCs1 x

arbitCs3 :: (Show a, Enum a, _) => a -> String
arbitCs3 x = arbitCs1 x

arbitCs4 :: (Eq a, _) => a -> String
arbitCs4 x = arbitCs1 x

arbitCs5 :: (Eq a, Enum a, Show a, _) => a -> String
arbitCs5 x = arbitCs1 x
