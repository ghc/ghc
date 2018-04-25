{-# LANGUAGE GADTs #-}
module Main where

data T t a where
  T :: (Foldable t, Eq a) => t a -> T t a

{-# NOINLINE go #-}
go :: T [] a -> Int -> Int
go (T _) i = foldr (+) 0 [1..i]

main = print (go (T [1::Int]) 20000)
