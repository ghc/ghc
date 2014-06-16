{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Main where

main :: IO ()
main =  do
  let k1 = func1 1 :: Int
  print $ show k1
  let k2 = outer :: Int
  print $ show k2

outer :: Class3 a => a
outer = func1 1

class Class1 a where
  func1 :: Int -> a

class Class1 a => Class2 a where
  func2 :: Int -> a

class Class2 a => Class3 a where
  func3 :: Int -> a

instance Class1 Int where
  func1 = id

instance Class3 a => Class2 a where
  func2 = func3

instance Class3 Int where
  func3 = id

--
-- "1"
-- tests: <<loop>>
