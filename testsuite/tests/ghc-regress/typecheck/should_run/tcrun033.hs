{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

-- This test made GHC 6.3 build a superclass loop, in
-- the instance ClassA a String declaration

module Main where

class (Sat (a -> b -> String), ClassB b) => ClassA a b

class ClassB a where
  fun :: a -> String

class Sat x where
   sat :: x

instance ClassA a b => Sat (a -> b -> String) where
  sat = const fun

instance ClassA a String
-- Badness was that the ClassB superclass dict was loopy
--
-- Needs Sat (a -> String -> String), ClassB String
-- --> ClassA a String, ClassB String
-- and adding ClassA gives superclass ClassB.

instance ClassB String where
  fun = id

main = print ((sat :: Int -> String -> String) 3 "hello")
