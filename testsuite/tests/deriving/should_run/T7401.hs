{-# LANGUAGE EmptyDataDeriving #-}
module Main where

import Data.Function

data Foo
  deriving (Eq, Ord, Read, Show)

foo1 :: Foo
foo1 = fix id

foo2 :: Foo
foo2 = let x = y
           y = x
        in y

main :: IO ()
main = do
  print (foo1 ==        foo2)
  print (foo1 `compare` foo2)
