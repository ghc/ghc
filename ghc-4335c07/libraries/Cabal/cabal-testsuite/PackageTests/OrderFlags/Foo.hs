module Foo where

x :: IO Int
x = return 5

f :: IO Int
f = do x
       return 3
