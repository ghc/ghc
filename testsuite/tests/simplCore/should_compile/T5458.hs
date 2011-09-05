module Foo where

type T a = Int

f :: T a -> Bool -> [Int]
f x b = case b of
               True  -> f x b
               False -> x : f x (x > 19)
