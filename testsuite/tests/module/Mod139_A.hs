module Mod139_A where

data Foo = Bar

class C a where
  m1 :: a -> Int
  
instance C Int where
  m1 _ = 2

x = 'x'



