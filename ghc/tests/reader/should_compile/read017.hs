-- !!! Checking that empty declarations are permitted.
module ShouldSucceed where


class Foo a where

class Foz a

x = 2 where 
y = 3

instance Foo Int where

f = f where g = g where
type T = Int
