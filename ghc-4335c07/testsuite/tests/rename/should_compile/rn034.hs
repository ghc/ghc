-- !!! Checking that empty declarations are permitted.
module ShouldCompile where


class Foo a where

class Foz a

x = 2 where 
y = 3

instance Foo Int where
