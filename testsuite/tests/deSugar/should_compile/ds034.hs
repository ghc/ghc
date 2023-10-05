-- !!! mutually-recursive methods in an instance declaration
--
module ShouldCompile where

class Foo a where
    op1 :: a -> a 
    op2 :: a -> a 

instance Foo Int where
    op1 x = op2 x
    op2 y = op1 y
