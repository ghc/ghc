-- !!! small class decl with local polymorphism;
-- !!! "easy" to check default methods and such...
-- !!! (this is the example given in TcClassDcl)
--
module ShouldSucceed where

class Foo a where
    op1 :: a -> Bool
    op2 :: Ord b => a -> b -> b -> b

    op1 x = True
    op2 x y z = if (op1 x) && (y < z) then y else z

instance Foo Int where {}

instance Foo a => Foo [a] where {}
