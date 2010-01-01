-- !!! test of cyclic default methods
--
class Foo a where
    op1 :: Fractional b => a -> b -> Bool
    op2 :: Fractional b => a -> b -> Bool
    op3 :: Fractional b => a -> b -> Bool
    op4 :: Fractional b => a -> b -> Bool
    op5 :: Fractional b => a -> b -> Bool
    op6 :: Fractional b => a -> b -> Bool

    -- each depends on the next:
    op1 a b = not (op2 a b)
    op2 a b = not (op3 a b)
    op3 a b = not (op4 a b)
    op4 a b = not (op5 a b)
    op5 a b = not (op6 a b)
    op6 a b = not (op1 a b)

-- now some instance decls to break the cycle:
instance Foo Int where
    op1 a b = a == 42

instance Foo Char where
    op1 a b = a == 'c'

instance Foo a => Foo [a] where
    op1 a b = null a

-- try it:
main = do
    putStr (show (op2 (3::Int)    3.14159))
    putStr (show (op2 'X' 	  3.14159))
    putStr (show (op2 ([]::[Char])3.14159))
