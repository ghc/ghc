-- !!! Class and instance decl

module Test where

class K a where
	op1 :: a -> a -> a
	op2 :: Int -> a

instance K Int where
	op1 a b = a+b
	op2 x   = x

instance K Bool where
	op1 a b = a
	-- Pick up the default decl for op2
	
instance K [a] where
	op3 a = a	-- Oops!  Isn't a class op of K
	
