{-# LANGUAGE UndecidableInstances, ExistentialQuantification,
              ScopedTypeVariables, Rank2Types #-}

--	Modular arithmetic, due to Dale Thurston

-- Here's a way to mimic dependent types using existential types,
-- illustrated by an implementation of modular arithmetic.  To try it
-- out, load modulus.hs and try something like
--	inModulus (mkModulus (1234567890123::Integer)) (^ 98765432198765) 2
-- to compute 2 to the 98765432198765'th power modulo 1234567890123.

-- The key is the definitions at the top of TypeVal.hs:
-- 
--   class TypeVal a t | t -> a where
--       -- typeToVal should ignore its argument.
--       typeToVal :: t -> a
--   
--   data Wrapper a = forall t . (TypeVal a t) => Wrapper t
--   
--   class ValToType a where
--      valToType :: a -> Wrapper a
-- 
-- `valToType' takes a value `x' and returns a (wrapped version of a)
-- fake value in a new type; from the new type, `x' can be recovered by
-- applying typeToVal.
--
-- This code works under ghc.  It uses existentially quantified data
-- constructors, scoped type variables, and explicit universal
-- quantification.


module Main where

import TypeVal

default (Integer)

main = print (map (inModulus (mkModulus (1234567890123::Integer)) (^98765432198765)) [2..1000])



data Mod s a = Mod {value::a} deriving (Eq, Show)

data Modulus a = forall s. TypeVal a s => Modulus (a -> Mod s a) (Mod s a -> a)

mkModulus :: (ValToType a, Integral a) => a -> Modulus a
mkModulus x = case valToType x of {Wrapper (y :: t) ->
	      Modulus normalize (value :: Mod t a -> a)}

normalize :: forall a s. (TypeVal a s, Integral a) => a -> Mod s a
normalize x = (Mod (x `mod` typeToVal (undefined::s)))

inModulus :: Modulus a -> (forall s . TypeVal a s =>  Mod s a -> Mod s a)
	        -> a -> a
inModulus (Modulus in_ out) f x = out (f (in_ x))

instance (TypeVal a s, Integral a) => Num (Mod s a) where
    Mod x + Mod y = normalize (x + y)
    Mod x - Mod y = normalize (x - y)
    negate (Mod x) = normalize (negate x)
    Mod x * Mod y = normalize (x * y)
    fromInteger a = normalize (fromInteger a)
