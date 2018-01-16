{-
	This module defines a Normal cless and a function normal
	used for forcing evaluation.  The idea is originally
	from Colin and has been modified to do recursive
	force evaluation.  To prevent repeated force, the
	expression to be forced should be packed with a flag
	indicating if a force has been done.  Functions
	which support this idea are provided.

	XZ, 19/2/92
-}

module Norm where

{-
		class Normal
-}

infixr 3 `andAnd` -- partain
andAnd a b = if a then
		if b then True
		     else error "andAnd: 2nd argument not True"
	     else
		error "andAnd: first argument not True\n"
-- end partain

data Norm_able a = Norm_pack Bool a deriving ()

class Normal a where
	normal :: a -> Bool

instance (Normal a) => Normal [a] where
	normal (x:xs) = normal x `andAnd` normal xs
	normal _ = True

instance (Normal a, Normal b) => Normal (a,b) where
	normal (x,y) = normal x `andAnd` normal y

instance (Normal a, Normal b, Normal c) => Normal (a,b,c) where
	normal (x,y,z) = normal x `andAnd` normal y `andAnd` normal z

instance
	(Normal a, Normal b, Normal c,
	 Normal d, Normal e, Normal f) =>
	Normal (a,b,c,d,e,f) where
	normal (x,y,z,u,v,w) =
		normal x `andAnd` normal y `andAnd` normal z `andAnd`
		normal u `andAnd` normal v `andAnd` normal w

{- not in 1.3:
instance (Normal a, Normal b) =>
	Normal (Assoc a b) where
	normal (i:=v) = normal i `andAnd` normal v
-}

instance Normal Bool where
	normal True = True
	normal _ = True

instance Normal Int where
	normal 0 = True
	normal _ = True

instance Normal Float where
	normal 0 = True
	normal _ = True

instance Normal Double where
	normal 0 = True
	normal _ = True

{-
instance Normal ( a -> b ) where
	normal _ = True
-}

pack_obj obj = Norm_pack (normal obj) obj

normalize_obj (Norm_pack b _) = b

retrieve_obj (Norm_pack _ obj) = obj
