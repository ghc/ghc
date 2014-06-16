{- This program shows up a bug in the handling of
   the monomorphism restriction in an earlier version of
   ghc.  With ghc 0.18 and before, f gets a type with
   an unbound type variable, which shows up in the
   interface file.  Reason: it was being monomorphised.

   Simon PJ
-}

module ShouldSucceed where


g :: Num a => Bool -> a -> b -> a
g b x y = if b then x+x else x-x

-- Everything is ok if this signature is put in
-- but the program should be perfectly legal without it.
-- f :: Num a => a -> b -> a
f = g True

h y x = f (x::Int) y
	-- This use of f binds the overloaded monomorphic
	-- type to Int
