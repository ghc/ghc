{-# OPTIONS -fglasgow-exts #-}

import PrelGHC

class  EEq a  where
    (===), (/==)		:: a -> a -> Bool

--    x /= y		= not (x == y)
--    x == y		= not (x /= y)
--    x /= y		=  True
    (/==) x y            = mynot  ((===) x y)
    x === y		=  True

data EOrdering = ELT | EEQ | EGT 

mynot True = False
mynot False = True

{-
class  (EEq a) => EOrd a  where
    ecompare             :: a -> a -> EOrdering
    (<<), (<<=), (>>>=), (>>>):: a -> a -> Bool
    emax, emin		:: a -> a -> a

-- An instance of Ord should define either compare or <=
-- Using compare can be more efficient for complex types.
    ecompare x y
	    | x === y    = EEQ
	    | x <<= y    = ELT	-- NB: must be '<=' not '<' to validate the
				-- above claim about the minimal things that can
				-- be defined for an instance of Ord
	    | otherwise = EGT

    x <<= y  = case ecompare x y of { EGT -> False; _other -> True }
    x <<	 y  = case ecompare x y of { ELT -> True;  _other -> False }
    x >>>= y  = case ecompare x y of { ELT -> False; _other -> True }
    x >>>	 y  = case ecompare x y of { EGT -> True;  _other -> False }

	-- These two default methods use '>' rather than compare
	-- because the latter is often more expensive
    emax x y = if x >>> y then x else y
    emin x y = if x >>> y then y else x
-}

data EInt = EI Int#

ezeroInt, eoneInt, etwoInt, emaxInt, eminInt :: EInt
ezeroInt = EI 0#
eoneInt  = EI 1#
etwoInt  = EI 2#
eminInt  = EI (-2147483648#)	-- GHC <= 2.09 had this at -2147483647
emaxInt  = EI 2147483647#
eeqInt	(EI x) (EI y) = x ==# y
eneInt	(EI x) (EI y) = x /=# y

instance EEq EInt where
    (===) x y = x `eeqInt` y
    (/==) x y = x `eneInt` y

main = putStr (if (ezeroInt === eoneInt) then "no!\n" else "yes!\n")

