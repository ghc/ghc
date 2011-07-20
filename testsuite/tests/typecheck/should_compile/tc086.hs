{-
  From: Marc van Dongen <dongen@cs.ucc.ie>
  Date: Sat, 31 May 1997 19:57:46 +0100 (BST)

   panic! (the `impossible' happened):
           tcLookupTyVar:a_r6F

   Please report it as a compiler bug to glasgow-haskell-bugs@dcs.gla.ac.uk.


If the instance definition for (*) at the end of this toy module
is replaced by the definition that is commented, this all compiles
fine. Strange, because the two implementations are equivalent modulo
the theory {(*) = multiply}.

Remove the `multiply :: a -> a -> a' part, and it compiles without
problems.


SPJ note: the type signature on "multiply" should be
	multiply :: Group a => a -> a -> a

-}

module ShouldSucceed( Group, Ring ) where

import qualified Prelude( Ord(..), Eq(..), Num(..) )
import Prelude hiding( Ord(..), Eq(..), Num(..) )

class Group a where
  compare     :: a -> a -> Prelude.Ordering
  fromInteger :: Integer -> a
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  zero :: a
  one  :: a
  zero = fromInteger 0
  one  = fromInteger 1

-- class (Group a) => Ring a where
-- (*) :: a -> a -> a
-- (*) a b =
--                  case (compare a zero) of
--                    EQ -> zero
--                    LT -> zero - ((*) (zero - a) b)
--                    GT -> case compare a one of
--                            EQ -> b
--                            _  -> b + ((*) (a - one) b)

class (Group a) => Ring a where
  (*) :: a -> a -> a
  (*) a b = multiply a b
          where multiply :: Group b => b -> b -> b
                multiply a b
                  = case (compare a zero) of
                      EQ -> zero
                      LT -> zero - (multiply (zero - a) b)
                      GT -> case compare a one of
                              EQ -> b
                              _  -> b + (multiply (a - one) b)
