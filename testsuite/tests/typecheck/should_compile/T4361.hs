{-# LANGUAGE FlexibleContexts #-}

-- This test comes from Sergei Mechveliani's DoCon system

module Pol3_ (moduloBasisx) where

class CommutativeRing a
class CommutativeRing a => LinSolvRing a
class LinSolvRing a => EuclideanRing a

instance EuclideanRing a => LinSolvRing (Pol a)	       -- XXXX
instance CommutativeRing a => CommutativeRing (Pol a)

data Pol a = MkPol

upLinSolvRing :: LinSolvRing a => a -> ()
upLinSolvRing = undefined

moduloBasisx :: (LinSolvRing (Pol a), CommutativeRing a) => Pol a -> ()
moduloBasisx p = let x :: ()
                     x = upLinSolvRing p
                 in ()

  -- This is very delicate!  The contraint (LinSolvRing (Pol a))
  -- arises in the RHS of x, and we must be careful *not* to simplify
  -- it with the instance declaration "XXXX", else we get the 
  -- unsatisfiable constraint (EuclideanRing a). In effect, the
  -- given constraint in the type sig for moduleBasisx overlaps
  -- with the top level declaration.

