{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Language.Haskell.Syntax.BooleanFormula(
  BooleanFormula(..), LBooleanFormula,
  mkVar, mkFalse, mkTrue, mkBool, mkAnd, mkOr,
  BooleanFormulaDefault(..)
  ) where

import Prelude hiding ( init, last )
import Data.List ( nub )
import Language.Haskell.Syntax.Extension

-- types
type LBooleanFormula p = XRec p (BooleanFormula p)
data BooleanFormula p
       = Var    (XBFVar p)    (LIdP p)
       | And    (XBFAnd p)    [LBooleanFormula p]
       | Or     (XBFOr p)     [LBooleanFormula p]
       | Parens (XBFParens p) (LBooleanFormula p)
       | XBooleanFormula !(XXBooleanFormula p)

class BooleanFormulaDefault p where
   bfAnnAnd :: XBFAnd p
   bfAnnOr  :: XBFOr p

-- instances
deriving instance (Eq (LIdP p), Eq (LBooleanFormula p),
                   Eq (XBFVar p),
                   Eq (XBFAnd p),
                   Eq (XBFOr p),
                   Eq (XBFParens p),
                   Eq (XXBooleanFormula p)
                   ) => Eq (BooleanFormula p)

-- smart constructors
-- see note [Simplification of BooleanFormulas]
mkVar :: XBFVar p ->  LIdP p -> BooleanFormula p
mkVar = Var

mkFalse, mkTrue :: forall p. BooleanFormulaDefault p => BooleanFormula p
mkFalse = Or (bfAnnOr @p) []
mkTrue = And (bfAnnAnd @p) []

-- Convert a Bool to a BooleanFormula
mkBool :: BooleanFormulaDefault p => Bool -> BooleanFormula p
mkBool False = mkFalse
mkBool True  = mkTrue

-- Make a conjunction, and try to simplify
mkAnd :: forall p. (UnXRec p, Eq (LIdP p), Eq (LBooleanFormula p), BooleanFormulaDefault p)
      => [LBooleanFormula p] -> BooleanFormula p
mkAnd = maybe mkFalse (mkAnd' . nub . concat) . mapM fromAnd
  where
  -- See Note [Simplification of BooleanFormulas]
  fromAnd :: LBooleanFormula p -> Maybe [LBooleanFormula p]
  fromAnd bf = case unXRec @p bf of
    (And _ xs) -> Just xs
     -- assume that xs are already simplified
     -- otherwise we would need: fromAnd (And xs) = concat <$> traverse fromAnd xs
    (Or _ [])  -> Nothing
     -- in case of False we bail out, And [..,mkFalse,..] == mkFalse
    _        -> Just [bf]
  mkAnd' [x] = unXRec @p x
  mkAnd' xs = And (bfAnnAnd @p) xs

mkOr :: forall p. (UnXRec p, Eq (LIdP p), Eq (LBooleanFormula p), BooleanFormulaDefault p)
     => [LBooleanFormula p] -> BooleanFormula p
mkOr = maybe mkTrue (mkOr' . nub . concat) . mapM fromOr
  where
  -- See Note [Simplification of BooleanFormulas]
  fromOr bf = case unXRec @p bf of
    (Or _ xs)  -> Just xs
    (And _ []) -> Nothing
    _          -> Just [bf]
  mkOr' [x] = unXRec @p x
  mkOr' xs = Or (bfAnnOr @p) xs
