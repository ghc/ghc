{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Language.Haskell.Syntax.BooleanFormula(
  BooleanFormula(..), LBooleanFormula,
  mkVar, mkFalse, mkTrue, mkBool, mkAnd, mkOr
  ) where

import Prelude hiding ( init, last )
import Data.List ( nub )
import Language.Haskell.Syntax.Extension (XRec, LIdP)


-- types
type LBooleanFormula p = XRec p (BooleanFormula p)
data BooleanFormula p = Var (LIdP p) | And [BooleanFormula p] | Or [BooleanFormula p]
                      | Parens (BooleanFormula p)

-- instances
deriving instance Eq (LIdP p) => Eq (BooleanFormula p)

-- smart constructors
-- see note [Simplification of BooleanFormulas]
mkVar :: LIdP p -> BooleanFormula p
mkVar = Var

mkFalse, mkTrue :: BooleanFormula p
mkFalse = Or []
mkTrue = And []

-- Convert a Bool to a BooleanFormula
mkBool :: Bool -> BooleanFormula p
mkBool False = mkFalse
mkBool True  = mkTrue

-- Make a conjunction, and try to simplify
mkAnd :: Eq (LIdP p) => [BooleanFormula p] -> BooleanFormula p
mkAnd = maybe mkFalse (mkAnd' . nub . concat) . mapM fromAnd
  where
  -- See Note [Simplification of BooleanFormulas]
  fromAnd :: BooleanFormula p -> Maybe [BooleanFormula p]
  fromAnd bf = case bf of
    (And xs) -> Just xs
     -- assume that xs are already simplified
     -- otherwise we would need: fromAnd (And xs) = concat <$> traverse fromAnd xs
    (Or [])  -> Nothing
     -- in case of False we bail out, And [..,mkFalse,..] == mkFalse
    _        -> Just [bf]
  mkAnd' [x] = x
  mkAnd' xs = And xs

mkOr :: Eq (LIdP p) => [BooleanFormula p] -> BooleanFormula p
mkOr = maybe mkTrue (mkOr' . nub . concat) . mapM fromOr
  where
  -- See Note [Simplification of BooleanFormulas]
  fromOr bf = case  bf of
    (Or xs)  -> Just xs
    (And []) -> Nothing
    _        -> Just [bf]
  mkOr' [x] = x
  mkOr' xs  = Or xs
