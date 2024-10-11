{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Language.Haskell.Syntax.BooleanFormula(
  BooleanFormula(..),
  mkVar, mkFalse, mkTrue, mkBool, mkAnd, mkOr
  ) where

import Prelude hiding ( init, last )
import Data.List ( nub )

-- types
data BooleanFormula a = Var a | And [BooleanFormula a] | Or [BooleanFormula a]
                      | Parens (BooleanFormula a)

                      deriving (Eq, Functor, Foldable, Traversable)
-- smart constructors
-- see note [Simplification of BooleanFormulas]
mkVar :: a -> BooleanFormula a
mkVar = Var

mkFalse, mkTrue :: BooleanFormula a
mkFalse = Or []
mkTrue = And []

-- Convert a Bool to a BooleanFormula
mkBool :: Bool -> BooleanFormula a
mkBool False = mkFalse
mkBool True  = mkTrue

-- Make a conjunction, and try to simplify
mkAnd :: Eq a => [BooleanFormula a] -> BooleanFormula a
mkAnd = maybe mkFalse (mkAnd' . nub . concat) . mapM fromAnd
  where
  -- See Note [Simplification of BooleanFormulas]
  fromAnd :: BooleanFormula a -> Maybe [BooleanFormula a]
  fromAnd bf = case bf of
    (And xs) -> Just xs
     -- assume that xs are already simplified
     -- otherwise we would need: fromAnd (And xs) = concat <$> traverse fromAnd xs
    (Or [])  -> Nothing
     -- in case of False we bail out, And [..,mkFalse,..] == mkFalse
    _        -> Just [bf]
  mkAnd' [x] = x
  mkAnd' xs = And xs

mkOr :: Eq a => [BooleanFormula a] -> BooleanFormula a
mkOr = maybe mkTrue (mkOr' . nub . concat) . mapM fromOr
  where
  -- See Note [Simplification of BooleanFormulas]
  fromOr bf = case  bf of
    (Or xs)  -> Just xs
    (And []) -> Nothing
    _        -> Just [bf]
  mkOr' [x] = x
  mkOr' xs  = Or xs
