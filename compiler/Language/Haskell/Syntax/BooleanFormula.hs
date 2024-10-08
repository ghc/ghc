{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Language.Haskell.Syntax.BooleanFormula(
  BooleanFormula(..), LBooleanFormula,
  mkVar, mkFalse, mkTrue, mkBool, mkAnd, mkOr,
  bfExplMap, bfExplTraverse) where

import Prelude hiding ( init, last )
import Data.List ( nub )
import Language.Haskell.Syntax.Extension (XRec, UnXRec (..), LIdP)


-- types
type LBooleanFormula p = XRec p (BooleanFormula p)
data BooleanFormula p = Var (LIdP p) | And [LBooleanFormula p] | Or [LBooleanFormula p]
                      | Parens (LBooleanFormula p)

-- instances
deriving instance (Eq (LIdP p), Eq (LBooleanFormula p)) => Eq (BooleanFormula p)

-- jury rigged map and traverse functions.
-- if we had Functor/Traversable (LbooleanFormula p) we could use as a constraint
-- we wouldn't neeed the first higher order argument, but because LBooleanformula
-- is a type synonym that's no can do.
bfExplMap :: ((BooleanFormula p -> BooleanFormula p') -> LBooleanFormula p -> LBooleanFormula p')
          -> (LIdP p -> LIdP p')
          -> BooleanFormula p -> BooleanFormula p'
bfExplMap lbfMap f = go
  where
    go (Var    a  ) = Var     $ f a
    go (And    bfs) = And     $ map (lbfMap go) bfs
    go (Or     bfs) = Or      $ map (lbfMap go) bfs
    go (Parens bf ) = Parens  $ lbfMap go bf

bfExplTraverse  :: Applicative f
                => ((BooleanFormula p -> f (BooleanFormula p')) -> LBooleanFormula p -> f (LBooleanFormula p'))
                -> (LIdP p -> f (LIdP p'))
                -> BooleanFormula p -> f (BooleanFormula p')
bfExplTraverse lbfTraverse f  = go
  where
    go (Var    a  ) = Var    <$> f a
    go (And    bfs) = And    <$> traverse @[] (lbfTraverse go) bfs
    go (Or     bfs) = Or     <$> traverse @[] (lbfTraverse go) bfs
    go (Parens bf ) = Parens <$> lbfTraverse go bf

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
mkAnd :: forall p. (UnXRec p, Eq (LIdP p), Eq (LBooleanFormula p)) => [LBooleanFormula p] -> BooleanFormula p
mkAnd = maybe mkFalse (mkAnd' . nub . concat) . mapM fromAnd
  where
  -- See Note [Simplification of BooleanFormulas]
  fromAnd :: LBooleanFormula p -> Maybe [LBooleanFormula p]
  fromAnd bf = case unXRec @p bf of
    (And xs) -> Just xs
     -- assume that xs are already simplified
     -- otherwise we would need: fromAnd (And xs) = concat <$> traverse fromAnd xs
    (Or [])  -> Nothing
     -- in case of False we bail out, And [..,mkFalse,..] == mkFalse
    _        -> Just [bf]
  mkAnd' [x] = unXRec @p x
  mkAnd' xs = And xs

mkOr :: forall p. (UnXRec p, Eq (LIdP p), Eq (LBooleanFormula p)) => [LBooleanFormula p] -> BooleanFormula p
mkOr = maybe mkTrue (mkOr' . nub . concat) . mapM fromOr
  where
  -- See Note [Simplification of BooleanFormulas]
  fromOr bf = case unXRec @p bf of
    (Or xs)  -> Just xs
    (And []) -> Nothing
    _        -> Just [bf]
  mkOr' [x] = unXRec @p x
  mkOr' xs = Or xs
