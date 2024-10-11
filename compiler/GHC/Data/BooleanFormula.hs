{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------
-- | Boolean formulas without quantifiers and without negation.
-- Such a formula consists of variables, conjunctions (and), and disjunctions (or).
--
-- This module is used to represent minimal complete definitions for classes.
--
module GHC.Data.BooleanFormula (
        module Language.Haskell.Syntax.BooleanFormula,
        isFalse, isTrue,
        eval, simplify, isUnsatisfied,
        implies, impliesAtom,
        pprBooleanFormula, pprBooleanFormulaNice, pprBooleanFormulaNormal
  ) where

import Data.List ( intersperse )
import Data.List.NonEmpty ( NonEmpty (..), init, last )

import GHC.Prelude hiding ( init, last )
import GHC.Types.Unique
import GHC.Types.Unique.Set
import GHC.Utils.Outputable
import Language.Haskell.Syntax.BooleanFormula


{-
Note [Simplification of BooleanFormulas]
~~~~~~~~~~~~~~~~~~~~~~
The smart constructors (`mkAnd` and `mkOr`) do some attempt to simplify expressions. In particular,
 1. Collapsing nested ands and ors, so
     `(mkAnd [x, And [y,z]]`
    is represented as
     `And [x,y,z]`
    Implemented by `fromAnd`/`fromOr`
 2. Collapsing trivial ands and ors, so
     `mkAnd [x]` becomes just `x`.
    Implemented by mkAnd' / mkOr'
 3. Conjunction with false, disjunction with true is simplified, i.e.
     `mkAnd [mkFalse,x]` becomes `mkFalse`.
 4. Common subexpression elimination:
     `mkAnd [x,x,y]` is reduced to just `mkAnd [x,y]`.

This simplification is not exhaustive, in the sense that it will not produce
the smallest possible equivalent expression. For example,
`Or [And [x,y], And [x]]` could be simplified to `And [x]`, but it currently
is not. A general simplifier would need to use something like BDDs.

The reason behind the (crude) simplifier is to make for more user friendly
error messages. E.g. for the code
  > class Foo a where
  >     {-# MINIMAL bar, (foo, baq | foo, quux) #-}
  > instance Foo Int where
  >     bar = ...
  >     baz = ...
  >     quux = ...
We don't show a ridiculous error message like
    Implement () and (either (`foo' and ()) or (`foo' and ()))
-}

----------------------------------------------------------------------
-- Evaluation and simplification
----------------------------------------------------------------------

isFalse :: BooleanFormula a -> Bool
isFalse (Or []) = True
isFalse _ = False

isTrue :: BooleanFormula a -> Bool
isTrue (And []) = True
isTrue _ = False

eval :: (a -> Bool) -> BooleanFormula a -> Bool
eval f (Var x)    = f x
eval f (And xs)   = all (eval f) xs
eval f (Or xs)    = any (eval f) xs
eval f (Parens x) = eval f x

-- Simplify a boolean formula.
-- The argument function should give the truth of the atoms, or Nothing if undecided.
simplify  :: Eq a
          => (a ->  Maybe Bool)
          -> BooleanFormula a
          -> BooleanFormula a
simplify f (Var a) = case f a of
  Nothing -> Var a
  Just b  -> mkBool b
simplify f (And xs)   = mkAnd (map (simplify f) xs)
simplify f (Or xs)    = mkOr  (map (simplify f) xs)
simplify f (Parens x) = simplify f x

-- Test if a boolean formula is satisfied when the given values are assigned to the atoms
-- if it is, returns Nothing
-- if it is not, return (Just remainder)
isUnsatisfied :: Eq a
              => (a -> Bool)
              -> BooleanFormula a
              -> Maybe (BooleanFormula a)
isUnsatisfied f bf
    | isTrue bf' = Nothing
    | otherwise  = Just bf'
  where
  f' x = if f x then Just True else Nothing
  bf' = simplify f' bf

-- prop_simplify:
--   eval f x == True   <==>  isTrue  (simplify (Just . f) x)
--   eval f x == False  <==>  isFalse (simplify (Just . f) x)

-- If the boolean formula holds, does that mean that the given atom is always true?
impliesAtom :: Eq a => BooleanFormula a -> a-> Bool
Var x  `impliesAtom` y = x == y
And xs `impliesAtom` y = any (`impliesAtom` y) xs
-- we have all of xs, so one of them implying y is enough
Or  xs `impliesAtom` y = all (`impliesAtom` y) xs
Parens x `impliesAtom` y =  x `impliesAtom` y

implies :: Uniquable a => BooleanFormula a -> BooleanFormula a -> Bool
implies e1 e2 = go (Clause emptyUniqSet [e1]) (Clause emptyUniqSet [e2])
  where
    go :: Uniquable a => Clause a -> Clause a -> Bool
    go l@Clause{ clauseExprs = hyp:hyps } r =
        case hyp of
            Var x | memberClauseAtoms x r -> True
                  | otherwise -> go (extendClauseAtoms l x) { clauseExprs = hyps } r
            Parens hyp' -> go l { clauseExprs = hyp':hyps }     r
            And hyps'  -> go l { clauseExprs =  hyps' ++ hyps } r
            Or hyps'   -> all (\hyp' -> go l { clauseExprs = hyp':hyps } r) hyps'
    go l r@Clause{ clauseExprs = con:cons } =
        case con of
            Var x | memberClauseAtoms x l -> True
                  | otherwise -> go l (extendClauseAtoms r x) { clauseExprs = cons }
            Parens con' -> go l r { clauseExprs = con':cons }
            And cons'   -> all (\con' -> go l r { clauseExprs = con':cons }) cons'
            Or cons'    -> go l r { clauseExprs = cons' ++ cons }
    go _ _ = False

-- A small sequent calculus proof engine.
data Clause a = Clause {
        clauseAtoms :: UniqSet a,
        clauseExprs :: [BooleanFormula a]
    }
extendClauseAtoms :: Uniquable a => Clause a -> a -> Clause a
extendClauseAtoms c x = c { clauseAtoms = addOneToUniqSet (clauseAtoms c) x }

memberClauseAtoms :: Uniquable a => a -> Clause a -> Bool
memberClauseAtoms x c = x `elementOfUniqSet` clauseAtoms c

----------------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------------

-- Pretty print a BooleanFormula,
-- using the arguments as pretty printers for Var, And and Or respectively
pprBooleanFormula'  :: (Rational -> a -> SDoc)
                    -> (Rational -> [SDoc] -> SDoc)
                    -> (Rational -> [SDoc] -> SDoc)
                    -> Rational -> BooleanFormula a -> SDoc
pprBooleanFormula' pprVar pprAnd pprOr = go
  where
  go p (Var x)  = pprVar p x
  go p (And []) = cparen (p > 0) empty
  go p (And xs) = pprAnd p (map (go 3) xs)
  go _ (Or  []) = keyword $ text "FALSE"
  go p (Or  xs) = pprOr p (map (go 2) xs)
  go p (Parens x) = go p x

-- Pretty print in source syntax, "a | b | c,d,e"
pprBooleanFormula :: (Rational -> a -> SDoc)
                  -> Rational -> BooleanFormula a -> SDoc
pprBooleanFormula pprVar = pprBooleanFormula' pprVar pprAnd pprOr
  where
  pprAnd p = cparen (p > 3) . fsep . punctuate comma
  pprOr  p = cparen (p > 2) . fsep . intersperse vbar

-- Pretty print human in readable format, "either `a' or `b' or (`c', `d' and `e')"?
pprBooleanFormulaNice :: Outputable a => BooleanFormula a -> SDoc
pprBooleanFormulaNice = pprBooleanFormula' pprVar pprAnd pprOr 0
  where
  pprVar _ = quotes . ppr
  pprAnd p = cparen (p > 1) . pprAnd'
  pprAnd' [] = empty
  pprAnd' [x,y] = x <+> text "and" <+> y
  pprAnd' (x:xs) = fsep (punctuate comma (init (x:|xs))) <> text ", and" <+> last (x:|xs)
  pprOr p xs = cparen (p > 1) $ text "either" <+> sep (intersperse (text "or") xs)

instance OutputableBndr a => Outputable (BooleanFormula a) where
  ppr = pprBooleanFormulaNormal

pprBooleanFormulaNormal :: OutputableBndr a => BooleanFormula a -> SDoc
pprBooleanFormulaNormal = go
  where
    go (Var x)    = pprPrefixOcc x
    go (And xs)   = fsep $ punctuate comma (map go xs)
    go (Or [])    = keyword $ text "FALSE"
    go (Or xs)    = fsep $ intersperse vbar (map go xs)
    go (Parens x) = parens (go x)
