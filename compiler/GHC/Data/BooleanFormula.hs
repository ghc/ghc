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
        bfMap, bfTraverse,
        eval, simplify, isUnsatisfied,
        implies, impliesAtom,
        pprBooleanFormula, pprBooleanFormulaNice, pprBooleanFormulaNormal
  ) where

import Data.List ( intersperse )
import Data.List.NonEmpty ( NonEmpty (..), init, last )

import GHC.Prelude hiding ( init, last )
import GHC.Types.Unique
import GHC.Types.Unique.Set
import GHC.Types.SrcLoc (unLoc)
import GHC.Utils.Outputable
import GHC.Parser.Annotation ( SrcSpanAnnBF )
import GHC.Hs.Extension (GhcPass (..), OutputableBndrId)
import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.BooleanFormula


----------------------------------------------------------------------
-- Boolean formula type and smart constructors
----------------------------------------------------------------------

type instance Anno (BooleanFormula (GhcPass p)) = SrcSpanAnnBF

type instance XBFVar           (GhcPass _) = NoExtField
type instance XBFAnd           (GhcPass _) = NoExtField
type instance XBFOr            (GhcPass _) = NoExtField
type instance XBFParens        (GhcPass _) = NoExtField
type instance XXBooleanFormula (GhcPass _) = DataConCantHappen

instance BooleanFormulaDefault (GhcPass p) where
  bfAnnAnd = noExtField
  bfAnnOr = noExtField


-- if we had Functor/Traversable (LbooleanFormula p) we could use that
-- as a constraint and we wouldn't need to specialize to just GhcPass p,
-- but becuase LBooleanFormula is a type synonym such a constraint is
-- impossible.

-- BooleanFormula can't be an instance of functor because it can't lift
-- arbitrary functions `a -> b`, only functions of type `LIdP a -> LIdP b`
-- ditto for Traversable.
bfMap :: (LIdP (GhcPass p) -> LIdP (GhcPass p'))
      -> BooleanFormula (GhcPass p) -> BooleanFormula (GhcPass p')
bfMap f = go
  where
    go (Var    x a  ) = Var    x $ f a
    go (And    x bfs) = And    x $ map (fmap go) bfs
    go (Or     x bfs) = Or     x $ map (fmap go) bfs
    go (Parens x bf ) = Parens x $ fmap go bf

bfTraverse  :: Applicative f
            => (LIdP (GhcPass p) -> f (LIdP (GhcPass p')))
            -> BooleanFormula (GhcPass p)
            -> f (BooleanFormula (GhcPass p'))
bfTraverse f = go
  where
    go (Var    x a  ) = Var    x <$> f a
    go (And    x bfs) = And    x <$> traverse @[] (traverse go) bfs
    go (Or     x bfs) = Or     x <$> traverse @[] (traverse go) bfs
    go (Parens x bf ) = Parens x <$> traverse go bf



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

isFalse :: BooleanFormula (GhcPass p) -> Bool
isFalse (Or _ []) = True
isFalse _ = False

isTrue :: BooleanFormula (GhcPass p) -> Bool
isTrue (And _ []) = True
isTrue _ = False

eval :: (LIdP (GhcPass p) -> Bool) -> BooleanFormula (GhcPass p) -> Bool
eval f (Var _ x)    = f x
eval f (And _ xs)   = all (eval f . unLoc) xs
eval f (Or  _ xs)   = any (eval f . unLoc) xs
eval f (Parens _ x) = eval f (unLoc x)

-- Simplify a boolean formula.
-- The argument function should give the truth of the atoms, or Nothing if undecided.
simplify :: forall p. Eq (LIdP (GhcPass p))
          => (LIdP (GhcPass p) ->  Maybe Bool)
          -> BooleanFormula (GhcPass p)
          -> BooleanFormula (GhcPass p)
simplify f (Var x a) = case f a of
  Nothing -> Var x a
  Just b  -> mkBool b
simplify f (And _ xs) = mkAnd (map (fmap (simplify f)) xs)
simplify f (Or _ xs)  = mkOr  (map (fmap (simplify f)) xs)
simplify f (Parens _ x) = simplify f (unLoc x)

-- Test if a boolean formula is satisfied when the given values are assigned to the atoms
-- if it is, returns Nothing
-- if it is not, return (Just remainder)
isUnsatisfied :: Eq (LIdP (GhcPass p))
              => (LIdP (GhcPass p) -> Bool)
              -> BooleanFormula (GhcPass p)
              -> Maybe (BooleanFormula (GhcPass p))
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
impliesAtom :: Eq (IdP (GhcPass p)) => BooleanFormula (GhcPass p) -> LIdP (GhcPass p) -> Bool
Var _ x  `impliesAtom` y = (unLoc x) == (unLoc y)
And _ xs `impliesAtom` y = any (\x -> unLoc x `impliesAtom` y) xs
             -- we have all of xs, so one of them implying y is enough
Or _ xs `impliesAtom` y = all (\x -> unLoc x `impliesAtom` y) xs
Parens _ x `impliesAtom` y = unLoc x `impliesAtom` y

implies :: (Uniquable (IdP (GhcPass p))) => BooleanFormula (GhcPass p) -> BooleanFormula (GhcPass p) -> Bool
implies e1 e2 = go (Clause emptyUniqSet [e1]) (Clause emptyUniqSet [e2])
  where
    go :: Uniquable (IdP (GhcPass p)) => Clause (GhcPass p) -> Clause (GhcPass p) -> Bool
    go l@Clause{ clauseExprs = hyp:hyps } r =
        case hyp of
            Var _ x | memberClauseAtoms (unLoc x) r -> True
                    | otherwise -> go (extendClauseAtoms l (unLoc x)) { clauseExprs = hyps } r
            Parens _ hyp' -> go l { clauseExprs = unLoc hyp':hyps }     r
            And _ hyps'  -> go l { clauseExprs = map unLoc hyps' ++ hyps } r
            Or _ hyps'   -> all (\hyp' -> go l { clauseExprs = unLoc hyp':hyps } r) hyps'
    go l r@Clause{ clauseExprs = con:cons } =
        case con of
            Var _ x | memberClauseAtoms (unLoc x) l -> True
                    | otherwise -> go l (extendClauseAtoms r (unLoc x)) { clauseExprs = cons }
            Parens _ con' -> go l r { clauseExprs = unLoc con':cons }
            And _ cons'   -> all (\con' -> go l r { clauseExprs = unLoc con':cons }) cons'
            Or _ cons'    -> go l r { clauseExprs = map unLoc cons' ++ cons }
    go _ _ = False

-- A small sequent calculus proof engine.
data Clause p = Clause {
        clauseAtoms :: UniqSet (IdP p),
        clauseExprs :: [BooleanFormula p]
    }
extendClauseAtoms :: Uniquable (IdP p) => Clause p -> IdP p -> Clause p
extendClauseAtoms c x = c { clauseAtoms = addOneToUniqSet (clauseAtoms c) x }

memberClauseAtoms :: Uniquable (IdP p) => IdP p -> Clause p -> Bool
memberClauseAtoms x c = x `elementOfUniqSet` clauseAtoms c

----------------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------------

-- Pretty print a BooleanFormula,
-- using the arguments as pretty printers for Var, And and Or respectively
pprBooleanFormula'  :: (Rational -> LIdP (GhcPass p) -> SDoc)
                    -> (Rational -> [SDoc] -> SDoc)
                    -> (Rational -> [SDoc] -> SDoc)
                    -> Rational -> BooleanFormula (GhcPass p) -> SDoc
pprBooleanFormula' pprVar pprAnd pprOr = go
  where
  go p (Var _ x)  = pprVar p x
  go p (And _ []) = cparen (p > 0) empty
  go p (And _ xs) = pprAnd p (map (go 3 . unLoc) xs)
  go _ (Or  _ []) = keyword $ text "FALSE"
  go p (Or  _ xs) = pprOr p (map (go 2 . unLoc) xs)
  go p (Parens _ x) = go p (unLoc x)

-- Pretty print in source syntax, "a | b | c,d,e"
pprBooleanFormula :: (Rational -> LIdP (GhcPass p) -> SDoc)
                  -> Rational -> BooleanFormula (GhcPass p) -> SDoc
pprBooleanFormula pprVar = pprBooleanFormula' pprVar pprAnd pprOr
  where
  pprAnd p = cparen (p > 3) . fsep . punctuate comma
  pprOr  p = cparen (p > 2) . fsep . intersperse vbar

-- Pretty print human in readable format, "either `a' or `b' or (`c', `d' and `e')"?
pprBooleanFormulaNice :: Outputable (LIdP (GhcPass p)) => BooleanFormula (GhcPass p) -> SDoc
pprBooleanFormulaNice = pprBooleanFormula' pprVar pprAnd pprOr 0
  where
  pprVar _ = quotes . ppr
  pprAnd p = cparen (p > 1) . pprAnd'
  pprAnd' [] = empty
  pprAnd' [x,y] = x <+> text "and" <+> y
  pprAnd' (x:xs) = fsep (punctuate comma (init (x:|xs))) <> text ", and" <+> last (x:|xs)
  pprOr p xs = cparen (p > 1) $ text "either" <+> sep (intersperse (text "or") xs)

instance OutputableBndrId p => Outputable (BooleanFormula (GhcPass p)) where
  ppr = pprBooleanFormulaNormal

pprBooleanFormulaNormal :: OutputableBndrId p => BooleanFormula (GhcPass p) -> SDoc
pprBooleanFormulaNormal = go
  where
    go (Var _ x)    = pprPrefixOcc (unLoc x)
    go (And _ xs)   = fsep $ punctuate comma (map (go . unLoc) xs)
    go (Or _ [])    = keyword $ text "FALSE"
    go (Or _ xs)    = fsep $ intersperse vbar (map (go . unLoc) xs)
    go (Parens _ x) = parens (go $ unLoc x)
    go (XBooleanFormula _) = text "XBooleanFormula"
