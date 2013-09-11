--------------------------------------------------------------------------------
-- | Boolean formulas without negation (qunatifier free)

{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable,
             DeriveTraversable #-}

module BooleanFormula (
        BooleanFormula(..),
        mkFalse, mkTrue, mkAnd, mkOr, mkVar,
        isFalse, isTrue,
        eval, simplify, isUnsatisfied,
        implies, impliesAtom,
        pprBooleanFormula, pprBooleanFormulaNice
  ) where

import Data.List ( nub, intersperse )
import Data.Data
import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )

import MonadUtils
import Outputable
import Binary

----------------------------------------------------------------------
-- Boolean formula type and smart constructors
----------------------------------------------------------------------

data BooleanFormula a = Var a | And [BooleanFormula a] | Or [BooleanFormula a]
  deriving (Eq, Data, Typeable, Functor, Foldable, Traversable)

mkVar :: a -> BooleanFormula a
mkVar = Var

mkFalse, mkTrue :: BooleanFormula a
mkFalse = Or []
mkTrue = And []

mkBool :: Bool -> BooleanFormula a
mkBool False = mkFalse
mkBool True  = mkTrue

mkAnd :: Eq a => [BooleanFormula a] -> BooleanFormula a
mkAnd = maybe mkFalse (mkAnd' . nub) . concatMapM fromAnd
  where
  fromAnd :: BooleanFormula a -> Maybe [BooleanFormula a]
  fromAnd (And xs) = Just xs
     -- assume that xs are already simplified
     -- otherwise we would need: fromAnd (And xs) = concat <$> traverse fromAnd xs
  fromAnd (Or []) = Nothing -- in case of False we bail out, And [..,mkFalse,..] == mkFalse
  fromAnd x = Just [x]
  mkAnd' [x] = x
  mkAnd' xs = And xs

mkOr :: Eq a => [BooleanFormula a] -> BooleanFormula a
mkOr = maybe mkTrue (mkOr' . nub) . concatMapM fromOr
  where
  fromOr (Or xs) = Just xs
  fromOr (And []) = Nothing
  fromOr x = Just [x]
  mkOr' [x] = x
  mkOr' xs = Or xs

----------------------------------------------------------------------
-- Evaluation and simplificiation
----------------------------------------------------------------------

isFalse :: BooleanFormula a -> Bool
isFalse (Or []) = True
isFalse _ = False

isTrue :: BooleanFormula a -> Bool
isTrue (And []) = True
isTrue _ = False

eval :: (a -> Bool) -> BooleanFormula a -> Bool
eval f (Var x)  = f x
eval f (And xs) = all (eval f) xs
eval f (Or xs)  = any (eval f) xs

-- Simplify a boolean formula.
-- The argument function should give the truth of the atoms, or Nothing if undecided.
simplify :: Eq a => (a -> Maybe Bool) -> BooleanFormula a -> BooleanFormula a
simplify f (Var a) = case f a of
  Nothing -> Var a
  Just b  -> mkBool b
simplify f (And xs) = mkAnd (map (simplify f) xs)
simplify f (Or xs) = mkOr (map (simplify f) xs)

-- Test if a boolean formula is satisfied when the given values are assigned to the atoms
-- if it is, returns Nothing
-- if it is not, return (Just remainder)
isUnsatisfied :: Eq a => (a -> Bool) -> BooleanFormula a -> Maybe (BooleanFormula a)
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
impliesAtom :: Eq a => BooleanFormula a -> a -> Bool
Var x  `impliesAtom` y = x == y
And xs `impliesAtom` y = any (`impliesAtom` y) xs -- we have all of xs, so one of them implying y is enough
Or  xs `impliesAtom` y = all (`impliesAtom` y) xs

implies :: Eq a => BooleanFormula a -> BooleanFormula a -> Bool
x `implies` Var y  = x `impliesAtom` y
x `implies` And ys = all (x `implies`) ys
x `implies` Or ys  = any (x `implies`) ys

----------------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------------

pprBooleanFormula' :: (Rational -> a -> SDoc)
                   -> (Rational -> [SDoc] -> SDoc)
                   -> (Rational -> [SDoc] -> SDoc)
                   -> Rational -> BooleanFormula a -> SDoc
pprBooleanFormula' pprVar pprAnd pprOr = go
  where
  go p (Var x)  = pprVar p x
  go p (And []) = cparen (p > 0) $ empty
  go p (And xs) = pprAnd p (map (go 3) xs)
  go _ (Or  []) = keyword $ text "FALSE"
  go p (Or  xs) = pprOr p (map (go 2) xs)

-- Pretty print in source syntax, "a | b | c,d,e"
pprBooleanFormula :: (Rational -> a -> SDoc) -> Rational -> BooleanFormula a -> SDoc
pprBooleanFormula pprVar = pprBooleanFormula' pprVar pprAnd pprOr
  where
  pprAnd p = cparen (p > 3) . fsep . punctuate comma
  pprOr  p = cparen (p > 2) . fsep . intersperse (text "|")

-- Pretty print human in readable format, "either `a' or `b' or (`c', `d' and `e')"?
pprBooleanFormulaNice :: Outputable a => BooleanFormula a -> SDoc
pprBooleanFormulaNice = pprBooleanFormula' pprVar pprAnd pprOr 0
  where
  pprVar _ = quotes . ppr
  pprAnd p = cparen (p > 1) . pprAnd'
  pprAnd' [] = empty
  pprAnd' [x,y] = x <+> text "and" <+> y
  pprAnd' xs@(_:_) = fsep (punctuate comma (init xs)) <> text ", and" <+> last xs
  pprOr p xs = cparen (p > 1) $ text "either" <+> sep (intersperse (text "or") xs)

instance Outputable a => Outputable (BooleanFormula a) where
  pprPrec = pprBooleanFormula pprPrec

----------------------------------------------------------------------
-- Binary
----------------------------------------------------------------------

instance Binary a => Binary (BooleanFormula a) where
  put_ bh (Var x)  = putByte bh 0 >> put_ bh x
  put_ bh (And xs) = putByte bh 1 >> put_ bh xs
  put_ bh (Or  xs) = putByte bh 2 >> put_ bh xs

  get bh = do
    h <- getByte bh
    case h of
      0 -> Var <$> get bh
      1 -> And <$> get bh
      _ -> Or  <$> get bh
