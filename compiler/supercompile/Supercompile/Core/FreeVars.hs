{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Supercompile.Core.FreeVars (
    module Supercompile.Core.FreeVars,
    module VarSet,
    tyVarsOfType, tyCoVarsOfCo
  ) where

import Supercompile.Core.Syntax

import Supercompile.Utilities

import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable

import CoreFVs
import VarSet
import Coercion (tyCoVarsOfCo)
import Var      (isTyVar)
import Type     (tyVarsOfType)


type FreeVars = VarSet
type BoundVars = VarSet


(varFreeVars',                termFreeVars,                termFreeVars',                altsFreeVars,                valueFreeVars,                valueFreeVars')                = mkFreeVars (\f (I e) -> f e)
(fvedVarFreeVars',            fvedTermFreeVars,            fvedTermFreeVars',            fvedAltsFreeVars,            fvedValueFreeVars,            fvedValueFreeVars')            = mkFreeVars (\_ (FVed fvs _) -> fvs)
(sizedFVedVarFreeVars',       sizedFVedTermFreeVars,       sizedFVedTermFreeVars',       sizedFVedAltsFreeVars,       sizedFVedValueFreeVars,       sizedFVedValueFreeVars')       = mkFreeVars (\_ (Comp (Sized _ (FVed fvs _))) -> fvs)
(taggedVarFreeVars',          taggedTermFreeVars,          taggedTermFreeVars',          taggedAltsFreeVars,          taggedValueFreeVars,          taggedValueFreeVars')          = mkFreeVars (\f (Tagged _ e) -> f e)
(taggedSizedFVedVarFreeVars', taggedSizedFVedTermFreeVars, taggedSizedFVedTermFreeVars', taggedSizedFVedAltsFreeVars, taggedSizedFVedValueFreeVars, taggedSizedFVedValueFreeVars') = mkFreeVars (\_ (Comp (Tagged _ (Comp (Sized _ (FVed fvs _))))) -> fvs)

{-# INLINE mkFreeVars #-}
mkFreeVars :: (forall a. (a -> FreeVars) -> ann a -> FreeVars)
           -> (Var              -> FreeVars,
               ann (TermF ann)  -> FreeVars,
               TermF ann        -> FreeVars,
               [AltF ann]       -> FreeVars,
               ann (ValueF ann) -> FreeVars,
               ValueF ann       -> FreeVars)
mkFreeVars rec = (unitVarSet, term, term', alternatives, value, value')
  where
    term = rec term'
    term' (Var x)            = unitVarSet x
    term' (Value v)          = value' v
    term' (TyApp e ty)       = typ ty `unionVarSet` term e
    term' (App e x)          = term e `extendVarSet` x
    term' (PrimOp _ tys es)  = unionVarSets (map typ tys) `unionVarSet` unionVarSets (map term es)
    term' (Case e x ty alts) = typ ty `unionVarSet` term e `unionVarSet` nonRecBinderFreeVars x (alternatives alts)
    term' (Let x e1 e2)      = term e1 `unionVarSet` nonRecBinderFreeVars x (term e2)
    term' (LetRec xes e)     = (unionVarSets (map term es) `unionVarSet` term e `unionVarSet` unionVarSets (map idFreeVars xs)) `delVarSetList` xs
      where (xs, es) = unzip xes
    term' (Cast e co)        = term e `unionVarSet` tyCoVarsOfCo co
    
    value = rec value'
    value' (Indirect x)    = unitVarSet x
    value' (TyLambda x e)  = term e `delVarSet` x
    value' (Lambda x e)    = nonRecBinderFreeVars x (term e)
    value' (Data _ tys xs) = unionVarSets (map typ tys) `unionVarSet` mkVarSet xs
    value' (Literal _)     = emptyVarSet
    value' (Coercion co)   = tyCoVarsOfCo co
    
    alternatives = unionVarSets . map alternative
    
    alternative (altcon, e) = altConFreeVars altcon $ term e
    
    typ = tyVarsOfType

nonRecBinderFreeVars :: Var -> FreeVars -> FreeVars
nonRecBinderFreeVars x fvs | isTyVar x = fvs `delVarSet` x
                           | otherwise = (fvs `delVarSet` x) `unionVarSet` idFreeVars x

nonRecBindersFreeVars :: [Var] -> FreeVars -> FreeVars
nonRecBindersFreeVars xs = flip (foldr nonRecBinderFreeVars) xs

altConBoundVars :: AltCon -> [Var]
altConBoundVars (DataAlt _ as xs) = as ++ xs
altConBoundVars (LiteralAlt _)    = []
altConBoundVars _                 = []

altConFreeVars :: AltCon -> FreeVars -> FreeVars
altConFreeVars (DataAlt _ as xs) = (`delVarSetList` as) . nonRecBindersFreeVars xs
altConFreeVars (LiteralAlt _)    = id
altConFreeVars DefaultAlt        = id


coercedFreeVars :: (a -> FreeVars) -> Coerced a -> FreeVars
coercedFreeVars f (Nothing,      x) = f x
coercedFreeVars f (Just (co, _), x) = f x `unionVarSet` tyCoVarsOfCo co


data FVed a = FVed { freeVars :: !FreeVars, fvee :: !a }

instance Copointed FVed where
    extract = fvee

instance Functor FVed where
    fmap f (FVed fvs x) = FVed fvs (f x)

instance Foldable.Foldable FVed where
    foldMap f (FVed _ x) = f x

instance Traversable.Traversable FVed where
    traverse f (FVed fvs x) = pure (FVed fvs) <*> f x

instance Show1 FVed where
    showsPrec1 prec (FVed fvs x) = showParen (prec >= appPrec) (showString "FVed" . showsPrec appPrec (varSetElems fvs) . showsPrec appPrec x)

instance Eq1 FVed where
    eq1 (FVed fvs1 x1) (FVed fvs2 x2) = varSetElems fvs1 == varSetElems fvs2 && x1 == x2

instance Ord1 FVed where
    compare1 (FVed fvs1 x1) (FVed fvs2 x2) = (x1, varSetElems fvs1) `compare` (x2, varSetElems fvs2)

instance Outputable1 FVed where
    pprPrec1 prec (FVed _ x) = pprPrec prec x

instance Show a => Show (FVed a) where
    showsPrec = showsPrec1

instance Eq a => Eq (FVed a) where
    (==) = eq1

instance Ord a => Ord (FVed a) where
    compare = compare1

instance Outputable a => Outputable (FVed a) where
    pprPrec = pprPrec1


type FVedTerm = FVed (TermF FVed)
type FVedAlt = AltF FVed
type FVedValue = ValueF FVed


instance Symantics FVed where
    var = fvedTerm . Var
    value = fmap Value . fvedValue
    tyApp e = fvedTerm . TyApp e
    app e = fvedTerm . App e
    primOp pop tys = fvedTerm . PrimOp pop tys
    case_ e x ty = fvedTerm . Case e x ty
    let_ x e1 = fvedTerm . Let x e1
    letRec xes = fvedTerm . LetRec xes
    cast e = fvedTerm . Cast e

fvedVar :: Var -> FVed Var
fvedVar x = FVed (taggedVarFreeVars' x) x

fvedValue :: ValueF FVed -> FVed FVedValue
fvedValue v = FVed (fvedValueFreeVars' v) v

fvedTerm :: TermF FVed -> FVedTerm
fvedTerm e = FVed (fvedTermFreeVars' e) e
