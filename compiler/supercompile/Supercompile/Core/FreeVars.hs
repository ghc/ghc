{-# LANGUAGE Rank2Types, FlexibleInstances #-}
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
import Coercion (tyCoVarsOfCo)
import VarSet
import Type (tyVarsOfType)


type FreeVars = VarSet
type BoundVars = VarSet


(termVarFreeVars',            termFreeVars,                termFreeVars',                altsFreeVars,                valueFreeVars,                valueFreeVars')                = mkFreeVars (\f (I e) -> f e)
(fvedTermVarFreeVars',        fvedTermFreeVars,            fvedTermFreeVars',            fvedAltsFreeVars,            fvedValueFreeVars,            fvedValueFreeVars')            = mkFreeVars (\_ (FVed fvs _) -> fvs)
(sizedFVedVarFreeVars',       sizedFVedTermFreeVars,       sizedFVedTermFreeVars',       sizedFVedAltsFreeVars,       sizedFVedValueFreeVars,       sizedFVedValueFreeVars')       = mkFreeVars (\_ (Comp (Sized _ (FVed fvs _))) -> fvs)
(taggedTermVarFreeVars',      taggedTermFreeVars,          taggedTermFreeVars',          taggedAltsFreeVars,          taggedValueFreeVars,          taggedValueFreeVars')          = mkFreeVars (\f (Tagged _ e) -> f e)
(taggedSizedFVedVarFreeVars', taggedSizedFVedTermFreeVars, taggedSizedFVedTermFreeVars', taggedSizedFVedAltsFreeVars, taggedSizedFVedValueFreeVars, taggedSizedFVedValueFreeVars') = mkFreeVars (\_ (Comp (Tagged _ (Comp (Sized _ (FVed fvs _))))) -> fvs)

{-# INLINE mkFreeVars #-}
mkFreeVars :: (forall a. (a -> FreeVars) -> ann a -> FreeVars)
           -> (Var              -> FreeVars,
               ann (TermF ann)  -> FreeVars,
               TermF ann        -> FreeVars,
               [AltF ann]       -> FreeVars,
               ann (ValueF ann) -> FreeVars,
               ValueF ann       -> FreeVars)
mkFreeVars rec = (var', term, term', alternatives, value, value')
  where
    var' = idFreeVars
    
    term = rec term'
    term' (Var x)            = var' x
    term' (Value v)          = value' v
    term' (TyApp e ty)       = typ ty `unionVarSet` term e
    term' (App e x)          = idFreeVars x `unionVarSet` term e
    term' (PrimOp _ es)      = unionVarSets $ map term es
    term' (Case e x ty alts) = typ ty `unionVarSet` term e `unionVarSet` (alternatives alts `delVarSet` x)
    term' (LetRec xes e)     = (unionVarSets (map term es) `unionVarSet` term e) `delVarSetList` xs
      where (xs, es) = unzip xes
    term' (Cast e co)        = term e `unionVarSet` tyCoVarsOfCo co
    
    value = rec value'
    value' (Indirect x)    = idFreeVars x
    value' (TyLambda x e)  = term e `delVarSet` x
    value' (Lambda x e)    = term e `delVarSet` x
    value' (Data _ tys xs) = unionVarSets $ map typ tys ++ map idFreeVars xs
    value' (Literal _)     = emptyVarSet
    
    alternatives = unionVarSets . map alternative
    
    alternative (altcon, e) = altConFreeVars altcon $ term e
    
    typ = tyVarsOfType

altConOpenFreeVars :: AltCon -> (BoundVars, FreeVars) -> (BoundVars, FreeVars)
altConOpenFreeVars (DataAlt _ xs) (bvs, fvs) = (bvs `extendVarSetList` xs, fvs)
altConOpenFreeVars (LiteralAlt _) (bvs, fvs) = (bvs, fvs)
altConOpenFreeVars _              (bvs, fvs) = (bvs, fvs)

altConFreeVars :: AltCon -> FreeVars -> FreeVars
altConFreeVars (DataAlt _ xs) = (`delVarSetList` xs)
altConFreeVars (LiteralAlt _) = id
altConFreeVars DefaultAlt     = id


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


type FVedTerm = FVed (TermF FVed)
type FVedAlt = AltF FVed
type FVedValue = ValueF FVed


instance Symantics FVed where
    var = fvedTerm . Var
    value = fmap Value . fvedValue
    tyApp e = fvedTerm . TyApp e
    app e = fvedTerm . App e
    primOp pop = fvedTerm . PrimOp pop
    case_ e x ty = fvedTerm . Case e x ty
    letRec xes = fvedTerm . LetRec xes
    cast e = fvedTerm . Cast e

fvedVar :: Var -> FVed Var
fvedVar x = FVed (taggedTermVarFreeVars' x) x

fvedValue :: ValueF FVed -> FVed FVedValue
fvedValue v = FVed (fvedValueFreeVars' v) v

fvedTerm :: TermF FVed -> FVedTerm
fvedTerm e = FVed (fvedTermFreeVars' e) e
