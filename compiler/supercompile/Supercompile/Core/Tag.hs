{-# LANGUAGE RankNTypes #-}
module Supercompile.Core.Tag where

import Supercompile.Utilities

import Supercompile.Core.FreeVars
import Supercompile.Core.Size
import Supercompile.Core.Syntax


tagTerm :: UniqSupply -> Term -> TaggedTerm
tagTerm = mkTagger (\i f (I e) -> Tagged (mkTag (getKey i)) (f e))

tagFVedTerm :: UniqSupply -> SizedFVedTerm -> TaggedSizedFVedTerm
tagFVedTerm = mkTagger (\i f e -> Comp (Tagged (mkTag (getKey i)) (fmap f e)))


{-# INLINE mkTagger #-}
mkTagger :: (forall a b. Unique -> (a -> b) -> ann a -> ann' b)
         -> UniqSupply -> ann (TermF ann) -> ann' (TermF ann')
mkTagger rec = term
  where
    term ids = rec i (term' ids')
      where (i, ids') = takeUniqFromSupply ids
    term' ids e = case e of
        Var x            -> Var x
        Value v          -> Value (value' ids v)
        TyApp e ty       -> TyApp (term ids e) ty
        App e x          -> App (term ids e) x
        PrimOp pop es    -> PrimOp pop (zipWith term idss' es)
          where idss' = listSplitUniqSupply ids
        Case e x ty alts -> Case (term ids0' e) x ty (alternatives ids1' alts)
          where (ids0', ids1') = splitUniqSupply ids
        LetRec xes e     -> LetRec (zipWith (\ids'' (x, e) -> (x, term ids'' e)) idss' xes) (term ids1' e)
          where (ids0', ids1') = splitUniqSupply ids
                idss' = listSplitUniqSupply ids0'
        Cast e co        -> Cast (term ids e) co

    value' ids (co, rv) = (co, rvalue' ids rv)

    rvalue' ids rv = case rv of
        Indirect x   -> Indirect x
        TyLambda x e -> TyLambda x (term ids e)
        Lambda x e   -> Lambda x (term ids e)
        Data dc xs   -> Data dc xs
        Literal l    -> Literal l

    alternatives = zipWith alternative . listSplitUniqSupply
    
    alternative ids (con, e) = (con, term ids e)


(taggedTermToTerm,              taggedAltsToAlts,              taggedValueToValue,              taggedValue'ToValue')              = mkDetag (\f e -> I (f (tagee e)))
(fVedTermToTerm,                fVedAltsToAlts,                fVedValueToValue,                fVedValue'ToValue')                = mkDetag (\f e -> I (f (fvee e)))
(taggedSizedFVedTermToTerm,     taggedSizedFVedAltsToAlts,     taggedSizedFVedValueToValue,     taggedSizedFVedValue'ToValue')     = mkDetag (\f e -> I (f (fvee (sizee (unComp (tagee (unComp e)))))))
(taggedSizedFVedTermToFVedTerm, taggedSizedFVedAltsToFVedAlts, taggedSizedFVedValueToFVedValue, taggedSizedFVedValue'ToFVedValue') = mkDetag (\f e -> FVed (freeVars (sizee (unComp (tagee (unComp e))))) (f (extract e)))


{-# INLINE mkDetag #-}
mkDetag :: (forall a b. (a -> b) -> ann a -> ann' b)
        -> (ann (TermF ann)  -> ann' (TermF ann'),
            [AltF ann]       -> [AltF ann'],
            ann (ValueF ann) -> ann' (ValueF ann'),
            ValueF ann       -> ValueF ann')
mkDetag rec = (term, alternatives, value, value')
  where
    term = rec term'
    term' e = case e of
        Var x            -> Var x
        Value v          -> Value (value' v)
        TyApp e ty       -> TyApp (term e) ty
        App e x          -> App (term e) x
        PrimOp pop es    -> PrimOp pop (map term es)
        Case e x ty alts -> Case (term e) x ty (alternatives alts)
        LetRec xes e     -> LetRec (map (second term) xes) (term e)
        Cast e co        -> Cast (term e) co

    value = rec value'
    value' (mb_co, rv) = (mb_co, rvalue' rv)
    
    rvalue' (Indirect x)   = Indirect x
    rvalue' (TyLambda x e) = TyLambda x (term e)
    rvalue' (Lambda x e)   = Lambda x (term e)
    rvalue' (Data dc xs)   = Data dc xs
    rvalue' (Literal l)    = Literal l

    alternatives = map (second term)
