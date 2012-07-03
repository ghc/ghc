{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Supercompile.Core.Tag where

import Supercompile.Utilities

import Supercompile.Core.FreeVars
import Supercompile.Core.Size
import Supercompile.Core.Syntax

import Literal (hashLiteral)
import Unique  (mkPrimOpIdUnique)
import qualified PrimOp as PrimOp (primOpTag)


tagTerm :: UniqSupply -> Term -> TaggedTerm
tagTerm = mkTagger (\tg (I e) -> Tagged tg e)

tagFVedTerm :: UniqSupply -> SizedFVedTerm -> TaggedSizedFVedTerm
tagFVedTerm = mkTagger (\tg e -> Comp (Tagged tg e))


-- The guiding principle behind these two functions is that ideally there should only
-- be one tag for a particular value. If we give every occurrence of (:) in the input
-- program a different tag we can get weird situations (like gen_regexps) where programs
-- are specialised on very long repititions of the same constructor.
--
-- The special treatment of PrimOp has a similar reason, and is necessary because I started
-- treating PrimOp specially and unfolding it without going through the wrapper if it is
-- saturated. This saves us from ANFing the arguments to a primop, which is cool!

uniqueToTag :: Unique -> Tag
uniqueToTag = mkTag . negate . abs . getKey -- Works well because (hashLiteral l) is always positive

dataConTag :: DataCon -> Tag
dataConTag = uniqueToTag . getUnique -- Don't use dataConTag because tags are shared between DC families, and [], True and all dictionary all get the same tag!!

literalTag :: Literal -> Tag
literalTag = mkTag . hashLiteral

primOpTag :: PrimOp -> Tag
primOpTag = uniqueToTag . mkPrimOpIdUnique . PrimOp.primOpTag


{-# INLINE mkTagger #-}
mkTagger :: (Copointed ann, Functor ann')
         => (forall a. Tag -> ann a -> ann' a)
         -> UniqSupply -> ann (TermF ann) -> ann' (TermF ann')
mkTagger rec = term
  where
    tag_rec ids orig f = rec (mkTag (getKey i)) (replace orig (f ids'))
      where (i, ids') = takeUniqFromSupply ids

    replace orig hole = fmap (const hole) orig

    term ids e = case extract e of
        Var x             -> tag $ \_ -> Var x
        Value v           -> value ids e v
        TyApp e ty        -> tag $ \ids -> TyApp (term ids e) ty
        CoApp e co        -> tag $ \ids -> CoApp (term ids e) co
        App e x           -> tag $ \ids -> App (term ids e) x
        PrimOp pop tys es -> rec (primOpTag pop) $ replace e $ let idss' = listSplitUniqSupply ids
                                                               in PrimOp pop tys (zipWith term idss' es)
        Case e x ty alts  -> tag $ \ids -> let (ids0', ids1') = splitUniqSupply ids
                                           in Case (term ids0' e) x ty (alternatives ids1' alts)
        Let x e1 e2       -> tag $ \ids -> let (ids0', ids1') = splitUniqSupply ids
                                           in Let x (term ids0' e1) (term ids1' e2)
        LetRec xes e      -> tag $ \ids -> let (ids0', ids1') = splitUniqSupply ids
                                               idss' = listSplitUniqSupply ids0'
                                           in LetRec (zipWith (\ids'' (x, e) -> (x, term ids'' e)) idss' xes) (term ids1' e)
        Cast e co         -> tag $ \ids -> Cast (term ids e) co
      where tag = tag_rec ids e

    value ids e v = fmap Value $ case v of
        TyLambda x e       -> tag $ \ids -> TyLambda x (term ids e)
        Lambda x e         -> tag $ \ids -> Lambda x (term ids e)
        Data dc tys cos xs -> rec (dataConTag dc) $ replace e (Data dc tys cos xs)
        Literal l          -> rec (literalTag l)  $ replace e (Literal l)
        Coercion co        -> tag $ \_ -> Coercion co
      where tag = tag_rec ids e

    alternatives = zipWith alternative . listSplitUniqSupply
    
    alternative ids (con, e) = (con, term ids e)


(taggedTermToTerm,              taggedTermToTerm',              taggedAltsToAlts,              taggedValueToValue,              taggedValue'ToValue')              = mkDetag (\f e -> I (f (tagee e)))
(fVedTermToTerm,                fVedTermToTerm',                fVedAltsToAlts,                fVedValueToValue,                fVedValue'ToValue')                = mkDetag (\f e -> I (f (fvee e)))
(taggedSizedFVedTermToTerm,     taggedSizedFVedTermToTerm',     taggedSizedFVedAltsToAlts,     taggedSizedFVedValueToValue,     taggedSizedFVedValue'ToValue')     = mkDetag (\f e -> I (f (fvee (sizee (unComp (tagee (unComp e)))))))
(taggedSizedFVedTermToFVedTerm, taggedSizedFVedTermToFVedTerm', taggedSizedFVedAltsToFVedAlts, taggedSizedFVedValueToFVedValue, taggedSizedFVedValue'ToFVedValue') = mkDetag (\f e -> FVed (freeVars (sizee (unComp (tagee (unComp e))))) (f (extract e)))


{-# INLINE mkDetag #-}
mkDetag :: (forall a b. (a -> b) -> ann a -> ann' b)
        -> (ann (TermF ann)  -> ann' (TermF ann'),
            TermF ann        -> TermF ann',
            [AltF ann]       -> [AltF ann'],
            ann (ValueF ann) -> ann' (ValueF ann'),
            ValueF ann       -> ValueF ann')
mkDetag rec = (term, term', alternatives, value, value')
  where
    term = rec term'
    term' e = case e of
        Var x             -> Var x
        Value v           -> Value (value' v)
        TyApp e ty        -> TyApp (term e) ty
        CoApp e co        -> CoApp (term e) co
        App e x           -> App (term e) x
        PrimOp pop tys es -> PrimOp pop tys (map term es)
        Case e x ty alts  -> Case (term e) x ty (alternatives alts)
        Let x e1 e2       -> Let x (term e1) (term e2)
        LetRec xes e      -> LetRec (map (second term) xes) (term e)
        Cast e co         -> Cast (term e) co

    value = rec value'
    value' (TyLambda a e)       = TyLambda a (term e)
    value' (Lambda x e)         = Lambda x (term e)
    value' (Data dc tys cos xs) = Data dc tys cos xs
    value' (Literal l)          = Literal l
    value' (Coercion co)        = Coercion co

    alternatives = map (second term)
