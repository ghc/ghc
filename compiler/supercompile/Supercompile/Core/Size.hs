{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Supercompile.Core.Size where

import Supercompile.Core.FreeVars
import Supercompile.Core.Syntax

import Supercompile.Utilities


type SizedTerm = Sized (TermF Sized)

type SizedFVedTerm = (O Sized FVed) (TermF (O Sized FVed))
type SizedFVedAlt = AltF (O Sized FVed)
type SizedFVedValue = ValueF (O Sized FVed)

type TaggedSizedFVedTerm = (O Tagged (O Sized FVed)) (TermF (O Tagged (O Sized FVed)))
type TaggedSizedFVedAlt = AltF (O Tagged (O Sized FVed))
type TaggedSizedFVedValue = ValueF (O Tagged (O Sized FVed))


(termSize,                termSize',                altsSize,                valueSize,                valueSize')                = mkSize (\f (I e) -> f e)
(fvedTermSize,            fvedTermSize',            fvedAltsSize,            fvedValueSize,            fvedValueSize')            = mkSize (\f (FVed _ e) -> f e)
(sizedTermSize,           sizedTermSize',           sizedAltsSize,           sizedValueSize,           sizedValueSize')           = mkSize (\_ (Sized sz _) -> sz)
(sizedFVedTermSize,       sizedFVedTermSize',       sizedFVedAltsSize,       sizedFVedValueSize,       sizedFVedValueSize')       = mkSize (\_ (Comp (Sized sz (FVed _ _))) -> sz)
(taggedSizedFVedTermSize, taggedSizedFVedTermSize', taggedSizedFVedAltsSize, taggedSizedFVedValueSize, taggedSizedFVedValueSize') = mkSize (\_ (Comp (Tagged _ (Comp (Sized sz (FVed _ _))))) -> sz)

{-# INLINE mkSize #-}
mkSize :: (forall a. (a -> Size) -> ann a -> Size)
       -> (ann (TermF ann)  -> Size,
           TermF ann        -> Size,
           [AltF ann]       -> Size,
           ann (ValueF ann) -> Size,
           ValueF ann       -> Size)
mkSize rec = (term, term', alternatives, value, value')
  where
    term = rec term'
    term' e = 1 + case e of
        Var _           -> 0
        Value v         -> value' v - 1 -- Slight hack here so that we don't get +2 size on values
        TyApp e _       -> term e
        App e _         -> term e
        PrimOp _ _ es   -> sum (map term es)
        Case e _ _ alts -> term e + alternatives alts
        Let _ e1 e2     -> term e1 + term e2
        LetRec xes e    -> sum (map (term . snd) xes) + term e
        Cast e _        -> term e
    
    value = rec value'
    value' v = 1 + case v of
        Indirect _   -> 0
        TyLambda _ e -> term e
        Lambda _ e   -> term e
        Data _ _ _   -> 0
        Literal _    -> 0
        Coercion _   -> 0
    
    alternatives = sum . map alternative
    
    alternative = term . snd


instance Symantics (O Sized FVed) where
    var = sizedFVedTerm . Var
    value = fmap Value . sizedFVedValue
    tyApp e = sizedFVedTerm . TyApp e
    app e = sizedFVedTerm . App e
    primOp pop tys = sizedFVedTerm . PrimOp pop tys
    case_ e x ty = sizedFVedTerm . Case e x ty
    let_ x e1 = sizedFVedTerm . Let x e1
    letRec xes = sizedFVedTerm . LetRec xes
    cast e = sizedFVedTerm . Cast e

sizedFVedValue :: SizedFVedValue -> (O Sized FVed) SizedFVedValue
sizedFVedValue v = Comp (Sized (sizedFVedValueSize' v) (FVed (sizedFVedValueFreeVars' v) v))

sizedFVedTerm :: TermF (O Sized FVed) -> SizedFVedTerm
sizedFVedTerm e = Comp (Sized (sizedFVedTermSize' e) (FVed (sizedFVedTermFreeVars' e) e))
