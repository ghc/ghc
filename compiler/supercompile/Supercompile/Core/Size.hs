{-# LANGUAGE Rank2Types, FlexibleInstances #-}
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


(varSize',                termSize,                termSize',                altsSize,                valueSize,                valueSize')                = mkSize (\f (I e) -> f e)
(fvedVarSize',            fvedTermSize,            fvedTermSize',            fvedAltsSize,            fvedValueSize,            fvedValueSize')            = mkSize (\f (FVed _ e) -> f e)
(sizedVarSize',           sizedTermSize,           sizedTermSize',           sizedAltsSize,           sizedValueSize,           sizedValueSize')           = mkSize (\_ (Sized sz _) -> sz)
(sizedFVedVarSize',       sizedFVedTermSize,       sizedFVedTermSize',       sizedFVedAltsSize,       sizedFVedValueSize,       sizedFVedValueSize')       = mkSize (\_ (Comp (Sized sz (FVed _ _))) -> sz)
(taggedSizedFVedVarSize', taggedSizedFVedTermSize, taggedSizedFVedTermSize', taggedSizedFVedAltsSize, taggedSizedFVedValueSize, taggedSizedFVedValueSize') = mkSize (\_ (Comp (Tagged _ (Comp (Sized sz (FVed _ _))))) -> sz)

{-# INLINE mkSize #-}
mkSize :: (forall a. (a -> Size) -> ann a -> Size)
       -> (Var              -> Size,
           ann (TermF ann)  -> Size,
           TermF ann        -> Size,
           [AltF ann]       -> Size,
           ann (ValueF ann) -> Size,
           ValueF ann       -> Size)
mkSize rec = (var', term, term', alternatives, value, value')
  where
    var' = const 0
    
    term = rec term'
    term' e = 1 + case e of
        Var x           -> var' x
        Value v         -> value' v - 1 -- Slight hack here so that we don't get +2 size on values
        TyApp e _       -> term e
        App e x         -> term e + var' x
        PrimOp _ es     -> sum (map term es)
        Case e _ _ alts -> term e + alternatives alts
        LetRec xes e    -> sum (map (term . snd) xes) + term e
        Cast e _        -> term e
    
    value = rec value'
    value' v = 1 + case v of
        Indirect _   -> 0
        TyLambda _ e -> term e
        Lambda _ e   -> term e
        Data _ _     -> 0
        Literal _    -> 0
    
    alternatives = sum . map alternative
    
    alternative = term . snd


{-
instance Symantics Sized where
    var = sizedTerm . Var
    value = sizedTerm . Value
    app e = sizedTerm . App e
    primOp pop = sizedTerm . PrimOp pop
    case_ e = sizedTerm . Case e
    letRec xes e = sizedTerm (LetRec xes e)

sizedTerm :: TermF Sized -> SizedTerm
sizedTerm e = Sized (sizedTermSize' e) e
-}

instance Symantics (O Sized FVed) where
    var = sizedFVedTerm . Var
    value = fmap Value . sizedFVedValue
    tyApp e = sizedFVedTerm . TyApp e
    app e = sizedFVedTerm . App e
    primOp pop = sizedFVedTerm . PrimOp pop
    case_ e x ty = sizedFVedTerm . Case e x ty
    letRec xes = sizedFVedTerm . LetRec xes
    cast e = sizedFVedTerm . Cast e

sizedFVedVar :: Var -> (O Sized FVed) Var
sizedFVedVar x = Comp (Sized (sizedFVedVarSize' x) (FVed (sizedFVedVarFreeVars' x) x))

sizedFVedValue :: SizedFVedValue -> (O Sized FVed) SizedFVedValue
sizedFVedValue v = Comp (Sized (sizedFVedValueSize' v) (FVed (sizedFVedValueFreeVars' v) v))

sizedFVedTerm :: TermF (O Sized FVed) -> SizedFVedTerm
sizedFVedTerm e = Comp (Sized (sizedFVedTermSize' e) (FVed (sizedFVedTermFreeVars' e) e))
