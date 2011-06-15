module Supercompile.Evaluator.Syntax where

import Supercompile.Evaluator.Deeds

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming
import Supercompile.Core.Size
import Supercompile.Core.Syntax
import Supercompile.Core.Tag

import Supercompile.Utilities

import qualified Data.Map as M
import Data.Traversable (Traversable(..))


type Anned = O Tagged (O Sized FVed)
type AnnedTerm = Anned (TermF Anned)
type AnnedValue = ValueF Anned
type AnnedAlt = AltF Anned

annee :: Anned a -> a
annee = extract

annedSize :: Anned a -> Size
annedSize = size . unComp . tagee . unComp

annedFreeVars :: Anned a -> FreeVars
annedFreeVars = freeVars . sizee . unComp . tagee . unComp

annedTag :: Anned a -> Tag
annedTag = tag . unComp


annedVarFreeVars' = taggedSizedFVedVarFreeVars'
annedTermFreeVars = taggedSizedFVedTermFreeVars
annedTermFreeVars' = taggedSizedFVedTermFreeVars'
annedValueFreeVars = taggedSizedFVedValueFreeVars
annedValueFreeVars' = taggedSizedFVedValueFreeVars'
annedAltsFreeVars = taggedSizedFVedAltsFreeVars

annedVarSize' = taggedSizedFVedVarSize'
annedTermSize' = taggedSizedFVedTermSize'
annedTermSize = taggedSizedFVedTermSize
annedValueSize' = taggedSizedFVedValueSize'
annedValueSize = taggedSizedFVedValueSize
annedAltsSize = taggedSizedFVedAltsSize

renameAnnedTerm = renameTaggedSizedFVedTerm :: InScopeSet -> Renaming -> AnnedTerm -> AnnedTerm
renameAnnedValue = renameTaggedSizedFVedValue
renameAnnedValue' = renameTaggedSizedFVedValue'
renameAnnedAlts = renameTaggedSizedFVedAlts

detagAnnedTerm = taggedSizedFVedTermToFVedTerm
detagAnnedTerm' = taggedSizedFVedTermToFVedTerm'
detagAnnedValue = taggedSizedFVedValueToFVedValue
detagAnnedValue' = taggedSizedFVedValue'ToFVedValue'
detagAnnedAlts = taggedSizedFVedAltsToFVedAlts


annedVar :: Tag -> Var -> Anned Var
annedVar   tg x = Comp (Tagged tg (Comp (Sized (annedVarSize' x)   (FVed (annedVarFreeVars' x)  x))))

annedTerm :: Tag -> TermF Anned -> AnnedTerm
annedTerm  tg e = Comp (Tagged tg (Comp (Sized (annedTermSize' e)  (FVed (annedTermFreeVars' e)  e))))

annedValue :: Tag -> ValueF Anned -> Anned AnnedValue
annedValue tg v = Comp (Tagged tg (Comp (Sized (annedValueSize' v) (FVed (annedValueFreeVars' v) v))))

annedAnswer :: Tag -> Answer -> Anned Answer
annedAnswer tg a = Comp (Tagged tg (Comp (Sized (answerSize' a) (FVed (answerFreeVars' a) a))))


toAnnedTerm :: UniqSupply -> Term -> AnnedTerm
toAnnedTerm tag_ids = tagFVedTerm tag_ids . reflect


type Answer = Coerced (In (ValueF Anned))

answerSize' :: Answer -> Size
answerSize' = annedTermSize' . answerToAnnedTerm' emptyInScopeSet

answerFreeVars' :: Answer -> FreeVars
answerFreeVars' = annedTermFreeVars' . answerToAnnedTerm' emptyInScopeSet

termToAnswer :: InScopeSet -> In AnnedTerm -> Maybe (Anned Answer)
termToAnswer iss (rn, anned_e) = flip traverse anned_e $ \e -> case e of
    Value v          -> Just (Nothing, (rn, v))
    Cast anned_e' co -> case extract anned_e' of
        Value v -> Just (Just (renameCoercion iss rn co, annedTag anned_e'), (rn, v))
        _       -> Nothing
    _ -> Nothing

data QA = Question (Out Var)
        | Answer   Answer

instance Outputable QA where
    pprPrec prec = pPrintPrec prec . qaToAnnedTerm' emptyInScopeSet

annedAnswerToAnnedTerm :: InScopeSet -> Anned Answer -> In AnnedTerm
annedAnswerToAnnedTerm iss anned_a = (mkIdentityRenaming (annedFreeVars anned_e), anned_e)
  where e' = answerToAnnedTerm' iss (annee anned_a)
        anned_e = annedTerm (annedTag anned_a) e'

answerToAnnedTerm' :: InScopeSet -> Answer -> TermF Anned
answerToAnnedTerm' iss (mb_co, (rn, v)) = case mb_co of
    Nothing       -> Value v'
    Just (co, tg) -> Cast (annedTerm tg (Value v')) co
  where v' = renameAnnedValue' iss rn v

qaToAnnedTerm' :: InScopeSet -> QA -> TermF Anned
qaToAnnedTerm' _   (Question x) = Var x
qaToAnnedTerm' iss (Answer a)   = answerToAnnedTerm' iss a


type UnnormalisedState = (Deeds, Heap, Stack, In AnnedTerm)
type State = (Deeds, Heap, Stack, Anned QA)

denormalise :: State -> UnnormalisedState
denormalise (deeds, h, k, qa) = case extract qa of
    Question x              -> (deeds, h, k, (mkIdentityRenaming (unitVarSet x), annedTerm tg (Var x)))
    Answer (mb_co, (rn, v)) -> (deeds, h, maybe id (\(co, tg) -> (Tagged tg (CastIt co) :)) mb_co k, (rn, annedTerm tg (Value v)))
  where tg = annedTag qa


-- Invariant: LetBound things cannot refer to LambdaBound things.
--
-- This is motivated by:
--  1. There is no point lambda-abstracting over things referred to by LetBounds because the resulting h-function would be
--     trapped under the appropriate let-binding anyway, at which point all the lambda-abstracted things would be in scope as FVs.
--  2. It allows (but does not require) the matcher to look into the RHS of LetBound stuff (rather than just doing nominal
--     matching).
data HowBound = InternallyBound | LambdaBound | LetBound
              deriving (Eq, Show)

instance Outputable HowBound where
    ppr = text . show

data HeapBinding = HB { howBound :: HowBound, heapBindingMeaning :: Either (Maybe Tag) (In AnnedTerm) }

pPrintPrecAnned :: (Outputable1 f, Outputable a)
                => (f a -> FreeVars)
                -> (InScopeSet -> Renaming -> f a -> f a)
                -> Rational -> In (f a) -> SDoc
pPrintPrecAnned fvs rename prec in_e = pprPrec prec $ Wrapper1 $ renameIn (rename (mkInScopeSet (inFreeVars fvs in_e))) in_e

pPrintPrecAnnedAlts :: In [AnnedAlt] -> [(AltCon, PrettyFunction)]
pPrintPrecAnnedAlts in_alts = map (second (\e -> PrettyFunction $ \prec -> pprPrec prec (Wrapper1 e))) $ renameIn (renameAnnedAlts (mkInScopeSet (inFreeVars annedAltsFreeVars in_alts))) in_alts

pPrintPrecAnnedValue :: Rational -> In (Anned AnnedValue) -> SDoc
pPrintPrecAnnedValue prec in_e = pPrintPrec prec $ extract $ renameIn (renameAnnedValue (mkInScopeSet (inFreeVars annedValueFreeVars in_e))) in_e

pPrintPrecAnnedTerm :: Rational -> In AnnedTerm -> SDoc
pPrintPrecAnnedTerm prec in_e = pprPrec prec $ Wrapper1 $ renameIn (renameAnnedTerm (mkInScopeSet (inFreeVars annedTermFreeVars in_e))) in_e

pPrintPrecAnnedAnswer :: Rational -> Anned Answer -> SDoc
pPrintPrecAnnedAnswer prec a = pprPrec prec $ Wrapper1 $ fmap (\a -> PrettyFunction $ \prec -> pPrintPrecAnswer prec a) a

pPrintPrecAnswer :: (Outputable a) => Rational -> (Maybe (Coercion, Tag), a) -> SDoc
pPrintPrecAnswer prec (Nothing,       v) = pPrintPrec prec v
pPrintPrecAnswer prec (Just (co, tg), v) = pPrintPrecCast prec (Wrapper1 $ Tagged tg v) co

instance Outputable HeapBinding where
    pprPrec prec (HB how mb_in_e) = case how of
        InternallyBound -> either (const empty) (pPrintPrecAnnedTerm prec) mb_in_e
        LambdaBound     -> text "λ" <> angles (either (const empty) (pPrintPrecAnnedTerm noPrec) mb_in_e)
        LetBound        -> text "l" <> angles (either (const empty) (pPrintPrecAnnedTerm noPrec) mb_in_e)

lambdaBound :: HeapBinding
lambdaBound = HB LambdaBound (Left Nothing)

internallyBound :: In AnnedTerm -> HeapBinding
internallyBound in_e = HB InternallyBound (Right in_e)

environmentallyBound :: Tag -> HeapBinding
environmentallyBound tg = HB LetBound (Left (Just tg))

type PureHeap = M.Map (Out Var) HeapBinding
data Heap = Heap PureHeap InScopeSet

instance Outputable Heap where
    pprPrec prec (Heap h _) = pprPrec prec h


type Stack = [Tagged StackFrame]
data StackFrame = Apply (Out Var)
                | TyApply (Out Type)
                | Scrutinise (Out Var) (Out Type) (In [AnnedAlt])
                | PrimApply PrimOp [Anned Answer] [In AnnedTerm]
                | Update (Out Var)
                | CastIt (Out Coercion)

instance Outputable StackFrame where
    pprPrec prec kf = case kf of
        Apply x'                  -> pPrintPrecApp prec (PrettyDoc $ text "[_]") x'
        TyApply ty'               -> pPrintPrecApp prec (PrettyDoc $ text "[_]") ty'
        Scrutinise x' _ty in_alts -> pPrintPrecCase prec (PrettyDoc $ text "[_]") x' (pPrintPrecAnnedAlts in_alts)
        PrimApply pop in_vs in_es -> pPrintPrecPrimOp prec pop (map (PrettyFunction . flip pPrintPrecAnnedAnswer) in_vs ++ map (PrettyFunction . flip pPrintPrecAnnedTerm) in_es)
        Update x'                 -> pPrintPrecApp prec (PrettyDoc $ text "update") x'
        CastIt co'                -> pPrintPrecCast prec (PrettyDoc $ text "[_]") co'


heapBindingTerm :: HeapBinding -> Maybe (In AnnedTerm)
heapBindingTerm = either (const Nothing) Just . heapBindingMeaning

heapBindingTag :: HeapBinding -> Maybe Tag
heapBindingTag = either id (Just . annedTag . snd) . heapBindingMeaning

-- | Size of HeapBinding for Deeds purposes
heapBindingSize :: HeapBinding -> Size
heapBindingSize (HB InternallyBound (Right (_, e))) = annedSize e
heapBindingSize _                                   = 0

-- | Size of StackFrame for Deeds purposes
stackFrameSize :: StackFrame -> Size
stackFrameSize kf = 1 + case kf of
    Apply _                  -> 0
    TyApply _                -> 0
    Scrutinise _ _ (_, alts) -> annedAltsSize alts
    PrimApply _ as in_es     -> sum (map annedSize as ++ map (annedTermSize . snd) in_es)
    Update _                 -> 0
    CastIt _                 -> 0

stateSize :: State -> Size
stateSize (_, h, k, qa) = heapSize h + stackSize k + annedSize qa
  where heapSize (Heap h _) = sum (map heapBindingSize (M.elems h))
        stackSize = sum . map (stackFrameSize . tagee)

addStateDeeds :: Deeds -> (Deeds, a, b, c) -> (Deeds, a, b, c)
addStateDeeds extra_deeds (deeds, h, k, in_e) = (extra_deeds + deeds, h, k, in_e)

releaseHeapBindingDeeds :: Deeds -> HeapBinding -> Deeds
releaseHeapBindingDeeds deeds hb = deeds + heapBindingSize hb

releasePureHeapDeeds :: Deeds -> PureHeap -> Deeds
releasePureHeapDeeds = M.fold (flip releaseHeapBindingDeeds)

releaseStackDeeds :: Deeds -> Stack -> Deeds
releaseStackDeeds = foldl' (\deeds kf -> deeds + stackFrameSize (tagee kf))

releaseUnnormalisedStateDeed :: UnnormalisedState -> Deeds
releaseUnnormalisedStateDeed (deeds, Heap h _, k, (_, e)) = releaseStackDeeds (releasePureHeapDeeds (deeds + annedSize e) h) k

releaseStateDeed :: State -> Deeds
releaseStateDeed (deeds, Heap h _, k, a) = releaseStackDeeds (releasePureHeapDeeds (deeds + annedSize a) h) k
