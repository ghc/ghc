{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Supercompile.Evaluator.Syntax where

#include "HsVersions.h"

import Supercompile.Evaluator.Deeds

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming
import Supercompile.Core.Size
import Supercompile.Core.Syntax
import Supercompile.Core.Tag

import Supercompile.Utilities

import Id       (Id, idType)
import PrimOp   (primOpType)
import Type     (applyTy, applyTys, isUnLiftedType)
import Pair     (pSnd)
import Coercion (coercionType, coercionKind)

import qualified Data.Map as M


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

annedToTagged :: Anned a -> Tagged a
annedToTagged x = Tagged (annedTag x) (annee x)

renameAnned :: In (Anned a) -> Anned (In a)
renameAnned (rn, Comp (Tagged tg (Comp (Sized sz (FVed fvs x)))))
  = Comp (Tagged tg (Comp (Sized sz (FVed (renameFreeVars rn fvs) (rn, x)))))


renamedValue :: AnnedValue -> In AnnedValue
renamedValue v = (mkIdentityRenaming (annedValueFreeVars' v), v)

renamedTerm :: AnnedTerm -> In AnnedTerm
renamedTerm e = (mkIdentityRenaming (annedTermFreeVars e), e)


annedVarFreeVars' = taggedSizedFVedVarFreeVars'
annedTermFreeVars = taggedSizedFVedTermFreeVars
annedTermFreeVars' = taggedSizedFVedTermFreeVars'
annedValueFreeVars = taggedSizedFVedValueFreeVars
annedValueFreeVars' = taggedSizedFVedValueFreeVars'
annedAltsFreeVars = taggedSizedFVedAltsFreeVars

annedTermSize' = taggedSizedFVedTermSize'
annedTermSize = taggedSizedFVedTermSize
annedValueSize' = taggedSizedFVedValueSize'
annedValueSize = taggedSizedFVedValueSize
annedAltsSize = taggedSizedFVedAltsSize

renameAnnedTerm = renameTaggedSizedFVedTerm :: InScopeSet -> Renaming -> AnnedTerm -> AnnedTerm
renameAnnedValue = renameTaggedSizedFVedValue
renameAnnedValue' = renameTaggedSizedFVedValue'
renameAnnedAlts = renameTaggedSizedFVedAlts

annedTermToTerm = taggedSizedFVedTermToTerm
annedTermToFVedTerm = taggedSizedFVedTermToFVedTerm
annedTermToFVedTerm' = taggedSizedFVedTermToFVedTerm'
annedValueToFVedValue = taggedSizedFVedValueToFVedValue
annedValueToFVedValue' = taggedSizedFVedValue'ToFVedValue'
annedAltsToFVedAlts = taggedSizedFVedAltsToFVedAlts


annedTerm :: Tag -> TermF Anned -> AnnedTerm
annedTerm  tg e = Comp (Tagged tg (Comp (Sized (annedTermSize' e)  (FVed (annedTermFreeVars' e)  e))))

annedValue :: Tag -> ValueF Anned -> Anned AnnedValue
annedValue tg v = Comp (Tagged tg (Comp (Sized (annedValueSize' v) (FVed (annedValueFreeVars' v) v))))

annedAnswer :: Tag -> Answer -> Anned Answer
annedAnswer tg a = Comp (Tagged tg (Comp (Sized (answerSize' a) (FVed (answerFreeVars' a) a))))


toAnnedTerm :: UniqSupply -> Term -> AnnedTerm
toAnnedTerm tag_ids = tagFVedTerm tag_ids . reflect


-- NB: the way tags are laid out in an Answer is rather "funny". Unlike most other uses of CastBy,
-- the tag in the CastBy (if present) should be wrapped around the *uncast* value, NOT around the
-- *cast* value.
--
-- This means that if we want correct tag propagation we have to be very careful when we use an
-- existing CastBy in the construction of an Answer.
type Answer = Coerced (In (ValueF Anned))

answerSize' :: Answer -> Size
answerSize' = annedTermSize' . answerToAnnedTerm' emptyInScopeSet

answerFreeVars' :: Answer -> FreeVars
answerFreeVars' = annedTermFreeVars' . answerToAnnedTerm' emptyInScopeSet

termToAnswer :: InScopeSet -> In AnnedTerm -> Maybe (Anned Answer)
termToAnswer iss in_anned_e = flip traverse (renameAnned in_anned_e) $ \(rn, e) -> case e of
    Value v          -> Just (Uncast, (rn, v))
    Cast anned_e' co -> case extract anned_e' of
        Value v -> Just (castBy (renameCoercion iss rn co) (annedTag anned_e'), (rn, v))
        _       -> Nothing
    _ -> Nothing

data QA = Question (Out Id)
        | Answer   Answer

instance Outputable QA where
    pprPrec prec = pPrintPrec prec . qaToAnnedTerm' emptyInScopeSet

caseAnnedQA :: Anned QA -> Either (Anned (Out Id)) (Anned Answer)
caseAnnedQA anned_qa = case extract anned_qa of
    Question anned_q -> Left  (fmap (const anned_q) anned_qa)
    Answer   anned_a -> Right (fmap (const anned_a) anned_qa)

-- NB: one of the callers in Speculate depends (rather delicately) on the fact that
-- the renaming returned in the Question case mentions all of the things in the supplied
-- InScopeSet, even though it could get away with just mentioning the single Var.
--
-- TODO: perhaps we can do something more satisfactory?
annedQAToInAnnedTerm :: InScopeSet -> Anned QA -> In AnnedTerm
annedQAToInAnnedTerm iss anned_qa = case caseAnnedQA anned_qa of
    Left  anned_q -> annedQuestionToInAnnedTerm iss anned_q
    Right anned_a -> annedAnswerToInAnnedTerm iss anned_a


annedQuestionToInAnnedTerm :: InScopeSet -> Anned (Out Id) -> In AnnedTerm
annedQuestionToInAnnedTerm iss anned_q = (mkInScopeIdentityRenaming iss, fmap Var anned_q)


annedAnswerToInAnnedTerm :: InScopeSet -> Anned Answer -> In AnnedTerm
annedAnswerToInAnnedTerm iss anned_a = case annee anned_a of
  (Uncast,          (rn, v)) -> (rn,                                                                 fmap Value $ annedValue (annedTag anned_a) v)
  (CastBy co co_tg, (rn, v)) -> (mkInScopeIdentityRenaming iss, annedTerm (annedTag anned_a) $ Cast (fmap Value $ annedValue co_tg            $ renameAnnedValue' iss rn v) co)

answerToAnnedTerm' :: InScopeSet -> Answer -> TermF Anned
answerToAnnedTerm' iss (mb_co, (rn, v)) = case mb_co of
    Uncast       -> e'
    CastBy co tg -> Cast (annedTerm tg e') co
  where e' = Value $ renameAnnedValue' iss rn v

castAnnedAnswer :: InScopeSet -> Anned Answer -> Out CastBy -> Anned Answer
castAnnedAnswer _   anned_a Uncast           = anned_a
castAnnedAnswer iss anned_a (CastBy co' tg') = snd $ castTaggedAnswer iss (annedToTagged anned_a) (co', tg')

castTaggedAnswer :: InScopeSet -> Tagged Answer -> (NormalCo, Tag) -> (Maybe Tag, Anned Answer)
castTaggedAnswer iss (Tagged tg (cast_by, in_v)) (co', tg') = (mb_dumped, annedAnswer tg' (castBy co'' tg, in_v))
  where (mb_dumped, co'') = case cast_by of Uncast              -> (Nothing,        co')
                                            CastBy co dumped_tg -> (Just dumped_tg, mkTransCo iss co co')

castAnnedTerm :: CastBy -> AnnedTerm -> AnnedTerm
castAnnedTerm Uncast         e = e
castAnnedTerm (CastBy co tg) e = annedTerm tg (Cast e co)

qaToAnnedTerm' :: InScopeSet -> QA -> TermF Anned
qaToAnnedTerm' _   (Question x) = Var x
qaToAnnedTerm' iss (Answer a)   = answerToAnnedTerm' iss a

qaToAnswer :: QA -> Maybe Answer
qaToAnswer qa = case qa of Answer a -> Just a; Question _ -> Nothing


type Generalised = Bool


type UnnormalisedState = (Deeds, Heap, Stack, In AnnedTerm)
type State = (Deeds, Heap, Stack, Anned QA)

-- NB: denormalise could actually eagerly put any frame arising from CastBy into the stack, but this is more modular:
denormalise :: State -> UnnormalisedState
denormalise (deeds, h@(Heap _ ids), k, qa) = (deeds, h, k, annedQAToInAnnedTerm ids qa)


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

-- Left (Left gen) == no term, previously generalised if gen
-- Left (Right tg) == no term, bound by environment with tag tg (used by LetBound only I think)
-- Right in_e      == term in_e
data HeapBinding = HB { howBound :: HowBound, heapBindingMeaning :: Either (Either Generalised Tag) (In AnnedTerm) }

pPrintPrecAnned :: Outputable (f a)
                => (f a -> FreeVars)
                -> (InScopeSet -> Renaming -> f a -> f a)
                -> Rational -> In (f a) -> SDoc
pPrintPrecAnned fvs rename prec in_e = pprPrec prec $ renameIn (rename (mkInScopeSet (inFreeVars fvs in_e))) in_e

pPrintPrecAnnedAlts :: In [AnnedAlt] -> [(AltCon, PrettyFunction)]
pPrintPrecAnnedAlts in_alts = map (second asPrettyFunction) $ renameIn (renameAnnedAlts (mkInScopeSet (inFreeVars annedAltsFreeVars in_alts))) in_alts

pPrintPrecAnnedValue :: Rational -> In (Anned AnnedValue) -> SDoc
pPrintPrecAnnedValue prec in_e = pPrintPrec prec $ extract $ renameIn (renameAnnedValue (mkInScopeSet (inFreeVars annedValueFreeVars in_e))) in_e

pPrintPrecAnnedTerm :: Rational -> In AnnedTerm -> SDoc
pPrintPrecAnnedTerm prec in_e = pprPrec prec $ renameIn (renameAnnedTerm (mkInScopeSet (inFreeVars annedTermFreeVars in_e))) in_e

pPrintPrecAnnedAnswer :: Rational -> Anned Answer -> SDoc
pPrintPrecAnnedAnswer prec a = pprPrec prec $ fmap (\a -> PrettyFunction $ \prec -> pPrintPrecAnswer prec a) a

pPrintPrecAnswer :: (Outputable a) => Rational -> (CastBy, a) -> SDoc
pPrintPrecAnswer prec (Uncast,       v) = pPrintPrec prec v
pPrintPrecAnswer prec (CastBy co tg, v) = pPrintPrecCast prec (Tagged tg v) co

instance Outputable HeapBinding where
    pprPrec prec (HB how mb_in_e) = case how of
        InternallyBound -> either (const empty) (pPrintPrecAnnedTerm prec) mb_in_e
        LambdaBound     -> text "λ" <> angles (either (either (\gen -> if gen then text "?" else empty) (const empty)) (pPrintPrecAnnedTerm noPrec) mb_in_e)
        LetBound        -> text "l" <> angles (either (either (\gen -> if gen then text "?" else empty) (const empty)) (pPrintPrecAnnedTerm noPrec) mb_in_e)

lambdaBound :: HeapBinding
lambdaBound = HB LambdaBound (Left (Left False))

generalised :: HeapBinding
generalised = HB LambdaBound (Left (Left True))

internallyBound :: In AnnedTerm -> HeapBinding
internallyBound in_e = HB InternallyBound (Right in_e)

environmentallyBound :: Tag -> HeapBinding
environmentallyBound tg = HB LetBound (Left (Right tg))

letBound :: In AnnedTerm -> HeapBinding
letBound in_e = HB LetBound (Right in_e)

-- The Heap might contain bindings for TyVars as well, but will only map them to lambdaBound
type PureHeap = M.Map (Out Var) HeapBinding
data Heap = Heap PureHeap InScopeSet

instance Outputable Heap where
    pprPrec prec (Heap h _) = pprPrec prec h


type Stack = Train (Tagged StackFrame) Generalised
data StackFrame = TyApply (Out Type)
                | CoApply (Out NormalCo)
                | Apply (Out Id)
                | Scrutinise (Out Id) (Out Type) (In [AnnedAlt])
                | PrimApply PrimOp [Out Type] [Anned Answer] [In AnnedTerm]
                | StrictLet (Out Id) (In AnnedTerm)
                | Update (Out Id)
                | CastIt (Out NormalCo)

instance Outputable StackFrame where
    pprPrec prec kf = case kf of
        TyApply ty'                    -> pPrintPrecApp prec (PrettyDoc $ text "[_]") ty'
        CoApply co'                    -> pPrintPrecApp prec (PrettyDoc $ text "[_]") co'
        Apply x'                       -> pPrintPrecApp prec (PrettyDoc $ text "[_]") x'
        Scrutinise x' _ty in_alts      -> pPrintPrecCase prec (PrettyDoc $ text "[_]") x' (pPrintPrecAnnedAlts in_alts)
        PrimApply pop tys' in_vs in_es -> pPrintPrecPrimOp prec pop tys' (map (PrettyFunction . flip pPrintPrecAnnedAnswer) in_vs ++ map (PrettyFunction . flip pPrintPrecAnnedTerm) in_es)
        StrictLet x' in_e2             -> pPrintPrecLet prec x' (PrettyDoc $ text "[_]") (PrettyFunction $ flip pPrintPrecAnnedTerm in_e2)
        Update x'                      -> pPrintPrecApp prec (PrettyDoc $ text "update") x'
        CastIt co'                     -> pPrintPrecCast prec (PrettyDoc $ text "[_]") co'


stateType :: State -> Type
stateType (_, _, k, qa) = stackType k (qaType qa)

stackType :: Stack -> Type -> Type
stackType k ty = trainCarFoldl' (flip stackFrameType) ty k

stackFrameType :: Tagged StackFrame -> Type -> Type
stackFrameType = stackFrameType' . tagee

stackFrameType' :: StackFrame -> Type -> Type
stackFrameType' kf hole_ty = case kf of
    TyApply ty                    -> hole_ty `applyTy` ty
    CoApply co                    -> hole_ty `applyFunTy` coercionType co
    Apply x                       -> hole_ty `applyFunTy` idType x
    Scrutinise _ ty _             -> ty
    PrimApply pop tys in_as in_es -> ((primOpType pop `applyTys` tys) `applyFunTys` map answerType in_as) `applyFunTy` hole_ty `applyFunTys` map (\in_e@(rn, e) -> termType (renameAnnedTerm (mkInScopeSet (inFreeVars annedFreeVars in_e)) rn e)) in_es
    StrictLet _ in_e@(rn, e)      -> termType (renameAnnedTerm (mkInScopeSet (inFreeVars annedFreeVars in_e)) rn e)
    Update _                      -> hole_ty
    CastIt co                     -> pSnd (coercionKind co)

qaType :: Anned QA -> Type
qaType anned_qa = case caseAnnedQA anned_qa of
    Left  anned_q -> idType (extract anned_q)
    Right anned_a ->answerType anned_a

answerType :: Anned Answer -> Type
answerType a = case annee a of
    (CastBy co _, _)       -> pSnd (coercionKind co)
    (Uncast,      (rn, v)) -> valueType (renameAnnedValue' (mkInScopeSet (annedFreeVars a)) rn v)


heapBindingTerm :: HeapBinding -> Maybe (In AnnedTerm)
heapBindingTerm = either (const Nothing) Just . heapBindingMeaning

{-# INLINE heapBindingTag #-} -- Showed up as causing 2% of allocation in a run despite resulting Maybe never being stored
heapBindingTag :: HeapBinding -> Maybe Tag
heapBindingTag = either (either (const Nothing) Just) (Just . annedTag . snd) . heapBindingMeaning

-- | Size of HeapBinding for Deeds purposes
heapBindingSize :: HeapBinding -> Size
heapBindingSize (HB InternallyBound (Right (_, e))) = annedSize e
heapBindingSize _                                   = 0

-- | Size of StackFrame for Deeds purposes
stackFrameSize :: StackFrame -> Size
stackFrameSize kf = 1 + case kf of
    TyApply _                -> 0
    CoApply _                -> 0
    Apply _                  -> 0
    Scrutinise _ _ (_, alts) -> annedAltsSize alts
    PrimApply _ _ as in_es   -> sum (map annedSize as ++ map (annedTermSize . snd) in_es)
    StrictLet _ (_, e)       -> annedTermSize e
    Update _                 -> 0
    CastIt _                 -> 0

heapSize :: Heap -> Size
heapSize (Heap h _) = sum (map heapBindingSize (M.elems h))

stackSize :: Stack -> Size
stackSize = trainCarFoldl' (\size kf -> size + stackFrameSize (tagee kf)) 0

stateSize :: State -> Size
stateSize (_, h, k, qa) = heapSize h + stackSize k + annedSize qa

unnormalisedStateSize :: UnnormalisedState -> Size
unnormalisedStateSize (_, h, k, (_, e)) = heapSize h + stackSize k + annedSize e


addStateDeeds :: Deeds -> (Deeds, a, b, c) -> (Deeds, a, b, c)
addStateDeeds extra_deeds (deeds, h, k, in_e) = (extra_deeds `plusDeeds` deeds, h, k, in_e)

releaseHeapBindingDeeds :: Deeds -> HeapBinding -> Deeds
releaseHeapBindingDeeds deeds hb = deeds `releaseDeeds` heapBindingSize hb

releasePureHeapDeeds :: Deeds -> PureHeap -> Deeds
releasePureHeapDeeds = M.fold (flip releaseHeapBindingDeeds)

releaseStackFrameDeeds :: Deeds -> Tagged StackFrame -> Deeds
releaseStackFrameDeeds deeds kf = deeds `releaseDeeds` stackFrameSize (tagee kf)

releaseStackDeeds :: Deeds -> Stack -> Deeds
releaseStackDeeds = trainCarFoldl' releaseStackFrameDeeds

releaseUnnormalisedStateDeed :: UnnormalisedState -> Deeds
releaseUnnormalisedStateDeed (deeds, Heap h _, k, (_, e)) = releaseStackDeeds (releasePureHeapDeeds (deeds `releaseDeeds` annedSize e) h) k

releaseStateDeed :: State -> Deeds
releaseStateDeed (deeds, Heap h _, k, a) = releaseStackDeeds (releasePureHeapDeeds (deeds `releaseDeeds` annedSize a) h) k


stackToCast :: Stack -> Maybe CastBy
stackToCast (Loco _)                             = Just Uncast
stackToCast (Tagged tg (CastIt co) `Car` Loco _) = Just (CastBy co tg)
stackToCast _                                    = Nothing


-- Unlifted bindings are irritating. They mean that the PureHeap has an implicit order that we need to carefully
-- preserve when we turn it back into a term: unlifted bindings must be bound by a "let".
--
-- An alternative to this would be to record the binding struture in the PureHeap itself, but that would get pretty
-- fiddly (in particuar, update frames would need to hold a "cursor" saying where in the PureHeap to update upon
-- completion). It's probably better to take the complexity hit here and now.
bindManyMixedLiftedness :: Symantics ann => (ann (TermF ann) -> FreeVars) -> [(Var, ann (TermF ann))] -> ann (TermF ann) -> ann (TermF ann)
bindManyMixedLiftedness get_fvs = go
  where go []  = id
        go xes = case takeFirst (\(x, _) -> isUnLiftedType (idType x)) xes of
            Nothing                 -> letRec xes
            Just ((x, e), rest_xes) -> go xes_above . let_ x e . go xes_below
              where (xes_above, xes_below) = partition_one (unitVarSet x) rest_xes

        partition_one bvs_below xes | bvs_below' == bvs_below = (xes_above, xes_below)
                                    | otherwise               = second (xes_below ++) $ partition_one bvs_below' xes_above
          where (xes_below, xes_above) = partition (\(_, e) -> get_fvs e `intersectsVarSet` bvs_below) xes
                bvs_below' = bvs_below `unionVarSet` mkVarSet (map fst xes_below)
