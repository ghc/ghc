{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Supercompile.Evaluator.Syntax where

#include "HsVersions.h"

import Supercompile.Evaluator.Deeds

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming
import Supercompile.Core.Size
import Supercompile.Core.Syntax
import Supercompile.Core.Tag

import Supercompile.StaticFlags
import Supercompile.Utilities

import Id       (Id, idType, zapIdOccInfo)
import PrimOp   (primOpType)
import Type     (applyTy, applyTys, isUnLiftedType, splitTyConApp_maybe, mkTyVarTy)
import Pair     (pSnd)
import Coercion (coercionType, coercionKind, mkCoVarCo)

import qualified Data.Map as M
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable


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

annedVar :: Tag -> Out Var -> Anned Var
annedVar tg x = Comp (Tagged tg (Comp (Sized 1 (FVed (annedVarFreeVars' x) x))))

annedAnswer :: Tag -> Answer -> Anned Answer
annedAnswer tg a = Comp (Tagged tg (Comp (Sized (answerSize' a) (FVed (answerFreeVars' a) a))))

annedCoercedAnswer :: Tag -> Coerced Answer -> Anned (Coerced Answer)
annedCoercedAnswer tg cast_a = Comp (Tagged tg (Comp (Sized (coercedSize answerSize' cast_a) (FVed (coercedFreeVars answerFreeVars' cast_a) cast_a))))

annedQA :: Tag -> QA -> Anned QA
annedQA tg (Question x) = fmap Question (annedVar tg x)
annedQA tg (Answer a)   = fmap Answer (annedAnswer tg a)


toAnnedTerm :: UniqSupply -> Term -> AnnedTerm
toAnnedTerm tag_ids = tagFVedTerm tag_ids . reflect


mkVarCastBy :: Tag -> Out Var -> CastBy -> In AnnedTerm
mkVarCastBy tg_y y' cast_by = (mkIdentityRenaming (castByFreeVars cast_by `extendVarSet` y'), cast_by `castAnnedTerm` annedTerm tg_y (Var y'))


-- NB: the way tags are laid out in an Answer is rather "funny". Unlike most other uses of CastBy,
-- the tag in the CastBy (if present) should be wrapped around the *uncast* value, NOT around the
-- *cast* value.
--
-- This means that if we want correct tag propagation we have to be very careful when we use an
-- existing CastBy in the construction of an Answer.

answerSize' :: Answer -> Size
answerSize' = annedValueSize' . snd

answerFreeVars' :: Answer -> FreeVars
answerFreeVars' = inFreeVars annedValueFreeVars'

type Question = Out Id
type Answer = In AnnedValue

data QAG answer = Question Question
                | Answer   answer

instance Functor QAG where
    fmap = Traversable.fmapDefault

instance Foldable QAG where
    foldMap = Traversable.foldMapDefault

instance Traversable QAG where
    traverse _ (Question x) = pure $ Question x
    traverse f (Answer a)   = fmap Answer (f a)

type QA = QAG Answer

instance Outputable QA where
    pprPrec prec = pPrintPrec prec . qaToAnnedTerm' emptyInScopeSet

caseAnnedQA :: Anned QA -> Either (Anned Question) (Anned Answer)
caseAnnedQA anned_qa = case extract anned_qa of
    Question anned_q -> Left  (fmap (const anned_q) anned_qa)
    Answer   anned_a -> Right (fmap (const anned_a) anned_qa)

annedQAToInAnnedTerm :: InScopeSet -> Anned QA -> In AnnedTerm
annedQAToInAnnedTerm iss anned_qa = case caseAnnedQA anned_qa of
    Left  anned_q -> (mkInScopeIdentityRenaming iss, fmap Var anned_q)
    Right anned_a -> annedAnswerToInAnnedTerm anned_a

termToCastAnswer :: InScopeSet -> In AnnedTerm -> Maybe (Anned (Coerced Answer))
termToCastAnswer iss in_anned_e = flip traverse (renameAnned in_anned_e) $ \(rn, e) -> case e of
    Value v          -> Just (Uncast, (rn, v))
    Cast anned_e' co -> case extract anned_e' of
        Value v -> Just (castBy (renameCoercion iss rn co) (annedTag anned_e'), (rn, v))
        _       -> Nothing
    _ -> Nothing

castAnswer :: InScopeSet -> Tag -> NormalCo -> Tagged (Coerced Answer) -> Tagged (Coerced Answer)
castAnswer _   tg_co co (Tagged tg_a    (Uncast, a))          = Tagged tg_co (CastBy co tg_a, a)
castAnswer ids tg_co co (Tagged _tg_co' (CastBy co' tg_a, a)) = Tagged tg_co (castBy (mkTransCo ids co co') tg_a, a)

castByAnswer :: InScopeSet -> CastBy -> Tagged (Coerced Answer) -> Tagged (Coerced Answer)
castByAnswer _   Uncast            cast_a = cast_a
castByAnswer ids (CastBy co tg_co) cast_a = castAnswer ids tg_co co cast_a

castAnswerSize :: Coerced Answer -> Size
castAnswerSize = coercedSize answerSize'

castAnnedQAToInAnnedTerm :: InScopeSet -> Anned QA -> CastBy -> In AnnedTerm
castAnnedQAToInAnnedTerm iss anned_qa cast_by = (mkInScopeIdentityRenaming iss, castAnnedTerm cast_by (renameIn (renameAnnedTerm iss) (annedQAToInAnnedTerm iss anned_qa)))

annedAnswerToInAnnedTerm :: Anned Answer -> In AnnedTerm
annedAnswerToInAnnedTerm = taggedAnswerToInAnnedTerm . annedToTagged

taggedAnswerToInAnnedTerm :: Tagged Answer -> In AnnedTerm
taggedAnswerToInAnnedTerm (Tagged tg_a (rn, v)) = (rn, fmap Value $ annedValue tg_a v)

taggedCastAnswerToInAnnedTerm :: InScopeSet -> Tagged (Coerced Answer) -> In AnnedTerm
taggedCastAnswerToInAnnedTerm _   (Tagged tg_a  (Uncast,         a)) = taggedAnswerToInAnnedTerm (Tagged tg_a a)
taggedCastAnswerToInAnnedTerm iss (Tagged tg_co (CastBy co tg_a, a)) = (mkInScopeIdentityRenaming iss, annedTerm tg_co (renameIn (renameAnnedTerm iss) (taggedAnswerToInAnnedTerm (Tagged tg_a a)) `Cast` co))

answerToAnnedTerm' :: InScopeSet -> Answer -> TermF Anned
answerToAnnedTerm' iss (rn, v) = Value $ renameAnnedValue' iss rn v

coercedAnswerToAnnedTerm' :: InScopeSet -> Coerced Answer -> TermF Anned
coercedAnswerToAnnedTerm' iss (Uncast,       e) = answerToAnnedTerm' iss e
coercedAnswerToAnnedTerm' iss (CastBy co tg, e) = annedTerm tg (answerToAnnedTerm' iss e) `Cast` co

{-
-- TODO: callers are probably wrong for deeds
castAnnedAnswer :: InScopeSet -> Anned Answer -> Out CastBy -> Anned Answer
castAnnedAnswer _   anned_a Uncast           = anned_a
castAnnedAnswer iss anned_a (CastBy co' tg') = snd $ castTaggedAnswer iss (annedToTagged anned_a) (co', tg')

castTaggedAnswer :: InScopeSet -> Tagged Answer -> (NormalCo, Tag) -> (Maybe Tag, Anned Answer)
castTaggedAnswer iss (Tagged tg (cast_by, in_v)) (co', tg') = (mb_dumped, annedAnswer tg' (castBy co'' tg, in_v))
  where (mb_dumped, co'') = case cast_by of Uncast              -> (Nothing,        co')
                                            CastBy co dumped_tg -> (Just dumped_tg, mkTransCo iss co co')
-}

castAnnedTerm :: CastBy -> AnnedTerm -> AnnedTerm
castAnnedTerm Uncast         e = e
castAnnedTerm (CastBy co tg) e = annedTerm tg (Cast e co)

qaToAnnedTerm' :: InScopeSet -> QA -> TermF Anned
qaToAnnedTerm' _   (Question x) = Var x
qaToAnnedTerm' iss (Answer a)   = answerToAnnedTerm' iss a

qaToAnswer :: QA -> Maybe Answer
qaToAnswer qa = case qa of Answer a -> Just a; Question _ -> Nothing


type Generalised = Bool

mayInstantiate :: InstanceMatching -> Generalised -> Bool
mayInstantiate NoInstances            _   = False
mayInstantiate InstancesOfGeneralised gen = gen
mayInstantiate AllInstances           _   = True


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

pPrintPrecAnnedCastAnswer :: Rational -> Anned (Coerced Answer) -> SDoc
pPrintPrecAnnedCastAnswer prec a = pprPrec prec $ fmap (\a -> PrettyFunction $ \prec -> pPrintPrecCoerced prec a) a

instance Outputable HeapBinding where
    pprPrec prec (HB how mb_in_e) = case how of
        InternallyBound -> either (const empty) (pPrintPrecAnnedTerm prec) mb_in_e
        LambdaBound     -> text "λ" <> angles (either (either (\gen -> if gen then text "?" else empty) (const empty)) (pPrintPrecAnnedTerm noPrec) mb_in_e)
        LetBound        -> text "l" <> angles (either (either (\gen -> if gen then text "?" else empty) (const empty)) (pPrintPrecAnnedTerm noPrec) mb_in_e)

heapBindingCheap :: HeapBinding -> Bool
heapBindingCheap = either (const True) (termIsCheap . snd) . heapBindingMeaning

lambdaBound :: HeapBinding
lambdaBound = HB LambdaBound (Left (Left False))

generalisedLambdaBound :: HeapBinding
generalisedLambdaBound = HB LambdaBound (Left (Left True))

internallyBound :: In AnnedTerm -> HeapBinding
internallyBound in_e = HB InternallyBound (Right in_e)

environmentallyBound :: Tag -> HeapBinding
environmentallyBound tg = HB LetBound (Left (Right tg))

letBound :: In AnnedTerm -> HeapBinding
letBound in_e = HB LetBound (Right in_e)

-- INVARIANT: the Heap might contain bindings for TyVars as well, but will only map them to lambdaBound/generalised
-- TODO: when we lambda-abstract over lambdaBounds, we implicitly rely on the fact that the lambdaBound IdInfo will work
-- out properly (unfortunately lambda-bounds can't be brought into scope all at the same time). We should probably fix
-- this -- perhaps by zapping all lambdaBound IdInfo when we abstract.
type PureHeap = M.Map (Out Var) HeapBinding
data Heap = Heap PureHeap InScopeSet

instance Outputable Heap where
    pprPrec prec (Heap h _) = pprPrec prec h


-- INVARIANT: no adjacent frames in the patterns:
--  Cast, Cast
--  Update, Update
--  Update, Cast, Update
-- NB: Cast, Update, Cast *is* allowed
type Stack = Train (Tagged StackFrame) Generalised
type StackFrame = StackFrameG (Anned (Coerced Answer)) (In AnnedTerm) (In [AnnedAlt])
data StackFrameG answer term alts = TyApply (Out Type)
                                  | CoApply (Out NormalCo)
                                  | Apply (Out Id)
                                  | Scrutinise (Out Id) (Out Type) alts
                                  | PrimApply PrimOp [Out Type] [answer] [term]
                                  | StrictLet (Out Id) term
                                  | Update (Out Id)
                                  | CastIt (Out NormalCo)

instance Outputable StackFrame where
    pprPrec prec kf = case kf of
        TyApply ty'                    -> pPrintPrecApp prec (PrettyDoc $ text "[_]") ty'
        CoApply co'                    -> pPrintPrecApp prec (PrettyDoc $ text "[_]") co'
        Apply x'                       -> pPrintPrecApp prec (PrettyDoc $ text "[_]") x'
        Scrutinise x' _ty in_alts      -> pPrintPrecCase prec (PrettyDoc $ text "[_]") x' (pPrintPrecAnnedAlts in_alts)
        PrimApply pop tys' in_vs in_es -> pPrintPrecPrimOp prec pop tys' (map (PrettyFunction . flip pPrintPrecAnnedCastAnswer) in_vs ++ map (PrettyFunction . flip pPrintPrecAnnedTerm) in_es)
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
    PrimApply pop tys in_as in_es -> ((primOpType pop `applyTys` tys) `applyFunTys` map (coercedType answerType . annee) in_as) `applyFunTy` hole_ty `applyFunTys` map (\in_e@(rn, e) -> termType (renameAnnedTerm (mkInScopeSet (inFreeVars annedFreeVars in_e)) rn e)) in_es
    StrictLet _ in_e@(rn, e)      -> termType (renameAnnedTerm (mkInScopeSet (inFreeVars annedFreeVars in_e)) rn e)
    Update _                      -> hole_ty
    CastIt co                     -> pSnd (coercionKind co)

qaType :: Anned QA -> Type
qaType anned_qa = case caseAnnedQA anned_qa of
    Left  anned_q -> idType (extract anned_q)
    Right anned_a -> answerType (extract anned_a)

coercedType :: (a -> Type)
            -> Coerced a -> Type
coercedType typ (Uncast,      e) = typ e
coercedType _   (CastBy co _, _) = pSnd (coercionKind co)

answerType :: Answer -> Type
answerType (rn, v) = valueType (renameAnnedValue' (mkInScopeSet (inFreeVars annedValueFreeVars' (rn, v))) rn v)


heapBindingTerm :: HeapBinding -> Maybe (In AnnedTerm)
heapBindingTerm = either (const Nothing) Just . heapBindingMeaning

heapBindingLambdaBoundness :: HeapBinding -> Maybe Generalised
heapBindingLambdaBoundness = either (either Just (const Nothing)) (const Nothing) . heapBindingMeaning

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
heapSize (Heap h _) = pureHeapSize h

pureHeapSize :: PureHeap -> Size
pureHeapSize = sum . map heapBindingSize . M.elems

stackSize :: Stack -> Size
stackSize = trainCarFoldl' (\size kf -> size + stackFrameSize (tagee kf)) 0

stateSize :: State -> Size
stateSize (_, h, k, qa) = heapSize h + stackSize k + annedSize qa

unnormalisedStateSize :: UnnormalisedState -> Size
unnormalisedStateSize (_, h, k, (_, e)) = heapSize h + stackSize k + annedSize e


-- Detects three kinds of state:
--  1. < H | v | >
--  2. < H, x |-> v | x | >
--  3. < H | x | K > (x \notin |dom|(H))
--
-- Note that in the final case it is OK for x to either be free or to be bound by the stack
isStateIrreducible :: State -> Bool
isStateIrreducible (_, Heap h _, k, anned_qa) = case annee anned_qa of
    Answer _    -> stack_empty
    Question x' -> maybe True (\hb -> case heapBindingMeaning hb of Left _ -> True; Right (_, e) -> termIsValue e && stack_empty) $ M.lookup x' h
  where stack_empty = isJust $ isCastStack_maybe k

isStackEmpty :: Stack -> Bool
isStackEmpty (Loco _) = True
isStackEmpty _        = False

isPureHeapEmpty :: PureHeap -> Bool
isPureHeapEmpty = Foldable.all (isJust . heapBindingLambdaBoundness)


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


isCastStack_maybe :: Stack -> Maybe CastBy
isCastStack_maybe = fmap fst . isTrivialStack_maybe

isTrivialStack_maybe :: Stack -> Maybe (CastBy, Maybe (Tagged Var, CastBy))
isTrivialStack_maybe k = case k of
    (Tagged tg (CastIt co) `Car` k) -> fmap ((,) (CastBy co tg)) $ isTrivialValueStack_maybe k
    _                               -> fmap ((,) Uncast)         $ isTrivialValueStack_maybe k
  where
    isTrivialValueStack_maybe :: Stack -> Maybe (Maybe (Tagged Var, CastBy))
    isTrivialValueStack_maybe k = case peelValueStack k of
        (mb_peeled, Loco _) -> Just mb_peeled
        _                   -> Nothing

    peelValueStack :: Stack -> (Maybe (Tagged Var, CastBy), Stack)
    peelValueStack (Tagged x_tg (Update x) `Car` Tagged co_tg (CastIt co) `Car` k) = (Just (Tagged x_tg x, CastBy co co_tg), k)
    peelValueStack (Tagged x_tg (Update x) `Car` k)                                = (Just (Tagged x_tg x, Uncast), k)
    peelValueStack k                                                               = (Nothing, k)

peelUpdateStack :: Stack -> (Maybe (CastBy, Tagged Var), Stack)
peelUpdateStack (Tagged co_tg (CastIt co) `Car` Tagged x_tg (Update x) `Car` k) = (Just (CastBy co co_tg, Tagged x_tg x), k)
peelUpdateStack (Tagged x_tg (Update x) `Car` k)                                = (Just (Uncast, Tagged x_tg x), k)
peelUpdateStack k                                                               = (Nothing, k)


-- NB: I used to source the tag for the positive information from the tag of the case branch RHS, but that
-- leads to WAY TOO MUCH specialisation for examples like gen_regexps because we get lots of e.g. cons cells
-- that are all given different tags.
altConToValue :: Type -> AltCon -> Maybe (Anned AnnedValue)
altConToValue ty (DataAlt dc as qs xs) = do
    (_, univ_tys) <- splitTyConApp_maybe ty
    Just (annedValue (dataConTag dc) (Data dc (univ_tys ++ map mkTyVarTy as) (map mkCoVarCo qs) xs))
altConToValue _  (LiteralAlt l) = Just (annedValue (literalTag l) (Literal l))
altConToValue _  DefaultAlt     = Nothing -- NB: could actually put an indirection in the heap in this case, for fun..

zapAltConIdOccInfo :: AltCon -> AltCon
zapAltConIdOccInfo (DataAlt dc as qs xs) = DataAlt dc as qs (map zapIdOccInfo xs)
zapAltConIdOccInfo (LiteralAlt l)        = LiteralAlt l
zapAltConIdOccInfo DefaultAlt            = DefaultAlt


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
