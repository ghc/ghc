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
import Type     (applyTy, applyTys, mkForAllTy, mkFunTy, splitFunTy, eqType)
import Pair     (pSnd)
import DataCon  (dataConWorkId)
import Literal  (literalType)
import Coercion (coercionKind, coercionType)

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

renameAnned :: In (Anned a) -> Anned (In a)
renameAnned (rn, Comp (Tagged tg (Comp (Sized sz (FVed fvs x)))))
  = Comp (Tagged tg (Comp (Sized sz (FVed (renameFreeVars rn fvs) (rn, x)))))


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

detagAnnedTerm = taggedSizedFVedTermToFVedTerm
detagAnnedTerm' = taggedSizedFVedTermToFVedTerm'
detagAnnedValue = taggedSizedFVedValueToFVedValue
detagAnnedValue' = taggedSizedFVedValue'ToFVedValue'
detagAnnedAlts = taggedSizedFVedAltsToFVedAlts


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
termToAnswer iss in_anned_e = flip traverse (renameAnned in_anned_e) $ \(rn, e) -> case e of
    Value v          -> Just (Nothing, (rn, v))
    Cast anned_e' co -> case extract anned_e' of
        Value v -> Just (Just (renameCoercion iss rn co, annedTag anned_e'), (rn, v))
        _       -> Nothing
    _ -> Nothing

data QA = Question (Out Id)
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

pPrintPrecAnswer :: (Outputable a) => Rational -> (Maybe (Coercion, Tag), a) -> SDoc
pPrintPrecAnswer prec (Nothing,       v) = pPrintPrec prec v
pPrintPrecAnswer prec (Just (co, tg), v) = pPrintPrecCast prec (Tagged tg v) co

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

letBound :: In AnnedTerm -> HeapBinding
letBound in_e = HB LetBound (Right in_e)

-- The Heap might contain bindings for TyVars as well, but will only map them to lambdaBound
type PureHeap = M.Map (Out Var) HeapBinding
data Heap = Heap PureHeap InScopeSet

instance Outputable Heap where
    pprPrec prec (Heap h _) = pprPrec prec h


type Stack = [Tagged StackFrame]
data StackFrame = Apply (Out Id)
                | TyApply (Out Type)
                | Scrutinise (Out Id) (Out Type) (In [AnnedAlt])
                | PrimApply PrimOp [Out Type] [Anned Answer] [In AnnedTerm]
                | StrictLet (Out Id) (In AnnedTerm)
                | Update (Out Id)
                | CastIt (Out Coercion)

instance Outputable StackFrame where
    pprPrec prec kf = case kf of
        Apply x'                       -> pPrintPrecApp prec (PrettyDoc $ text "[_]") x'
        TyApply ty'                    -> pPrintPrecApp prec (PrettyDoc $ text "[_]") ty'
        Scrutinise x' _ty in_alts      -> pPrintPrecCase prec (PrettyDoc $ text "[_]") x' (pPrintPrecAnnedAlts in_alts)
        PrimApply pop tys' in_vs in_es -> pPrintPrecPrimOp prec pop tys' (map (PrettyFunction . flip pPrintPrecAnnedAnswer) in_vs ++ map (PrettyFunction . flip pPrintPrecAnnedTerm) in_es)
        StrictLet x' in_e2             -> pPrintPrecLet prec x' (PrettyDoc $ text "[_]") (PrettyFunction $ flip pPrintPrecAnnedTerm in_e2)
        Update x'                      -> pPrintPrecApp prec (PrettyDoc $ text "update") x'
        CastIt co'                     -> pPrintPrecCast prec (PrettyDoc $ text "[_]") co'


stateType :: State -> Type
stateType (_, _, k, qa) = stackType k (qaType qa)

stackType :: Stack -> Type -> Type
stackType k ty = foldl' (flip stackFrameType) ty k

stackFrameType :: Tagged StackFrame -> Type -> Type
stackFrameType kf hole_ty = case tagee kf of
    Apply x                       -> hole_ty `applyFunTy` idType x
    TyApply ty                    -> hole_ty `applyTy` ty
    Scrutinise _ ty _             -> ty
    PrimApply pop tys in_as in_es -> ((primOpType pop `applyTys` tys) `applyFunTys` map answerType in_as) `applyFunTys` map (\in_e@(rn, e) -> termType (renameAnnedTerm (mkInScopeSet (inFreeVars annedFreeVars in_e)) rn e)) in_es
    StrictLet _ in_e@(rn, e)      -> termType (renameAnnedTerm (mkInScopeSet (inFreeVars annedFreeVars in_e)) rn e)
    Update _                      -> hole_ty
    CastIt co                     -> pSnd (coercionKind co)

qaType :: Anned QA -> Type
qaType qa = case annee qa of
    Question x' -> idType x'
    Answer a    -> answerType (fmap (const a) qa)

answerType :: Anned Answer -> Type
answerType a = case annee a of
    (Just (co, _), _)       -> pSnd (coercionKind co)
    (Nothing,      (rn, v)) -> valueType (renameAnnedValue' (mkInScopeSet (annedFreeVars a)) rn v)

valueType :: Copointed ann => ValueF ann -> Type
valueType (Indirect x)    = idType x
valueType (TyLambda x e)  = x `mkForAllTy` termType e
valueType (Lambda x e)    = idType x `mkFunTy` termType e
valueType (Data dc as xs) = (idType (dataConWorkId dc) `applyTys` as) `applyFunTys` map idType xs
valueType (Literal l)     = literalType l
valueType (Coercion co)   = coercionType co

termType :: Copointed ann => ann (TermF ann) -> Type
termType e = case extract e of
    Var x             -> idType x
    Value v           -> valueType v
    App e x           -> termType e `applyFunTy` idType x
    TyApp e a         -> termType e `applyTy` a
    PrimOp pop tys es -> (primOpType pop `applyTys` tys) `applyFunTys` map termType es
    Case _ _ ty _     -> ty
    Let _ _ e         -> termType e
    LetRec _ e        -> termType e
    Cast _ co         -> pSnd (coercionKind co)

applyFunTy :: Type -> Type -> Type
applyFunTy fun_ty got_arg_ty = ASSERT2(got_arg_ty `eqType` expected_arg_ty, text "applyFunTy:" <+> ppr got_arg_ty <+> ppr expected_arg_ty) res_ty
  where (expected_arg_ty, res_ty) = splitFunTy fun_ty

applyFunTys :: Type -> [Type] -> Type
applyFunTys = foldl' applyFunTy


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
    PrimApply _ _ as in_es   -> sum (map annedSize as ++ map (annedTermSize . snd) in_es)
    StrictLet _ (_, e)       -> annedTermSize e
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
