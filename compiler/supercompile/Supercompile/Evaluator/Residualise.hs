module Supercompile.Evaluator.Residualise (
    residualiseState, residualiseHeapBinding,
    
    pPrintHeap,

    StatePrettiness(..), fullStatePrettiness, quietStatePrettiness,
    pPrintFullState, pPrintFullUnnormalisedState
  ) where

import Supercompile.Evaluator.Deeds
import Supercompile.Evaluator.Syntax

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming
import Supercompile.Core.Syntax

import Supercompile.Utilities

import Var (isLocalId)

import Data.Either
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Ord


residualiseState :: State -> (Deeds, Out [(Var, PrettyFunction)], Out FVedTerm, Generalised)
residualiseState s = (deeds, floats_static, bindManyMixedLiftedness fvedTermFreeVars floats_nonstatic e, gen)
 where (deeds, floats_static, floats_nonstatic, e, gen) = residualiseUnnormalisedState (denormalise s)

residualiseUnnormalisedState :: UnnormalisedState -> (Deeds, Out [(Var, PrettyFunction)], Out [(Var, FVedTerm)], Out FVedTerm, Generalised)
residualiseUnnormalisedState (deeds, heap, k, in_e) = (deeds, floats_static, floats_nonstatic, e, gen)
  where (floats_static, floats_nonstatic, e, gen) = residualiseHeap heap (\ids -> residualiseStack ids k (residualiseTerm ids in_e))

residualiseAnswer :: InScopeSet -> Answer -> Out FVedTerm
residualiseAnswer ids = fvedTerm . detagAnnedTerm' . answerToAnnedTerm' ids

residualiseTerm :: InScopeSet -> In AnnedTerm -> Out FVedTerm
residualiseTerm ids = detagAnnedTerm . renameIn (renameAnnedTerm ids)

residualiseHeap :: Heap -> (InScopeSet -> ((Out [(Var, PrettyFunction)], Out [(Var, FVedTerm)]), Out FVedTerm, Generalised)) -> (Out [(Var, PrettyFunction)], Out [(Var, FVedTerm)], Out FVedTerm, Generalised)
residualiseHeap (Heap h ids) resid_body = (floats_static_h ++ floats_static_k, floats_nonstatic_h ++ floats_nonstatic_k, e, gen)
  where (floats_static_h, floats_nonstatic_h) = residualisePureHeap ids h
        ((floats_static_k, floats_nonstatic_k), e, gen) = resid_body ids

residualisePureHeap :: InScopeSet -> PureHeap -> (Out [(Var, PrettyFunction)], Out [(Var, FVedTerm)])
residualisePureHeap ids h = partitionEithers [fmapEither ((,) x') ((,) x') (residualiseHeapBinding ids hb) | (x', hb) <- M.toList h]

residualiseHeapBinding :: InScopeSet -> HeapBinding -> Either (Out PrettyFunction) (Out FVedTerm)
residualiseHeapBinding ids (HB InternallyBound (Right in_e)) = Right (residualiseTerm ids in_e)
residualiseHeapBinding _   hb                                = Left (asPrettyFunction hb)

residualiseStack :: InScopeSet -> Stack -> Out FVedTerm -> ((Out [(Var, PrettyFunction)], Out [(Var, FVedTerm)]), Out FVedTerm, Generalised)
residualiseStack _   (Loco gen) e_body = (([], []), e_body, gen)
residualiseStack ids (Car kf k) e_body = first3 ((static_floats ++) *** (nonstatic_floats ++)) $ residualiseStack ids k e
  where ((static_floats, nonstatic_floats), e) = residualiseStackFrame ids (tagee kf) e_body

residualiseStackFrame :: InScopeSet -> StackFrame -> Out FVedTerm -> ((Out [(Var, PrettyFunction)], Out [(Var, FVedTerm)]), Out FVedTerm)
residualiseStackFrame _   (TyApply ty')               e  = (([], []), e `tyApp` ty')
residualiseStackFrame _   (CoApply co')               e  = (([], []), e `coApp` co')
residualiseStackFrame _   (Apply x2')                 e1 = (([], []), e1 `app` x2')
residualiseStackFrame ids (Scrutinise x' ty in_alts)  e  = (([], []), case_ e x' ty (detagAnnedAlts $ renameIn (renameAnnedAlts ids) in_alts))
residualiseStackFrame ids (PrimApply pop tys' as es') e  = (([], []), primOp pop tys' (map (residualiseAnswer ids . annee) as ++ e : map (residualiseTerm ids) es'))
residualiseStackFrame ids (StrictLet x' in_e2)        e1 = (([], []), let_ x' e1 (residualiseTerm ids in_e2))
residualiseStackFrame _   (Update x')                 e  = (([], [(x', e)]), var x')
residualiseStackFrame _   (CastIt co')                e  = (([], []), e `cast` co')


pPrintHeap :: Heap -> SDoc
pPrintHeap (Heap h ids) = pPrint $ map (first (PrettyDoc . pPrintBndr LetBind)) $ floats_static_h ++ [(x, asPrettyFunction1 e) | (x, e) <- floats_nonstatic_h]
  where (floats_static_h, floats_nonstatic_h) = residualisePureHeap ids h

data StatePrettiness = SP { includeLams :: Bool, includeStatics :: Bool, excludeBindings :: S.Set Var }

fullStatePrettiness, quietStatePrettiness :: StatePrettiness
fullStatePrettiness = SP True True S.empty
quietStatePrettiness = SP False False S.empty

pPrintFullState :: StatePrettiness -> State -> SDoc
pPrintFullState sp = pPrintFullUnnormalisedState sp . denormalise

pPrintFullUnnormalisedState :: StatePrettiness -> UnnormalisedState -> SDoc
pPrintFullUnnormalisedState sp state
  = {-# SCC "pPrintFullUnnormalisedState" #-}
    text "Deeds:" <+> pPrint deeds $$ (if includeStatics sp then pPrint (map (first (PrettyDoc . pPrintBndr LetBind)) floats_static) else empty) $$ body $$ (if null floats_nonstatic_excluded then empty else ppr (S.fromList (map fst floats_nonstatic_excluded)))
  where (deeds, floats_static, floats_nonstatic_unfiltered, e, gen) = residualiseUnnormalisedState state
        (floats_nonstatic_excluded, floats_nonstatic) = partition (flip S.member (excludeBindings sp) . fst) floats_nonstatic_unfiltered
        floats_nonstatic_pretty
          | includeLams sp = map (second asPrettyFunction) floats_nonstatic
          | otherwise      = map snd $ sortBy (comparing (Down . fst)) $
                               [(non_lam, (x, if non_lam then asPrettyFunction e
                                                         else PrettyFunction (\_ -> text "..." <+> braces (hsep [ppr x <> char ',' | x <- varSetElems (fvedTermFreeVars e), isLocalId x]))))
                               | (x, e) <- floats_nonstatic
                               , let non_lam = case extract e of Value (Lambda _ _) -> False; Value (TyLambda _ _) -> False; _ -> True]
        body = pPrintPrecWhere noPrec floats_nonstatic_pretty (PrettyDoc ((if includeStatics sp && gen then char '?' else empty) <> angleBrackets (pPrint e)))
