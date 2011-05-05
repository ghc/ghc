{-# LANGUAGE PatternGuards #-}
module Supercompile.Evaluator.FreeVars (
    inFreeVars,
    heapBindingFreeVars,
    pureHeapBoundVars, stackBoundVars, stackFrameBoundVars, stackFrameFreeVars,
    pureHeapVars, stateFreeVars, stateAllFreeVars, stateLetBounders, stateLambdaBounders, stateInternalBounders, stateUncoveredVars,
    module Supercompile.Core.FreeVars
  ) where

import Supercompile.Evaluator.Syntax

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming

import Supercompile.Utilities

import qualified Data.Map as M


-- | Finds the set of things "referenced" by a 'HeapBinding': this is only used to construct tag-graphs
heapBindingFreeVars :: HeapBinding -> FreeVars
heapBindingFreeVars = maybe emptyVarSet (inFreeVars annedTermFreeVars) . heapBindingTerm

-- | Returns all the variables bound by the heap that we might have to residualise in the splitter
pureHeapBoundVars :: PureHeap -> BoundVars
pureHeapBoundVars = mkVarSet . M.keys -- I think its harmless to include variables bound by phantoms in this set

-- | Returns all the variables bound by the stack that we might have to residualise in the splitter
stackBoundVars :: Stack -> BoundVars
stackBoundVars = unionVarSets . map (stackFrameBoundVars . tagee)

stackFrameBoundVars :: StackFrame -> BoundVars
stackFrameBoundVars = fst . stackFrameOpenFreeVars

stackFrameFreeVars :: StackFrame -> FreeVars
stackFrameFreeVars = snd . stackFrameOpenFreeVars

stackFrameOpenFreeVars :: StackFrame -> (BoundVars, FreeVars)
stackFrameOpenFreeVars kf = case kf of
    Apply x'                 -> (emptyVarSet, unitVarSet x')
    TyApply ty'              -> (emptyVarSet, tyVarsOfType ty')
    Scrutinise x' ty in_alts -> (emptyVarSet, (inFreeVars annedAltsFreeVars in_alts `delVarSet` x') `unionVarSet` tyVarsOfType ty)
    PrimApply _ as in_es     -> (emptyVarSet, unionVarSets (map annedFreeVars as) `unionVarSet` unionVarSets (map (inFreeVars annedTermFreeVars) in_es))
    Update x'                -> (unitVarSet x', emptyVarSet)
    CastIt co'               -> (emptyVarSet, tyVarsOfType co')


-- | Computes the variables bound and free in a state
stateVars :: (Heap, Stack, In (Anned a)) -> (HowBound -> BoundVars, FreeVars)
pureHeapVars :: PureHeap -> (HowBound -> BoundVars, FreeVars)
(stateVars, pureHeapVars) = (\(Heap h _, k, in_e) -> finish $ pureHeapOpenFreeVars h (stackOpenFreeVars k (inFreeVars annedFreeVars in_e)),
                             \h -> finish $ pureHeapOpenFreeVars h (emptyVarSet, emptyVarSet))
  where
    finish ((bvs_internal, bvs_lambda, bvs_let), fvs) = (\how -> case how of InternallyBound -> bvs_internal; LambdaBound -> bvs_lambda; LetBound -> bvs_let, fvs)
    
    pureHeapOpenFreeVars :: PureHeap -> (BoundVars, FreeVars) -> ((BoundVars, BoundVars, BoundVars), FreeVars)
    pureHeapOpenFreeVars h (bvs_internal, fvs) = (\f -> M.foldrWithKey f ((bvs_internal, emptyVarSet, emptyVarSet), fvs) h) $ \x' hb ((bvs_internal, bvs_lambda, bvs_let), fvs) -> (case howBound hb of
        InternallyBound -> (bvs_internal `extendVarSet` x', bvs_lambda, bvs_let)
        LambdaBound     -> (bvs_internal, bvs_lambda `extendVarSet` x', bvs_let)
        LetBound        -> (bvs_internal, bvs_lambda, bvs_let `extendVarSet` x'),
        fvs `unionVarSet` heapBindingFreeVars hb)
    
    stackOpenFreeVars :: Stack -> FreeVars -> (BoundVars, FreeVars)
    stackOpenFreeVars k fvs = (unionVarSets *** (unionVarSet fvs . unionVarSets)) . unzip . map (stackFrameOpenFreeVars . tagee) $ k


-- | Returns (an overapproximation of) the free variables that the state would have if it were residualised right now (i.e. variables bound by phantom bindings *are* in the free vars set)
stateFreeVars :: (Heap, Stack, In (Anned a)) -> FreeVars
stateFreeVars s = fvs `minusVarSet` bvs InternallyBound
  where (bvs, fvs) = stateVars s

stateAllFreeVars :: (Heap, Stack, In (Anned a)) -> FreeVars
stateAllFreeVars = snd . stateVars

stateLetBounders :: (Heap, Stack, In (Anned a)) -> BoundVars
stateLetBounders = ($ LetBound) . fst . stateVars

stateLambdaBounders :: (Heap, Stack, In (Anned a)) -> BoundVars
stateLambdaBounders = ($ LambdaBound) . fst . stateVars

stateInternalBounders :: (Heap, Stack, In (Anned a)) -> BoundVars
stateInternalBounders = ($ InternallyBound) . fst . stateVars

stateUncoveredVars :: (Heap, Stack, In (Anned a)) -> FreeVars
stateUncoveredVars s = fvs `minusVarSet` bvs InternallyBound `minusVarSet` bvs LetBound `minusVarSet` bvs LambdaBound
  where (bvs, fvs) = stateVars s
