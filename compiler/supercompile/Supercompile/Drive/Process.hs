{-# LANGUAGE BangPatterns, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Supercompile.Drive.Process (SCStats(..), supercompile) where

#include "HsVersions.h"

import Supercompile.Drive.Match
import Supercompile.Drive.Split

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming
--import Supercompile.Core.Size
import Supercompile.Core.Syntax
import Supercompile.Core.Tag

import Supercompile.Evaluator.Deeds
import Supercompile.Evaluator.Evaluate
import Supercompile.Evaluator.FreeVars
import Supercompile.Evaluator.Residualise
import Supercompile.Evaluator.Syntax

import Supercompile.Termination.Combinators
--import Supercompile.Termination.Extras
--import Supercompile.Termination.TagSet
import Supercompile.Termination.TagBag
--import Supercompile.Termination.TagGraph
import Supercompile.Termination.Generaliser

import Supercompile.StaticFlags
import Supercompile.Utilities

import Var        (isTyVar)
import Id         (mkLocalId)
import Name       (Name, mkSystemVarName)
import FastString (mkFastString)
import CoreUtils  (mkPiTypes)

import qualified Data.Foldable as Foldable
import qualified Data.Map as M
import Data.Monoid
import Data.Ord
import qualified Data.Set as S


pprTraceSC :: String -> SDoc -> a -> a
pprTraceSC = pprTrace
--pprTraceSC _ _ x = x


-- The termination argument is a but subtler due to HowBounds but I think it still basically works.
-- Key to the modified argument is that tieback cannot be prevented by any HeapBinding with HowBound /= LambdaBound:
-- so we have to be careful to record tags on those guys.
rEDUCE_WQO :: TTest State
rEDUCE_WQO = wQO
-- rEDUCE_WQO | not rEDUCE_TERMINATION_CHECK = postcomp (const generaliseNothing) unsafeNever
--            | otherwise                    = wQO

wQO :: TTest State
mK_GENERALISER :: State -> State -> Generaliser
(wQO, mK_GENERALISER) = embedWithTagBags tAG_COLLECTION
-- wQO = wqo2
--   where
--     wqo0 = case tAG_COLLECTION of TagBag tbt -> embedWithTagBags tbt
--                                   TagGraph   -> embedWithTagGraphs
--                                   TagSet     -> embedWithTagSets
--     wqo1 | pARTITIONED_REFINEMENT = partitionedRefinement wqo0
--          | otherwise              = wqo0
--     wqo2 | sUB_GRAPHS = subGraphGeneralisation wqo1
--          | otherwise  = wqo1


data SCStats = SCStats {
    stat_reduce_stops :: !Int,
    stat_sc_stops :: !Int
  }

seqSCStats :: SCStats -> a -> a
seqSCStats (SCStats !_ !_) x = x

instance Monoid SCStats where
    mempty = SCStats {
        stat_reduce_stops = 0,
        stat_sc_stops = 0
      }
    stats1 `mappend` stats2 = SCStats {
        stat_reduce_stops = stat_reduce_stops stats1 + stat_reduce_stops stats2,
        stat_sc_stops = stat_sc_stops stats1 + stat_sc_stops stats2
      }


supercompile :: M.Map Var Term -> Term -> (SCStats, Term)
supercompile unfoldings e = pprTraceSC "all input FVs" (ppr input_fvs) $ second fVedTermToTerm $ runScpM $ liftM snd $ sc (mkHistory (cofmap fst wQO)) S.empty state
  where anned_e = toAnnedTerm tag_ids e
        input_fvs = annedTermFreeVars anned_e
        state = normalise ((bLOAT_FACTOR - 1) * annedSize anned_e, Heap (M.fromDistinctAscList anned_h_kvs) (mkInScopeSet input_fvs), [], (mkIdentityRenaming input_fvs, anned_e))
        
        (tag_ids, anned_h_kvs) = mapAccumL add_one_heap_binding tagUniqSupply (varSetElems input_fvs)
          where add_one_heap_binding tag_ids0 x' = (tag_ids2, (x', hb))
                  where (hb, tag_ids2) = case M.lookup x' unfoldings of
                                            Nothing | let (i, tag_ids1) = takeUniqFromSupply tag_ids0
                                                    -> (environmentallyBound (mkTag (getKey i)), tag_ids1)
                                            Just e  | let (tag_unf_ids, tag_ids1) = splitUniqSupply tag_ids0
                                                          anned_e = toAnnedTerm tag_unf_ids e
                                                    -> (letBound (mkIdentityRenaming (annedFreeVars anned_e), anned_e), tag_ids1)

--
-- == Bounded multi-step reduction ==
--

-- We used to garbage-collect in the evaluator, when we executed the rule for update frames. This had two benefits:
--  1) We don't have to actually update the heap or even claim a new deed
--  2) We make the supercompiler less likely to terminate, because GCing so tends to reduce TagBag sizes
--
-- However, this caused problems with speculation: to prevent incorrectly garbage collecting bindings from the invisible "enclosing"
-- heap when we speculated one of the bindings from the heap, we had to pass around an extra "live set" of parts of the heap that might
-- be referred to later on. Furthermore:
--  * Finding FVs when executing every update step was a bit expensive (though they were memoized on each of the State components)
--  * This didn't GC cycles (i.e. don't consider stuff from the Heap that was only referred to by the thing being removed as "GC roots")
--  * It didn't seem to make any difference to the benchmark numbers anyway
--
-- You might think a good alternative approach is to:
-- 1. Drop dead update frames in transitiveInline (which is anyway responsible for ensuring there is no dead stuff in the stack)
-- 2. "Squeeze" just before the matcher: this shorts out indirections-to-indirections and does update-frame stack squeezing.
--    You might also think that it would be cool to just do this in normalisation, but then when normalising during specualation the enclosing
--    context wouldn't get final_rned :-(
--
-- HOWEVER. That doesn't work properly because normalisation itself can introduce dead bindings - i.e. in order to be guaranteed to
-- catch all the junk we have to GC normalised bindings, not the pre-normalised ones that transitiveInline sees. So instead I did
-- both points 1 and 2 right just before we go to the matcher.
--
-- HOWEVER. Simon suggested something that made me realise that actually we could do squeezing of consecutive update frames and
-- indirection chains in the evaluator (and thus the normaliser) itself, which is even cooler. Thus all that is left to do in the
-- GC is to make a "global" analysis that drops stuff that is definitely dead. We *still* want to run this just before the matcher because
-- although dead heap bindings don't bother it, it would be confused by dead update frames.
--
-- TODO: have the garbage collector collapse (let x = True in x) to (True) -- but note that this requires onceness analysis
gc :: State -> (PureHeap, State)
gc _state@(deeds0, Heap h ids, k, in_e) = ASSERT2(isEmptyVarSet (stateUncoveredVars gced_state), ppr ("gc", stateUncoveredVars gced_state, PrettyDoc (pPrintFullState _state), PrettyDoc (pPrintFullState gced_state)))
                                          (h_dead, gced_state)
  where
    gced_state = (deeds2, Heap h' ids, k', in_e)
    
    -- We have to use stateAllFreeVars here rather than stateFreeVars because in order to safely prune the live stack we need
    -- variables bound by k to be part of the live set if they occur within in_e or the rest of the k
    live0 = stateAllFreeVars (deeds0, Heap M.empty ids, k, in_e)
    (deeds1, h_dead, h', live1) = inlineLiveHeap deeds0 h live0
    -- Collecting dead update frames doesn't make any new heap bindings dead since they don't refer to anything
    (deeds2, k') = pruneLiveStack deeds1 k live1
    
    inlineLiveHeap :: Deeds -> PureHeap -> FreeVars -> (Deeds, PureHeap, PureHeap, FreeVars)
    inlineLiveHeap deeds h live = (deeds `releasePureHeapDeeds` h_dead, h_dead, h_live, live')
      where
        (h_dead, h_live, live') = heap_worker h M.empty live
        
        -- This is just like Split.transitiveInline, but simpler since it never has to worry about running out of deeds:
        heap_worker :: PureHeap -> PureHeap -> FreeVars -> (PureHeap, PureHeap, FreeVars)
        heap_worker h_pending h_output live
          = if live == live'
            then (h_pending', h_output', live')
            else heap_worker h_pending' h_output' live'
          where 
            (h_pending_kvs', h_output', live') = M.foldrWithKey consider_inlining ([], h_output, live) h_pending
            h_pending' = M.fromDistinctAscList h_pending_kvs'
        
            consider_inlining x' hb (h_pending_kvs, h_output, live)
              | x' `elemVarSet` live = (h_pending_kvs,            M.insert x' hb h_output, live `unionVarSet` heapBindingFreeVars hb)
              | otherwise            = ((x', hb) : h_pending_kvs, h_output,                live)
    
    pruneLiveStack :: Deeds -> Stack -> FreeVars -> (Deeds, Stack)
    pruneLiveStack deeds k live = (deeds `releaseStackDeeds` k_dead, k_live)
      where (k_live, k_dead) = partition (\kf -> case tagee kf of Update x' -> x' `elemVarSet` live; _ -> True) k


type AlreadySpeculated = S.Set Var

-- Note [Order of speculation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- It is quite important that are insensitive to dependency order. For example:
--
--  let id x = x
--      idish = id id
--  in e
--
-- If we speculated idish first without any information about what id is, it will be irreducible. If we do it the other way
-- around (or include some information about id) then we will get a nice lambda. This is why we speculate each binding with
-- *the entire rest of the heap* also present.
--
-- Note [Nested speculation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Naturally, we want to speculate the nested lets that may arise from the speculation process itself. For example, when
-- speculating this:
--
-- let x = let y = 1 + 1
--         in Just y
-- in \z -> ...
--
-- After we speculate to discover the Just we want to speculate the (1 + 1) computation so we can push it down into the lambda
-- body along with the enclosing Just.
--
-- We do this by adding new let-bindings arising from speculation to the list of pending bindings. Importantly, we add them
-- to the end of the pending list to implement a breadth-first search. This is important so that we tend to do more speculation
-- towards the root of the group of let-bindings (which seems like a reasonable heuristic).
--
-- The classic important case to think about is:
--
-- let ones = 1 : ones
--     xs = map (+1) ones
--
-- Speculating xs gives us:
--
-- let ones = 1 : ones
--     xs = x0 : xs0
--     x0 = 1 + 1
--     xs0 = map (+1) ones
--
-- We can speculate x0 easily, but speculating xs0 gives rise to a x1 and xs1 of the same form. We must avoid looping here.
speculate :: AlreadySpeculated -> (SCStats, State) -> (AlreadySpeculated, (SCStats, State))
speculate speculated (stats, (deeds, Heap h ids, k, in_e)) = (M.keysSet h, (stats', (deeds', Heap (h_non_values_speculated `M.union` h_speculated_ok `M.union` h_speculated_failure) ids', k, in_e)))
  where
    (h_values, h_non_values) = M.partition (maybe False (termIsValue . snd) . heapBindingTerm) h
    (h_non_values_unspeculated, h_non_values_speculated) = (h_non_values `exclude` speculated, h_non_values `restrict` speculated)

    (stats', deeds', h_speculated_ok, h_speculated_failure, ids') = runSpecM (speculateManyMap (mkHistory (cofmap fst wQO)) h_non_values_unspeculated) (stats, deeds, h_values, M.empty, ids)
    
    speculateManyMap hist = speculateMany hist . concatMap M.toList . topologicalSort heapBindingFreeVars
    speculateMany hist = mapM_ (speculateOne hist)
    
    speculateOne :: History (State, SpecM ()) -> (Out Var, HeapBinding) -> SpecM ()
    speculateOne hist (x', hb)
      | HB InternallyBound (Right in_e) <- hb
      = (\rb -> try_speculation in_e rb) `catchSpecM` speculation_failure
      | otherwise
      = speculation_failure
      where
        speculation_failure = modifySpecState $ \(stats, deeds, h_speculated_ok, h_speculated_failure, ids) -> ((stats, deeds, h_speculated_ok, M.insert x' hb h_speculated_failure, ids), ())
        try_speculation in_e rb = do
          let go no_change@(stats, deeds, h_speculated_ok, h_speculated_failure, ids) = case terminate hist (state, rb) of
                  Stop (_old_state, rb) -> (no_change, rb)
                  Continue hist -> case reduce state of
                      (extra_stats, (deeds, Heap h_speculated_ok' ids, [], qa))
                        | Answer a <- annee qa
                        , let h_unspeculated = h_speculated_ok' M.\\ h_speculated_ok
                              in_e' = annedAnswerToAnnedTerm (mkInScopeSet (annedFreeVars qa)) (fmap (const a) qa)
                        -> ((stats `mappend` extra_stats, deeds, M.insert x' (internallyBound in_e') h_speculated_ok, h_speculated_failure, ids), speculateManyMap hist h_unspeculated)
                      _ -> (no_change, speculation_failure)
                where state = normalise (deeds, Heap h_speculated_ok ids, [], in_e)
          modifySpecState go >>= id

type SpecState = (SCStats, Deeds, PureHeap, PureHeap, InScopeSet)
newtype SpecM a = SpecM { unSpecM :: SpecState -> (SpecState -> a -> SpecState) -> SpecState }

instance Functor SpecM where
    fmap = liftM

instance Monad SpecM where
    return x = SpecM $ \s k -> k s x
    mx >>= fxmy = SpecM $ \s k -> unSpecM mx s (\s x -> unSpecM (fxmy x) s k)

modifySpecState :: (SpecState -> (SpecState, a)) -> SpecM a
modifySpecState f = SpecM $ \s k -> case f s of (s, x) -> k s x

runSpecM :: SpecM () -> SpecState -> SpecState
runSpecM spec state = unSpecM spec state (\state () -> state)

catchSpecM :: ((forall b. SpecM b) -> SpecM ()) -> SpecM () -> SpecM ()
catchSpecM mx mcatch = SpecM $ \s k -> unSpecM (mx (SpecM $ \_s _k -> unSpecM mcatch s k)) s k

reduce :: State -> (SCStats, State)
reduce orig_state = go (mkHistory rEDUCE_WQO) orig_state
  where
    -- NB: it is important that we ensure that reduce is idempotent if we have rollback on. I use this property to improve memoisation.
    go hist state = -- traceRender ("reduce:step", pPrintFullState state) $
                    case step state of
        Nothing -> (mempty, state)
        Just state' -> case terminate hist state of
          Continue hist' -> go hist' state'
          Stop old_state -> trace "reduce-stop" $ (mempty { stat_reduce_stops = 1 }, if rEDUCE_ROLLBACK then old_state else state') -- TODO: generalise?


--
-- == The drive loop ==
--

data Promise f = P {
    fun        :: Var,         -- Name assigned in output program
    abstracted :: [Out Var],   -- Abstracted over these variables
    meaning    :: f State      -- Minimum adequate term. Nothing if this promise has been superceded by one with less free variables (this will only occur in the fulfilments)
  }

instance MonadStatics ScpM where
    bindCapturedFloats' = bindFloats

-- Note [Floating h-functions past the let-bound variables to which they refer]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- This seems like a reasonable thing to do because some variables will become free after supercompilation.
-- However, there really isn't much point doing the float because I won't be able to tie back to the floated thing
-- in any other branch.
--
-- Indeed, allowing such tiebacks may be a source of bugs! Consider a term like:
--
--  x |-> <10>
--  x + 5
--
-- After supercompilation, we will have:
--
--  15
--
-- Since we check the *post supercompilation* free variables here, that h function could be floated
-- upwards, so it is visible to later supercompilations. But what if our context had looked like:
--
--   (let x = 10 in x + 5, let x = 11 in x + 5)
--
-- Since we only match phantoms by name, we are now in danger of tying back to this h-function when we
-- supercompile the second component of the pair!
--
-- Conclusion: don't bother with this rubbish.
--
-- Note [Variables reachable from let-bindings]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- TODO: we shouldn't lambda-abstract over any variables reachable via the let-bound thing. Doing so needlessly
-- passes them around via lambdas when they will always be available in the closure.
--
-- Consider this example:
--
--   \y -> let x = \z -> .. too big to inline ... y ...
---        in (... x ..., ... x ...)
--
-- When supercompliing each component of the pair we might feel tempted to generate h-functions lambda abstracted over
-- y, but doing so is pointless (just hides information from GHC) since the result will be trapped under the x binding anyway.
fulfilmentRefersTo :: FreeVars -> Fulfilment -> Maybe (Out Var)
fulfilmentRefersTo extra_statics (promise, e') = guard (Foldable.any (`elemVarSet` extra_statics) (fvedTermFreeVars e' `unionVarSet` extra_fvs)) >> return (fun promise)
  where
    -- We bind floats with phantoms bindings where those phantom bindings are bound.
    --
    -- For wrappers introduced by --refine-fvs, we still need to use (fvedTermFreeVars e') because that will include
    -- the wrapped h-function (e.g. the h83' wrapper for h83). This also applies (though more rarely) for non-wrappers
    -- because looking at the fvedTermFreeVars is the only way we can learn about what h-functions they require.
    extra_fvs = maybe emptyVarSet stateLetBounders (meaning promise)

-- Used at the end of supercompilation to extract just those h functions that are actually referred to.
-- More often than not, this will be *all* the h functions, but if we don't discard h functions on rollback
-- then this is not necessarily the case!
fulfilmentReferredTo :: FreeVars -> Fulfilment -> Maybe FreeVars
fulfilmentReferredTo fvs (promise, e') = guard (fun promise `elemVarSet` fvs) >> return (fvedTermFreeVars e')

-- We do need a fixed point here to identify the full set of h-functions to residualise.
-- The reason is that even if a static variable is not free in an output h-function, we might
-- have created (and make reference to) some h-function that *does* actually refer to one
-- of the static variables.
-- See also Note [Phantom variables and bindings introduced by scrutinisation]
partitionFulfilments :: (a -> fulfilment -> Maybe b)  -- ^ Decide whether a fulfilment should be residualised given our current a, returning a new b if so
                     -> ([b] -> a)                    -- ^ Combine bs of those fufilments being residualised into a new a
                     -> a                             -- ^ Used to decide whether the fufilments right here are suitable for residualisation
                     -> [fulfilment]                  -- ^ Fulfilments to partition
                     -> ([fulfilment], [fulfilment])  -- ^ Fulfilments that should be bound and those that should continue to float, respectively
partitionFulfilments p combine = go
  where go x fs -- traceRender ("partitionFulfilments", x, map (fun . fst) fs) False = undefined
                | null fs_now' = ([], fs) 
                | otherwise    = first (fs_now' ++) $ go (combine xs') fs'
                where (fs_now_xs', fs') = extractJusts (\fulfilment -> fmap ((,) fulfilment) $ p x fulfilment) fs
                      (fs_now', xs') = unzip fs_now_xs'

-- Note [Where to residualise fulfilments with FVs]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Be careful of this subtle problem:
--
--  let h6 = D[e1]
--      residual = ...
--      h7 = D[... let residual = ...
--                 in Just residual]
--  in ...
--
-- If we first drive e1 and create a fulfilment for the h6 promise, then when driving h7 we will eventually come across a residual binding for the
-- "residual" variable. If we aren't careful, we will notice that "residual" is a FV of the h6 fulfilment and residualise it deep within h7. But
-- what if the body of the outermost let drove to something referring to h6? We have a FV - disaster!
--
-- The right thing to do is to make sure that fulfilments created in different "branches" of the process tree aren't eligible for early binding in
-- that manner, but we still want to tie back to them if possible. The bindFloats function achieves this by carefully shuffling information between the
-- fulfilments and promises parts of the monadic-carried state.
bindFloats :: FreeVars -> ScpM a -> (Out [(Var, FVedTerm)] -> a -> ScpM r) {- These h-functions are still visible in this continuation! -} -> ScpM r
bindFloats extra_statics mx mcont
  = ScpM $ \e s k -> unScpM mx (e { promises = mapMaybe fulfilmentPromise (fulfilments s) ++ promises e, fulfilmentStack = (fulfilments s, extra_statics) : fulfilmentStack e }) (s { fulfilments = [] }) (kontinue e s k)
  where
    kontinue e s k x s' = -- traceRender ("bindFloats", [(fun p, fvedTermFreeVars e) | (p, e) <- fs_now], [(fun p, fvedTermFreeVars e) | (p, e) <- fs_later]) $
                          --k (fulfilmentsToBinds fs_now, x) (s' { fulfilments = fs_later ++ fulfilments s })
                          unScpM (mcont (fulfilmentsToBinds fs_now) x) (e { promises = mapMaybe fulfilmentPromise fs_now ++ promises e }) (s' { fulfilments = fs_later ++ fulfilments s }) k
      where (fs_now, fs_later) = partitionFulfilments fulfilmentRefersTo mkVarSet extra_statics (fulfilments s')

fulfilmentsToBinds :: [Fulfilment] -> Out [(Var, FVedTerm)]
fulfilmentsToBinds fs = sortBy (comparing ((read :: String -> Int) . dropLastWhile (== '\'') . drop 1 . varString . fst)) [(fun p, e') | (p, e') <- fs]

freshHName :: ScpM (Name, Name)
freshHName = ScpM $ \_e s k -> k (expectHead "freshHName" (names s)) (s { names = tail (names s) })

fulfilmentPromise :: Fulfilment -> Maybe (Promise Identity)
fulfilmentPromise (P fun abstracted (Just meaning), _) = Just (P fun abstracted (I meaning))
fulfilmentPromise _                                    = Nothing

getPromises :: ScpM [Promise Identity]
getPromises = ScpM $ \e s k -> k (mapMaybe fulfilmentPromise (fulfilments s) ++ promises e) s

getPromiseNames :: ScpM [Var]
getPromiseNames = ScpM $ \e s k -> k (map (fun . fst) (fulfilments s) ++ map fun (promises e)) s

promise :: Promise Identity -> Name -> ScpM (a, Out FVedTerm) -> ScpM (a, Out FVedTerm)
promise p x' opt = ScpM $ \e s k -> {- traceRender ("promise", fun p, abstracted p) $ -} unScpM (mx p) (e { promises = p : promises e, depth = 1 + depth e }) s k
  where
    mx p = do
      (a, optimised_e) <- opt
      -- We have a little trick here: we can reduce the number of free variables our "h" functions abstract over if we discover that after supercompilation some
      -- variables become dead. This lets us get some of the good stuff from absence analysis: we can actually reduce the number of loop-carried vars like this.
      -- It is particularly important to do this trick when we have unfoldings, because functions get a ton more free variables in that case.
      --
      -- If some of the fufilments we have already generated refer to us, we need to fix them up because their application sites will apply more arguments than we
      -- actually need. We aren't able to do anything about the stuff they spuriously allocate as a result, but we can make generate a little wrapper that just discards
      -- those arguments. With luck, GHC will inline it and good things will happen.
      --
      -- TODO: we can generate the wrappers in a smarter way now that we can always see all possible fulfilments?
      let optimised_fvs = fvedTermFreeVars optimised_e
          abstracted_set = mkVarSet (abstracted p)
          abstracted'_set = optimised_fvs `intersectVarSet` abstracted_set -- We still don't want to abstract over e.g. phantom bindings
          abstracted'_list = sortLambdaBounds $ varSetElems abstracted'_set
          fun' = mkLocalId x' (abstracted'_list `mkPiTypes` stateType (unI (meaning p)))
      ScpM $ \_e s k -> let fs' | abstracted_set == abstracted'_set || not rEFINE_FULFILMENT_FVS
                                 -- If the free variables are totally unchanged, there is nothing to be gained from clever fiddling
                                = (P { fun = fun p, abstracted = abstracted p, meaning = Just (unI (meaning p)) }, tyVarIdLambdas (abstracted p) optimised_e) : fulfilments s
                                | otherwise
                                 -- If the free variable set has got smaller, we can fulfill our old promise with a simple wrapper around a new one with fewer free variables
                                = (P { fun = fun p, abstracted = abstracted p, meaning = Nothing }, tyVarIdLambdas (abstracted p) (var fun' `tyVarIdApps` abstracted'_list)) :
                                  (P { fun = fun',  abstracted = abstracted'_list, meaning = Just (unI (meaning p)) }, tyVarIdLambdas abstracted'_list optimised_e) : fulfilments s
                        in k () (s { fulfilments = fs' })
      
      fmap (((abstracted_set `unionVarSet` stateLetBounders (unI (meaning p))) `unionVarSet`) . mkVarSet) getPromiseNames >>=
        \fvs -> ASSERT2(optimised_fvs `subVarSet` fvs, ppr ("sc: FVs", fun p, optimised_fvs `minusVarSet` fvs, fvs, optimised_e)) return ()
      
      return (a, var (fun p) `tyVarIdApps` abstracted p)


data ScpEnv = ScpEnv {
    promises        :: [Promise Identity],
    fulfilmentStack :: [([Fulfilment], FreeVars)], -- Fulfilments at each level and the free variables of bindCapturedFloats that caused them to pushed.
                                                   -- We guarantee that promises for each these are already present in the promises field.
                                                   -- I have to store these in the monad-carried information because catchScpM has to be able to restore
                                                   -- (a subset of) them when rollback is initiated. See also Note [Where to residualise fulfilments with FVs]
    depth           :: Int
  }

type Fulfilment = (Promise Maybe, Out FVedTerm)

data ScpState = ScpState {
    names       :: [(Name, Name)],
    fulfilments :: [Fulfilment], -- Fulfilments at the current level only
    stats       :: SCStats
  }

newtype ScpM a = ScpM { unScpM :: ScpEnv -> ScpState -> (a -> ScpState -> (SCStats, Out FVedTerm)) -> (SCStats, Out FVedTerm) }

instance Functor ScpM where
    fmap = liftM

instance Monad ScpM where
    return x = ScpM $ \_e s k -> k x s
    (!mx) >>= fxmy = ScpM $ \e s k -> unScpM mx e s (\x s -> unScpM (fxmy x) e s k)

runScpM :: ScpM (Out FVedTerm) -> (SCStats, Out FVedTerm)
runScpM me = unScpM me init_e init_s (\e' s -> (stats s, letRecSmart (fulfilmentsToBinds $ fst $ partitionFulfilments fulfilmentReferredTo unionVarSets (fvedTermFreeVars e') (fulfilments s)) e'))
  where
    init_e = ScpEnv { promises = [], fulfilmentStack = [], depth = 0 }
    init_s = ScpState { names = h_names, fulfilments = [], stats = mempty }
    
    -- We need to create a name supply with *pairs* of Names because if we refine the fulfilment FVs we will create two bindings for each h-function
    (ids1, ids2) = splitUniqSupply hFunctionsUniqSupply
    h_names = zipWith3 (\i uniq uniq' -> (mkSystemVarName uniq  (mkFastString ('h' : show (i :: Int))),
                                          mkSystemVarName uniq' (mkFastString ('h' : show (i :: Int) ++ "'"))))
                       [1..] (uniqsFromSupply ids1) (uniqsFromSupply ids2)

catchScpM :: ((forall b. c -> ScpM b) -> ScpM a) -- ^ Action to try: supplies a function than can be called to "raise an exception". Raising an exception restores the original ScpEnv and ScpState
          -> (c -> ScpM a)                       -- ^ Handler deferred to if an exception is raised
          -> ScpM a                              -- ^ Result from either the main action or the handler
catchScpM f_try f_abort = ScpM $ \e s k -> unScpM (f_try (\c -> ScpM $ \e' s' _k' ->
    unScpM (f_abort c) e (if False -- dISCARD_FULFILMENTS_ON_ROLLBACK
                          then s
                          else let not_completed = mkVarSet (map fun (promises e')) `minusVarSet` mkVarSet (map fun (promises e))
                                   (fss_candidates, _fss_common) = splitByReverse (fulfilmentStack e) (fulfilmentStack e')
                                   
                                   -- Since we are rolling back we need to float as many of the fulfilments created in between here and the rollback point
                                   -- upwards. This means that we don't lose the work that we already did to supercompile those bindings.
                                   --
                                   -- The approach is to accumulate a set of floating fulfilments that I try to move past each statics set one at a time,
                                   -- from inside (deeper in the tree) to the outside (closer to top level).
                                   go fs_floating [] = fs_floating
                                   go fs_floating ((fs_candidates, extra_statics):fss_candidates) = go fs_ok fss_candidates
                                      where (_fs_discard, fs_ok) = partitionFulfilments fulfilmentRefersTo mkVarSet (not_completed `unionVarSet` extra_statics) (fs_candidates ++ fs_floating)
                               in s' { fulfilments = go [] fss_candidates ++ fulfilments s })
                         k)) e s k

addStats :: SCStats -> ScpM ()
addStats scstats = ScpM $ \_e s k -> k () (let scstats' = stats s `mappend` scstats in scstats' `seqSCStats` s { stats = scstats' })


type RollbackScpM = (State, State) -> ScpM (Deeds, Out FVedTerm)

sc  :: History (State, RollbackScpM) -> AlreadySpeculated -> State -> ScpM (Deeds, Out FVedTerm)
sc' :: History (State, RollbackScpM) -> AlreadySpeculated -> State -> ScpM (Deeds, Out FVedTerm)
sc  hist = rollbackBig (memo (sc' hist))
sc' hist speculated state = (\raise -> check raise) `catchScpM` \(old_state, state) -> stop old_state state hist -- TODO: I want to use the original history here, but I think doing so leads to non-term as it contains rollbacks from "below us" (try DigitsOfE2)
  where
    check this_rb = case terminate hist (state, this_rb) of
                      Continue hist' -> continue hist'
                      Stop (old_state, rb) -> maybe (stop old_state state hist) ($ (old_state, state)) $ guard sC_ROLLBACK >> Just rb
    stop old_state state hist = do addStats $ mempty { stat_sc_stops = 1 }
                                   maybe (trace "sc-stop: no generalisation" $ split state) (trace "sc-stop: generalisation") (generalise (mK_GENERALISER old_state state) state) (sc hist speculated) -- Keep the trace exactly here or it gets floated out by GHC
    continue hist = do traceRenderScpM "reduce end (continue)" (PrettyDoc (pPrintFullState state'))
                       addStats stats
                       split state' (sc hist speculated')
      where (speculated', (stats, state')) = (if sPECULATION then speculate speculated else ((,) speculated)) $ reduce state -- TODO: experiment with doing admissability-generalisation on reduced terms. My suspicion is that it won't help, though (such terms are already stuck or non-stuck but loopy: throwing stuff away does not necessarily remove loopiness).

memo :: (AlreadySpeculated -> State -> ScpM (Deeds, Out FVedTerm))
     ->  AlreadySpeculated -> State -> ScpM (Deeds, Out FVedTerm)
memo opt speculated state0 = do
    let (_, state1) = gc state0 -- Necessary because normalisation might have made some stuff dead
    
    ps <- getPromises
    case [ (p, (releaseStateDeed state0, var (fun p) `tyVarIdApps` tb_dynamic_vs))
         | p <- ps
         , Just rn_lr <- [(\res -> if isNothing res then pprTraceSC "no match:" (ppr (fun p)) res else res) $
                           match (unI (meaning p)) state1]
          -- NB: because I can trim reduce the set of things abstracted over above, it's OK if the renaming derived from the meanings renames vars that aren't in the abstracted list, but NOT vice-versa
         -- , let bad_renames = S.fromList (abstracted p) S.\\ M.keysSet (unRenaming rn_lr) in ASSERT2(S.null bad_renames, text "Renaming was inexhaustive:" <+> pPrint bad_renames $$ pPrint (fun p) $$ pPrintFullState (unI (meaning p)) $$ pPrint rn_lr $$ pPrintFullState state3) True
         , let rn_fvs = map (rename -- ("tieback: FVs for " ++ showSDoc (pPrint (fun p) $$ text "Us:" $$ pPrint state3 $$ text "Them:" $$ pPrint (meaning p)))
                                    rn_lr) -- NB: If tb contains a dead PureHeap binding (hopefully impossible) then it may have a free variable that I can't rename, so "rename" will cause an error. Not observed in practice yet.
               tb_dynamic_vs = rn_fvs (abstracted p)
         ] of
      (_p, res):_ -> {- traceRender ("tieback", pPrintFullState state3, fst res) $ -} do
        traceRenderScpM "=sc" (fun _p, PrettyDoc (pPrintFullState state1), res)
        return res
      [] -> {- traceRender ("new drive", pPrintFullState state3) $ -} do
        let vs = stateLambdaBounders state1
            vs_list = sortLambdaBounds $ varSetElems vs
        
        -- NB: promises are lexically scoped because they may refer to FVs
        (x, x') <- freshHName
        promise (P { fun = mkLocalId x (vs_list `mkPiTypes` stateType state1), abstracted = vs_list, meaning = I state1 }) x' $
          do
            traceRenderScpM ">sc" (x, PrettyDoc (pPrintFullState state1))
            -- FIXME: this is the site of the Dreadful Hack that makes it safe to match on reduced terms yet *drive* unreduced ones
            -- I only add non-internally bound junk to the input heap because:
            --  a) Thats the only stuff I *need* to add to make sure the FVs etc match up properly
            --  b) New InternallyBound stuff might be created by reduction and then swiftly become dead, and I don't want to push that down
            --     gratutiously. Furthermore, the Ids for that stuff might clash with those still-to-be-allocated in the state0 IdSupply.
            --
            -- Note that since the reducer only looks into non-internal *value* bindings doing this does not cause work duplication, only value duplication
            --
            -- FIXME: I'm not acquiring deeds for these....
            res <- opt speculated state1
            traceRenderScpM "<sc" (x, PrettyDoc (pPrintFullState state1), res)
            return res

sortLambdaBounds :: [Var] -> [Var]
sortLambdaBounds = sortBy (comparing (not . isTyVar)) -- True type variables go first since coercion/value variables may reference them

-- Several design choices here:
--
--  1. How to account for size of specialisations created during drive? Presumably ones that eventually get shared should be given a discount, but how?
--
--  2. How to continue if we do roll back. Currently I throw away any specialisations created in the process, but this seems uncool.
rollbackBig :: (AlreadySpeculated -> State -> ScpM (Deeds, Out FVedTerm))
            ->  AlreadySpeculated -> State -> ScpM (Deeds, Out FVedTerm)
rollbackBig opt speculated state
  -- | rOLLBACK_BIG = ScpM $ \e s k -> unScpM (opt speculated state) e s $ \(deeds', term') s' -> let too_big = fvedTermSize term' + sum [fvedTermSize term' | (p, term') <- fulfilments s', not (fun p `elem` map (fun . fst) (fulfilments s))] > bLOAT_FACTOR * stateSize state
  --                                                                                              in if too_big then k (case residualiseState state of (deeds, _, e') -> (deeds, e')) s else k (deeds', term') s'
  | otherwise = opt speculated state

traceRenderScpM :: Outputable a => String -> a -> ScpM ()
traceRenderScpM msg x = ScpM (\e s k -> k (depth e) s) >>= \depth -> pprTraceSC msg (nest depth $ pPrint x) $ return ()
