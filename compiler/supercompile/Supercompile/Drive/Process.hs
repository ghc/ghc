{-# LANGUAGE RankNTypes #-}
module Supercompile.Drive.Process (
    pprTraceSC,

    rEDUCE_WQO, wQO, mK_GENERALISER,

    TagAnnotations, tagAnnotations, tagSummary,

    prepareTerm,

    SCStats(..), seqSCStats,

    reduce, reduce', gc,
    AlreadySpeculated, nothingSpeculated,
    speculate,

    AbsVar(..), mkLiveAbsVar, renameAbsVar, absVarLambdas, applyAbsVars, stateAbsVars
  ) where

#include "HsVersions.h"

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming
--import Supercompile.Core.Size
import Supercompile.Core.Syntax

import Supercompile.Drive.Split (ResidTags)

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
import Supercompile.Utilities hiding (Monad(..))

import Name       (getOccString)
import Var        (isId, isTyVar, varType, setVarType)
import Id         (idType, idOccInfo, zapFragileIdInfo, setIdOccInfo)
import Type       (isUnLiftedType, mkTyVarTy)
import Coercion   (isCoVar, mkCoVarCo, mkUnsafeCo, coVarKind_maybe, mkCoercionType)
import TyCon      (PrimRep(..))
import Type       (typePrimRep, splitTyConApp_maybe)
import TysPrim
import TysWiredIn (unitTy)
import Literal
import VarEnv     (uniqAway)

import qualified Control.Monad as Monad
import Data.Ord
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Monoid
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


type TagAnnotations = IM.IntMap [String]

tagSummary :: TagAnnotations -> Int -> Int -> ResidTags -> String
tagSummary anns precision n resid_tags = unlines $ take n [intercalate "." ann ++ "\t" ++ show occs ++ "(" ++ show init_occs ++ ")" | (ann, (init_occs, occs)) <- sortBy (comparing (Down . snd . snd)) (M.toList ann_occs)]
  where ann_occs = M.unionsWith (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) [M.singleton (take precision ann) (1, occs) | (tag, occs) <- IM.toList resid_tags, let Just ann = IM.lookup tag anns]
        --total_occs = M.fold (+) 0 ann_occs

tagAnnotations :: State -> TagAnnotations
tagAnnotations (_, Heap h _, k, qa) = IM.unions [go_term (extAnn x []) e | (x, hb) <- M.toList h, Just (_, e) <- [heapBindingTerm hb]] `IM.union` go_qa e_ann qa `IM.union` resid_tags
  where
    extAnn x ann = showSDoc (ppr x):ann

    (e_ann, resid_tags) = foldr (\kf (ann, resid_tags) -> second (`IM.union` resid_tags) $ go_kf ann kf) ([], IM.empty) k
    
    go_qa :: [String] -> Anned QA -> TagAnnotations
    go_qa ann qa = IM.insert (tagInt (annedTag qa)) ann $ go_qa' ann (annee qa)

    go_qa' ann (Question _) = IM.empty
    go_qa' ann (Answer a)   = go_answer' ann a

    go_term :: [String] -> AnnedTerm -> TagAnnotations
    go_term ann e = IM.insert (tagInt (annedTag e)) ann $ go_term' ann (annee e)

    go_term' ann e = case e of
      Var _ -> IM.empty
      Value v -> go_value' ann v
      TyApp e _ -> go_term ann e
      CoApp e _ -> go_term ann e
      App e _   -> go_term ann e
      PrimOp _ _ es   -> IM.unions (map (go_term ann) es)
      Case e x _ alts -> go_term (extAnn x ann) e `IM.union` IM.unions [go_term ann e | (_, e) <- alts]
      Let x e1 e2     -> go_term (extAnn x ann) e1 `IM.union` go_term ann e2
      LetRec xes e    -> IM.unions [go_term (extAnn x ann) e | (x, e) <- xes] `IM.union` go_term ann e
      Cast e _        -> go_term ann e
    
    go_value' ann v = case v of
        Indirect _   -> IM.empty
        Literal _    -> IM.empty
        Coercion _   -> IM.empty
        TyLambda _ e -> go_term ann e
        Lambda   _ e -> go_term ann e
        Data _ _ _ _ -> IM.empty

    go_answer :: [String] -> Anned Answer -> TagAnnotations
    go_answer ann a = IM.insert (tagInt (annedTag a)) ann $ go_answer' ann (annee a)

    go_answer' ann (_, (_, v)) = go_value' ann v

    go_kf :: [String] -> Tagged StackFrame -> ([String], TagAnnotations)
    go_kf ann kf = second (IM.insert (tagInt (tag kf)) ann) $ go_kf' ann (tagee kf)

    go_kf' ann kf = case kf of
      TyApply _ -> (ann, IM.empty)
      CoApply _ -> (ann, IM.empty)
      Apply _   -> (ann, IM.empty)
      Scrutinise x _ (_, alts) -> (extAnn x ann, IM.unions [go_term ann e | (_, e) <- alts])
      PrimApply _ _ as es -> (ann, IM.unions [go_answer ann a | a <- as] `IM.union` IM.unions [go_term ann e | (_, e) <- es])
      StrictLet x (_, e)  -> (extAnn x ann, go_term ann e)
      Update x            -> (extAnn x ann, IM.empty)
      CastIt _            -> (ann, IM.empty)


prepareTerm :: M.Map Var Term -> Term -> State
prepareTerm unfoldings e = pprTraceSC "unfoldings" (pPrintPrecLetRec noPrec (M.toList unfoldings) (PrettyDoc (text "<stuff>"))) $
                           pprTraceSC "all input FVs" (ppr input_fvs) $
                           state
  where (tag_ids0, tag_ids1) = splitUniqSupply tagUniqSupply
        anned_e = toAnnedTerm tag_ids0 e
        
        ((input_fvs, tag_ids2), h_unfoldings) = mapAccumL add_one_unfolding (annedTermFreeVars anned_e, tag_ids1) (M.toList unfoldings)
          where add_one_unfolding (input_fvs', tag_ids1) (x', e) = ((input_fvs'', tag_ids2), (x', letBound (renamedTerm anned_e)))
                    where (tag_unf_ids, tag_ids2) = splitUniqSupply tag_ids1
                          anned_e = toAnnedTerm tag_unf_ids e
                          input_fvs'' = input_fvs' `unionVarSet` annedFreeVars anned_e
        
        (_, h_fvs) = mapAccumL add_one_fv tag_ids2 (varSetElems input_fvs)
          where add_one_fv tag_ids2 x' = (tag_ids3, (x', environmentallyBound (mkTag (getKey i))))
                    where (i, tag_ids3) = takeUniqFromSupply tag_ids2
        
        -- NB: h_fvs might contain bindings for things also in h_unfoldings, so union them in the right order
        deeds = Deeds { sizeLimit = (bLOAT_FACTOR - 1) * annedSize anned_e, stepLimit = (bLOAT_FACTOR - 1) * annedSize anned_e }
        state = normalise (deeds, Heap (M.fromList h_unfoldings `M.union` M.fromList h_fvs) (mkInScopeSet input_fvs), [], (mkIdentityRenaming input_fvs, anned_e))

        -- FIXME: instead of adding unfoldings as Let, (in order to sidestep the bug where Let stuff will be underspecialised)
        -- we should add it them as normal bindings but pre-initialise the memo cache. Of course this will be bad in the case
        -- where the existing RHSes for the unfoldings could be optimised by supercompilation, but perhaps we don't care.
        --
        -- Be sure to preinitialise the cache for all vanilla applications up to idArity, as well. By doing this we basically
        -- assert that GHC's RHSes are optimially efficient for limited kinds of context.

        -- TODO: I didn't implement this idea. It has to go into the splitter anyway.
        --
        -- We can treat the top level specially in the following way - any HeapBindings in the initial state that
        -- *have no free variables* may be turned into LetBound things. This has the following benefits:
        --  1. Matching such things can be done nominally
        --  2. The supercompiler pretty-printer output is nicer (we often omit LetBound things from the output)
        --
        -- The no-bound-FVs check accomplishes two things:
        --  1. It satisfies the invariant that no LambdaBound thing can be referred to by a LetBound thing (FIXME: still required??)
        --  2. It ensures that residualising that binding right here does not cause remove some linearity
        --  3. We sidestep the bug that can occur where specialisations of LetBound HeapBindings are not made
        --     if one of their free variables get refined by a case expression (FIXME: not really??)
        --
        -- It is uniquely OK to do this at *top* level because otherwise we will end up trapping specialisations
        -- lower down in the specialised output where they cannot be reused.


data SCStats = SCStats {
    stat_reduce_stops :: !Int,
    stat_sc_stops :: !Int
  }

seqSCStats :: SCStats -> a -> a
seqSCStats (SCStats a b) x = a `seq` b `seq` x

instance Monoid SCStats where
    mempty = SCStats {
        stat_reduce_stops = 0,
        stat_sc_stops = 0
      }
    stats1 `mappend` stats2 = SCStats {
        stat_reduce_stops = stat_reduce_stops stats1 + stat_reduce_stops stats2,
        stat_sc_stops = stat_sc_stops stats1 + stat_sc_stops stats2
      }


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
gc :: State -> State
gc _state@(deeds0, Heap h ids, k, in_e) = ASSERT2(isEmptyVarSet (stateUncoveredVars gced_state), ppr (stateUncoveredVars gced_state, PrettyDoc (pPrintFullState False _state), PrettyDoc (pPrintFullState False gced_state)))
                                          gced_state
  where
    gced_state = (deeds2, Heap h' ids, k', in_e)
    
    -- We have to use stateAllFreeVars here rather than stateFreeVars because in order to safely prune the live stack we need
    -- variables bound by k to be part of the live set if they occur within in_e or the rest of the k
    live0 = stateAllFreeVars (deeds0, Heap M.empty ids, k, in_e)
    (deeds1, _h_dead, h', live1) = inlineLiveHeap deeds0 h live0
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
        
            -- NB: It's important that type variables become live after inlining a binding, or we won't
            -- necessarily lambda-abstract over all the free type variables of a h-function
            consider_inlining x' hb (h_pending_kvs, h_output, live)
              | x' `elemVarSet` live = (h_pending_kvs,            M.insert x' hb h_output, live `unionVarSet` heapBindingFreeVars hb `unionVarSet` varBndrFreeVars x')
              | otherwise            = ((x', hb) : h_pending_kvs, h_output,                live)
    
    pruneLiveStack :: Deeds -> Stack -> FreeVars -> (Deeds, Stack)
    pruneLiveStack deeds k live = (deeds `releaseStackDeeds` k_dead, k_live)
      where (k_live, k_dead) = partition (\kf -> case tagee kf of Update x' -> x' `elemVarSet` live; _ -> True) k


type AlreadySpeculated = S.Set Var

nothingSpeculated :: AlreadySpeculated
nothingSpeculated = S.empty

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

    (stats', deeds', h_speculated_ok, h_speculated_failure, ids') = runSpecM (speculateManyMap (mkLinearHistory (cofmap fst wQO)) h_non_values_unspeculated) (stats, deeds, h_values, M.empty, ids)
    
    speculateManyMap hist = speculateMany hist . concatMap M.toList . topologicalSort heapBindingFreeVars
    speculateMany hist = mapM_ (speculateOne hist)
    
    speculateOne :: LinearHistory (State, SpecM ()) -> (Out Var, HeapBinding) -> SpecM ()
    speculateOne hist (x', hb)
      | HB InternallyBound (Right in_e) <- hb
      = (\rb -> try_speculation in_e rb) `catchSpecM` speculation_failure
      | otherwise
      = speculation_failure
      where
        speculation_failure = modifySpecState $ \(stats, deeds, h_speculated_ok, h_speculated_failure, ids) -> ((stats, deeds, h_speculated_ok, M.insert x' hb h_speculated_failure, ids), ())
        try_speculation in_e rb = Monad.join (modifySpecState go)
          where go no_change@(stats, deeds, h_speculated_ok, h_speculated_failure, ids) = case terminate hist (state, rb) of
                    Stop (_old_state, rb) -> (no_change, rb)
                    Continue hist -> case reduce' state of
                        (extra_stats, (deeds, Heap h_speculated_ok' ids, [], qa))
                          | Just a <- traverse qaToAnswer qa
                          , let h_unspeculated = h_speculated_ok' M.\\ h_speculated_ok
                                in_e' = annedAnswerToInAnnedTerm (mkInScopeSet (annedFreeVars a)) a
                          -> ((stats `mappend` extra_stats, deeds, M.insert x' (internallyBound in_e') h_speculated_ok, h_speculated_failure, ids), speculateManyMap hist h_unspeculated)
                        _ -> (no_change, speculation_failure)
                  where state = normalise (deeds, Heap h_speculated_ok ids, [], in_e)

type SpecState = (SCStats, Deeds, PureHeap, PureHeap, InScopeSet)
newtype SpecM a = SpecM { unSpecM :: SpecState -> (SpecState -> a -> SpecState) -> SpecState }

instance Functor SpecM where
    fmap = liftM

instance Monad.Monad SpecM where
    return x = SpecM $ \s k -> k s x
    mx >>= fxmy = SpecM $ \s k -> unSpecM mx s (\s x -> unSpecM (fxmy x) s k)

modifySpecState :: (SpecState -> (SpecState, a)) -> SpecM a
modifySpecState f = SpecM $ \s k -> case f s of (s, x) -> k s x

runSpecM :: SpecM () -> SpecState -> SpecState
runSpecM spec state = unSpecM spec state (\state () -> state)

catchSpecM :: ((forall b. SpecM b) -> SpecM ()) -> SpecM () -> SpecM ()
catchSpecM mx mcatch = SpecM $ \s k -> unSpecM (mx (SpecM $ \_s _k -> unSpecM mcatch s k)) s k

reduce :: State -> State
reduce = snd . reduce'

reduce' :: State -> (SCStats, State)
reduce' orig_state = go (mkLinearHistory rEDUCE_WQO) orig_state
  where
    -- NB: it is important that we ensure that reduce is idempotent if we have rollback on. I use this property to improve memoisation.
    go hist state
      = -- traceRender ("reduce:step", pPrintFullState state) $
        case step state of
          Just (deeds, heap, k, e)
           | Just deeds' <- if bOUND_STEPS then claimStep deeds else Just deeds
           , let state' = (deeds', heap, k, e)
           -> case terminate hist state of
            Continue hist' -> go hist' state'
            Stop old_state -> pprTrace "reduce-stop" (pPrintFullState False old_state $$ pPrintFullState False state) 
                              -- let smmrse s@(_, _, _, qa) = pPrintFullState s $$ case annee qa of Question _ -> text "Question"; Answer _ -> text "Answer" in
                              -- pprPreview2 "reduce-stop" (smmrse old_state) (smmrse state) $
                              (mempty { stat_reduce_stops = 1 }, if rEDUCE_ROLLBACK then old_state else state') -- TODO: generalise?
          _ -> (mempty, state)



--
-- == Abstracted variables ==
--

-- As Var, BUT we promise that the deadness information is correct on Ids. This lets
-- us apply "undefined" instead of an actual argument.
--
-- We *can't* just use AbsVar = Var because we need to be able to mark TyVars
-- as dead - not just Ids.
data AbsVar = AbsVar {
    absVarDead :: Bool, -- ^ Dead variables will not be free in the result of applyAbsVars, and there are no guarantees about shadowing
                        -- Live variables *will* be free in the result of applyAbsVars. Guaranteed not to shadow other AbsVars/any h-functions
    absVarVar :: Var    -- ^ The 'Var' itself
  }

mkLiveAbsVar :: Var -> AbsVar
mkLiveAbsVar x = AbsVar { absVarDead = False, absVarVar = x }

-- We map *all* occurrences of dead TyVars to this type, to ensure that dead TyVars in the
-- type of applied Ids match the applied dead TyVars. This type can be any closed type, as long
-- as we use it consistently!
deadTy :: Type
deadTy = unitTy

renameAbsVarType :: M.Map Var Var -> Var -> Var
renameAbsVarType rn x = x `setVarType` renameType (mkInScopeSet as) complete_rn ty
  where ty = varType x
        as = tyVarsOfType ty
        complete_rn = mkTyVarRenaming [(a, case M.lookup a rn of Nothing -> deadTy; Just a' -> mkTyVarTy a') | a <- varSetElems as]

-- If a variable is not present in the input renaming, we assume that it has become dead
-- and set the deadness information accordingly
renameAbsVar :: M.Map Var Var -> AbsVar -> AbsVar
renameAbsVar rn (AbsVar { absVarDead = dead, absVarVar = x })
  | dead
  = AbsVar { absVarDead = True,  absVarVar = renameAbsVarType rn x }
  | otherwise
  = AbsVar { absVarDead = False, absVarVar = renameAbsVarType rn (M.findWithDefault (pprPanic "renameAbsVar" (ppr x)) x rn) }

absVarLambdas :: Symantics ann => [AbsVar] -> ann (TermF ann) -> ann (TermF ann)
absVarLambdas xs = tyVarIdLambdas (map absVarVar xs)

applyAbsVars :: Symantics ann => Var -> [AbsVar] -> ann (TermF ann)
applyAbsVars x xs = snd (foldl go (unitVarSet x, var x) xs)
  where
   go (fvs, e) absx = case absVarDead absx of
    True -> (fvs, case () of
      () -- We can encounter TyVars, where we should be able to instantiate them any way:
         | isTyVar x
         -> e `tyApp` deadTy
         
         -- Dead CoVars are easy:
         | Just (ty1, ty2) <- coVarKind_maybe x
         -> let_ x (coercion (mkUnsafeCo ty1 ty2)) (e `app` x)
         
         -- A pretty cute hack for lifted bindings, though potentially quite confusing!
         -- If you want to put "undefined" here instead then watch out: this counts
         -- as an extra free variable, so it might trigger the assertion in Process.hs
         -- that checks that the output term has no more FVs than the input.
         | not (isUnLiftedType ty)
         -> letRec [(x, var x)] (e `app` x)
         
         -- We have to be more creative for *unlifted* bindings, since building a loop
         -- changes the meaning of the program. Literals first:
         | Just (tc, []) <- splitTyConApp_maybe ty
         , Just lit <- absentLiteralOf tc
         -> let_ x (literal lit) (e `app` x)
         
         -- If we get here we are getting desperate need to get *really* creative.
         -- Just choose some value with the same *representation* as what we want and then
         -- cast it to the right type:
         | let (e_repr_ty, e_repr) = case typePrimRep ty of
                 VoidRep   -> (mkCoercionType unitTy unitTy, coercion (mkUnsafeCo unitTy unitTy))
                 IntRep    -> (intPrimTy,    literal (mkMachInt 0))
                 WordRep   -> (wordPrimTy,   literal (mkMachWord 0))
                 Int64Rep  -> (int64PrimTy,  literal (mkMachInt64 0))
                 Word64Rep -> (word64PrimTy, literal (mkMachWord64 0))
                 AddrRep   -> (addrPrimTy,   literal nullAddrLit)
                 FloatRep  -> (floatPrimTy,  literal (mkMachChar 'x'))
                 DoubleRep -> (doublePrimTy, literal (mkMachDouble 0))
                 -- Unlifted thing of PtrRep: yes, this can really happen (ByteArray# etc)
                 PtrRep    -> pprPanic "applyAbsVars: dead unlifted variable with PrimRep PtrRep: FIXME" (ppr ty)
         -> let_ x (e_repr `cast` mkUnsafeCo e_repr_ty ty) (e `app` x))
         where shadowy_x = absVarVar absx
               x = uniqAway (mkInScopeSet fvs) shadowy_x
               ty = idType x
    False -> (fvs `extendVarSet` x, case () of
      () | isTyVar x
         -> e `tyApp` mkTyVarTy x
   
         | isCoVar x
         -> e `coApp` mkCoVarCo x_zapped
   
         | otherwise
         -> e `app` x_zapped)
         where x = absVarVar absx
               x_zapped = zapFragileIdInfo x
               -- NB: make sure we zap the "fragile" info because the FVs of the unfolding are
               -- not necessarily in scope.

stateAbsVars :: State -> [Var]
stateAbsVars = sortLambdaBounds . varSetElems . stateLambdaBounders

sortLambdaBounds :: [Var] -> [Var]
sortLambdaBounds = sortBy (comparing (not . isTyVar)) -- True type variables go first since coercion/value variables may reference them
