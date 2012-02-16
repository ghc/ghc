{-# LANGUAGE RankNTypes #-}
module Supercompile.Drive.Process (
    pprTraceSC,

    rEDUCE_WQO, wQO, mK_GENERALISER,

    ParentChildren, emptyParentChildren, addChild, childrenSummary, depthHistogram, deepestPath,

    TagAnnotations, tagAnnotations, tagSummary,

    prepareTerm,

    SCStats(..), seqSCStats,

    reduce, reduceWithFlag, reduceWithStats, gc,
    AlreadySpeculated, nothingSpeculated,
    speculate,

    AbsVar(..), mkLiveAbsVar, renameAbsVar, absVarLambdas, applyAbsVars, stateAbsVars,

    extraOutputFvs
  ) where

#include "HsVersions.h"

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming
--import Supercompile.Core.Size
import Supercompile.Core.Syntax
import Supercompile.Core.Tag (dataConTag, literalTag)

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

import Var        (isId, isTyVar, varType, setVarType)
import Id         (idType, zapFragileIdInfo, localiseId)
import MkId       (voidArgId, realWorldPrimId, mkPrimOpId)
import Type       (isUnLiftedType, mkTyVarTy)
import Coercion   (isCoVar, mkCoVarCo, mkUnsafeCo, coVarKind_maybe)
import CoreUtils  (mkPiTypes)
import TyCon      (PrimRep(..))
import Type       (eqType, mkFunTy, mkTyConApp, typePrimRep, splitTyConApp_maybe)
import TysPrim
import TysWiredIn (unitTy, unboxedPairDataCon, unboxedPairTyCon)
import MkCore     (mkWildValBinder)
import PrimOp     (PrimOp(MyThreadIdOp))
import Literal
import VarEnv
import Util       (fstOf3, thirdOf3)

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


type ParentChildren = M.Map (Maybe Var) [(Var, (State, Bool))]

emptyParentChildren :: ParentChildren
emptyParentChildren = M.empty

addChild :: Maybe Var -> Var -> State -> Bool -> ParentChildren -> ParentChildren
addChild mb_parent child child_state gen = M.alter (\mb_children -> Just ((child, (child_state, gen)) : (mb_children `orElse` []))) mb_parent

childrenSummary :: ParentChildren -> String
childrenSummary parent_children = unlines [maybe "<root>" varString mb_parent ++ ": " ++ intercalate " " (map show child_counts)  | (mb_parent, child_counts :: [Int]) <- ordered_counts]
  where descendant_counts = flip M.map parent_children $ \children -> map ((+1) . sum . flip (M.findWithDefault [] . Just) descendant_counts . fst) children
        ordered_counts = sortBy (comparing (Down . sum . snd)) (M.toList descendant_counts)

depthHistogram :: ParentChildren -> SDoc
depthHistogram parent_children = maybe empty (\overall_depth_summary -> vcat [ppr depth <> char ',' <+> ppr count | (depth, count) <- IM.toList overall_depth_summary]) (M.lookup Nothing summary_map)
  where depth_map :: M.Map (Maybe Var) Int
        depth_map = M.foldrWithKey (\mb_fun children so_far -> let Just depth = M.lookup mb_fun depth_map
                                                               in foldr (\(child_fun, _) -> M.insert (Just child_fun) (depth + 1)) so_far children)
                                   (M.singleton Nothing 0) parent_children

        summary_map :: M.Map (Maybe Var) (IM.IntMap Int)
        summary_map = flip M.mapWithKey parent_children $ \mb_fun children -> let summaries = [M.findWithDefault IM.empty (Just child_fun) summary_map | (child_fun, _) <- children]
                                                                                  Just depth = M.lookup mb_fun depth_map
                                                                              in IM.unionsWith (+) (IM.singleton depth 1:summaries)

-- NB: there may be many deepest paths
deepestPath :: [(Var, FVedTerm)] -> ParentChildren -> SDoc
deepestPath fulfils parent_children = maybe empty (\(_, deepest_from_root) -> show_meaning_chains deepest_from_root $$ summarise_leaves (map last deepest_from_root)) mb_deepest_from_root
  where deepest :: M.Map (Maybe Var) (Int, [[(Var, (State, Bool))]])
        deepest = flip M.map parent_children $ \children -> maximumByFst [(depth + 1, (fun, state):states) | (fun, state) <- children, let (depth, statess) = M.findWithDefault (0, [[]]) (Just fun) deepest, states <- statess]
        
        mb_deepest_from_root = M.lookup Nothing deepest

        fulfils_map :: M.Map Var FVedTerm
        fulfils_map = M.fromList fulfils

        show_fulfils_chain :: [Var] -> SDoc
        show_fulfils_chain = flip (pPrintPrecLetRec noPrec) (PrettyDoc (text "...")) . mapMaybe (\x -> fmap ((,) x) $ M.lookup x fulfils_map)

        show_meaning_chains = vcat . zipWith (\i states -> hang (text "Deepest Chain" <+> ppr (i :: Int)) 2 (show_meaning_chain M.empty states $$ show_fulfils_chain (map fst states))) [1..]

        show_meaning_chain :: M.Map Var Bool -> [(Var, (State, Bool))] -> SDoc
        show_meaning_chain _         [] = empty
        show_meaning_chain known_bvs ((fun, (state@(_, Heap h _, _, _), gen)):states)
          = hang (ppr fun <+> (if gen then text "(GENERALISED)" else empty)) 2 (pPrintFullState (quietStatePrettiness { excludeBindings = unchanged_bvs }) state) $$
            show_meaning_chain known_bvs' states
          where known_bvs'  = M.map (maybe False (termIsValue . snd) . heapBindingTerm) h
                unchanged_bvs = M.keysSet (M.filter id (M.intersectionWith (==) known_bvs known_bvs'))
        
        summarise_leaves :: [(Var, (State, Bool))] -> SDoc
        summarise_leaves leaves = vcat [ppr fun <> text ":" <+> maybe (text "?") showValueGroup (biggest_value_group state) | (fun, (state, _)) <- leaves]

        biggest_value_group :: State -> Maybe (Var, VarEnv AnnedValue)
        biggest_value_group = safeHead . stateValueGroups

        maximumByFst :: Ord a => [(a, b)] -> (a, [b])
        maximumByFst xys = case maximumsComparing fst xys of
          ((x, y):xys) -> (x, y:map snd xys)
          []           -> error "maximumByFst"

-- NB: groups come out in order largest to smallest, and no groups are included if they are subsumed by another one
stateValueGroups :: State -> [(Var, VarEnv AnnedValue)]
stateValueGroups (_, Heap h ids, _, _) = subsume [] (sortBy (comparing (Down . length . varEnvElts . snd)) (map extend h_values_list))
  where
    -- For the purposes of this function, we only care about data structures that could derive from
    -- positive information propagation -- i.e. we discard lambdas
    h_values_list = [(x, renameAnnedValue' ids rn v) | (x, hb) <- M.toList h, Just (rn, Value v) <- [fmap (second extract) (heapBindingTerm hb)], case v of TyLambda _ _ -> False; Lambda _ _ -> False; _ -> True]
    h_values = mkVarEnv h_values_list

    extend (x, v) = (x, go (unitVarEnv x v))
      where
        go heap
          | isEmptyVarEnv extra_heap
          = heap
          | otherwise
          = go (heap `plusVarEnv` extra_heap)
          where heap_fvs = foldVarEnv (\v fvs -> annedValueFreeVars' v `unionVarSet` fvs) emptyVarSet heap `minusVarEnv` heap
                extra_heap = h_values `restrictVarEnv` heap_fvs
    
    subsume _       [] = []
    subsume already ((root, group):groups)
      | any (group `is_sub_env`) already
      = subsume already groups
      | otherwise
      = (root, group) : subsume (group:already) groups
    
    is_sub_env s1 s2 = isEmptyVarEnv (s1 `minusVarEnv` s2)

-- Quick and dirty hack for showing groups of values in something other than ANF
showValueGroup :: (Var, VarEnv AnnedValue) -> SDoc
showValueGroup (root, group) = go emptyVarSet noPrec root
  where
    go shown prec x = case lookupVarEnv group x of
      Just v | not (x `elemVarSet` shown)  -- Break loops in recursive structures
             , let shown' = shown `extendVarSet` x
             -> case v of Indirect y         -> go shown' prec y
                          Data dc tys cos ys -> pPrintPrecApps prec dc ([asPrettyFunction ty | ty <- tys] ++ [asPrettyFunction co | co <- cos] ++ [PrettyFunction (\prec -> go shown' prec y) | y <- ys])
                          _ -> pprPrec prec v
      _ -> pprPrec prec x


type TagAnnotations = IM.IntMap [String]

tagSummary :: TagAnnotations -> Int -> Int -> ResidTags -> String
tagSummary anns precision n resid_tags = unlines $ take n [intercalate "." ann ++ "\t" ++ show occs ++ "(" ++ show init_occs ++ ")" | (ann, (init_occs, occs)) <- sortBy (comparing (Down . snd . snd)) (M.toList ann_occs)]
  where ann_occs = M.unionsWith (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) [M.singleton (take precision ann) (1 :: Int, occs) | (tag, occs) <- IM.toList resid_tags, let Just ann = IM.lookup tag anns]
        --total_occs = M.fold (+) 0 ann_occs

tagAnnotations :: State -> TagAnnotations
tagAnnotations (_, Heap h _, k, qa) = IM.unions [go_term (extAnn x []) e | (x, hb) <- M.toList h, Just (_, e) <- [heapBindingTerm hb]] `IM.union` go_qa e_ann qa `IM.union` resid_tags
  where
    extAnn x ann = showSDoc (ppr x):ann

    (e_ann, resid_tags) = trainCarFoldr (\kf (ann, resid_tags) -> second (`IM.union` resid_tags) $ go_kf ann kf) ([], IM.empty) k
    
    go_qa :: [String] -> Anned QA -> TagAnnotations
    go_qa ann qa = IM.insert (tagInt (annedTag qa)) ann $ go_qa' ann (annee qa)

    go_qa' _   (Question _) = IM.empty
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
      Case e x _ alts -> go_term (extAnn x ann) e `IM.union` IM.unions [go_alt ann alt `IM.union` go_term ann e | (alt, e) <- alts]
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

    go_alt :: [String] -> AltCon -> TagAnnotations
    go_alt ann (DataAlt dc _ _ _) = IM.singleton (tagInt (dataConTag dc)) ann
    go_alt ann (LiteralAlt l)     = IM.singleton (tagInt (literalTag l))  ann
    go_alt _   DefaultAlt         = IM.empty

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



prepareTerm :: M.Map Var Term -> Term -> (State,                        -- For use without memo-cache preinitalization
                                          ([(State, FVedTerm)], State)) -- With preinitialization
prepareTerm unfoldings e = pprTraceSC "unfoldings" (pPrintPrecLetRec noPrec (M.toList unfoldings) (PrettyDoc (text "<stuff>"))) $
                           pprTraceSC "all input FVs" (ppr input_fvs) $
                           (state, (preinit_with, preinit_state))
  where (tag_ids0, tag_ids1) = splitUniqSupply tagUniqSupply
        anned_e = toAnnedTerm tag_ids0 e
        
        ((input_fvs, tag_ids2), h_unfoldings) = mapAccumL add_one_unfolding (annedTermFreeVars anned_e, tag_ids1) (M.toList unfoldings)
          where add_one_unfolding (input_fvs', tag_ids1) (x', e) = ((input_fvs'', tag_ids2), (x', renamedTerm anned_e))
                    where (tag_unf_ids, tag_ids2) = splitUniqSupply tag_ids1
                          anned_e = toAnnedTerm tag_unf_ids e
                          input_fvs'' = input_fvs' `unionVarSet` varBndrFreeVars x' `unionVarSet` annedFreeVars anned_e
        
        (_, h_fvs) = mapAccumL add_one_fv tag_ids2 (varSetElems input_fvs)
          where add_one_fv tag_ids2 x' = (tag_ids3, (x', environmentallyBound (mkTag (getKey i))))
                    where (i, tag_ids3) = takeUniqFromSupply tag_ids2
        
        -- NB: h_fvs might contain bindings for things also in h_unfoldings, so union them in the right order
        deeds = Deeds { sizeLimit = (bLOAT_FACTOR - 1) * annedSize anned_e, stepLimit = (bLOAT_FACTOR - 1) * annedSize anned_e }
        rn = mkIdentityRenaming input_fvs
        ids = mkInScopeSet input_fvs
        mk_heap how_bound = Heap (M.fromList (map (second how_bound) h_unfoldings) `M.union` M.fromList h_fvs) ids
        
        state = normalise (deeds, mk_heap letBound, Loco False, (rn, anned_e))
        
        preinit_state = normalise (deeds, preinit_heap, Loco False, (rn, anned_e))
        preinit_heap = mk_heap internallyBound

        -- NB: we assume that unfoldings are guaranteed to be cheap and hence duplicatiable. I think this is reasonable.
        preinit_with = [(gc (normalise (maxBound, heap', Loco False, anned_e')), accessor_e)
                       | (x', anned_e) <- h_unfoldings
                       , (heap', accessor_e, anned_e') <- eta preinit_heap (var x') anned_e]

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

-- Especially when we do eager value splitting, we might never actually match against the RHS of a binding like (map = \f xs -> ...).
-- This "hack" is designed to work around this issue by doing some eager value splitting of our own on lambdas.
eta :: Heap -> FVedTerm -> In AnnedTerm -> [(Heap, FVedTerm, In AnnedTerm)]
eta heap@(Heap h ids) accessor_e0 in_e = (heap, accessor_e0, in_e) : case termToAnswer ids in_e of
  Just anned_a | (a_cast, (rn, v)) <- extract anned_a
               , let accessor_e1 = case a_cast of Uncast      -> accessor_e0
                                                  CastBy co _ -> accessor_e0 `cast` mkSymCo ids co
                     mb_res@(~(Just (_, x, _))) = case v of
                        Lambda   x e_body -> Just (accessor_e1 `app`   x',           x, e_body)
                        TyLambda a e_body -> Just (accessor_e1 `tyApp` mkTyVarTy x', a, e_body)
                        _                 -> Nothing
                     (ids', rn', x') = renameNonRecBinder ids rn x
               , Just (accessor_e2, _, e_body) <- mb_res
               -> eta (Heap (M.insert x' lambdaBound h) ids') accessor_e2 (rn', e_body)
  _            -> []


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
gc _state@(deeds0, Heap h ids, k, in_e) = ASSERT2(stateUncoveredVars gced_state `subVarSet` stateUncoveredVars _state, ppr (stateUncoveredVars gced_state, PrettyDoc (pPrintFullState quietStatePrettiness _state), PrettyDoc (pPrintFullState quietStatePrettiness gced_state)))
                                          gced_state -- We do not insist that *no* variables are uncovered because when used from the speculator this may not be true
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
    pruneLiveStack init_deeds k live = trainFoldr (\kf (deeds, k_live) -> if (case tagee kf of Update x' -> x' `elemVarSet` live; _ -> True)
                                                                          then (deeds, kf `Car` k_live)
                                                                          else (deeds `releaseStackFrameDeeds` kf, k_live))
                                                  (\gen deeds -> (deeds, Loco gen)) init_deeds k


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
--
-- Note [Speculation termination]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- I was trying to work out why speculation was so slow on programs like $fReadInteger that used parser combinators.
-- I never really worked it out, but I did find a possible problem. Consider this program:
--
--  foo |-> \x -> let f = let a = snd3 (foo x) in \y -> ...
--                    g = let b = thd3 (foo x) in \y -> ...
--                    h = let c = fst3 (foo x) in \y -> ...
--                in (f, g, h)
--  bar |-> foo z
--
-- With a tree-like history, we might speculate like this:
--   bar -> f -> a -> b -> c -> a (!RB)
--          g -> b -> c -> a -> b (!RB)
--          h -> c -> a -> b -> c (!RB)
--
-- So we end up making O(n^2) calls to reduce.
--
-- My approach to preventing this sort of problem is to *thread* the history. The only thing we have to be careful of
-- is that we don't roll back to a "node" which has already itself been rolled back (since that leads to a loop). This
-- concern motivates us also threading the "forbidden" set and keeping track of "parents" using ids from "ids".
--
-- With this approach we get something more like:
--   bar -> f -> a -> b -> c -> a (!RB)
--          g -> b (!STOP)
--          h -> c (!STOP)
speculate :: AlreadySpeculated -> (SCStats, State) -> (AlreadySpeculated, (SCStats, State))
speculate speculated (stats, (deeds, Heap h ids, k, in_e)) = (M.keysSet h, (stats', (deeds', Heap (h_non_values_speculated `M.union` h_speculated_ok `M.union` h_speculated_failure) ids', k, in_e)))
  where
    (h_values, h_non_values) = M.partition (maybe False (termIsValue . snd) . heapBindingTerm) h
    (h_non_values_unspeculated, h_non_values_speculated) = (h_non_values `exclude` speculated, h_non_values `restrict` speculated)

    ((_hist, _forbidden, ids'), (stats', deeds', h_speculated_ok, h_speculated_failure)) = runSpecM (speculateManyMap [] h_non_values_unspeculated) ((mkLinearHistory (cofmap fstOf3 wQO), [], ids), (stats, deeds, h_values, M.empty))
    
    speculateManyMap parents = speculateMany parents . concatMap M.toList . topologicalSort heapBindingFreeVars
    speculateMany parents = mapM_ (speculateOne parents)
    
    speculateOne :: [Var] -> (Out Var, HeapBinding) -> SpecM ()
    speculateOne parents (x', hb)
      | spec_trace "speculate" (ppr x') False
      = undefined
      | HB InternallyBound (Right in_e) <- hb
      = --pprTrace "speculateOne" (ppr (x', annedTag (snd in_e))) $
        (\rb -> try_speculation in_e rb) `catchSpecM` speculation_failure (Just parents')
      | otherwise
      = speculation_failure Nothing
      where
        parents' = x' : parents
        spec_trace msg doc = pprTrace (replicate (length parents) ' ' ++ msg) doc
        speculation_failure mb_forbidden = modifySpecState $ \((hist, forbidden, ids), (stats, deeds, h_speculated_ok, h_speculated_failure)) -> (((hist, maybe id (:) mb_forbidden forbidden, ids), (stats, deeds, h_speculated_ok, M.insert x' hb h_speculated_failure)), ())
        try_speculation in_e rb = Monad.join (modifySpecState go)
          where go no_change@((hist, forbidden, ids), (stats, deeds, h_speculated_ok, h_speculated_failure)) = case terminate hist (gc state, parents', SpecM $ spec_trace "rolled back to" (ppr x') . unSpecM rb) of
                    Stop (_gced_old_state, old_parents', rb)
                      -> spec_trace "speculation denied" (ppr x' {- $$ pPrintFullState quietStatePrettiness (gc state) $$ pPrintFullState quietStatePrettiness _gced_old_state -})
                         (no_change, {- speculation_failure Nothing -} if any (`isSuffixOf` old_parents') forbidden then speculation_failure Nothing else rb) -- Don't allow rollback to rolled back region
                    Continue hist -> case reduceWithStats state of
                        (extra_stats, (deeds, Heap h_speculated_ok' ids, Loco _, qa))
                          | Just a <- traverse qaToAnswer qa
                          , let h_unspeculated = h_speculated_ok' M.\\ h_speculated_ok
                                in_e' = annedAnswerToInAnnedTerm (mkInScopeSet (annedFreeVars a)) a
                          -> (((hist, forbidden, ids), (stats `mappend` extra_stats, deeds, M.insert x' (internallyBound in_e') h_speculated_ok, h_speculated_failure)), speculateManyMap parents' h_unspeculated)
                        _ -> (no_change, speculation_failure Nothing)
                  where state = normalise (deeds, Heap h_speculated_ok ids, Loco False, in_e)
                        -- NB: try to avoid dead bindings in the state using 'gc' before the termination test so
                        -- that the termination condition is more lenient. This showed up in practice, in a version
                        -- of LetRec.hs where we had:
                        --   let dead = xs in 1 : xs `embed` let ys = 1 : ys in ys
                        -- (Because the tag on the ys/xs indirections was the cons-cell tag)
                        --
                        -- It's very important that we don' tjust gc the state itself because some of the h_speculated_ok
                        -- bindings might be live in the original state supplied to speculate, and we don't want to drop them!

type SpecState = ((LinearHistory (State, [Var], SpecM ()), [[Var]], InScopeSet), -- Kept during a rollback
                  (SCStats, Deeds, PureHeap, PureHeap))                          -- Discarded during a rollback
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
catchSpecM mx mcatch = SpecM $ \s@(_, sr) k -> unSpecM (mx (SpecM $ \(sl, _) _k -> unSpecM mcatch (sl, sr) k)) s k

reduce :: State -> State
reduce = thirdOf3 . reduce'

reduceWithFlag :: State -> (Bool, State)
reduceWithFlag state = case reduce' state of (reduced, _, state') -> (reduced, state')

reduceWithStats :: State -> (SCStats, State)
reduceWithStats state = case reduce' state of (_, stats, state') -> (stats, state')

reduce' :: State -> (Bool, SCStats, State)
reduce' orig_state = go False (mkLinearHistory rEDUCE_WQO) orig_state
  where
    -- NB: it is important that we ensure that reduce is idempotent if we have rollback on. I use this property to improve memoisation.
    go can_step hist state
      = -- traceRender ("reduce:step", pPrintFullState state) $
        case step state of
          Just (deeds, heap, k, e)
           | Just deeds' <- if bOUND_STEPS then claimStep deeds else Just deeds
           , let state' = (deeds', heap, k, e)
           -> case terminate hist state of
            Continue hist' -> go True hist' state'
            Stop old_state -> pprTrace "reduce-stop" {- (pPrintFullState quietStatePrettiness old_state $$ pPrintFullState quietStatePrettiness state) -} empty
                              -- let smmrse s@(_, _, _, qa) = pPrintFullState s $$ case annee qa of Question _ -> text "Question"; Answer _ -> text "Answer" in
                              -- pprPreview2 "reduce-stop" (smmrse old_state) (smmrse state) $
                              (can_step, mempty { stat_reduce_stops = 1 }, if rEDUCE_ROLLBACK then old_state else state') -- TODO: generalise?
           | otherwise -> (True, mempty, state)
          _ -> (can_step, mempty, state)



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

-- NB: it's important that we use localiseId on the absVarVar at binding sites, or else if we start
-- with a state where a global Id is lambda-bound (because there is no unfolding) we might end up having an h-function
-- that is lambda-abstracted over a global Id, which causes the assembler to barf.
--
-- (We will rely on a later simplifier run to propagate the local Id bindings down to the possibly-global Id use sites)
--
-- This will rarely happpen in practice because global variables should be Let-bound in the heap, which would prevent
-- us from lambda-abstracting over them. However, it can happen if the global is abstracted due to generalisation,
-- such as when the let-bound thing binds a (:) and we generalise away some other (:).
absVarBinder :: AbsVar -> Var
absVarBinder = localiseVar . absVarVar

localiseVar :: Var -> Var
localiseVar x | isId x    = localiseId x
              | otherwise = x

absVarLambdas :: Symantics ann => [AbsVar] -> ann (TermF ann) -> ann (TermF ann)
absVarLambdas xs = tyVarIdLambdas (map absVarBinder xs)

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
         
         -- Special-case RealWorld# because it occurs so often and we can save a "let" and
         -- "cast" in the output syntax by doing so:
         --
         -- (NB: the use of realWorldPrimId here and in the VoidRep case below means we have
         -- to special-case realWorldPrimId in the post-SC free-variable sanity checks)
         | ty `eqType` realWorldStatePrimTy
         -> e `app` realWorldPrimId

         -- If we get here we are getting desperate need to get *really* creative.
         -- Just choose some value with the same *representation* as what we want and then
         -- cast it to the right type:
         | let (e_repr_ty, e_repr) = case typePrimRep ty of
                 -- This causes the simplifier to fail with an error about (!!) out of bounds because
                 -- it tries to reduce the cast coercion, causing it to decompose the unsafe coercion:
                 --VoidRep   -> (mkCoercionType unitTy unitTy, coercion (mkUnsafeCo unitTy unitTy))
                 VoidRep   -> (realWorldStatePrimTy, var realWorldPrimId)
                 IntRep    -> (intPrimTy,            literal (mkMachInt 0))
                 WordRep   -> (wordPrimTy,           literal (mkMachWord 0))
                 Int64Rep  -> (int64PrimTy,          literal (mkMachInt64 0))
                 Word64Rep -> (word64PrimTy,         literal (mkMachWord64 0))
                 AddrRep   -> (addrPrimTy,           literal nullAddrLit)
                 FloatRep  -> (floatPrimTy,          literal (mkMachChar 'x'))
                 DoubleRep -> (doublePrimTy,         literal (mkMachDouble 0))
                 -- Unlifted thing of PtrRep: yes, this can really happen (ByteArray# etc)
                 -- This is the most annoying case because there is no convenient global name of the right type
                 PtrRep    -> (threadIdPrimTy,       case_ (var (mkPrimOpId MyThreadIdOp) `app` realWorldPrimId)
                                                           (mkWildValBinder (unboxedPairTyCon `mkTyConApp` [realWorldStatePrimTy, threadIdPrimTy]))
                                                           threadIdPrimTy [(DataAlt unboxedPairDataCon [] [] [mkWildValBinder realWorldStatePrimTy, x_tid], var x_tid)])
                   where x_tid = x `setVarType` threadIdPrimTy
         -> let_ x (e_repr `cast` mkUnsafeCo e_repr_ty ty) (e `app` x))
         where shadowy_x = absVarBinder absx
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

-- NB: we abstract over RealWorld# as well (cf WwLib). Two reasons:
--  1. If the h-function is unlifted, this delays its evaluation (so its effects, if any, do not happen too early).
--     This is also necessary since h-functions will be bound in one letrec after supercompilation is complete.
--  2. This expresses to GHC that we don't necessarily want the work in h-functions to be shared.
stateAbsVars :: Maybe FreeVars -> State -> ([AbsVar], Type)
stateAbsVars mb_lvs state = (abstracted, realWorldStatePrimTy `mkFunTy` (vs_list `mkPiTypes` state_ty))
  where vs_list = sortLambdaBounds (varSetElems (stateLambdaBounders state))
        state_ty = stateType state
        abstracted = AbsVar { absVarDead = True, absVarVar = voidArgId } :
                     map (\v -> AbsVar { absVarDead = maybe False (not . (v `elemVarSet`)) mb_lvs, absVarVar = v }) vs_list

sortLambdaBounds :: [Var] -> [Var]
sortLambdaBounds = sortBy (comparing (not . isTyVar)) -- True type variables go first since coercion/value variables may reference them


-- | Free variables that are allowed to be in the output term even though they weren't in the input (in addition to h-function names)
extraOutputFvs :: FreeVars
extraOutputFvs = unitVarSet realWorldPrimId
