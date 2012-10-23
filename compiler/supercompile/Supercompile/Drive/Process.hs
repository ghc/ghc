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

    AbsVar(..), mkLiveAbsVar, absVarLambdas, applyAbsVars, stateAbsVars,

    extraOutputFvs
  ) where

#include "HsVersions.h"

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming
--import Supercompile.Core.Size
import Supercompile.Core.Syntax
import Supercompile.Core.Tag

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

import Var        (isTyVar, isId, tyVarKind, varType, setVarType)
import Id         (idType, zapIdOccInfo, zapFragileIdInfo, localiseId, isDictId, isGlobalId)
import MkId       (voidArgId, realWorldPrimId, mkPrimOpId)
import Coercion   (Coercion(..), isCoVar, mkCoVarCo, mkUnsafeCo, coVarKind)
import TyCon      (PrimRep(..))
import Type       (Kind, isUnLiftedType, mkTyVarTy, eqType, mkFunTy, mkPiTypes, mkTyConApp, typePrimRep, splitTyConApp_maybe)
import Kind       (isUnliftedTypeKind)
import TysPrim
import TysWiredIn (unboxedPairDataCon, unboxedPairTyCon)
import MkCore     (mkWildValBinder, quantVarLe)
import PrimOp     (PrimOp(MyThreadIdOp))
import Literal
import VarEnv
import Util       (fstOf3, thirdOf3, sortLe)

import qualified Control.Monad as Monad
import Data.Ord
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Monoid (Monoid(mappend, mempty))
import qualified Data.Set as S
import qualified Data.Graph as G


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

sET_WQO :: TTest State
sET_WQO = cofmap (tagBagTagSet . stateTags) finiteT


type ParentChildren = M.Map (Maybe Var) [(Var, (State, Bool))]

emptyParentChildren :: ParentChildren
emptyParentChildren = M.empty

addChild :: Maybe Var -> Var -> State -> Bool -> ParentChildren -> ParentChildren
addChild mb_parent child child_state gen = M.alter (\mb_children -> Just ((child, (child_state, gen)) : (mb_children `orElse` []))) mb_parent

-- Shows nodes in the graph arranged by number of descendants:
childrenSummary :: ParentChildren -> String
childrenSummary parent_children = unlines [intercalate " " (map (maybe "<root>" varString) mb_parents) ++ ": " ++ intercalate " " (map show child_counts)
                                          | (child_counts :: [Int], mb_parents) <- grouped_counts]
  where descendant_counts = flip M.map parent_children $ \children -> map ((+1) . sum . flip (M.findWithDefault [] . Just) descendant_counts . fst) children
        ordered_counts = sortBy (comparing (Down . sum . snd)) (M.toList descendant_counts)
        grouped_counts = runs snd fst ordered_counts

-- Shows the number of children at each depth:
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

-- Shows the deepest path encountered and the big values on the route there:
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
             -> case v of Data dc tys cos ys -> pPrintPrecApps prec dc ([asPrettyFunction ty | ty <- tys] ++ [asPrettyFunction co | co <- cos] ++ [PrettyFunction (\prec -> go shown' prec y) | y <- ys])
                          _ -> pprPrec prec v
      _ -> pprPrec prec x


type TagAnnotations = IM.IntMap [String]

-- Shows a guesstimate about what bits of original syntax residualised syntax was based on.
-- For each "cost centre", shows information in this format:
--   #of bits of syntax(# of distinct tags)
tagSummary :: TagAnnotations -> Int -> Int -> ResidTags -> String
tagSummary anns precision n resid_tags = unlines $ take n [intercalate "." ann ++ "\t" ++ show_occs occs | (ann, occs) <- show_sorted_ann_occs] ++ ["Other:\t" ++ show_occs rest_occs]
  where ann_occs = M.unionsWith plus_occs [M.singleton (take precision ann) (1 :: Int, occs) | (tag, occs) <- IM.toList resid_tags, let Just ann = IM.lookup tag anns]
        plus_occs (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
        show_occs (init_occs, occs) = show occs ++ "(" ++ show init_occs ++ ")"
        sorted_ann_occs = sortBy (comparing (Down . snd . snd)) (M.toList ann_occs)
        (show_sorted_ann_occs, rest_sorted_ann_occs) = splitAt n sorted_ann_occs
        rest_occs = foldl' plus_occs (0, 0) (map snd rest_sorted_ann_occs)

tagAnnotations :: State -> TagAnnotations
tagAnnotations (_, Heap h _, k, qa) = IM.unions [go_term (extAnn x []) e | (x, hb) <- M.toList h, Just (_, e) <- [heapBindingTerm hb]] `IM.union` go_qa e_ann qa `IM.union` resid_tags
  where
    extAnn x ann = showSDoc (ppr x):ann

    insert' ann tg tag_anns = IM.insert (tagInt tg) ann tag_anns

    insert ann tg (Nothing,    tag_anns) = insert'  ann  tg tag_anns
    insert _   tg (Just (ann), tag_anns) = insert' [ann] tg tag_anns

    (e_ann, resid_tags) = trainCarFoldr (\kf (ann, resid_tags) -> second (`IM.union` resid_tags) $ go_kf ann kf) ([], IM.empty) k
    
    go_qa :: [String] -> Anned QA -> TagAnnotations
    go_qa ann qa = insert ann (annedTag qa) $ go_qa' ann (annee qa)

    go_qa' _   (Question _) = (Nothing, IM.empty)
    go_qa' ann (Answer a)   = go_answer' ann a

    go_term :: [String] -> AnnedTerm -> TagAnnotations
    go_term ann e = insert ann (annedTag e) $ go_term' ann (annee e)

    go_term' ann e = case e of
      Var _ -> (Nothing, IM.empty)
      Value v -> go_value' ann v
      TyApp e _ -> (Nothing, go_term ann e)
      CoApp e _ -> (Nothing, go_term ann e)
      App e _   -> (Nothing, go_term ann e)
      PrimOp _ _ es   -> (Nothing, IM.unions (map (go_term ann) es))
      Case e x _ alts -> (Nothing, go_term (extAnn x ann) e `IM.union` IM.unions [go_alt_con alt_con $ go_term ann e | (alt_con, e) <- alts])
      Let x e1 e2     -> (Nothing, go_term (extAnn x ann) e1 `IM.union` go_term ann e2)
      LetRec xes e    -> (Nothing, IM.unions [go_term (extAnn x ann) e | (x, e) <- xes] `IM.union` go_term ann e)
      Cast e _        -> (Nothing, go_term ann e)

    go_alt_con alt_con = case alt_con of
      DataAlt dc _ _ _ -> insert' [show dc] (dataConTag dc)
      LiteralAlt l     -> insert' [show l]  (literalTag l)
      DefaultAlt       -> id
    
    -- NB: this is carefully set up so that we map all those tags that are likely to
    -- be literalTags/dataConTags that occur multiple times in *all* tagged terms to
    -- the same annotation.
    go_value' ann v = case v of
        Literal l     -> (Just (show l),  IM.empty)
        Coercion _    -> (Nothing,        IM.empty)
        TyLambda _ e  -> (Nothing,        go_term ann e)
        Lambda   _ e  -> (Nothing,        go_term ann e)
        Data dc _ _ _ -> (Just (show dc), IM.empty)

    go_cast_answer :: [String] -> Anned (Coerced Answer) -> TagAnnotations
    go_cast_answer ann a = insert ann (annedTag a) $ go_answer' ann (snd (annee a))

    go_answer' ann (_, v) = go_value' ann v

    go_kf :: [String] -> Tagged StackFrame -> ([String], TagAnnotations)
    go_kf ann kf = second (insert' ann (tag kf)) $ go_kf' ann (tagee kf)

    go_kf' ann kf = case kf of
      TyApply _ -> (ann, IM.empty)
      CoApply _ -> (ann, IM.empty)
      Apply _   -> (ann, IM.empty)
      Scrutinise x _ (_, alts) -> (extAnn x ann, IM.unions [go_term ann e | (_, e) <- alts])
      PrimApply _ _ as es -> (ann, IM.unions [go_cast_answer ann a | a <- as] `IM.union` IM.unions [go_term ann e | (_, e) <- es])
      StrictLet x (_, e)  -> (extAnn x ann, go_term ann e)
      Update x            -> (extAnn x ann, IM.empty)
      CastIt _            -> (ann, IM.empty)



-- Be very careful when you change this function. At one point it accounted for 10% of
-- supercompile runtime, so I went to a lot of trouble to deforest everything in sight.
prepareTerm :: M.Map Var Term -> Term -> (S.Set Var,                    -- Names of all unfoldings in the input heaps
                                          ([(Var,   FVedTerm)], [(State, FVedTerm)], State), -- For use without memo-cache preinitalization
                                          (                     [(State, FVedTerm)], State)) -- Without expression-carrying "let"-binding in the heaps (may still contain non-expression-carrying "let"-bindings)
prepareTerm unfoldings e = {-# SCC "prepareTerm" #-}
                           pprTraceSC "unfoldings" (pPrintPrecLetRec noPrec (M.toList unfoldings) (PrettyDoc (text "<stuff>"))) $
                           pprTraceSC "all input FVs" (ppr (input_fvs `delVarSetList` S.toList unfolding_bvs)) $
                           (unfolding_bvs, pprTraceSC "no-preinit unfoldings" (pPrintPrecLetRec noPrec (M.toList h'') (PrettyDoc (text "<stuff>")))
                                                      (h''_must_be_bound, letty_preinit_with, state),
                                           (preinit_with, preinit_state))
  where (tag_ids0, tag_ids1) = splitUniqSupply tagUniqSupply
        anned_e = toAnnedTerm tag_ids0 e
        
        -- NB: the code below assumes h_unfoldings is in ascending order so it can get a little speed boost
        (input_fvs, tag_ids2, h_unfoldings) = foldr3WithKey' add_one_unfolding (annedTermFreeVars anned_e, tag_ids1, []) unfoldings
          where add_one_unfolding x' e (input_fvs', tag_ids1, h_unfoldings) = (input_fvs'', tag_ids2, (x', renamedTerm anned_e):h_unfoldings)
                    where !(tag_unf_ids, tag_ids2) = splitUniqSupply tag_ids1
                          anned_e = toAnnedTerm tag_unf_ids e
                          input_fvs'' = input_fvs' `unionVarSet` varBndrFreeVars x' `unionVarSet` annedFreeVars anned_e
        
        -- A funny thing can happen here. We can have an unfolding like foo:
        --  foo = Just bar
        -- Which mentions a variable bar which (when used as a *binder*, not an *occurrence*) would
        -- have free variables, like:
        --  {-# INLINE bar #-}
        --  bar = .. bar .. baz
        -- But we might not necessarily have a bar binding in the unfoldings. So we might end up
        -- adding an environmentallyBound binding for bar to h_fvs which then has an unbound free
        -- variable baz. This would violate our heap invariant!
        --
        -- My solution is to make the construction of h_fvs into a fixed point where the set of
        -- variables to make environmentally bound grows until it encompasses all the FVs of those
        -- variables themselves.
        --
        -- I think it would also be OK to zap the IdInfo of environmentally bound stuff, but that isn't
        -- optimal because the simplifier won't be able to restore the IdInfo of *global* Ids, so we might
        -- pessimisize all later simplifications.
        (input_fvs', h_fvs) = (closure emptyVarSet input_fvs, M.fromDistinctAscList $ snd $ foldToMapAccumL foldVarSet add_one_fv tag_ids2 input_fvs')
          -- NB: foldVarSet is a right fold, so this use of fromDistinctAscList is justified
          where
            closure all_input_fvs new_fvs
              | isEmptyVarSet new_fvs = all_input_fvs
              | otherwise             = closure all_input_fvs' new_fvs'
                where all_input_fvs' = all_input_fvs `unionVarSet` new_fvs
                      new_fvs' = foldVarSet (\x' -> (varBndrFreeVars x' `unionVarSet`)) emptyVarSet new_fvs `minusVarSet` all_input_fvs'
            add_one_fv tag_ids x' = (tag_ids', (x', environmentallyBound (mkTag (getKey i))))
              where !(i, tag_ids') = takeUniqFromSupply tag_ids

        unfolding_bvs = S.fromDistinctAscList (map fst h_unfoldings)
        deeds = Deeds { sizeLimit = (bLOAT_FACTOR - 1) * annedSize anned_e, stepLimit = (bLOAT_FACTOR - 1) * annedSize anned_e }
        (speculated, (_stats, deeds', Heap h' ids')) = speculateHeap S.empty (mempty, deeds, Heap (M.fromDistinctAscList (map (second2 internallyBound) h_unfoldings) `M.union` h_fvs) ids)
        -- FIXME: use speculated

        -- NB: h_fvs might contain bindings for things also in h_unfoldings, so union them in the right order
        rn = mkIdentityRenaming input_fvs'
        ids = mkInScopeSet input_fvs'
        {-# INLINE heap_binding_is_cheap #-}
        heap_binding_is_cheap = maybe True (termIsCheap . snd) . heapBindingTerm

        -- When *not* doing memocache preinitialization, we still want to be able to speculate the unfoldings to
        -- discover more values (after all, the evaluator can only inline LetBound values). But this might cause
        -- us to create new top level bindings e.g. when speculating (x = let a = y |> co in D a).
        --
        -- The speculated heap' contain *internally bound* top level bindings (or the speculator won't do anything
        -- to them) so we must be careful to change them to *let bound* before we put them in the user-visible heap.
        
        -- First, eliminate any non-cheap unfolding bindings and any cheap binding that refers to any one
        -- of them *that is not one of the original bindings*
        --
        -- Consider these unfoldings:
        --  meth1 = \x -> ..
        --  meth2 = fib 100
        --  dict = D meth1 meth2
        --
        -- We don't want to make the whole dict unavailable just because meth2 isn't cheap
        --
        -- However in this case:
        --  foo = let a = fib 100 in D a
        --
        -- We do want to make "foo" unavailable as there is no cheap unfolding we can give it
        -- that ensures that we share the work of "fib 100" with all other modules.
        (eliminate_set_noncheap, h'_cheap) = funny_partition (\hb -> hb { howBound = LetBound }) (not . heap_binding_is_cheap) h'
        h'' = go eliminate_set_noncheap h'_cheap
          where go eliminate_set h
                  | isEmptyVarSet eliminate_set' = h_trimmed
                  | otherwise                    = go eliminate_set' h_trimmed
                  where (eliminate_set', h_trimmed) = funny_partition id (\hb -> heapBindingFreeVars hb `intersectsVarSet` eliminate_set) h
        -- Given a predicate and a heap, returns pair of:
        --  1. The satisfying ids that are NOT available as imports
        --  2. Heap containing just the unsatisfying elements
        --
        -- Also takes a "fiddle" function which is used to modify the unsatisfying
        -- elements, so we can get just a *little* bit more deforestation.
        {-# INLINE funny_partition #-}
        funny_partition fiddle p h = foldl2WithKey' go (emptyVarSet, M.empty) h
          where go (killed, kept) x' hb
                 | p hb = if x' `S.member` unfolding_bvs
                           then (killed, kept)
                           else (extendVarSet killed x', kept)
                 | otherwise = (killed, M.insert x' (fiddle hb) kept)

        -- Secondly, pull out any remaining bindings (which must be cheap) that didn't exist in the
        -- unspeculated heap. These will be our new top-level bindings (and are guaranteed to be LocalIds).
        (h''_binds_globalid, h''_was_floated) = M.partitionWithKey (\x' _ -> x' `S.member` unfolding_bvs) h''
        h''_must_be_bound = [ (x', annedTermToFVedTerm (renameIn (renameAnnedTerm ids') in_e))
                            | (x', hb) <- M.toList h''_was_floated
                            , Just in_e <- [heapBindingTerm hb] ]

        heap = Heap (h'' `M.union` h_fvs) ids'
        state = normalise (deeds', heap, Loco False, (rn, anned_e))
        
        -- FIXME: update other comments about memocache preinit
        -- We can and should do memocache preinit even if we still use "let"-bindings in the heap with associated terms
        letty_preinit_with = [ res
                             | (x', hb) <- M.toList h''_binds_globalid
                             , Just in_e_def <- [heapBindingTerm hb]
                             , res <- eta [] heap (var x') in_e_def (annedTerm (annedTag (snd in_e_def)) (Var x'))]
        
        -- When doing memocache preinitialization, we don't want to include in the final heap any binding originating
        -- from evaluating the top-level that cannot be proven to be a value, or else we risk work duplication
        preinit_h'_cheap = M.filter heap_binding_is_cheap h'
        -- Global variables are SO ANNOYING. If we don't localise both the binding sites and the occurrence sites
        -- then preinit tieback won't work.
        preinit_rewrite (rn, e) = renamedTerm (rewriteGlobals (\x -> if x `M.member` preinit_h'_cheap then localiseId x else x) (renameAnnedTerm ids' rn e))
        preinit_h_avail = M.map (\hb -> hb { heapBindingMeaning = fmap preinit_rewrite (heapBindingMeaning hb) }) (M.mapKeysMonotonic localiseVar preinit_h'_cheap)
        preinit_state = normalise (deeds', preinit_heap, Loco False, preinit_rewrite (rn, anned_e))
        preinit_heap = Heap (preinit_h_avail `M.union` h_fvs) ids'

        -- NB: we assume that unfoldings are guaranteed to be cheap and hence duplicatiable. I think this is reasonable.
        preinit_with = [ res
                       | (x', in_e_def) <- h_unfoldings
                       , res <- eta [] preinit_heap (var x') in_e_def (annedTerm (annedTag (snd in_e_def)) (Var x'))]

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
--
-- It's cool to use normalise here instead of termToAnswer just in case we can do some sweet stuff like look through
-- lets in the body or maybe even simplify a case given the stuff in the heap. (NB: there this does not cause work dup.)
-- I won't go as far as using "speculate" because in that case "eta" could potentially yield an infinite list
-- (for some non-HM-typeable programs).
--
-- NB: given an unfolding (f = \x y -> e) will return memo entries for all the states (\x y -> e), (\y -> e), e, f, (f x) and (f x y).
-- Because we do not reduce before matching, both are necessary!
eta :: [Var] -> Heap -> FVedTerm -> In AnnedTerm -> AnnedTerm -> [(State, FVedTerm)]
eta xs' heap tieback_e1 in_e_def e_call1 = res in_e_def : res (renamedTerm e_call1) : case normalise (maxBound, heap, Loco False, in_e_def) of
    (_, Heap h ids, k, anned_qa)
      | Answer (rn, v) <- extract anned_qa
      , Just a_cast <- isCastStack_maybe k
      , let (tieback_e2, e_call2) = case a_cast of
              Uncast       -> (tieback_e1,               e_call1)
              CastBy co tg -> (tieback_e1 `cast` sym_co, annedTerm tg (e_call1 `Cast` sym_co))
                where sym_co = mkSymCo ids co
            mb_res@(~(Just (_, x, _, _))) = case v of
               -- NB: must zap binder occ info in case it is marked as dead, we can't mark the occurrence site we use for the fulfilment as dead!
               Lambda   x e_def_body -> Just (tieback_e2 `app`   x',           zapIdOccInfo x, e_def_body, annedTerm (annedTag e_def_body) (e_call2 `App`   x'))
               TyLambda a e_def_body -> Just (tieback_e2 `tyApp` mkTyVarTy x', a,              e_def_body, annedTerm (annedTag e_def_body) (e_call2 `TyApp` mkTyVarTy x'))
               _                     -> Nothing
            (ids', rn', x') = renameNonRecBinder ids rn x
      , Just (tieback_e2, _, e_def_body, e_call_body) <- mb_res
      -> eta (x':xs') (Heap (M.insert x' lambdaBound h) ids') tieback_e2 (rn', e_def_body) e_call_body
    _ -> []
  where res anned_e' = (case gc (normalise (maxBound, heap, Loco False, anned_e')) of (steps, Heap h ids, k, in_e') -> (steps, Heap (foldr (\x' -> M.insert x' lambdaBound) h xs') ids, k, in_e'), tieback_e1)
          -- NB: have to add the xs' back into the State after GC because some lambdaBounds from eta-expansion
          -- might not referenced in the body of the expanded lambda, but WILL referenced from the
          -- application to x' that we use to fulfill, in which case gcing would leave some variables free in that tieback!

-- FIXME: broken right now because we need to rewrite free variables within binders as well (i.e. rules, specialisations etc). Very tedious!
{-# INLINE rewriteGlobals #-}
rewriteGlobals :: (Var -> Var) -> AnnedTerm -> AnnedTerm
rewriteGlobals f = term
  where
    var' x | isGlobalId x = f x
           | otherwise    = x

    term e = annedTerm (annedTag e) (term' (extract e))

    term' (Var x)             = Var (var' x)
    term' (Value v)           = Value (value' v)
    term' (TyApp e ty)        = TyApp (term e) ty
    term' (CoApp e co)        = CoApp (term e) (coercion' co)
    term' (App e x)           = App (term e) (var' x)
    term' (PrimOp pop tys es) = PrimOp pop tys (map term es)
    term' (Case e x ty alts)  = Case (term e) x ty [(alt_con, term e) | (alt_con, e) <- alts]
    term' (Let x e1 e2)       = Let x (term e1) (term e2)
    term' (LetRec xes e)      = LetRec [(x, term e) | (x, e) <- xes] (term e)
    term' (Cast e co)         = Cast (term e) (coercion' co)

    value' (Literal l)          = Literal l
    value' (Coercion co)        = Coercion (coercion' co)
    value' (TyLambda a e)       = TyLambda a (term e)
    value' (Lambda x e)         = Lambda x (term e)
    value' (Data dc tys cos xs) = Data dc tys (map coercion' cos) xs

    coercion' (Refl ty)            = Refl ty
    coercion' (TyConAppCo tc cos)  = TyConAppCo tc (map coercion' cos)
    coercion' (AppCo co1 co2)      = AppCo (coercion' co1) (coercion' co2)
    coercion' (ForAllCo a co)      = ForAllCo a (coercion' co)
    coercion' (CoVarCo a)          = CoVarCo (var' a)
    coercion' (AxiomInstCo ax cos) = AxiomInstCo ax (map coercion' cos)
    coercion' (UnsafeCo ty1 ty2)   = UnsafeCo ty1 ty2
    coercion' (SymCo co)           = SymCo (coercion' co)
    coercion' (TransCo co1 co2)    = TransCo (coercion' co1) (coercion' co2)
    coercion' (NthCo i co)         = NthCo i (coercion' co)
    coercion' (InstCo co ty)       = InstCo (coercion' co) ty


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
-- == Speculation ==
--

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
--
-- Note [Old speculate]
-- ~~~~~~~~~~~~~~~~~~~~
--
-- An older verison of speculate only allowed speculated bindings to see *values* in the heap, and tried to
-- drive the heap in topological order so that as many dependent terms were reduces to values as possible
-- before reaching the terms that used them.
--
-- However, I don't seem to have a good justification for doing this rather than the more obvious
-- plan of just speculating heap bindings one by one in some arbitrary order but with all heap bindings
-- simultaneously available.
--
-- (CHSC introduced the values-only behavior in 29db9669 with no comments and in the same commit that added rollback,
-- and the behaviour was imported to GHC from there.)
--
-- Note [Rollback in threaded histories]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- I used to prevent any rollback at all when we detected a termination test failure against a non-ancestor.
-- But this leads to some annoying behaviour, such as a |iterate not True| binding getting speculated to:
--   let xs0 = x1 : xs1
--       x1 = False
--       xs1 = x2 : xs2
--       x2 = True
--       xs2 = x3 : xs3
--       x3 = not x2
--       xs3 = iterate not x3
--   in Just xs0
--
-- Which then prevents deforesting the |not| calls because we can't push the non-value x3 into the
-- drive of the recursive call |iterate not x3|, since it's used non-linearly!!!
--
-- I fixed this by rolling back to the common ancestor of the two states, which in this case means that
-- iterate doesn't get speculated at all. When doing this we have to be very careful that we don't
-- test the termination condition against values because otherwise if we speculated two different |True|
-- bindings at top level we would roll back to the start of "speculate" and as a result wouldn't speculate
-- anything in that heap at all!

-- Note [Lets created by reduce are special]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Consider:
--   let x = True
--   in iterate not x
--
-- Reduce [#]:
--   let x = True
--       y = not x
--       ys = iterate not y
--   in x : ys
--
-- Split (x is non-linear but value):
--   let x = True
--       y = not x
--   in iterate not y
--
-- Reduce [*]:
--   let x = True
--       y = not x
--       z = not y
--       zs = iterate not z
--   in y : zs
--
-- Split (y is non-linear but not value):
--   let z = not y
--   in iterate not z
--
-- So in fact we don't get good results without *any* speculation. But notice that if we try to speculate
-- with rollback to common ancestor, then the heap in step [#] will roll back so that no speculation
-- occurs at all, since speculating "iterate not y" eventually speculates something that embeds into "not x"
-- and so we roll back to the common ancestor with "y = not x" i.e. the top level!
--
-- For this reason I have decided to treat the top level of lets as special i.e. don't allow rolling back
-- in such a way that it prevents speculation of the entire top level. I'm still rolling back to an enclosing
-- speculateMany for the recursive calls, though..

-- FIXME: if I speculate roughly in dependency order then GHCs inlining heuristics will have more information
-- to work with in the reduce invocations
speculate :: AlreadySpeculated -> (SCStats, State) -> (AlreadySpeculated, (SCStats, State))
speculate speculated (stats, (deeds, heap, k, in_e)) = (speculated', (stats', (deeds', heap', k, in_e)))
  where (speculated', (stats', deeds', heap')) = speculateHeap speculated (stats, deeds, heap)

--type SpecRB = SpecHistory -> SpecM (SpecHistory, Deeds, Heap)
newtype SpecHistory = SH { unSH :: LinearHistory (State, Depth) }

-- FIXME: AlreadySpeculated should be renamed when we generalise since heap bindings may change name

type SpecRB = SpecHistory -> SpecM (SpecHistory, Deeds, InScopeSet, PureHeap)
type Depth = [(SpecRB, Var)]

speculateHeap :: AlreadySpeculated -> (SCStats, Deeds, Heap) -> (AlreadySpeculated, (SCStats, Deeds, Heap))
speculateHeap already_speculated (stats, deeds, heap) = (already_speculated, (stats, deeds', heap'))
  where (deeds', heap') = speculateHeapIdempotent (deeds, heap)

-- NB: for idempotence, it is necessary that:
--  1. We speculate bindings in dependency order
--  2. We only make things strictly preceding the current binding available in the heap
--  3. Mutually recursive groups identified by the dependency order thing must reduce to values
--     together or fail together, to ensure that non-values are reduced in a consistent environemnt
--     of values between two composed "speculate" calls (or else speculation of a letrec might break
--     the cycle so we speculate them as an acyclic loop next time, exposing more information)
--  4. History is not threaded at the top level (since speculation may put create some new top-level bindings,
--     and we don't want their new presence in the history causing the speculation of a later binding to roll back
--     further than it did last time)
--
-- NB: speculation of a cyclic SCC or non-top-level acyclic SCCs might fail in one order but succeed in another
--     due to the *history* (not value environment), how to resolve this? Well, FIXME (maybe using a symmetric history like (==) on TagBag?)
speculateHeapIdempotent :: (Deeds, Heap) -> (Deeds, Heap)
speculateHeapIdempotent (deeds, Heap init_h ids) = {-# SCC "speculate" #-} (deeds', Heap h' ids')
  where
    -- NB: must use set wqo so that order of traversal of the SCCs/elements within a CyclicSCC is irrelevant
    hist = SH $ mkLinearHistory (cofmap fst sET_WQO)

    (deeds', ids', h') = speculateTopHeap (deeds, ids) init_h
    --(deeds', ids', h') = runSpecM $ fmap (\(_hist, deeds, ids, h) -> (deeds, ids, h)) $ speculateNestedHeap (return ()) [] hist (deeds, ids, M.empty) h
    --(deeds', ids', h') = runSpecM $ callCC $ \k -> fmap (\(_hist, deeds, ids, h) -> (deeds, ids, h)) $ speculateNestedHeap (return ()) [(\_ -> k (deeds, ids, h), voidArgId)] hist (deeds, ids, M.empty) h

    speculateTopHeap :: (Deeds, InScopeSet) -> PureHeap -> (Deeds, InScopeSet, PureHeap)
    speculateTopHeap (deeds, ids) h = foldl' speculateTopSCC (deeds, ids, M.empty) (topologicalSort heapBindingFreeVars h)
      where speculateTopSCC (deeds, ids, h') scc = runSpecM $ fmap (\(_hist, deeds, ids, h) -> (deeds, ids, h)) $ speculateSCC (return ()) [] M.empty (hist, deeds, ids, h') scc

    speculateNestedHeap :: SpecM ()
                        -> Depth -> SpecHistory -> (Deeds, InScopeSet, PureHeap {- dom = X -}) -> PureHeap {- dom = Y && dom \disjoint X -}
                        -> SpecM (SpecHistory, Deeds, InScopeSet, PureHeap {- Y \subseteq dom && dom \disjoint X -})
    speculateNestedHeap fail_rb depth hist (deeds, ids, h) h_difference = foldM (speculateSCC fail_rb depth h) (hist, deeds, ids, M.empty) (topologicalSort heapBindingFreeVars h_difference)

    -- NB: CyclicSCCs only succeeds if all bindings in the group were totally driven to values, since anything else may break loops and hence change visibility on a subsequent drive
    speculateSCC :: SpecM ()
                 -> Depth -> PureHeap {- dom = X -} -> (SpecHistory, Deeds, InScopeSet, PureHeap {- dom = Y && dom \disjoint X -}) -> G.SCC (Out Var, HeapBinding) {- dom = Z && dom \disjoint X -}
                 -> SpecM (SpecHistory, Deeds, InScopeSet, PureHeap {- (Y \union Z) \subseteq dom && \dom \disjoint X -})
    speculateSCC fail_rb depth h (hist, deeds, ids, h') scc = case scc of
        G.AcyclicSCC (x', hb) -> fmap (\(hist, deeds, ids, h'_extra) -> (hist, deeds, ids, h'_extra `M.union` h')) $ speculateHB fail_rb depth hist (deeds, ids, h `M.union` h') x' hb
        G.CyclicSCC xhbs      -> callCC (\k -> let fail_rb' = fail_rb >> k (hist, deeds, ids, h' `M.union` M.fromList xhbs)
                                                   -- NB: the union in "go" might overwrite some things because we pass the same full incoming heap into each recursive call.
                                                   -- We put the accumulated heap on the left of the union (and set the initial heap to the incoming heap) to ensure that
                                                   -- any bindings present in h' are present unchanged in 
                                                   go (hist, deeds, ids, h'') (x', hb) = fmap (\(hist, deeds, ids, h''_extra) -> (hist, deeds, ids, h''_extra `M.union` h'')) $ speculateHB fail_rb' depth hist (deeds, ids, h `M.union` h') x' hb
                                               in foldM go (hist, deeds, ids, h') xhbs)

    -- NB: returned heap only includes the input binding and any new such bindings floated out of it
    speculateHB :: SpecM ()
                -> Depth -> SpecHistory -> (Deeds, InScopeSet, PureHeap {- dom = X -}) -> Out Var {- x -} -> HeapBinding
                -> SpecM (SpecHistory, Deeds, InScopeSet, PureHeap {- { x } \subseteq dom && dom \disjoint X -})
    speculateHB fail_rb depth hist (deeds, ids, h) x' hb = case hb of
      HB InternallyBound (Right in_e)
        | let state = normalise (deeds, Heap h ids, Loco False, in_e)
              (reduced, (deeds', Heap h' ids', k', qa')) = reduceWithFlag state
              -- NB: some of the bindings in h might have been newly reduced to values by the
              -- above call, but we'll just throw that work away for simplicity
              -- (this should be rare anyway since the bindings in h are already speculated)
              h_difference = h' M.\\ h
        , Just cast_by <- isCastStack_maybe k'
        , let hb' = internallyBound $ castAnnedQAToInAnnedTerm (mkInScopeSet (castByFreeVars cast_by `unionVarSet` annedFreeVars qa')) qa' cast_by
              recurse depth' hist' = fmap (\(hist, deeds, ids, h') -> (hist, deeds, ids, M.insert x' hb' h')) $ speculateNestedHeap fail_rb depth' hist' (deeds', ids', h) h_difference
        -> if not reduced -- Don't need to check the termination test (and risk rollback) if the focus
                          -- is already reduced (recursive call will be strictly smaller)
            then recurse depth hist
            else trce depth "reducing" (ppr x') $
                 catchSpecM (\rb   -> let depth' = (rb, x') : depth
                                      in case terminate (unSH hist) (gc state, depth') of
                                           Stop (_, old_depth) -> trce depth "stopped" (ppr x') $ commonAncestorRB old_depth depth hist
                                           Continue hist'      -> recurse depth' (SH hist'))
                            (\hist -> trce depth "caught" (ppr x')  $ fail_rb >> return (hist, deeds, ids, M.singleton x' hb))
        | otherwise
        -> trce depth "non-value" (ppr x') $ fail_rb >> return (hist, deeds, ids, M.singleton x' hb)
      _ ->                                              return (hist, deeds, ids, M.singleton x' hb)

    trce depth msg = pprTrace ("spec:" ++ replicate (length depth) ' ' ++ msg)

    commonAncestorRB :: Depth -> Depth -> SpecRB
    commonAncestorRB old_depth depth = head (thirdOf3 (listExtensionBy (\(old_rb, old_x) (_, x) -> if old_x == x then Just old_rb else Nothing) old_depth depth))

{-
type Depth = Train (SpecRB, Var) (SpecRB, Var)

-- NB: we can't use the domain of the heap incoming to the previous "reduce" call as the AlreadySpeculated
-- set because normalisation may have caused some unspeculated bindings to enter the heap since the last "speculate"
-- (e.g. consider splitting into the branch of a case where the branch has some top level lets)
speculateHeap :: AlreadySpeculated -> (SCStats, Deeds, Heap) -> (AlreadySpeculated, (SCStats, Deeds, Heap))
speculateHeap speculated (stats, deeds, heap@(Heap h _)) = {-# SCC "speculate" #-} (M.keysSet h', (mempty, deeds', heap'))
  where
    hist = SH $ mkLinearHistory (cofmap fst wQO)
    -- For iterate to fuse, it's important that you choose the second of these implementations:
    --(_, deeds', heap'@(Heap h' _)) = runSpecM (catchSpecM (\rb   -> speculateMany Loco (M.keysSet h S.\\ speculated) hist deeds heap)
    --                                                      (\hist -> return (hist, deeds, heap)))
    (deeds', heap'@(Heap h' _)) = S.foldr (\x' (deeds, heap) -> case runSpecM (speculateOne' Loco x' hist deeds heap) of (_, deeds, heap) -> (deeds, heap)) (deeds, heap) (M.keysSet h S.\\ speculated)

    -- For iterate to fuse, it doesn't matter which of these implementations you use:
    speculateMany :: ((SpecRB, Var) -> Depth) -> S.Set (Out Var) -> SpecHistory -> Deeds -> Heap -> SpecM (SpecHistory, Deeds, Heap)
    --speculateMany mk_depth xs' hist deeds heap = catchSpecM (\rb   -> foldM (\(hist, deeds, heap) x' -> speculateOne (mk_depth (rb, x')) x' hist deeds heap) (hist, deeds, heap) (S.toList xs'))
    --                                                        (\hist -> return (hist, deeds, heap))
    speculateMany mk_depth xs' hist deeds heap = foldM (\(hist, deeds, heap) x' -> speculateOne' mk_depth x' hist deeds heap) (hist, deeds, heap) (S.toList xs')

    speculateOne' mk_depth x' hist deeds heap = catchSpecM (\rb   -> speculateOne (mk_depth (rb, x')) x' hist deeds heap)
                                                           (\hist -> return (hist, deeds, heap))

    speculateOne :: Depth -> Out Var -> SpecHistory -> Deeds -> Heap -> SpecM (SpecHistory, Deeds, Heap)
    speculateOne depth x' hist deeds (Heap h ids) = case M.lookup x' h of
        Just (HB InternallyBound (Right in_e))
          | spec_trace "speculate" (ppr x') True
          , let state = normalise (deeds, Heap (M.delete x' h) ids, Loco False, in_e)
          -- I used to insist that evaluation should reach an *answer*, but actually it's really good if we
          -- get any cheap thing -- so questions are OK, and even cast questions are permissible (cast answers
          -- will never occur in practice).
          --
          -- A case where this makes a difference is if we have:
          --   (+)_selector $dNumInteger
          -- It will normally get speculated to a Question:
          --   GHC.Integer.plusInteger
          -- Due to the fact that plusInteger does not have an unfolding.
          --
          -- If the speculator only keeps Answers then it wouldn't keep this result,
          -- and thus if we had something like:
          --  (let add = (+)_selector $dNumInteger in (.. add .., .. add ..))
          -- Then we got a residual let-binding for "add", and the two h-functions
          -- corresponding to each component of the tuple were lambda-abstracted
          -- over "add"!
          , (reduced, (deeds', Heap h' ids', k', qa')) <- reduceWithFlag state
          --, if isNothing (isCastStack_maybe k') then spec_trace "speculate: not value" (ppr x') True else True
          -- TODO: might it not be cleaner to speculate the term with an Update frame for x' bunged on the end
          -- of the stack? In this case, I would not need to construct in_e' myself
          , Just cast_by <- isCastStack_maybe k'
          , let in_e' = castAnnedQAToInAnnedTerm (mkInScopeSet (castByFreeVars cast_by `unionVarSet` annedFreeVars qa')) qa' cast_by
            -- NB: try to avoid dead bindings in the state using 'gc' before the termination test so
            -- that the termination condition is more lenient. This showed up in practice, in a version
            -- of LetRec.hs where we had:
            --   let dead = xs in 1 : xs `embed` let ys = 1 : ys in ys
            -- (Because the tag on the ys/xs indirections was the cons-cell tag)
            --
            -- It's very important that we don't just gc the state itself because some of the h_speculated_ok
            -- bindings might be live in the original state supplied to speculate, and we don't want to drop them!

            -- NB: it's OK not to check termination if our state was a value (possibly in some heap) before reduction
            -- because we can't possibly recurse infinitely on such a path (due to normalisation size reduction principle)
          -> case (if not reduced then Continue (unSH hist) else terminate (unSH hist) (gc state, depth)) of
               Stop (_, old_depth) -> spec_trace "speculation denied" (ppr x' <+> ppr (snd (trainFirst old_depth))) $
                                      commonAncestorRB old_depth depth hist
               Continue hist'      -> speculateMany (`Car` depth) (M.keysSet h' S.\\ M.keysSet h) (SH hist') deeds' (Heap (M.insert x' (internallyBound in_e') h') ids')
        _ -> dont_speculate hist
      where dont_speculate hist' = return (hist', deeds, Heap h ids)
            spec_trace msg doc = pprTrace (replicate (trainLength depth) ' ' ++ msg) doc

            commonAncestorRB :: Depth -> Depth -> SpecRB
            commonAncestorRB old_depth depth = trainFirst (thirdOf3 (trainExtensionBy (\(old_rb, old_x) (_, x) -> if old_x == x then Just old_rb else Nothing) (\(old_rb, _top_x) _ -> old_rb) old_depth depth))
-}


type SpecM = ContT (Deeds, InScopeSet, PureHeap) Identity

runSpecM :: SpecM (Deeds, InScopeSet, PureHeap) -> (Deeds, InScopeSet, PureHeap)
runSpecM = unI . runContT

catchSpecM :: ((forall b. c -> SpecM b) -> SpecM a)
           -> (c -> SpecM a)
           -> SpecM a
catchSpecM try ctch = do
    ei <- callCC $ \f -> fmap Right $ try (f . Left)
    case ei of Left c  -> ctch c
               Right x -> return x


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
           -> case terminate hist (gc state) of
            Continue hist' -> go True hist' state'
            Stop old_state -> pprTrace "reduce-stop" {--} (pPrintFullState quietStatePrettiness old_state $$ pPrintFullState quietStatePrettiness state) {--} -- empty
                              -- let smmrse s@(_, _, _, qa) = pPrintFullState s $$ case annee qa of Question _ -> text "Question"; Answer _ -> text "Answer" in
                              -- pprPreview2 "reduce-stop" (smmrse old_state) (smmrse state) $
                              (can_step, mempty { stat_reduce_stops = 1 }, if rEDUCE_ROLLBACK then old_state else state') -- TODO: generalise?
           | otherwise -> pprTrace "reduce-stop(deeds)" empty $
                          (True, mempty, state)
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

instance Outputable AbsVar where
    ppr x = ppr (absVarVar x) <+> (if absVarDead x then text "(dead)" else empty)

mkLiveAbsVar :: Var -> AbsVar
mkLiveAbsVar x = AbsVar { absVarDead = False, absVarVar = x }

-- We map *all* occurrences of dead TyVars to this type, to ensure that dead TyVars in the
-- type of applied Ids match the applied dead TyVars. This type can be any closed type, as long
-- as we use it consistently!
--
-- NB: we can't use anyTypeOfKind for the unlifted kind because Any# is hardcoded as having Ptr
-- representation, which causes us to make some weird unsafeCoerces
deadTy :: Kind -> Type
deadTy k | isUnliftedTypeKind k = realWorldStatePrimTy
         | otherwise            = anyTypeOfKind k

renameAbsVarType :: Renaming -> Var -> Var
renameAbsVarType rn x = x `setVarType` renameType (mkInScopeSet as) rn ty
  where ty = varType x
        as = tyVarsOfType ty

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

applyAbsVars :: Symantics ann => Var -> Maybe Renaming -> [AbsVar] -> ann (TermF ann)
applyAbsVars x mb_xs_rn xs = snd $ snd $ foldl go (mkInScopeSet (unitVarSet x), (emptyRenaming, var x)) xs
  where
   go (fvs, (ty_rn, e)) absx = case absVarDead absx of
    -- NB: rather than faff around trying to rename IdInfo free variables for dead Ids, I'm just going to zap the info
    True -> (fvs, case () of
      () -- We can encounter TyVars, where we should be able to instantiate them any way:
         | isTyVar x
         , let dead_ty = deadTy (tyVarKind x)
         -> (insertTypeSubst ty_rn x dead_ty, e `tyApp` dead_ty)
         
         -- Dead CoVars are easy:
         | isCoVar x, let (ty1, ty2) = coVarKind x
         -> (ty_rn, let_ (zapFragileIdInfo x) (coercion (mkUnsafeCo ty1 ty2)) (e `app` x))
         
         -- A pretty cute hack for lifted bindings, though potentially quite confusing!
         -- If you want to put "undefined" here instead then watch out: this counts
         -- as an extra free variable, so it might trigger the assertion in Process.hs
         -- that checks that the output term has no more FVs than the input.
         | not (isUnLiftedType ty)
         -> (ty_rn, letRec [(zapFragileIdInfo x, var x)] (e `app` x))
         
         -- We have to be more creative for *unlifted* bindings, since building a loop
         -- changes the meaning of the program. Literals first:
         | Just (tc, []) <- splitTyConApp_maybe ty
         , Just lit <- absentLiteralOf tc
         -> (ty_rn, let_ (zapFragileIdInfo x) (literal lit) (e `app` x))
         
         -- Special-case RealWorld# because it occurs so often and we can save a "let" and
         -- "cast" in the output syntax by doing so:
         --
         -- (NB: the use of realWorldPrimId here and in the VoidRep case below means we have
         -- to special-case realWorldPrimId in the post-SC free-variable sanity checks)
         | ty `eqType` realWorldStatePrimTy
         -> (ty_rn, e `app` realWorldPrimId)

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
                   where x_tid = zapFragileIdInfo x `setVarType` threadIdPrimTy
         -> (ty_rn, let_ (zapFragileIdInfo x) (e_repr `cast` mkUnsafeCo e_repr_ty ty) (e `app` x)))
         where -- NB: dead binders are not present in the renaming, so don't attempt to look them up
               shadowy_x = renameAbsVarType ty_rn (absVarBinder absx)
               x = uniqAway fvs shadowy_x
               ty = idType x
    -- NB: for live AbsVars we don't need to sweat about making sure we zap the "fragile" info or setting
    -- the correct type on any Ids, because we assume that the correct info will be set up on the *binding*
    -- sites and propagated down here by a simplifier run.
    --
    -- We still need to extend the ty_rn if the AbsVar is a TyVar because we create *binding* sites for
    -- dead variables and if we have the renaming [a |-> Bool, y :: a |-> z] with a live and y dead, we want
    -- to ensure that new the binding site is attributed type Bool.
    False -> case () of
      () | isTyVar x
         , let ty = maybe (mkTyVarTy x) (flip lookupTyVarSubst x) mb_xs_rn
         -> (fvs `extendInScopeSetSet` tyVarsOfType ty, (insertTypeSubst ty_rn x ty, e `tyApp` ty))
   
         | isCoVar x
         , let co = maybe (mkCoVarCo x) (flip lookupCoVarSubst x) mb_xs_rn
         -> (fvs `extendInScopeSetSet` tyCoVarsOfCo co, (ty_rn, e `coApp` co))
   
         | otherwise
         , let y = maybe x (flip renameId x) mb_xs_rn
         -> (fvs `extendInScopeSet` y, (ty_rn, e `app` y))
         where x = absVarVar absx

-- NB: if there are no arguments, we abstract over RealWorld# as well (cf WwLib). Two reasons:
--  1. If the h-function is unlifted, this delays its evaluation (so its effects, if any, do not happen too early).
--     This is also necessary since h-functions will be bound in one letrec after supercompilation is complete.
--  2. In other cases, this expresses to GHC that we don't necessarily want the work in h-functions to be shared.
stateAbsVars :: Maybe FreeVars -> State -> ([AbsVar], Type)
stateAbsVars mb_lvs state
  | any (isId . absVarVar) abstracted
  = (abstracted,                                                       ty)
  | otherwise
  = (AbsVar { absVarDead = True, absVarVar = voidArgId } : abstracted, realWorldStatePrimTy `mkFunTy` ty)
  where vs_list = sortLe absVarLe (varSetElems (stateLambdaBounders state))
        ty = vs_list `mkPiTypes` stateType state

        abstracted = map (\v -> AbsVar { absVarDead = maybe False (not . (v `elemVarSet`)) mb_lvs, absVarVar = v }) vs_list

-- Our custom ordering function ensures we get the following ordering:
--  1. Kind variables
--  2. Type variables
--  3. Dictionary ids
--  4. Other ids
--
-- The reason we want to sort dictionary ids earlier is so that GHC's own Specialise
-- pass is able to specialise functions on them (it assumes they come after the type vars).
absVarLe :: Var -> Var -> Bool
absVarLe v1 v2
  | isId v1, isId v2 = isDictId v1 >= isDictId v2
  | otherwise        = quantVarLe v1 v2


-- | Free variables that are allowed to be in the output term even though they weren't in the input (in addition to h-function names)
extraOutputFvs :: FreeVars
extraOutputFvs = unitVarSet realWorldPrimId
