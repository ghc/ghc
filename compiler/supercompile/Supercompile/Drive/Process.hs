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
import Id         (idType, zapFragileIdInfo, localiseId, isDictId)
import MkId       (voidArgId, realWorldPrimId, mkPrimOpId)
import Coercion   (isCoVar, mkCoVarCo, mkUnsafeCo, coVarKind)
import TyCon      (PrimRep(..))
import Type       (Kind, isUnLiftedType, mkTyVarTy, eqType, mkFunTy, mkPiTypes, mkTyConApp, typePrimRep, splitTyConApp_maybe)
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
             -> case v of Indirect y         -> go shown' prec y
                          Data dc tys cos ys -> pPrintPrecApps prec dc ([asPrettyFunction ty | ty <- tys] ++ [asPrettyFunction co | co <- cos] ++ [PrettyFunction (\prec -> go shown' prec y) | y <- ys])
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
        Indirect _    -> (Nothing,        IM.empty)
        Literal l     -> (Just (show l),  IM.empty)
        Coercion _    -> (Nothing,        IM.empty)
        TyLambda _ e  -> (Nothing,        go_term ann e)
        Lambda   _ e  -> (Nothing,        go_term ann e)
        Data dc _ _ _ -> (Just (show dc), IM.empty)

    go_answer :: [String] -> Anned Answer -> TagAnnotations
    go_answer ann a = insert ann (annedTag a) $ go_answer' ann (annee a)

    go_answer' ann (_, (_, v)) = go_value' ann v

    go_kf :: [String] -> Tagged StackFrame -> ([String], TagAnnotations)
    go_kf ann kf = second (insert' ann (tag kf)) $ go_kf' ann (tagee kf)

    go_kf' ann kf = case kf of
      TyApply _ -> (ann, IM.empty)
      CoApply _ -> (ann, IM.empty)
      Apply _   -> (ann, IM.empty)
      Scrutinise x _ (_, alts) -> (extAnn x ann, IM.unions [go_term ann e | (_, e) <- alts])
      PrimApply _ _ as es -> (ann, IM.unions [go_answer ann a | a <- as] `IM.union` IM.unions [go_term ann e | (_, e) <- es])
      StrictLet x (_, e)  -> (extAnn x ann, go_term ann e)
      Update x            -> (extAnn x ann, IM.empty)
      CastIt _            -> (ann, IM.empty)



-- Be very careful when you change this function. At one point it accounted for 10% of
-- supercompile runtime, so I went to a lot of trouble to deforest everything in sight.
prepareTerm :: M.Map Var Term -> Term -> (S.Set Var,                    -- Names of all unfoldings in the input heaps
                                          ([(Var,   FVedTerm)], State), -- For use without memo-cache preinitalization
                                          ([(State, FVedTerm)], State)) -- With preinitialization
prepareTerm unfoldings e = {-# SCC "prepareTerm" #-}
                           pprTraceSC "unfoldings" (pPrintPrecLetRec noPrec (M.toList unfoldings) (PrettyDoc (text "<stuff>"))) $
                           pprTraceSC "all input FVs" (ppr (input_fvs `delVarSetList` S.toList unfolding_bvs)) $
                           (unfolding_bvs, pprTraceSC "no-preinit unfoldings" (pPrintPrecLetRec noPrec (M.toList h'') (PrettyDoc (text "<stuff>")))
                                                      (h''_must_be_bound, state),
                                           (preinit_with, preinit_state))
  where (tag_ids0, tag_ids1) = splitUniqSupply tagUniqSupply
        anned_e = toAnnedTerm tag_ids0 e
        
        -- NB: the code below assumes h_unfoldings is in ascending order so it can get a little speed boost
        (input_fvs, tag_ids2, h_unfoldings) = foldr3WithKey' add_one_unfolding (annedTermFreeVars anned_e, tag_ids1, []) unfoldings
          where add_one_unfolding x' e (input_fvs', tag_ids1, h_unfoldings) = (input_fvs'', tag_ids2, (x', renamedTerm anned_e):h_unfoldings)
                    where !(tag_unf_ids, tag_ids2) = splitUniqSupply tag_ids1
                          anned_e = toAnnedTerm tag_unf_ids e
                          input_fvs'' = input_fvs' `unionVarSet` varBndrFreeVars x' `unionVarSet` annedFreeVars anned_e
        
        -- NB: foldVarSet is a right fold, so this use of fromDistinctAscList is justified
        h_fvs = M.fromDistinctAscList $ snd $ foldToMapAccumL foldVarSet add_one_fv tag_ids2 input_fvs
          where add_one_fv tag_ids2 x' = (tag_ids3, (x', environmentallyBound (mkTag (getKey i))))
                    where !(i, tag_ids3) = takeUniqFromSupply tag_ids2

        unfolding_bvs = S.fromDistinctAscList (map fst h_unfoldings)
        deeds = Deeds { sizeLimit = (bLOAT_FACTOR - 1) * annedSize anned_e, stepLimit = (bLOAT_FACTOR - 1) * annedSize anned_e }
        (speculated, (_stats, deeds', Heap h' ids')) = speculateHeap S.empty (mempty, deeds, Heap (M.fromDistinctAscList (map (second2 internallyBound) h_unfoldings) `M.union` h_fvs) ids)
        -- FIXME: use speculated

        -- NB: h_fvs might contain bindings for things also in h_unfoldings, so union them in the right order
        rn = mkIdentityRenaming input_fvs
        ids = mkInScopeSet input_fvs
        {-# INLINE heap_binding_is_value #-}
        heap_binding_is_value = maybe True (termIsValue . snd) . heapBindingTerm

        -- When *not* doing memocache preinitialization, we still want to be able to speculate the unfoldings to
        -- discover more values (after all, the evaluator can only inline LetBound values). But this might cause
        -- us to create new top level bindings e.g. when speculating (x = let a = y |> co in D a).
        --
        -- The speculated heap' contain *internally bound* top level bindings (or the speculator won't do anything
        -- to them) so we must be careful to change them to *let bound* before we put them in the user-visible heap.
        
        -- First, eliminate any non-value unfolding bindings and any value binding that refers to any one
        -- of them *that is not one of the original bindings*
        --
        -- Consider these unfoldings:
        --  meth1 = \x -> ..
        --  meth2 = fib 100
        --  dict = D meth1 meth2
        --
        -- We don't want to make the whole dict unavailable just because meth2 isn't a value
        --
        -- However in this case:
        --  foo = let a = fib 100 in D a
        --
        -- We do want to make "foo" unavailable as there is no value unfolding we can give it
        -- that ensures that we share the work of "fib 100" with all other modules.
        (eliminate_set_nonvalue, h'_value) = funny_partition (\hb -> hb { howBound = LetBound }) (not . heap_binding_is_value) h'
        h'' = go eliminate_set_nonvalue h'_value
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

        -- Secondly, pull out any remaining bindings (which must be values) that didn't exist in the
        -- unspeculated heap. These will be our new top-level bindings.
        h''_must_be_bound = [ (x', annedTermToFVedTerm (renameIn (renameAnnedTerm ids') in_e))
                            | (x', hb) <- M.toList h''
                            , not (x' `S.member` unfolding_bvs)
                            , Just in_e <- [heapBindingTerm hb] ]

        state = normalise (deeds', Heap (h'' `M.union` h_fvs) ids', Loco False, (rn, anned_e))
        
        -- When doing memocache preinitialization, we don't want to include in the final heap any binding originating
        -- from evaluating the top-level that cannot be proven to be a value, or else we risk work duplication
        preinit_state = normalise (deeds', preinit_heap, Loco False, (rn, anned_e))
        preinit_heap = Heap (M.filter heap_binding_is_value h' `M.union` h_fvs) ids'

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
--
-- It's cool to use normalise here instead of termToAnswer just in case we can do some sweet stuff like look through
-- lets in the body or maybe even simplify a case given the stuff in the heap. (NB: there this does not cause work dup.)
-- I won't go as far as using "speculate" because in that case "eta" could potentially yield an infinite list
-- (for some non-HM-typeable programs).
eta :: Heap -> FVedTerm -> In AnnedTerm -> [(Heap, FVedTerm, In AnnedTerm)]
eta heap accessor_e0 in_e = (heap, accessor_e0, in_e) : case normalise (maxBound, heap, Loco False, in_e) of
  (_, Heap h ids, Loco _, anned_qa)
    | Answer (a_cast, (rn, v)) <- extract anned_qa
    , let accessor_e1 = case a_cast of Uncast      -> accessor_e0
                                       CastBy co _ -> accessor_e0 `cast` mkSymCo ids co
          mb_res@(~(Just (_, x, _))) = case v of
             Lambda   x e_body -> Just (accessor_e1 `app`   x',           x, e_body)
             TyLambda a e_body -> Just (accessor_e1 `tyApp` mkTyVarTy x', a, e_body)
             _                 -> Nothing
          (ids', rn', x') = renameNonRecBinder ids rn x
    , Just (accessor_e2, _, e_body) <- mb_res
    -> eta (Heap (M.insert x' lambdaBound h) ids') accessor_e2 (rn', e_body)
  _ -> []


{-# INLINE traverseWithKey #-}
-- INLINE so it gets inlined before specialisation, eta-expanded so any automatic SCCs
-- don't interfere with inlining (inlining doesn't look through SCCs for arguments!)
traverseWithKey :: Applicative t => (k -> a -> t b) -> M.Map k a -> t (M.Map k b)
#if (MIN_VERSION_containers(0,4,3))
traverseWithKey f kvs = M.traverseWithKey f kvs
#else
traverseWithKey f = traverse (uncurry f) . M.mapWithKey (\k v -> (k, v))
#endif

newtype State2L s1 s2 a = State2L { unState2L :: s1 -> s2 -> (s1, s2) }

instance Functor (State2L s1 s2) where
    fmap _ = State2L . unState2L

instance Applicative (State2L s1 s2) where
    pure _ = State2L (,)
    mf <*> mx = State2L $ \s1 s2 -> case unState2L mf s1 s2 of (s1, s2) -> unState2L mx s1 s2 -- NB: left side first

newtype State3R s1 s2 s3 a = State3R { unState3R :: s1 -> s2 -> s3 -> (s1, s2, s3) }

instance Functor (State3R s1 s2 s3) where
    fmap _ = State3R . unState3R

instance Applicative (State3R s1 s2 s3) where
    pure _ = State3R (,,)
    mf <*> mx = State3R $ \s1 s2 s3 -> case unState3R mx s1 s2 s3 of (s1, s2, s3) -> unState3R mf s1 s2 s3 -- NB: right side first

-- You might wonder why we don't just use foldlWithKey and make the accumulator into a pair. The reason is that
-- if we do that then (due to a problem in the strictness analyser, which I've emailed Simon about) the resulting
-- loop will allocate a new pair on every iteration. If we specialise the traverseWithKey code instead then we can
-- pass the components of the pair in seperate arguments and totally avoid the allocations. This is a Big Win,
-- since prepareTerm was accounting for 10% of all allocations in some of my tests.
{-# INLINE foldl2WithKey' #-}
foldl2WithKey' :: ((a1, a2) -> k -> v -> (a1, a2)) -> (a1, a2) -> M.Map k v -> (a1, a2)
foldl2WithKey' f (a1, a2) kvs = unState2L (traverseWithKey (\k v -> State2L $ \a1 a2 -> f (a1, a2) k v) kvs) a1 a2

{-# INLINE foldr3WithKey' #-}
foldr3WithKey' :: (k -> v -> (a1, a2, a3) -> (a1, a2, a3)) -> (a1, a2, a3) -> M.Map k v -> (a1, a2, a3)
foldr3WithKey' f (a1, a2, a3) kvs = unState3R (traverseWithKey (\k v -> State3R $ \a1 a2 a3 -> f k v (a1, a2, a3)) kvs) a1 a2 a3


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
speculate :: AlreadySpeculated -> (SCStats, State) -> (AlreadySpeculated, (SCStats, State))
speculate speculated (stats, (deeds, heap, k, in_e)) = (speculated', (stats', (deeds', heap', k, in_e)))
  where (speculated', (stats', deeds', heap')) = speculateHeap speculated (stats, deeds, heap)

speculateHeap :: AlreadySpeculated -> (SCStats, Deeds, Heap) -> (AlreadySpeculated, (SCStats, Deeds, Heap))
speculateHeap speculated (stats, deeds, Heap h ids) = {-# SCC "speculate" #-} (M.keysSet h, (stats', deeds', Heap (h_non_values_speculated `M.union` h_speculated_ok `M.union` h_speculated_failure) ids'))
  where
    (h_values, h_non_values) = M.partition (maybe False (termIsValue . snd) . heapBindingTerm) h
    (h_non_values_unspeculated, h_non_values_speculated) = (h_non_values `exclude` speculated, h_non_values `restrict` speculated)

    ((_hist, _forbidden, ids'), (stats', deeds', h_speculated_ok, h_speculated_failure)) = runSpecM (speculateManyMap [] h_non_values_unspeculated) ((mkLinearHistory (cofmap fstOf3 wQO), [], ids), (stats, deeds, h_values, M.empty))
    
    speculateManyMap parents = speculateMany parents . concatMap M.toList . topologicalSort heapBindingFreeVars
    speculateMany parents = mapM_ (speculateOne parents)
    
    speculateOne :: [Var] -> (Out Var, HeapBinding) -> SpecM ()
    speculateOne parents (x', hb)
      | HB InternallyBound (Right in_e) <- hb
      , spec_trace "speculate" (ppr x') True
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
                        (extra_stats, (deeds, Heap h_speculated_ok' ids, k, qa))
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
                          | Just cast_by <- stackToCast k
                          , let h_unspeculated = h_speculated_ok' M.\\ h_speculated_ok
                                -- NB: this "fmap" is safe for a rather delicate reason -- the renaming returned by annedQAToInAnnedTerm
                                -- is an identity renaming that includes at least all of the variables in the input InScopeSet, *IN THE CASE
                                -- THAT YOU PASS AN ANSWER*. This is just barely sufficient for our purposes due to the fact that "reduce" never
                                -- returns an Answer with a cast pending on the stack -- it only returns such a Question.
                                in_e' = fmap (castAnnedTerm cast_by) $ annedQAToInAnnedTerm (mkInScopeSet (castByFreeVars cast_by `unionVarSet` annedFreeVars qa)) qa
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
deadTy :: Kind -> Type
deadTy = anyTypeOfKind

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

applyAbsVars :: Symantics ann => Var -> Maybe (M.Map Var Var) -> [AbsVar] -> ann (TermF ann)
applyAbsVars x mb_xs_rn xs = snd $ snd $ foldl go (unitVarSet x, (emptyRenaming, var x)) xs
  where
   go (fvs, (ty_rn, e)) absx = case absVarDead absx of
    True -> (fvs, case () of
      () -- We can encounter TyVars, where we should be able to instantiate them any way:
         | isTyVar x
         , let dead_ty = deadTy (tyVarKind x)
         -> (insertTypeSubst ty_rn x dead_ty, e `tyApp` dead_ty)
         
         -- Dead CoVars are easy:
         | isCoVar x, let (ty1, ty2) = coVarKind x
         -> (ty_rn, let_ x (coercion (mkUnsafeCo ty1 ty2)) (e `app` x))
         
         -- A pretty cute hack for lifted bindings, though potentially quite confusing!
         -- If you want to put "undefined" here instead then watch out: this counts
         -- as an extra free variable, so it might trigger the assertion in Process.hs
         -- that checks that the output term has no more FVs than the input.
         | not (isUnLiftedType ty)
         -> (ty_rn, letRec [(x, var x)] (e `app` x))
         
         -- We have to be more creative for *unlifted* bindings, since building a loop
         -- changes the meaning of the program. Literals first:
         | Just (tc, []) <- splitTyConApp_maybe ty
         , Just lit <- absentLiteralOf tc
         -> (ty_rn, let_ x (literal lit) (e `app` x))
         
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
                   where x_tid = x `setVarType` threadIdPrimTy
         -> (ty_rn, let_ x (e_repr `cast` mkUnsafeCo e_repr_ty ty) (e `app` x)))
         where -- NB: dead binders are not present in the renaming, so don't attempt to look them up
               shadowy_x = renameAbsVarType ty_rn (absVarBinder absx)
               x = uniqAway (mkInScopeSet fvs) shadowy_x
               ty = idType x
    False -> (fvs `extendVarSet` x', case () of
      () | isTyVar x
         -> (insertTypeSubst ty_rn x (mkTyVarTy x'), e `tyApp` mkTyVarTy x')
   
         | isCoVar x
         -> (ty_rn, e `coApp` mkCoVarCo x'_zapped)
   
         | otherwise
         -> (ty_rn, e `app` x'_zapped))
         where x = absVarVar absx
               x' = renameAbsVarType ty_rn (maybe x (M.findWithDefault (pprPanic "renameAbsVar" (ppr x)) x) mb_xs_rn)
               x'_zapped = zapFragileIdInfo x'
               -- NB: make sure we zap the "fragile" info because the FVs of the unfolding are
               -- not necessarily in scope.

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
