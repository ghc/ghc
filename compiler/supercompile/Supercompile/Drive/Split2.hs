module Supercompile.Drive.Split2 (
    ResidTags, plusResidTags, emptyResidTags,

    split, instanceSplit, generaliseSplit
  ) where

#include "HsVersions.h"

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming
import Supercompile.Core.Syntax

import Supercompile.Evaluator.Deeds
import Supercompile.Evaluator.Evaluate (normalise)
import Supercompile.Evaluator.FreeVars
import Supercompile.Evaluator.Syntax

import Supercompile.GHC (coreAltConToAltCon, altConToCoreAltCon)

import Supercompile.Termination.Generaliser (Generaliser(..))

import Supercompile.StaticFlags
import Supercompile.Utilities hiding (tails)

import CoreUtils (filterAlts)
import Id        (idType, isDeadBinder, localiseId, isOneShotBndr)
import PrelNames (wildCardKey)
import Util      (thirdOf3)
import MonadUtils (concatMapM)
import qualified State

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM


type LGraph node edge = M.Map node (M.Map node edge)

filterEdges :: Ord node
            => (edge -> node -> Bool)
            -> LGraph node edge
            -> LGraph node edge
filterEdges keep_edge = M.map (M.mapMaybeWithKey (\n e -> if keep_edge e n then Just e else Nothing))

trimUnreachable :: Ord node
                => node
                -> LGraph node edge
                -> LGraph node edge
trimUnreachable root_n g = go (S.singleton root_n) S.empty
  where go n_todo n_done | S.null n_todo' = M.filterWithKey (\n _ -> n `S.member` n_done') g -- NB: all outgoing edges of retained nodes will still be present by definition
                         | otherwise      = go n_todo' n_done'
          where n_done' = n_todo `S.union` n_done
                n_todo' = S.fold (\n n_todo' -> M.keysSet (M.findWithDefault (error "trimUnreachable") n g) `S.union` n_todo') S.empty n_todo S.\\ n_done'

shortcutEdges :: forall node edge.
                 Ord node
              => (node -> Bool)
              -> (edge -> edge -> edge)         -- Used to join edges if after shortcutting there is more than one path from a node to another one
              -> (edge -> node -> edge -> edge) -- Used when joining two edges in a contiguous path
              -> LGraph node edge
              -> LGraph node edge
shortcutEdges should_shortcut combine_edges combine g = State.evalState visit_graph M.empty
  where
    visit_graph :: State.State (M.Map node [(node, edge)]) (LGraph node edge)
    visit_graph = liftM M.fromDistinctAscList $ sequence $ flip mapMaybe (M.toAscList g) $ \(n, ens) -> do
        guard (not (should_shortcut n))
        return $ liftM (((,) n) . M.fromListWith combine_edges) $ visit S.empty ens

    visit :: S.Set node -> M.Map node edge -> State.State (M.Map node [(node, edge)]) [(node, edge)]
    -- Given the outgoing edges for some node, returns all the outgoing edges for that node
    -- after shortcutting
    visit path ens = concatMapM (uncurry (visit' path)) (M.toList ens)

    visit' :: S.Set node -> node -> edge -> State.State (M.Map node [(node, edge)]) [(node, edge)]
    -- Given an edge label and the node reached via that label, returns all the nodes reached
    -- after shortcutting
    visit' path n' e | n' `S.member` path       = return []        -- Doesn't contribute any extra paths: all paths will considered by a caller
                     | not (should_shortcut n') = return [(n', e)] -- Won't be shortcutted away, no need to look further
                     | otherwise                = do
                        -- Since n' is not in the path, we can try to memoise
                        mb_res <- liftM (M.lookup n') State.get
                        res <- case mb_res of
                          Just res -> return res
                          Nothing  -> do
                            res <- visit (S.insert n' path) (M.findWithDefault (error "shortcutEdges") n' g)
                            State.modify (M.insert n' res)
                            return res
                        return $ map (second (combine e n')) res

-- Given a graph, returns:
--  1. An acyclic graph of the strongly connected components of the input graph.
--     Each SCC is identified by a unique Int.
--  2. A mapping from Ints to the "sub-graph" corresponding to each SCC. Each sub-graph
--     contains all the nodes in the SCC as well as any edges between those nodes.
--     Note that in particular the sub-graph for an acyclic SCC will contain exactly one node and no edges.
--
-- Uses an adaptation of Tarjan's algorithm <http://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm>
-- Returns SCCs in topological order (i.e. the SCC with no *incoming* edges is first in the output, and that with no *outgoing* edges is last)
sccs :: forall node edge.
        (Outputable node, Ord node)
     => LGraph node edge
     -> ([(Int, M.Map Int (M.Map (node, node) edge))],
         IM.IntMap (LGraph node edge))
sccs g = case State.execState strongconnect_graph (0, M.empty, [], [], IM.empty, M.empty, M.empty) of (_, _, _, sccs, scc_datas, _, _) -> (sccs, scc_datas)
  where
    -- Observations about Tarjan's algorithm:
    --  1. strongconnect(v) is only called if v.index is undefined
    --  2. Vertex v's lowlink is only mutated by strongconnect(v)
    --  3. Once index is set it is never changed
    --  4. Nodes occur in the stack in decreasing order of index
    --
    -- We can use these facts to build an implementation that makes minimal use of the state monad

    strongconnect_graph = flip traverseWithKey_ g $ \n ens -> do
      ix_defined <- liftM (\(_, ixs, _, _, _, _, _) -> n `M.member` ixs) State.get
      unless ix_defined $ void $ strongconnect n ens

    -- (strongconnect n ens) returns:
    --  1. Index of a node n' reachable from n such that that index[n'] < index[n],
    --     if possible. Otherwise returns index[n].
    --  2. Whether we didn't just create a new SCC containing n. If no new SCC was created then n is guaranteed
    --     to still be on the stack (which occurs iff we managed to find a suitable index[n'])
    --
    -- Precondition: there is no assigned index for n
    strongconnect :: node -> M.Map node edge
                  -> State.State (-- Next node index to assign
                                  Int,
                                  -- Mapping from nodes to their assigned index (if any)
                                  -- NB: after the node has been removed from the stack, we update the Int in the mapping
                                  -- to instead be the lowlink of the SCC it was assigned to. This is OK because we don't
                                  -- need the raw index of the node after that point: we only need record the fact that
                                  -- it had some index at a point in the past
                                  M.Map node Int,
                                  -- Stack containing expanded nodes that are not presently in a SCC
                                  [node],
                                  -- Work-in-progress graph of SCC
                                  [(Int, M.Map Int (M.Map (node, node) edge))],
                                  -- Work-in-progress SCC sub-graph mapping
                                  IM.IntMap (LGraph node edge),
                                  -- Records all discovered "internal" edges from expanded nodes to somewhere *within* their SCC
                                  M.Map node (M.Map node edge),
                                  -- Records all discovered "external" edges from the current SCC-in-progress to some other (already existant) SCC
                                  -- It might seem more obvious to use a [([(edge, node)], Int)] here, but that makes it awkward to common up multiple
                                  -- edges from this SCC going to the same external SCC
                                  M.Map Int (M.Map (node, node) edge))
                                 (Int, Bool)
    strongconnect n ens = do
      ix <- State.state $ \(next_ix, ixs, s, sccs, scc_datas, all_internal_ens, all_external_ens) -> (next_ix, (next_ix + 1, M.insert n next_ix ixs, n:s, sccs, scc_datas, all_internal_ens, all_external_ens))
      (lowlink, internal_ens, external_ens) <- (\f -> foldlWithKeyM' f (ix, M.empty, M.empty) ens) $ \(lowlink, internal_ens, external_ens) n' e -> do
        (mb_ix', in_s') <- liftM (\(_, ixs, s, _, _, _, _) -> (M.lookup n' ixs, n' `elem` s)) State.get
        (lowlink, mb_scc) <- case mb_ix' of
                                  -- Successor not yet visited: recurse on it
                                  -- Whether we add an internal or external edge depends on whether the recursive call created an SCC or not.
                                  -- If it did create an SCC, that SCC will be identified by lowlink'
          Nothing              -> do (lowlink', in_s') <- strongconnect n' (M.findWithDefault (pprPanic "sccs: unknown successor" (ppr n')) n' g)
                                     return (lowlink `min` lowlink', if in_s' then Nothing else Just lowlink')
                                  -- Successor is in the stack and hence the current SCC, so record an internal edge
          Just ix' | in_s'     -> return (lowlink `min` ix', Nothing)
                                  -- Successor visited but not in stack: it is already part of another SCC, so record an external edge
                                  -- NB: this makes use of my hack whereby ix' will actually be a SCC lowlink for such successors
                   | otherwise -> return (lowlink, Just ix')
        (internal_ens, external_ens) <- return $ case mb_scc of
                                          Nothing  -> (M.insert n' e internal_ens, external_ens)
                                          Just scc -> (internal_ens, M.insertWith (M.unionWith (error "strongconnect: non-distinct")) scc (M.singleton (n, n') e) external_ens)
        return (lowlink, internal_ens, external_ens)
      -- Record accumulated internal/external edges. We don't need to record them as we go along because they can only possibly be used by one of our callers, not our callees
      State.modify $ \(next_ix, ixs, s, sccs, scc_datas, all_internal_ens, all_external_ens) -> (next_ix, ixs, s, sccs, scc_datas, M.insert n internal_ens all_internal_ens, M.unionWith (M.unionWith (error "strongconnect: non-distinct")) external_ens all_external_ens)
      -- Since lowlink is at most ix, this condition can only be true if we failed to find a node reachable
      -- from n with a lower index. We use this as our cue to form a new SCC.
      in_s <- if (lowlink == ix)
               -- NB: because nodes on the stack are in decreasing order of index, this operation never pops a node with index < ix
              then do State.modify $ \(next_ix, ixs, s, sccs, scc_datas, all_internal_ens, all_external_ens) -> let (s_scc, _n:s') = span (/= n) s
                                                                                                                    scc = M.fromList [(n, M.findWithDefault (error "sccs") n all_internal_ens) | n <- n:s_scc]
                                                                                                                    -- Replace node indexes with the lowlink of the SCC they were assigned to (a small hack to save one map lookup):
                                                                                                                    ixs' = foldr (\n -> M.insert n lowlink) ixs (n:s_scc)
                                                                                                                in (next_ix, ixs', s', (lowlink, all_external_ens):sccs, IM.insert lowlink scc scc_datas, all_internal_ens, M.empty)
                      return False
              else return True
      -- Return this nodes final lowlink for use when computing the predecessors lowlink
      return (lowlink, in_s)


fromListDisjoint :: Ord k => [(k, v)] -> M.Map k v
fromListDisjoint = M.fromListWith (error "fromListDisjoint")

unionDisjoint :: Ord k => M.Map k v -> M.Map k v -> M.Map k v
unionDisjoint = M.unionWith (error "unionDisjoint")


type ResidTags = IM.IntMap Int

emptyResidTags :: ResidTags
emptyResidTags = IM.empty

oneResidTag :: Tag -> ResidTags
oneResidTag (TG i _) = IM.singleton (unFin i) 1

plusResidTags :: ResidTags -> ResidTags -> ResidTags
plusResidTags = IM.unionWith (+)

plusResidTagss :: [ResidTags] -> ResidTags
plusResidTagss = IM.unionsWith (+)


data Context = HeapContext Var
             | StackContext Int
             | FocusContext
             deriving (Eq, Ord)

instance Outputable Context where
    pprPrec prec (HeapContext x') = pprPrec prec x'
    pprPrec prec (StackContext i) = pprPrec prec i
    pprPrec _    FocusContext     = text "[_]"

data Entries = OneEntry | ManyEntries

instance Outputable Entries where
    ppr OneEntry    = text "1"
    ppr ManyEntries = text "Many"


varEdges :: Entries -> FreeVars -> M.Map Context Entries
varEdges ents xs = M.fromList [(HeapContext x, ents) | x <- varSetElems xs]

plusEntries :: Entries -> Entries -> Entries
plusEntries OneEntry OneEntry = OneEntry
plusEntries _        _        = ManyEntries

-- Used when the maps come from the same contexts
plusEntered :: M.Map Context Entries -> M.Map Context Entries -> M.Map Context Entries
plusEntered = M.unionWith plusEntries

-- Used when the maps come from two distinct contexts
multEntered :: M.Map Context Entries -> M.Map Context Entries -> M.Map Context Entries
multEntered = M.unionWith (\_ _ -> ManyEntries)


split :: (Applicative m, Monad m)
      => (State -> m            (Deeds, Out FVedTerm))
      -> State  -> m (ResidTags, Deeds, Out FVedTerm)
split opt (_deeds, heap, k, qa) = recurse opt $ push (S.singleton FocusContext) (heap, k, QAFocus qa)

instanceSplit :: (Applicative m, Monad m)
              => (State                             -> m            (Deeds, Out FVedTerm))
              -> (Deeds, Heap, Stack, Out FVedTerm) -> m (ResidTags, Deeds, Out FVedTerm)
instanceSplit opt (_deeds, heap, k, e) = recurse opt $ push (S.singleton FocusContext) (heap, k, OpaqueFocus e)

applyGeneraliser :: Generaliser -> State -> Maybe (S.Set Context)
applyGeneraliser gen (_deeds, Heap h _, k, qa) = fmap (\(gen_kfs, gen_xs) -> S.fromList $ map StackContext (IS.elems gen_kfs) ++ map HeapContext (varSetElems gen_xs)) $ case gENERALISATION of
    NoGeneralisation -> Nothing
    AllEligible -> guard (not (IS.null gen_kfs) || not (isEmptyVarSet gen_xs'')) >> return (gen_kfs, gen_xs'')
      where gen_kfs = IS.fromList [i   | (i, kf) <- named_k, generaliseStackFrame gen kf]
            gen_xs'' = mkVarSet [x'' | (x'', hb) <- M.toList h, generaliseHeapBinding gen x'' hb, ASSERT2(not (howBound hb == LambdaBound && isNothing (heapBindingTerm hb)), ppr (x'', hb, heapBindingTag hb)) True]
    StackFirst -> (guard (not (IS.null gen_kfs)) >> return (gen_kfs, emptyVarSet)) `mplus`
                  (guard (not (isEmptyVarSet gen_xs''))  >> return (IS.empty, gen_xs''))
      where gen_kfs = IS.fromList [i   | (i, kf) <- named_k, generaliseStackFrame gen kf]
            gen_xs'' = mkVarSet [x'' | (x'', hb) <- M.toList h, generaliseHeapBinding gen x'' hb, ASSERT2(not (howBound hb == LambdaBound && isNothing (heapBindingTerm hb)), ppr (x'', hb, heapBindingTag hb)) True]
    DependencyOrder want_first -> listToMaybe ((if want_first then id else reverse) possibilities)
      where -- We consider possibilities starting from the root of the term -- i.e. the bottom of the stack.
            -- This is motivated by how the interaction with subgraph generalisation for TreeFlip/TreeSum.
            -- FIXME: explain in more detail if this turns out to be sane.
            possibilities = findGeneralisable False emptyVarSet (reverse named_k) h
            
            findGeneralisable done_qa pending_xs' unreached_kfs unreached_hbs
               | done_qa && null pending_kfs && M.null pending_hbs
               = []
               | otherwise
               = [(gen_kf_is, gen_xs'') | not (IS.null gen_kf_is) || not (isEmptyVarSet gen_xs'')] ++
                 findGeneralisable done_qa' reached_xs' unreached_kfs' unreached_hbs'
              where
                (done_qa', extra_pending_xs') = if done_qa || not (null unreached_kfs) then (done_qa, emptyVarSet) else (True, annedFreeVars qa)
                (pending_kfs, unreached_kfs') = splitAt 1 unreached_kfs
                (pending_hbs, unreached_hbs') = M.partitionWithKey (\x' _hb -> x' `elemVarSet` (pending_xs' `unionVarSet` extra_pending_xs')) unreached_hbs
                
                gen_kf_is = IS.fromList [i  | (i, kf) <- pending_kfs, generaliseStackFrame gen kf]
                gen_xs'' = mkVarSet [x'' | (x'', hb) <- M.toList pending_hbs, generaliseHeapBinding gen x'' hb, ASSERT2(not (howBound hb == LambdaBound && isNothing (heapBindingTerm hb)), ppr (x'', hb, heapBindingTag hb)) True]
                
                reached_xs' = M.foldrWithKey (\_x' hb fvs -> heapBindingFreeVars hb `unionVarSet` fvs)
                                             (unionVarSets (map (stackFrameFreeVars . tagee . snd) pending_kfs))
                                             pending_hbs
  where named_k = [0..] `zip` trainCars k

generaliseSplit :: (Applicative m, Monad m)
                => (State -> m (Deeds, Out FVedTerm))
                -> Generaliser -> State -> Maybe (m (ResidTags, Deeds, Out FVedTerm))
generaliseSplit opt gen state@(_deeds, heap, k, qa) = flip fmap (applyGeneraliser gen state) $ \generalised -> recurse opt $ push generalised (heap, k, QAFocus qa)


recurse :: (Applicative m, Monad m)
        => (State -> m (Deeds, Out FVedTerm))
        -> (PushedHeap, PushedStack, PushedFocus) -> m (ResidTags, Deeds, Out FVedTerm)
recurse opt (h', k', focus') = recurseFocus opt focus' >>= recurseStack opt k' >>= recurseHeap opt h'

recurseFocus :: Applicative m
             => (State -> m (Deeds, Out FVedTerm))
             -> PushedFocus -> m (ResidTags, Deeds, Out FVedTerm)
recurseFocus opt (QAFocus (Tagged tg_qa qa)) = case qa of
    Question x -> pure (oneResidTag tg_qa, emptyDeeds, var x)
    Answer   a -> recurseAnswer opt (Tagged tg_qa a)
recurseFocus opt (TermFocus state) = liftA (uncurry ((,,) emptyResidTags)) $ opt state
recurseFocus _   (OpaqueFocus e)   = pure (emptyResidTags, emptyDeeds, e)

recurseAnswer :: Applicative m
              => (State -> m (Deeds, Out FVedTerm))
              -> Tagged (AnswerG (ValueG State)) -> m (ResidTags, Deeds, Out FVedTerm)
recurseAnswer opt (Tagged tg_a (cast_by, v)) = liftA (uncurry ((,,) (oneResidTag tg_a `plusResidTags` resid_tgs)) . second mk_cast) $ case v of
    Literal l          -> pure (emptyDeeds, value (Literal l))
    Coercion co        -> pure (emptyDeeds, value (Coercion co))
    Data dc tys cos xs -> pure (emptyDeeds, value (Data dc tys cos xs))
    TyLambda a state   -> liftA (second (value . TyLambda a)) $ opt state
    Lambda   x state   -> liftA (second (value . Lambda   x)) $ opt state
  where (resid_tgs, mk_cast) = case cast_by of Uncast          -> (emptyResidTags,    id)
                                               CastBy co tg_co -> (oneResidTag tg_co, (`cast` co))

recurseStack :: (Applicative m, Monad m)
             => (State -> m (Deeds, Out FVedTerm))
             -> PushedStack -> (ResidTags, Deeds, Out FVedTerm) -> m (ResidTags, Deeds, [(Var, FVedTerm)], FVedTerm)
recurseStack opt k (init_resid_tgs, init_deeds, init_e) = (\f -> foldM f (init_resid_tgs, init_deeds, [], init_e) k) $ \(resid_tgs, deeds, xes, e) (Tagged tg_kf kf) -> do
    (resid_tgs, deeds, xes, e) <- case kf of
      TyApply ty                  -> return (resid_tgs, deeds, xes, e `tyApp` ty)
      CoApply co                  -> return (resid_tgs, deeds, xes, e `coApp` co)
      Apply   x                   -> return (resid_tgs, deeds, xes, e `app`   x)
      Scrutinise x ty alts        -> liftM ((\(extra_deedss, alts') -> (resid_tgs, plusDeedss extra_deedss `plusDeeds` deeds, xes, case_ e x ty alts')) . unzip) $ forM alts $ \(alt_con, state) -> liftM (second ((,) alt_con)) $ opt state
      PrimApply pop tys as states -> (\(resid_tgss, as_deedss, as_es') (states_deedss, states_es') -> (plusResidTagss (resid_tgs:resid_tgss), plusDeedss as_deedss `plusDeeds` plusDeedss states_deedss `plusDeeds` deeds, xes, primOp pop tys (as_es' ++ e:states_es')))
                                       <$> liftM unzip3 (mapM (recurseAnswer opt) as) <*> liftM unzip (mapM opt states)
      StrictLet x state           -> liftM (\(extra_deeds, e') -> (resid_tgs, extra_deeds `plusDeeds` deeds, xes, let_ x e e')) $ opt state
      Update x                    -> return (resid_tgs, deeds, (x, e) : xes, var x)
      CastIt co                   -> return (resid_tgs, deeds, xes, e `cast` co)
    return (oneResidTag tg_kf `plusResidTags` resid_tgs, deeds, xes, e)

recurseHeap :: Monad m
            => (State -> m (Deeds, Out FVedTerm))
            -> PushedHeap -> (ResidTags, Deeds, [(Var, FVedTerm)], FVedTerm) -> m (ResidTags, Deeds, FVedTerm)
recurseHeap opt init_h (resid_tgs, init_deeds, init_xes, e)
  -- Unfortunately, it is necessary to remove elements from init_h that already have a residual binding in init_xes.
  -- The reason for this is that if the stack has an initial update and a value is in focus, we can get a residual
  -- binding for that from either the "stack" or the "heap" portion. What we must avoid is binding both in a let at the same time!
  = go (foldr (M.delete . fst) init_h init_xes) init_deeds init_xes
       (foldr (\(x, e) fvs -> varBndrFreeVars x `unionVarSet` fvedTermFreeVars e `unionVarSet` fvs) (fvedTermFreeVars e) init_xes)
  where go h deeds xes do_fvs
          -- | pprTrace "go" (ppr do_fvs $$ ppr (M.keysSet h)) False = undefined
          | M.null h_to_do = return (resid_tgs, deeds, bindManyMixedLiftedness fvedTermFreeVars xes e)
          | otherwise      = do (extra_deedss, extra_xes) <- liftM unzip $ mapM (\(x, e) -> {- pprTrace "go1" (ppr x) $ -} liftM (second ((,) x)) $ opt e) (M.toList h_to_do)
                                go h' (plusDeedss extra_deedss `plusDeeds` deeds) (extra_xes ++ xes)
                                   (foldr (\(x, e) do_fvs -> varBndrFreeVars x `unionVarSet` fvedTermFreeVars e `unionVarSet` do_fvs) emptyVarSet extra_xes)
         where (h_to_do, h') = M.partitionWithKey (\x _ -> x `elemVarSet` do_fvs) h

{-

-- Push as much stuff as possible transitively into heap bindings
-- ~~~
-- Expect z to be residualised, x and y to be pushed in
test1 = Just z
  where x = sumseq 100
        y = sumseq x
        z = sumseq y

-- Push stuff transitively in even if there are multiple syntactic occurrences
-- ~~~
-- Expect z to be residualised, w, x and y to be pushed in
test2 = Just z
  where w = sumseq 100
        x = sumseq w
        y = sumseq w
        z = sumseq x + sumseq y

-- Inline values even into non-linear contexts
-- ~~~
-- Expect that x is pushed into the lambda
test3 y ys = \_ -> head x
  where x = y:ys

-- Do not inline non-values into non-linear contexts
-- ~~~
-- Expect that x is residualised outside the lambda
test4 = \_ -> x+1
  where x = sumseq 100

-- Treat non-linearity due to cases appropriately
-- ~~~
-- Expect that y is pushed down instead of residualised
test5 x = if x then y + 1 else y + 2
  where y = sumseq 100

-- Even when the case braches themselves refer to a thing whose value depends on the scrut
-- ~~~
-- Expect that the update for ys and scrutinisation on ys is pushed into each branch
test6 :: Bool -> Int
test6 unk = case ys of x : _ -> x; [] -> 0
  where ys = if unk then 1 : ys else 2 : ys -- NB: case branches refer to "ys" bound by an update frame at time of split

-- Deal with cycles through the stack
-- ~~~
-- Expect that everything is residualised except z, which can be pushed into y
test7 :: Int -> Int
test7 unk = x + 2
  where z = sumseq x
        y = unk + z
        x = case y of 1 -> 2; _ -> 3

-- Allowing pushing of things which are cyclic due to loops
-- ~~~
-- Expect that xs is pushed into the z heap binding
test8 x = Just z
  where xs = cons x xs
        z = head xs

-- Push cyclic things even if they are mutually recursive
-- ~~~
-- Expect that xs and ys are pushed into the z heap binding
test9 :: Int -> Maybe Int
test9 x = Just z
  where xs = cons x ys
        ys = cons x xs
        z = head xs + head ys

-- Do not push cyclic things naively
-- ~~~
-- Expect that both xs and ys are residualised (I had a real bug where they weren't)
test10 x = (a, b)
  where xs = cons x ys
        ys = cons x xs
        a = head xs
        b = head ys

-- Preferentially push down values when we have a choice
-- ~~~
-- Expect that y is pushed into the lambda body and x is residualised around the lambda
-- (Note that it would also be "valid" to residualise f and push x into the heap binding,
-- to get resid around the f lambda. If f were a type lambda then we might even get fusion this way,
-- so this is something of a heuristic)
test11 = \_ -> f () + 1
  where x = sumseq 100
        f = \_ -> x + 1

-}


data PushFocus qa term = QAFocus qa
                       | TermFocus term
                       | OpaqueFocus FVedTerm

type PushedHeap  = M.Map (Out Var) State
type PushedStack = [Tagged (StackFrameG (Tagged (AnswerG (ValueG State))) State [AltG State])]
type PushedFocus = PushFocus (Tagged (QAG (ValueG State))) State

-- FIXME: deal with deeds in here
push :: S.Set Context
     -> (Heap, Stack, PushFocus (Anned QA) (In AnnedTerm))
     -> (PushedHeap, PushedStack, PushedFocus)
push generalised (Heap h ids, k, focus) = (h', k', focus')
  where -- NB: values or variables in focus which are immediately updated are a pain. We:
        --  1. Construct a bit of graph from the update frame which overwrites the normal HeapContext binding from splitStack to ensure
        --     that there is no edge from the variable to its update frame (we can just push the definition down, not the frame)
        --  2. Explicitly ensure that the thing bound by the update frame is considered cheap and hence shorcutted through in the graph
        --
        -- NB: in this case we may end up marking the y' bound by the update frame but not the frame itself (at index 0 or 1)
        (cheap_marked_k_head, h_k_head) = case fst (peelUpdateStack k) of
          Just (cast_by, Tagged _tg_y' y') | QAFocus qa <- focus -> (S.singleton (HeapContext y'), M.singleton y' (internallyBound (castAnnedQAToInAnnedTerm ids qa cast_by)))
          _                                                      -> (S.empty, M.empty)

        -- TODO: arguably I should try to get a QA for the thing in the focus. This will help in cases like where we MSG together:
        --  < H | v | >
        -- and:
        --  < H, H' | v | update f >
        -- Since ideally instance splitting the second state should allow us to drive H' with the value binding f |-> v. A similar argument applies to questions in focus.
        mb_scrut = case focus of QAFocus qa | Question x' <- annee qa -> Just x'; _ -> Nothing

        (verts_h,     prepare_h, mk_h)     = splitPureHeap ids (h `unionDisjoint` h_k_head)
        (verts_k,     prepare_k, mk_k)     = splitStack    ids k     mb_scrut
        (verts_focus,            mk_focus) = splitFocus    ids focus (FocusContext `S.member` generalised)

        -- We *always* mark values. This is really a rather interesting choice. If I have:
        --   x = e1
        --   y = v2[x]
        --   z = \a -> e3[y]
        --
        -- If I start supercompiling with y as the root, I might be able to fuse e1 into v2 if the occurrence
        -- within the RHS of y is Once. However, if I start from z then I may not be able to do this fusion if
        -- y occurs in a Many context because the value portion v2 will be moved down and the expression portion
        -- x will be left residualised above the lambda.
        --
        -- What I gain from this behaviour, of course, is that v2 may fuse with e3, which is probably more valuable
        -- in general anyway. 
        --
        -- NB: must explicitly avoid collapsing away any value nodes if they are marked as generalised
        --
        -- We have to remove any unreachable nodes, or they may pessimise my results by acting as extra "roots" and hence
        -- forcing more things to be unmarked. In particular, we have to watch out for:
        --   1. Vertices originating from dead heap bindings
        --   2. Heap verticies originating from on-stack updates that bind dead variables
        cheap_marked = (cheap_marked_k_head `S.union` S.fromDistinctAscList [HeapContext x' | (x', hb) <- M.toAscList h, maybe True (termIsCheap . snd) (heapBindingTerm hb)]) S.\\ generalised
        verts = trimUnreachable FocusContext $
                shortcutEdges (`S.member` cheap_marked)
                              plusEntries (\ent1 _ ent2 -> ent1 `plusEntries` ent2)
                              (verts_h `M.union` (verts_k `unionDisjoint` verts_focus)) -- NB: verts_h might mention a heap binding bound by a leading update frame in verts_k: h takes precedence
        extra_marked = solve generalised verts
        marked = cheap_marked `S.union` extra_marked

        -- Prepare a version of the heap and stack suitable for inlining
        h_prep_h           = prepare_h generalised marked
        (h_prep_k, k_prep) = prepare_k generalised marked
        h_prep             = h_prep_h `M.union` h_prep_k

        -- Produce final syntax with nested States
        h'     = mk_h     h_prep {- We never inline any stack information into the heap -}
        k'     = mk_k     h_prep k_prep
        focus' = mk_focus h_prep k_prep

solve :: S.Set Context
      -> LGraph Context Entries
      -> S.Set Context
solve generalised = M.keysSet . go_graph
  where
    go_graph = uncurry (flip $ go M.empty) . sccs

    -- NB: the input list is ascending, so lower indexes come first, so we process all predecessors of a SCC before the SCC itself
    go :: M.Map Context (Maybe Context)                         -- Successor |-> Just context (iff you end up in a *single* context by inlining into all predecessors, and which context that is).
       -> IM.IntMap (LGraph Context Entries)                    -- Information about the internal structure of each SCC
       -> [(Int, M.Map Int (M.Map (Context, Context) Entries))] -- Topologically sorted SCC graph
       -> M.Map Context Context                                 -- Marked contexts, mapped to the context they will end up in after inlining
    go _            _       []                             = M.empty
    go predecessors scc_map ((lowlink, external_ens):rest) = marks' `unionDisjoint` go predecessors' scc_map rest
      where
        scc = IM.findWithDefault (error "solveSCCs: no SCC info") lowlink scc_map
        -- 1. Find all entry points to the SCC
        -- 2. Check whether *all* the entry points can be marked
        --   a) If so, mark everything in the SCC
        --   b) Otherwise, meddle with the subgraph to remove all edges pointing to the entry points
        --      and recursively solve this subgraph (treating all nodes with no incoming edges as unmarkable roots),
        --      getting back a set possibly containing extra marks for some of the things in the SCC
        -- 3. Recurse with the new mark set
        predecessors_here = M.filterWithKey (\n _ -> n `M.member` scc) predecessors

        marks' = case M.null predecessors_here of
              -- No predecessors to whole SCC: this must be a root node (which incidentally may never be a self-cycle).
              -- A root node is either a FocusContext or an element of a SCC forced to be resid to break cycles in the
              -- induction step. We NEVER want to mark such a node.
              True -> M.empty
              -- SCC has predecessors
              False | Just common_ctxt <- foldr1 plusContext $ M.elems predecessors_here
                    , S.null (M.keysSet scc `S.intersection` generalised)
                    -- Inlining along *all* of the predecessors for *all* of the entry points arrives at a
                    -- common destination, and all of the SCC nodes are ungeneralised, so we can mark the whole SCC.
                    -- If even one node in the SCC is marked as generalised then we can't do a thing about it,
                    -- because marking even one entry node in the SCC will require us to mark that generalised
                    -- node as well, which is not allowed.
                    -> M.fromDistinctAscList [(scc_node, common_ctxt) | scc_node <- M.keys scc]
                    | otherwise
                    -- Inlining failed along at least one path. By a theorem, we can only mark an entry node of the SCC
                    -- if we can mark *every* entry node. Thus at this point we will force all such nodes to be unmarked
                    -- and then recursively solve the simplified SCC graph to see if we can inline anything that previously
                    -- participated in a cycle but was not itself an entry node.
                    , let force_unmarked = M.keysSet predecessors_here
                          scc_cut = filterEdges (\_ n' -> n' `S.notMember` force_unmarked) scc
                          -- NB: no need to specify any predecessors in this recursive call because all nodes with a
                          -- predecessor in a previous SCC have been force unmarked
                    -> go_graph scc_cut

        predecessors' = foldr (uncurry $ M.insertWith plusContext) predecessors
                              [ (ctxt', mb_destination)
                              | external_ens' <- M.elems external_ens
                              , ((ctxt, ctxt'), ent) <- M.toList external_ens'
                              , let mb_destination | Just dest_ctxt <- M.lookup ctxt marks' = Just dest_ctxt -- Marked, inherits final context (NB: in this case the edge annotation is irrelevant)
                                                   | ManyEntries <- ent                     = Nothing        -- Not marked and inlining would duplicate work, so prevent marking of successors
                                                   | otherwise                              = Just ctxt      -- Not marked, so any inlining (which would not duplicate work) stops here
                              ]

plusContext :: Maybe Context -> Maybe Context -> Maybe Context
plusContext (Just c1) (Just c2) | c1 == c2 = Just c1
plusContext _ _ = Nothing

splitFocus :: InScopeSet -> PushFocus (Anned QA) (In AnnedTerm) -> Generalised -> (LGraph Context Entries,
                                                                                   PureHeap -> IM.IntMap Stack -> PushedFocus)
splitFocus ids (QAFocus qa)     True  = splitQA ids qa
splitFocus ids (QAFocus qa)     False = (M.singleton FocusContext $ M.insert (StackContext 0) OneEntry (varEdges OneEntry (annedFreeVars qa)),
                                         \h_prep k_prep -> TermFocus (emptyDeeds, Heap h_prep ids, lookupStackPrep 0 k_prep, qa))
splitFocus ids (TermFocus in_e) True  = splitOpaque $ annedTermToFVedTerm $ renameIn (renameAnnedTerm ids) in_e
splitFocus ids (TermFocus in_e) False = (M.singleton FocusContext $ M.insert (StackContext 0) OneEntry e_verts,
                                         \h_prep k_prep -> TermFocus (mk_e h_prep k_prep))
  where (e_verts, mk_e) = splitTailKnownTerm ids 0 in_e
splitFocus _   (OpaqueFocus e') _     = splitOpaque e'

splitOpaque :: FVedTerm -> (LGraph Context Entries,
                            PureHeap -> IM.IntMap Stack -> PushedFocus)
splitOpaque e' = (M.singleton FocusContext $ M.insert (StackContext 0) ManyEntries $ varEdges ManyEntries (fvedTermFreeVars e'), \_ _ -> OpaqueFocus e')

splitQA :: InScopeSet -> Anned QA -> (LGraph Context Entries,
                                      PureHeap -> IM.IntMap Stack -> PushedFocus)
splitQA ids anned_qa = (M.singleton FocusContext $ M.insert (StackContext 0) ManyEntries qa_verts, \h_prep _k_prep -> QAFocus $ Tagged (annedTag anned_qa) (mk_untagged_qa h_prep))
  where (qa_verts, mk_untagged_qa) = case annee anned_qa of
          Question x' -> (M.singleton (HeapContext x') ManyEntries, \_ -> Question x')
          Answer a    -> second (Answer .) $ splitAnswer ids a

splitAnswer :: InScopeSet -> Answer -> (M.Map Context Entries,
                                        PureHeap -> AnswerG (ValueG State))
splitAnswer ids (cast_by, (rn, v)) = (plusEntered (varEdges ManyEntries (castByFreeVars cast_by)) v_verts, ((,) cast_by) . mk_v)
  where
    (v_verts, mk_v) = case renameValueG (,,) ids rn v of
        -- The isOneShotBndr check is really necessary if we want to fuse a top-level non-value with some consuming context in the IO monad.
        Lambda   x' ids_in_e  -> second (Lambda x' .)   $ split_lambda x' (if isOneShotBndr x' then OneEntry else ManyEntries) ids_in_e
        TyLambda a' ids_in_e  -> second (TyLambda a' .) $ split_lambda a' OneEntry                                             ids_in_e
        Literal l'            -> holeless_v $ Literal l'
        Coercion co'          -> holeless_v $ Coercion co'
        Data dc tys' cos' xs' -> holeless_v $ Data dc tys' cos' xs'
      where holeless_v v' = (varEdges ManyEntries $ valueGFreeVars' (const emptyVarSet) v', \_ -> v')

    split_lambda x' entries (ids', rn', e) = (varBndrEdges x' e_verts, mk_e . M.insert x' lambdaBound)
      where (e_verts, mk_e) = splitTerm ids' entries (rn', e)

splitTerm :: InScopeSet -> Entries -> In AnnedTerm -> (M.Map Context Entries,
                                                       PureHeap -> State)
splitTerm ids entries (rn, e) = (varEdges entries (annedTermFreeVars (renameAnnedTerm ids rn e)),
                                 \h_prep -> normalise (emptyDeeds, Heap h_prep ids, Loco False, (rn, e)))

splitTailKnownTerm :: InScopeSet -> Int -> In AnnedTerm -> (M.Map Context Entries,
                                                            PureHeap -> IM.IntMap Stack -> State)
splitTailKnownTerm ids frame (rn, e) = (varEdges OneEntry (annedTermFreeVars (renameAnnedTerm ids rn e)),
                                        \h_prep k_prep -> normalise (emptyDeeds, Heap h_prep ids, lookupStackPrep frame k_prep, (rn, e)))

-- NB: when driving a residual binding:
--   let x = D[e]
--   in ..
--
-- Arjan Boeijink suggested driving the following instead of D[e]:
--   D[< | e | update x>]
--
-- This can help propagate more positive information, e.g. if e contains an occurrence of x itself
--
-- I'm not doing this right now because I'm wary about the termination issues. We should also be careful that we
-- don't create loops as a result...

-- NB: we need to add elements to the graph even for empty lambdaBound bindings to avoid references to nonexistant nodes
splitPureHeap :: InScopeSet -> PureHeap -> (LGraph Context Entries,
                                            S.Set Context -> S.Set Context -> PureHeap,
                                            PureHeap -> PushedHeap)
splitPureHeap ids h = (M.fromDistinctAscList [ (HeapContext x', fmap fst mb_split_hb `orElse` M.empty)
                                             | (x', mb_split_hb) <- M.toAscList split_h ],
                       \generalised marked -> (\f -> M.mapWithKey f h) $ \x' hb -> if HeapContext x' `S.member` marked then hb else if HeapContext x' `S.member` generalised then generalisedLambdaBound else lambdaBound, -- FIXME: bugger around with howToBindCheap?
                       \h_prep -> (\f -> M.mapMaybe f split_h) $ \mb_split_hb -> do
                                    -- TODO: we could only include in the output those bindings that are either NOT marked for inlining,
                                    -- or are cheap (and thus had marking forced regardless of whether they are used in the residual).
                                    -- Similarly, it would be cool to exclude bindings arising from the first update frame to avoid messiness in recurseHeap
                                    (_, (how_bound, mk_e)) <- mb_split_hb
                                    guard (how_bound == InternallyBound)
                                    return (mk_e h_prep))
  where
    split_h :: M.Map Var (Maybe (M.Map Context Entries, (HowBound, PureHeap -> State)))
    split_h = flip M.map h $ \hb -> fmap ((second ((,) (howBound hb))) . splitTerm ids OneEntry) (heapBindingTerm hb)

-- NB: we need to add an explicit final frame to prevent the stack and QA graphs from having references to nonexistant nodes
splitStack :: InScopeSet -> Stack -> Maybe Var -> (LGraph Context Entries,
                                                   S.Set Context -> S.Set Context -> (PureHeap, IM.IntMap Stack),
                                                   PureHeap -> IM.IntMap Stack -> PushedStack)
splitStack ids k mb_scrut = go (fmap (\x' -> ((Uncast, x'), [])) mb_scrut, 0, [], \_ _ -> (M.empty, (Nothing, IM.empty)), \_ _ -> [] {- \_ _ -> ss_unknown_tail [] -}) k
  where
    finish_prep_k :: Generalised -> (Maybe (Int, Stack -> Stack), IM.IntMap Stack) -> IM.IntMap Stack
    finish_prep_k gen (mb_next_run, done_runs) = maybe id (\(next_run_frame, next_run) -> IM.insert next_run_frame (next_run (Loco gen))) mb_next_run done_runs

    go (_,         last_frame, verts, prep_k, mk_k) (Loco gen)                = (fromListDisjoint ((StackContext last_frame, M.empty):verts), (second (finish_prep_k gen) .) . prep_k, (reverse .) . mk_k)
    go (mb_scruts, frame,      verts, prep_k, mk_k) (Tagged tg_kf kf `Car` k) = go (mb_scruts', next_frame, verts' ++ verts, prep_k', mk_k') k
      where
        -- NB: we insert dummies into done_runs so we can signal to mk_k that frame was marked, even if the frame is not the first in a run of marked frames
        -- NB: this *does* produce the stack in the right order, since:
        --       xs == foldl (\rest x -> (. (x:)) rest) id xs []
        prep_k' generalised marked | StackContext frame `S.member` marked = (h_update,                                                   (Just $ second (. (Tagged tg_kf kf `Car`)) $ fromMaybe (frame, id) mb_next_run, IM.insert frame (error "prep_k': dummy") done_runs))
                                   | otherwise                            = (maybe id (flip M.insert lambdaBound) mb_update_x' h_update, (Nothing, finish_prep_k (StackContext frame `S.member` generalised) (mb_next_run, done_runs)))
          where (h_update, (mb_next_run, done_runs)) = prep_k generalised marked

        mk_k' h_prep k_prep | frame `IM.member` k_prep = mk_k h_prep k_prep -- If the frame was marked (inlined), we needn't residualise it
                            | otherwise                = Tagged tg_kf (kf_prep h_prep k_prep) : mk_k h_prep k_prep

        scruts_flat = maybe [] (uncurry (:)) mb_scruts

        next_frame = frame + 1
        verts' = (StackContext frame, M.insert (StackContext next_frame) (if know_tail then OneEntry else ManyEntries) edges):update_verts
        update_verts = case mb_update_x' of
          Just x' -> [(HeapContext x', M.singleton (StackContext frame) ManyEntries)]
          Nothing -> []

        (mb_scruts', mb_update_x', know_tail, edges, kf_prep) = case kf of
          TyApply ty'                  -> (Nothing, Nothing, False, M.empty, \_ _ -> TyApply ty')
          CoApply co'                  -> (Nothing, Nothing, False, varEdges ManyEntries (tyCoVarsOfCo co'), \_ _ -> CoApply co')
          Apply x'                     -> (Nothing, Nothing, False, M.singleton (HeapContext x') ManyEntries, \_ _ -> Apply x')
          Scrutinise x' ty' (rn, alts) -> (Nothing, Nothing, True, varBndrEdges x' $ foldr plusEntered M.empty alts_verts,
                                           \h_prep k_prep -> Scrutinise x' (stackType (lookupStackPrep next_frame k_prep) ty') (map (($ k_prep) . ($ h_prep)) mk_alts))
            where any_scrut_live = any (not . isDeadBinder . snd) scruts_flat
                  
                  -- These lines achieve two things:
                  --   1. Filter out any branches of the case which we know are impossible due to type refinement
                  --   2. Turn any remaining default cases into explicit constructors if possible (helps positive information propagation)
                  refined_alts | not rEFINE_ALTS = alts
                               | otherwise = [ (coreAltConToAltCon altcon xs, e)
                                             | (altcon, xs, e) <- thirdOf3 $ filterAlts (repeat wildCardKey) (idType x') []
                                                                                        [ (altcon', xs, e)
                                                                                        | (altcon, e) <- alts
                                                                                        , let (altcon', xs) = altConToCoreAltCon altcon ] ]

                  (alts_verts, mk_alts) = unzip [ (foldr varBndrEdges e_verts alt_bvs',
                                                   \h_prep k_prep -> let h_pos | pOSITIVE_INFORMATION
                                                                               , Just anned_v <- altConToValue (idType x') alt_con'
                                                                               , let anned_e = fmap Value anned_v
                                                                               -- State:  < | x :: A | [_] |> (co1 :: A ~ B), update (y :: B), [_] |> (co0 :: B ~ C), case ([_] :: C) ... >
                                                                               -- Scruts: [(co0, y), (co1, x)]
                                                                               -- Result: x |-> alt |> sym co0 `trans` sym co1
                                                                               --         y |-> alt |> sym co0
                                                                               = snd $ (\f -> foldl f (Uncast, M.empty) scruts_flat) $ \(overall_cast_by, h_pos) (cast_by', y') ->
                                                                                                let overall_cast_by' = mkTransCastBy ids' overall_cast_by (mkSymCastBy ids' cast_by')
                                                                                                    -- Localise the Id just in case this is the occurrence of a lambda-bound variable.
                                                                                                    -- We don't really want a Let-bound external name in the output!
                                                                                                in (overall_cast_by', M.insert (localiseId y') (internallyBound (renamedTerm (castAnnedTerm overall_cast_by' anned_e))) h_pos)
                                                                               | otherwise
                                                                               = M.empty
                                                                     in (alt_con', mk_e (h_pos `M.union` foldr (\y' -> M.insert y' lambdaBound) h_prep (x':alt_bvs')) k_prep))
                                                | (alt_con, e) <- refined_alts
                                                  -- We have to carefully zap OccInfo here because one of the case binders might be marked as dead,
                                                  -- yet could become live due to positive information propagation! 
                                                , let (ids', rn', alt_con') = renameAltCon ids rn (if any_scrut_live then zapAltConIdOccInfo alt_con else alt_con)
                                                      (e_verts, mk_e) = splitTailKnownTerm ids' next_frame (rn', e)
                                                      alt_bvs' = altConBoundVars alt_con' ]
          PrimApply pop tys' as in_es  -> (Nothing, Nothing, False, foldr multEntered M.empty (as_verts ++ es_verts),
                                           \h_prep _k_prep -> PrimApply pop tys' (map ($ h_prep) mk_as) (map ($ h_prep) mk_es))
            where (as_verts, mk_as) = unzip $ map (\anned_a -> second (Tagged (annedTag anned_a) .) $ splitAnswer ids (annee anned_a)) as
                  (es_verts, mk_es) = unzip $ map (splitTerm ids OneEntry) in_es
          StrictLet x' in_e            -> (Nothing, Nothing, True, varBndrEdges x' e_verts,
                                           \h_prep k_prep -> StrictLet x' (mk_e (M.insert x' lambdaBound h_prep) k_prep))
            where (e_verts, mk_e) = splitTailKnownTerm ids next_frame in_e
          CastIt co'                   -> (fmap (\((cast_by, x'), rest) -> ((castBy (maybe co' (\co'' -> mkTransCo ids co'' co') (castByCo cast_by)) tg_kf, x'), rest)) mb_scruts,
                                           Nothing, False, varEdges ManyEntries (tyCoVarsOfCo co'), \_ _ -> CastIt co')
          Update x'                    -> (Just ((Uncast, x'), scruts_flat),
                                           Just x', False, varEdges ManyEntries (varBndrFreeVars x'), \_ _ -> Update x')

lookupStackPrep :: Int -> IM.IntMap Stack -> Stack
lookupStackPrep = IM.findWithDefault (Loco False)

varBndrEdges :: Var -> M.Map Context Entries -> M.Map Context Entries
varBndrEdges x' verts = varEdges ManyEntries (varBndrFreeVars x') `plusEntered` M.delete (HeapContext x') verts
