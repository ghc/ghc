-- | An abstract interface for a fast reachability data structure constructed
-- from a 'GHC.Data.Graph.Directed' graph.
module GHC.Data.Graph.Directed.Reachability
  ( ReachabilityIndex

  -- * Constructing a reachability index
  , graphReachability, cyclicGraphReachability

  -- * Reachability queries
  , allReachable, allReachableMany
  , isReachable, isReachableMany
  )
  where

import GHC.Prelude
import GHC.Data.Maybe

import qualified Data.Graph as G
import Data.Graph ( Vertex, SCC(..) )

import Data.Array ((!))
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import GHC.Data.Graph.Directed.Internal

--------------------------------------------------------------------------------
-- * Reachability index
--------------------------------------------------------------------------------

-- | The abstract data structure for fast reachability queries
data ReachabilityIndex node = ReachabilityIndex {
    index :: IM.IntMap IS.IntSet,
    from_vertex :: Vertex -> node,
    to_vertex :: node -> Maybe Vertex
}

--------------------------------------------------------------------------------
-- * Construction
--------------------------------------------------------------------------------

-- | Construct a 'ReachabilityIndex' from an acyclic 'Graph'.
-- If the graph can have cycles, use 'cyclicGraphReachability'
graphReachability :: Graph node -> ReachabilityIndex node
graphReachability (Graph g from to) =
  ReachabilityIndex{index = reachableGraph, from_vertex = from, to_vertex = to}
    where
      reachableGraph :: IM.IntMap IS.IntSet
      reachableGraph = IM.fromList [(v, do_one v) | v <- G.vertices g]

      do_one v = IS.unions (IS.fromList (g ! v) : mapMaybe (flip IM.lookup reachableGraph) (g ! v))

-- | Construct a 'ReachabilityIndex' from a 'Graph' which may have cycles.
-- If this reachability index is just going to be used once, it may make sense
-- to use 'reachablesG' instead, which will traverse the reachable nodes without
-- constructing the index -- which may be faster.
cyclicGraphReachability :: Graph node -> ReachabilityIndex node
cyclicGraphReachability (Graph g from to) =
  ReachabilityIndex{index = reachableGraphCyclic, from_vertex = from, to_vertex = to}
    where
      reachableGraphCyclic :: IM.IntMap IS.IntSet
      reachableGraphCyclic = foldl' add_one_comp mempty comps

      neighboursOf v = g!v

      comps = scc g

      -- To avoid divergence on cyclic input, we build the result
      -- strongly connected component by component, in topological
      -- order. For each SCC, we know that:
      --
      --   * All vertices in the component can reach all other vertices
      --     in the component ("local" reachables)
      --
      --   * Other reachable vertices ("remote" reachables) must come
      --     from earlier components, either via direct neighbourhood, or
      --     transitively from earlier reachability map
      --
      -- This allows us to build the extension of the reachability map
      -- directly, without any self-reference, thereby avoiding a loop.
      add_one_comp :: IM.IntMap IS.IntSet -> SCC Vertex -> IM.IntMap IS.IntSet
      add_one_comp earlier (AcyclicSCC v) = IM.insert v all_remotes earlier
        where
          earlier_neighbours = neighboursOf v
          earlier_further = mapMaybe (flip IM.lookup earlier) earlier_neighbours
          all_remotes = IS.unions (IS.fromList earlier_neighbours : earlier_further)
      add_one_comp earlier (CyclicSCC vs) = IM.union (IM.fromList [(v, local v `IS.union` all_remotes) | v <- vs]) earlier
        where
          all_locals = IS.fromList vs
          local v = IS.delete v all_locals
              -- Arguably, for a cyclic SCC we should include each
              -- vertex in its own reachable set. However, this could
              -- lead to a lot of extra pain in client code to avoid
              -- looping when traversing the reachability map.
          all_neighbours = IS.fromList (concatMap neighboursOf vs)
          earlier_neighbours = all_neighbours IS.\\ all_locals
          earlier_further = mapMaybe (flip IM.lookup earlier) (IS.toList earlier_neighbours)
          all_remotes = IS.unions (earlier_neighbours : earlier_further)

--------------------------------------------------------------------------------
-- * Reachability queries
--------------------------------------------------------------------------------

-- | 'allReachable' returns the nodes reachable from the given @root@ node.
--
-- Properties:
--  * The list of nodes /does not/ include the @root@ node!
--  * The list of nodes is deterministically ordered, but according to an
--     internal order determined by the indices attributed to graph nodes.
--  * This function has $O(1)$ complexity.
--
-- If you need a topologically sorted list, consider using the functions exposed from 'GHC.Data.Graph.Directed' on 'Graph' instead.
allReachable :: ReachabilityIndex node -> node {-^ The @root@ node -} -> [node] {-^ All nodes reachable from @root@ -}
allReachable (ReachabilityIndex index from to) root = map from result
  where root_i = expectJust "reachableFrom" (to root)
        hits = {-# SCC "allReachable" #-} IM.lookup root_i index
        result = IS.toList $! expectJust "reachableFrom" hits

-- | 'allReachableMany' returns all nodes reachable from the many given @roots@.
--
-- Properties:
--  * The list of nodes /does not/ include the @roots@ node!
--  * The list of nodes is deterministically ordered, but according to an
--     internal order determined by the indices attributed to graph nodes.
--  * This function has $O(n)$ complexity where $n$ is the number of @roots@.
--
-- If you need a topologically sorted list, consider using the functions
-- exposed from 'GHC.Data.Graph.Directed' on 'Graph' instead ('reachableG').
allReachableMany :: ReachabilityIndex node -> [node] {-^ The @roots@ -} -> [node] {-^ All nodes reachable from all @roots@ -}
allReachableMany (ReachabilityIndex index from to) roots = map from (IS.toList hits)
  where roots_i = [ v | Just v <- map to roots ]
        hits = {-# SCC "allReachableMany" #-}
               IS.unions $ map (expectJust "reachablesG" . flip IM.lookup index) roots_i

-- | Fast reachability query.
--
-- On graph @g@ with nodes @a@ and @b@, @isReachable g a b@
-- asks whether @b@ can be reached through @g@ starting from @a@.
--
-- Properties:
--  * No self loops, i.e. @isReachable _ a a == False@
--  * This function has $O(1)$ complexity.
isReachable :: ReachabilityIndex node {-^ @g@ -}
            -> node -- ^ @a@
            -> node -- ^ @b@
            -> Bool -- ^ @b@ is reachable from @a@
isReachable (ReachabilityIndex index _ to) a b =
    IS.member b_i $
    expectJust "reachable" $ IM.lookup a_i index
  where a_i = expectJust "reachable:node not in graph" $ to a
        b_i = expectJust "reachable:node not in graph" $ to b

-- | Fast reachability query with many roots.
--
-- On graph @g@ with many nodes @roots@ and node @b@, @isReachableMany g as b@
-- asks whether @b@ can be reached through @g@ from any of the @roots@.
--
-- Properties:
--  * No self loops, i.e. @isReachableMany _ [a] a == False@
--  * This function is $O(n)$ in the number of roots
isReachableMany :: ReachabilityIndex node -- ^ @g@
                -> [node] -- ^ @roots@
                -> node -- ^ @b@
                -> Bool -- ^ @b@ is reachable from any of the @roots@
isReachableMany (ReachabilityIndex index _ to) roots b =
    IS.member b_i $
    IS.unions $
    map (expectJust "reachablesQuery" . flip IM.lookup index) roots_i
  where roots_i = [ v | Just v <- map to roots ]
        b_i = expectJust "reachablesQuery:node not in graph" $ to b

