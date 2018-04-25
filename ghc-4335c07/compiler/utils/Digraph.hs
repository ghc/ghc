-- (c) The University of Glasgow 2006

{-# LANGUAGE CPP, ScopedTypeVariables, ViewPatterns #-}

module Digraph(
        Graph, graphFromEdgedVerticesOrd, graphFromEdgedVerticesUniq,

        SCC(..), Node(..), flattenSCC, flattenSCCs,
        stronglyConnCompG,
        topologicalSortG, dfsTopSortG,
        verticesG, edgesG, hasVertexG,
        reachableG, reachablesG, transposeG,
        outdegreeG, indegreeG,
        vertexGroupsG, emptyG,
        componentsG,

        findCycle,

        -- For backwards compatibility with the simpler version of Digraph
        stronglyConnCompFromEdgedVerticesOrd,
        stronglyConnCompFromEdgedVerticesOrdR,
        stronglyConnCompFromEdgedVerticesUniq,
        stronglyConnCompFromEdgedVerticesUniqR,
    ) where

#include "HsVersions.h"

------------------------------------------------------------------------------
-- A version of the graph algorithms described in:
--
-- ``Lazy Depth-First Search and Linear IntGraph Algorithms in Haskell''
--   by David King and John Launchbury
--
-- Also included is some additional code for printing tree structures ...
--
-- If you ever find yourself in need of algorithms for classifying edges,
-- or finding connected/biconnected components, consult the history; Sigbjorn
-- Finne contributed some implementations in 1997, although we've since
-- removed them since they were not used anywhere in GHC.
------------------------------------------------------------------------------


import GhcPrelude

import Util        ( minWith, count )
import Outputable
import Maybes      ( expectJust )
import MonadUtils  ( allM )

-- Extensions
import Control.Monad    ( filterM, liftM, liftM2 )
import Control.Monad.ST

-- std interfaces
import Data.Maybe
import Data.Array
import Data.List hiding (transpose)
import Data.Array.ST
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Graph as G
import Data.Graph hiding (Graph, Edge, transposeG, reachable)
import Data.Tree
import Unique
import UniqFM

{-
************************************************************************
*                                                                      *
*      Graphs and Graph Construction
*                                                                      *
************************************************************************

Note [Nodes, keys, vertices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * A 'node' is a big blob of client-stuff

 * Each 'node' has a unique (client) 'key', but the latter
        is in Ord and has fast comparison

 * Digraph then maps each 'key' to a Vertex (Int) which is
        arranged densely in 0.n
-}

data Graph node = Graph {
    gr_int_graph      :: IntGraph,
    gr_vertex_to_node :: Vertex -> node,
    gr_node_to_vertex :: node -> Maybe Vertex
  }

data Edge node = Edge node node

data Node key payload = DigraphNode {
      node_payload :: payload,
      node_key :: key,
      node_dependencies :: [key] }
     -- The payload is user data, just carried around in this module
     -- The keys are ordered
     -- The [key] are the dependencies of the node;
     --    it's ok to have extra keys in the dependencies that
     --    are not the key of any Node in the graph

instance (Outputable a, Outputable b) => Outputable (Node  a b) where
  ppr (DigraphNode a b c) = ppr (a, b, c)

emptyGraph :: Graph a
emptyGraph = Graph (array (1, 0) []) (error "emptyGraph") (const Nothing)

-- See Note [Deterministic SCC]
graphFromEdgedVertices
        :: ReduceFn key payload
        -> [Node key payload]           -- The graph; its ok for the
                                        -- out-list to contain keys which aren't
                                        -- a vertex key, they are ignored
        -> Graph (Node key payload)
graphFromEdgedVertices _reduceFn []            = emptyGraph
graphFromEdgedVertices reduceFn edged_vertices =
  Graph graph vertex_fn (key_vertex . key_extractor)
  where key_extractor = node_key
        (bounds, vertex_fn, key_vertex, numbered_nodes) =
          reduceFn edged_vertices key_extractor
        graph = array bounds [ (v, sort $ mapMaybe key_vertex ks)
                             | (v, (node_dependencies -> ks)) <- numbered_nodes]
                -- We normalize outgoing edges by sorting on node order, so
                -- that the result doesn't depend on the order of the edges

-- See Note [Deterministic SCC]
-- See Note [reduceNodesIntoVertices implementations]
graphFromEdgedVerticesOrd
        :: Ord key
        => [Node key payload]           -- The graph; its ok for the
                                        -- out-list to contain keys which aren't
                                        -- a vertex key, they are ignored
        -> Graph (Node key payload)
graphFromEdgedVerticesOrd = graphFromEdgedVertices reduceNodesIntoVerticesOrd

-- See Note [Deterministic SCC]
-- See Note [reduceNodesIntoVertices implementations]
graphFromEdgedVerticesUniq
        :: Uniquable key
        => [Node key payload]           -- The graph; its ok for the
                                        -- out-list to contain keys which aren't
                                        -- a vertex key, they are ignored
        -> Graph (Node key payload)
graphFromEdgedVerticesUniq = graphFromEdgedVertices reduceNodesIntoVerticesUniq

type ReduceFn key payload =
  [Node key payload] -> (Node key payload -> key) ->
    (Bounds, Vertex -> Node key payload
    , key -> Maybe Vertex, [(Vertex, Node key payload)])

{-
Note [reduceNodesIntoVertices implementations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reduceNodesIntoVertices is parameterized by the container type.
This is to accomodate key types that don't have an Ord instance
and hence preclude the use of Data.Map. An example of such type
would be Unique, there's no way to implement Ord Unique
deterministically.

For such types, there's a version with a Uniquable constraint.
This leaves us with two versions of every function that depends on
reduceNodesIntoVertices, one with Ord constraint and the other with
Uniquable constraint.
For example: graphFromEdgedVerticesOrd and graphFromEdgedVerticesUniq.

The Uniq version should be a tiny bit more efficient since it uses
Data.IntMap internally.
-}
reduceNodesIntoVertices
  :: ([(key, Vertex)] -> m)
  -> (key -> m -> Maybe Vertex)
  -> ReduceFn key payload
reduceNodesIntoVertices fromList lookup nodes key_extractor =
  (bounds, (!) vertex_map, key_vertex, numbered_nodes)
  where
    max_v           = length nodes - 1
    bounds          = (0, max_v) :: (Vertex, Vertex)

    -- Keep the order intact to make the result depend on input order
    -- instead of key order
    numbered_nodes  = zip [0..] nodes
    vertex_map      = array bounds numbered_nodes

    key_map = fromList
      [ (key_extractor node, v) | (v, node) <- numbered_nodes ]
    key_vertex k = lookup k key_map

-- See Note [reduceNodesIntoVertices implementations]
reduceNodesIntoVerticesOrd :: Ord key => ReduceFn key payload
reduceNodesIntoVerticesOrd = reduceNodesIntoVertices Map.fromList Map.lookup

-- See Note [reduceNodesIntoVertices implementations]
reduceNodesIntoVerticesUniq :: Uniquable key => ReduceFn key payload
reduceNodesIntoVerticesUniq = reduceNodesIntoVertices listToUFM (flip lookupUFM)

{-
************************************************************************
*                                                                      *
*      SCC
*                                                                      *
************************************************************************
-}

type WorkItem key payload
  = (Node key payload,  -- Tip of the path
     [payload])         -- Rest of the path;
                        --  [a,b,c] means c depends on b, b depends on a

-- | Find a reasonably short cycle a->b->c->a, in a strongly
-- connected component.  The input nodes are presumed to be
-- a SCC, so you can start anywhere.
findCycle :: forall payload key. Ord key
          => [Node key payload]     -- The nodes.  The dependencies can
                                    -- contain extra keys, which are ignored
          -> Maybe [payload]        -- A cycle, starting with node
                                    -- so each depends on the next
findCycle graph
  = go Set.empty (new_work root_deps []) []
  where
    env :: Map.Map key (Node key payload)
    env = Map.fromList [ (node_key node, node) | node <- graph ]

    -- Find the node with fewest dependencies among the SCC modules
    -- This is just a heuristic to find some plausible root module
    root :: Node key payload
    root = fst (minWith snd [ (node, count (`Map.member` env)
                                           (node_dependencies node))
                            | node <- graph ])
    DigraphNode root_payload root_key root_deps = root


    -- 'go' implements Dijkstra's algorithm, more or less
    go :: Set.Set key   -- Visited
       -> [WorkItem key payload]        -- Work list, items length n
       -> [WorkItem key payload]        -- Work list, items length n+1
       -> Maybe [payload]               -- Returned cycle
       -- Invariant: in a call (go visited ps qs),
       --            visited = union (map tail (ps ++ qs))

    go _       [] [] = Nothing  -- No cycles
    go visited [] qs = go visited qs []
    go visited (((DigraphNode payload key deps), path) : ps) qs
       | key == root_key           = Just (root_payload : reverse path)
       | key `Set.member` visited  = go visited ps qs
       | key `Map.notMember` env   = go visited ps qs
       | otherwise                 = go (Set.insert key visited)
                                        ps (new_qs ++ qs)
       where
         new_qs = new_work deps (payload : path)

    new_work :: [key] -> [payload] -> [WorkItem key payload]
    new_work deps path = [ (n, path) | Just n <- map (`Map.lookup` env) deps ]

{-
************************************************************************
*                                                                      *
*      Strongly Connected Component wrappers for Graph
*                                                                      *
************************************************************************

Note: the components are returned topologically sorted: later components
depend on earlier ones, but not vice versa i.e. later components only have
edges going from them to earlier ones.
-}

{-
Note [Deterministic SCC]
~~~~~~~~~~~~~~~~~~~~~~~~
stronglyConnCompFromEdgedVerticesUniq,
stronglyConnCompFromEdgedVerticesUniqR,
stronglyConnCompFromEdgedVerticesOrd and
stronglyConnCompFromEdgedVerticesOrdR
provide a following guarantee:
Given a deterministically ordered list of nodes it returns a deterministically
ordered list of strongly connected components, where the list of vertices
in an SCC is also deterministically ordered.
Note that the order of edges doesn't need to be deterministic for this to work.
We use the order of nodes to normalize the order of edges.
-}

stronglyConnCompG :: Graph node -> [SCC node]
stronglyConnCompG graph = decodeSccs graph forest
  where forest = {-# SCC "Digraph.scc" #-} scc (gr_int_graph graph)

decodeSccs :: Graph node -> Forest Vertex -> [SCC node]
decodeSccs Graph { gr_int_graph = graph, gr_vertex_to_node = vertex_fn } forest
  = map decode forest
  where
    decode (Node v []) | mentions_itself v = CyclicSCC [vertex_fn v]
                       | otherwise         = AcyclicSCC (vertex_fn v)
    decode other = CyclicSCC (dec other [])
      where dec (Node v ts) vs = vertex_fn v : foldr dec vs ts
    mentions_itself v = v `elem` (graph ! v)


-- The following two versions are provided for backwards compatibility:
-- See Note [Deterministic SCC]
-- See Note [reduceNodesIntoVertices implementations]
stronglyConnCompFromEdgedVerticesOrd
        :: Ord key
        => [Node key payload]
        -> [SCC payload]
stronglyConnCompFromEdgedVerticesOrd
  = map (fmap node_payload) . stronglyConnCompFromEdgedVerticesOrdR

-- The following two versions are provided for backwards compatibility:
-- See Note [Deterministic SCC]
-- See Note [reduceNodesIntoVertices implementations]
stronglyConnCompFromEdgedVerticesUniq
        :: Uniquable key
        => [Node key payload]
        -> [SCC payload]
stronglyConnCompFromEdgedVerticesUniq
  = map (fmap node_payload) . stronglyConnCompFromEdgedVerticesUniqR

-- The "R" interface is used when you expect to apply SCC to
-- (some of) the result of SCC, so you don't want to lose the dependency info
-- See Note [Deterministic SCC]
-- See Note [reduceNodesIntoVertices implementations]
stronglyConnCompFromEdgedVerticesOrdR
        :: Ord key
        => [Node key payload]
        -> [SCC (Node key payload)]
stronglyConnCompFromEdgedVerticesOrdR =
  stronglyConnCompG . graphFromEdgedVertices reduceNodesIntoVerticesOrd

-- The "R" interface is used when you expect to apply SCC to
-- (some of) the result of SCC, so you don't want to lose the dependency info
-- See Note [Deterministic SCC]
-- See Note [reduceNodesIntoVertices implementations]
stronglyConnCompFromEdgedVerticesUniqR
        :: Uniquable key
        => [Node key payload]
        -> [SCC (Node key payload)]
stronglyConnCompFromEdgedVerticesUniqR =
  stronglyConnCompG . graphFromEdgedVertices reduceNodesIntoVerticesUniq

{-
************************************************************************
*                                                                      *
*      Misc wrappers for Graph
*                                                                      *
************************************************************************
-}

topologicalSortG :: Graph node -> [node]
topologicalSortG graph = map (gr_vertex_to_node graph) result
  where result = {-# SCC "Digraph.topSort" #-} topSort (gr_int_graph graph)

dfsTopSortG :: Graph node -> [[node]]
dfsTopSortG graph =
  map (map (gr_vertex_to_node graph) . flatten) $ dfs g (topSort g)
  where
    g = gr_int_graph graph

reachableG :: Graph node -> node -> [node]
reachableG graph from = map (gr_vertex_to_node graph) result
  where from_vertex = expectJust "reachableG" (gr_node_to_vertex graph from)
        result = {-# SCC "Digraph.reachable" #-} reachable (gr_int_graph graph) [from_vertex]

reachablesG :: Graph node -> [node] -> [node]
reachablesG graph froms = map (gr_vertex_to_node graph) result
  where result = {-# SCC "Digraph.reachable" #-}
                 reachable (gr_int_graph graph) vs
        vs = [ v | Just v <- map (gr_node_to_vertex graph) froms ]

hasVertexG :: Graph node -> node -> Bool
hasVertexG graph node = isJust $ gr_node_to_vertex graph node

verticesG :: Graph node -> [node]
verticesG graph = map (gr_vertex_to_node graph) $ vertices (gr_int_graph graph)

edgesG :: Graph node -> [Edge node]
edgesG graph = map (\(v1, v2) -> Edge (v2n v1) (v2n v2)) $ edges (gr_int_graph graph)
  where v2n = gr_vertex_to_node graph

transposeG :: Graph node -> Graph node
transposeG graph = Graph (G.transposeG (gr_int_graph graph))
                         (gr_vertex_to_node graph)
                         (gr_node_to_vertex graph)

outdegreeG :: Graph node -> node -> Maybe Int
outdegreeG = degreeG outdegree

indegreeG :: Graph node -> node -> Maybe Int
indegreeG = degreeG indegree

degreeG :: (G.Graph -> Table Int) -> Graph node -> node -> Maybe Int
degreeG degree graph node = let table = degree (gr_int_graph graph)
                            in fmap ((!) table) $ gr_node_to_vertex graph node

vertexGroupsG :: Graph node -> [[node]]
vertexGroupsG graph = map (map (gr_vertex_to_node graph)) result
  where result = vertexGroups (gr_int_graph graph)

emptyG :: Graph node -> Bool
emptyG g = graphEmpty (gr_int_graph g)

componentsG :: Graph node -> [[node]]
componentsG graph = map (map (gr_vertex_to_node graph) . flatten)
                  $ components (gr_int_graph graph)

{-
************************************************************************
*                                                                      *
*      Showing Graphs
*                                                                      *
************************************************************************
-}

instance Outputable node => Outputable (Graph node) where
    ppr graph = vcat [
                  hang (text "Vertices:") 2 (vcat (map ppr $ verticesG graph)),
                  hang (text "Edges:") 2 (vcat (map ppr $ edgesG graph))
                ]

instance Outputable node => Outputable (Edge node) where
    ppr (Edge from to) = ppr from <+> text "->" <+> ppr to

graphEmpty :: G.Graph -> Bool
graphEmpty g = lo > hi
  where (lo, hi) = bounds g

{-
************************************************************************
*                                                                      *
*      IntGraphs
*                                                                      *
************************************************************************
-}

type IntGraph = G.Graph

{-
------------------------------------------------------------
-- Depth first search numbering
------------------------------------------------------------
-}

-- Data.Tree has flatten for Tree, but nothing for Forest
preorderF           :: Forest a -> [a]
preorderF ts         = concat (map flatten ts)

{-
------------------------------------------------------------
-- Finding reachable vertices
------------------------------------------------------------
-}

-- This generalizes reachable which was found in Data.Graph
reachable    :: IntGraph -> [Vertex] -> [Vertex]
reachable g vs = preorderF (dfs g vs)

{-
------------------------------------------------------------
-- Total ordering on groups of vertices
------------------------------------------------------------

The plan here is to extract a list of groups of elements of the graph
such that each group has no dependence except on nodes in previous
groups (i.e. in particular they may not depend on nodes in their own
group) and is maximal such group.

Clearly we cannot provide a solution for cyclic graphs.

We proceed by iteratively removing elements with no outgoing edges
and their associated edges from the graph.

This probably isn't very efficient and certainly isn't very clever.
-}

type Set s    = STArray s Vertex Bool

mkEmpty      :: Bounds -> ST s (Set s)
mkEmpty bnds  = newArray bnds False

contains     :: Set s -> Vertex -> ST s Bool
contains m v  = readArray m v

include      :: Set s -> Vertex -> ST s ()
include m v   = writeArray m v True

vertexGroups :: IntGraph -> [[Vertex]]
vertexGroups g = runST (mkEmpty (bounds g) >>= \provided -> vertexGroupsS provided g next_vertices)
  where next_vertices = noOutEdges g

noOutEdges :: IntGraph -> [Vertex]
noOutEdges g = [ v | v <- vertices g, null (g!v)]

vertexGroupsS :: Set s -> IntGraph -> [Vertex] -> ST s [[Vertex]]
vertexGroupsS provided g to_provide
  = if null to_provide
    then do {
          all_provided <- allM (provided `contains`) (vertices g)
        ; if all_provided
          then return []
          else error "vertexGroup: cyclic graph"
        }
    else do {
          mapM_ (include provided) to_provide
        ; to_provide' <- filterM (vertexReady provided g) (vertices g)
        ; rest <- vertexGroupsS provided g to_provide'
        ; return $ to_provide : rest
        }

vertexReady :: Set s -> IntGraph -> Vertex -> ST s Bool
vertexReady provided g v = liftM2 (&&) (liftM not $ provided `contains` v) (allM (provided `contains`) (g!v))
