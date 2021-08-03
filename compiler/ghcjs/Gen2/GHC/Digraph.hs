
-- (c) The University of Glasgow 2006

{-# LANGUAGE CPP, ScopedTypeVariables #-}
-- For Functor SCC. ToDo: Remove me when 7.10 is released
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gen2.GHC.Digraph(
        Graph, graphFromEdgedVertices,
        graphFromVerticesAndAdjacency,
        SCC(..), Node, flattenSCC, flattenSCCs,
        stronglyConnCompG,
        topologicalSortG, dfsTopSortG,
        verticesG, edgesG, hasVertexG,
        reachableG, reachablesG, transposeG,
        outdegreeG, indegreeG,
        vertexGroupsG, emptyG,
        componentsG,

        findCycle,

        -- For backwards compatability with the simpler version of Digraph
        stronglyConnCompFromEdgedVertices, stronglyConnCompFromEdgedVerticesR,
    ) where

-- #include "HsVersions.h"

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

import Prelude
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
import Data.Ord
import Data.Array.ST
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Graph as G
import Data.Graph hiding (Graph, Edge, transposeG, reachable)
import Data.Tree

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

type Node key payload = (payload, key, [key])
     -- The payload is user data, just carried around in this module
     -- The keys are ordered
     -- The [key] are the dependencies of the node;
     --    it's ok to have extra keys in the dependencies that
     --    are not the key of any Node in the graph

emptyGraph :: Graph a
emptyGraph = Graph (array (1, 0) []) (error "emptyGraph") (const Nothing)

graphFromEdgedVertices
        :: Ord key
        => [Node key payload]           -- The graph; its ok for the
                                        -- out-list to contain keys which arent
                                        -- a vertex key, they are ignored
        -> Graph (Node key payload)
graphFromEdgedVertices []             = emptyGraph
graphFromEdgedVertices edged_vertices = Graph graph vertex_fn (key_vertex . key_extractor)
  where key_extractor (_, k, _) = k
        (bounds, vertex_fn, key_vertex, numbered_nodes) = reduceNodesIntoVertices edged_vertices key_extractor
        graph = array bounds [(v, mapMaybe key_vertex ks) | (v, (_, _, ks)) <- numbered_nodes]

graphFromVerticesAndAdjacency
        :: Ord key
        => [(node, key)]
        -> [(key, key)]  -- First component is source vertex key,
                         -- second is target vertex key (thing depended on)
                         -- Unlike the other interface I insist they correspond to
                         -- actual vertices because the alternative hides bugs. I can't
                         -- do the same thing for the other one for backcompat reasons.
        -> Graph (node, key)
graphFromVerticesAndAdjacency []       _     = emptyGraph
graphFromVerticesAndAdjacency vertices edges = Graph graph vertex_node (key_vertex . key_extractor)
  where key_extractor = snd
        (bounds, vertex_node, key_vertex, _) = reduceNodesIntoVertices vertices key_extractor
        key_vertex_pair (a, b) = (expectJust "graphFromVerticesAndAdjacency" $ key_vertex a,
                                  expectJust "graphFromVerticesAndAdjacency" $ key_vertex b)
        reduced_edges = map key_vertex_pair edges
        graph = buildG bounds reduced_edges

reduceNodesIntoVertices
        :: Ord key
        => [node]
        -> (node -> key)
        -> (Bounds, Vertex -> node, key -> Maybe Vertex, [(Int, node)])
reduceNodesIntoVertices nodes key_extractor = (bounds, (!) vertex_map, key_vertex, numbered_nodes)
  where
    max_v           = length nodes - 1
    bounds          = (0, max_v) :: (Vertex, Vertex)

    sorted_nodes    = sortBy (comparing key_extractor) nodes
    numbered_nodes  = zipWith (,) [0..] sorted_nodes

    key_map         = array bounds [(i, key_extractor node) | (i, node) <- numbered_nodes]
    vertex_map      = array bounds numbered_nodes

    --key_vertex :: key -> Maybe Vertex
    -- returns Nothing for non-interesting vertices
    key_vertex k = find 0 max_v
      where
        find a b | a > b = Nothing
                 | otherwise = let mid = (a + b) `div` 2
                               in case compare k (key_map ! mid) of
                                    LT -> find a (mid - 1)
                                    EQ -> Just mid
                                    GT -> find (mid + 1) b

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
    env = Map.fromList [ (key, node) | node@(_, key, _) <- graph ]

    -- Find the node with fewest dependencies among the SCC modules
    -- This is just a heuristic to find some plausible root module
    root :: Node key payload
    root = fst (minWith snd [ (node, count (`Map.member` env) deps)
                            | node@(_,_,deps) <- graph ])
    (root_payload,root_key,root_deps) = root


    -- 'go' implements Dijkstra's algorithm, more or less
    go :: Set.Set key   -- Visited
       -> [WorkItem key payload]        -- Work list, items length n
       -> [WorkItem key payload]        -- Work list, items length n+1
       -> Maybe [payload]               -- Returned cycle
       -- Invariant: in a call (go visited ps qs),
       --            visited = union (map tail (ps ++ qs))

    go _       [] [] = Nothing  -- No cycles
    go visited [] qs = go visited qs []
    go visited (((payload,key,deps), path) : ps) qs
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


-- The following two versions are provided for backwards compatability:
stronglyConnCompFromEdgedVertices
        :: Ord key
        => [Node key payload]
        -> [SCC payload]
stronglyConnCompFromEdgedVertices
  = map (fmap get_node) . stronglyConnCompFromEdgedVerticesR
  where get_node (n, _, _) = n

-- The "R" interface is used when you expect to apply SCC to
-- (some of) the result of SCC, so you dont want to lose the dependency info
stronglyConnCompFromEdgedVerticesR
        :: Ord key
        => [Node key payload]
        -> [SCC (Node key payload)]
stronglyConnCompFromEdgedVerticesR = stronglyConnCompG . graphFromEdgedVertices

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

-- Functor instance was added in 7.8, in containers 0.5.3.2 release
-- ToDo: Drop me when 7.10 is released.
#if __GLASGOW_HASKELL__ < 708
instance Functor SCC where
    fmap f (AcyclicSCC v) = AcyclicSCC (f v)
    fmap f (CyclicSCC vs) = CyclicSCC (fmap f vs)
#endif

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
