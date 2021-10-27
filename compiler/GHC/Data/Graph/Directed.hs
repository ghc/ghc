-- (c) The University of Glasgow 2006


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.Data.Graph.Directed (
        Graph, graphFromEdgedVerticesOrd, graphFromEdgedVerticesUniq,

        SCC(..), Node(..), flattenSCC, flattenSCCs,
        stronglyConnCompG,
        topologicalSortG,
        verticesG, edgesG, hasVertexG,
        reachableG, reachablesG, transposeG, allReachable, outgoingG,
        emptyG,

        findCycle,

        -- For backwards compatibility with the simpler version of Digraph
        stronglyConnCompFromEdgedVerticesOrd,
        stronglyConnCompFromEdgedVerticesOrdR,
        stronglyConnCompFromEdgedVerticesUniq,
        stronglyConnCompFromEdgedVerticesUniqR,

        -- Simple way to classify edges
        EdgeType(..), classifyEdges
        ) where

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


import GHC.Prelude

import GHC.Utils.Misc ( minWith, count )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.Maybe ( expectJust )

-- std interfaces
import Data.Maybe
import Data.Array
import Data.List ( sort )
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Graph as G
import Data.Graph hiding (Graph, Edge, transposeG, reachable)
import Data.Tree
import GHC.Types.Unique
import GHC.Types.Unique.FM
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S

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

{-| Representation for nodes of the Graph.

 * The @payload@ is user data, just carried around in this module

 * The @key@ is the node identifier.
   Key has an Ord instance for performance reasons.

 * The @[key]@ are the dependencies of the node;
   it's ok to have extra keys in the dependencies that
   are not the key of any Node in the graph
-}
data Node key payload = DigraphNode {
      node_payload :: payload, -- ^ User data
      node_key :: key, -- ^ User defined node id
      node_dependencies :: [key] -- ^ Dependencies/successors of the node
  }


instance (Outputable a, Outputable b) => Outputable (Node a b) where
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
This is to accommodate key types that don't have an Ord instance
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

reachableG :: Graph node -> node -> [node]
reachableG graph from = map (gr_vertex_to_node graph) result
  where from_vertex = expectJust "reachableG" (gr_node_to_vertex graph from)
        result = {-# SCC "Digraph.reachable" #-} reachable (gr_int_graph graph) [from_vertex]

outgoingG :: Graph node -> node -> [node]
outgoingG graph from = map (gr_vertex_to_node graph) result
  where from_vertex = expectJust "reachableG" (gr_node_to_vertex graph from)
        result = gr_int_graph graph ! from_vertex

-- | Given a list of roots return all reachable nodes.
reachablesG :: Graph node -> [node] -> [node]
reachablesG graph froms = map (gr_vertex_to_node graph) result
  where result = {-# SCC "Digraph.reachable" #-}
                 reachable (gr_int_graph graph) vs
        vs = [ v | Just v <- map (gr_node_to_vertex graph) froms ]

-- | Efficiently construct a map which maps each key to it's set of transitive
-- dependencies.
allReachable :: Ord key => Graph node -> (node -> key) -> M.Map key (S.Set key)
allReachable (Graph g from _) conv =
  M.fromList [(conv (from v), IS.foldr (\k vs -> conv (from k) `S.insert` vs) S.empty vs)
             | (v, vs) <- IM.toList int_graph]
  where
    int_graph = reachableGraph g

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

emptyG :: Graph node -> Bool
emptyG g = graphEmpty (gr_int_graph g)

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
preorderF ts         = concatMap flatten ts

{-
------------------------------------------------------------
-- Finding reachable vertices
------------------------------------------------------------
-}

-- This generalizes reachable which was found in Data.Graph
reachable    :: IntGraph -> [Vertex] -> [Vertex]
reachable g vs = preorderF (dfs g vs)

reachableGraph :: IntGraph -> IM.IntMap IS.IntSet
reachableGraph g = res
  where
    do_one v = IS.unions (IS.fromList (g ! v) : mapMaybe (flip IM.lookup res) (g ! v))
    res = IM.fromList [(v, do_one v) | v <- vertices g]

{-
************************************************************************
*                                                                      *
*                         Classify Edge Types
*                                                                      *
************************************************************************
-}

-- Remark: While we could generalize this algorithm this comes at a runtime
-- cost and with no advantages. If you find yourself using this with graphs
-- not easily represented using Int nodes please consider rewriting this
-- using the more general Graph type.

-- | Edge direction based on DFS Classification
data EdgeType
  = Forward
  | Cross
  | Backward -- ^ Loop back towards the root node.
             -- Eg backjumps in loops
  | SelfLoop -- ^ v -> v
   deriving (Eq,Ord)

instance Outputable EdgeType where
  ppr Forward = text "Forward"
  ppr Cross = text "Cross"
  ppr Backward = text "Backward"
  ppr SelfLoop = text "SelfLoop"

newtype Time = Time Int deriving (Eq,Ord,Num,Outputable)

--Allow for specialization
{-# INLINEABLE classifyEdges #-}

-- | Given a start vertex, a way to get successors from a node
-- and a list of (directed) edges classify the types of edges.
classifyEdges :: forall key. Uniquable key => key -> (key -> [key])
              -> [(key,key)] -> [((key, key), EdgeType)]
classifyEdges root getSucc edges =
    --let uqe (from,to) = (getUnique from, getUnique to)
    --in pprTrace "Edges:" (ppr $ map uqe edges) $
    zip edges $ map classify edges
  where
    (_time, starts, ends) = addTimes (0,emptyUFM,emptyUFM) root
    classify :: (key,key) -> EdgeType
    classify (from,to)
      | startFrom < startTo
      , endFrom   > endTo
      = Forward
      | startFrom > startTo
      , endFrom   < endTo
      = Backward
      | startFrom > startTo
      , endFrom   > endTo
      = Cross
      | getUnique from == getUnique to
      = SelfLoop
      | otherwise
      = pprPanic "Failed to classify edge of Graph"
                 (ppr (getUnique from, getUnique to))

      where
        getTime event node
          | Just time <- lookupUFM event node
          = time
          | otherwise
          = pprPanic "Failed to classify edge of CFG - not not timed"
            (text "edges" <> ppr (getUnique from, getUnique to)
                          <+> ppr starts <+> ppr ends )
        startFrom = getTime starts from
        startTo   = getTime starts to
        endFrom   = getTime ends   from
        endTo     = getTime ends   to

    addTimes :: (Time, UniqFM key Time, UniqFM key Time) -> key
             -> (Time, UniqFM key Time, UniqFM key Time)
    addTimes (time,starts,ends) n
      --Dont reenter nodes
      | elemUFM n starts
      = (time,starts,ends)
      | otherwise =
        let
          starts' = addToUFM starts n time
          time' = time + 1
          succs = getSucc n :: [key]
          (time'',starts'',ends') = foldl' addTimes (time',starts',ends) succs
          ends'' = addToUFM ends' n time''
        in
        (time'' + 1, starts'', ends'')
