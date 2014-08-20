%
% (c) The University of Glasgow 2006
%

\begin{code}
{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Digraph(
        Graph, graphFromVerticesAndAdjacency, graphFromEdgedVertices,

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

        -- No friendly interface yet, not used but exported to avoid warnings
        tabulate, preArr,
        components, undirected,
        back, cross, forward,
        path,
        bcc, do_label, bicomps, collect
    ) where

#include "HsVersions.h"

------------------------------------------------------------------------------
-- A version of the graph algorithms described in:
--
-- ``Lazy Depth-First Search and Linear IntGraph Algorithms in Haskell''
--   by David King and John Launchbury
--
-- Also included is some additional code for printing tree structures ...
------------------------------------------------------------------------------


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
\end{code}

%************************************************************************
%*                                                                      *
%*      Graphs and Graph Construction
%*                                                                      *
%************************************************************************

Note [Nodes, keys, vertices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * A 'node' is a big blob of client-stuff

 * Each 'node' has a unique (client) 'key', but the latter
        is in Ord and has fast comparison

 * Digraph then maps each 'key' to a Vertex (Int) which is
        arranged densely in 0.n

\begin{code}
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
\end{code}

%************************************************************************
%*                                                                      *
%*      SCC
%*                                                                      *
%************************************************************************

\begin{code}
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
\end{code}

%************************************************************************
%*                                                                      *
%*      SCC
%*                                                                      *
%************************************************************************

\begin{code}
data SCC vertex = AcyclicSCC vertex
                | CyclicSCC  [vertex]

instance Functor SCC where
    fmap f (AcyclicSCC v) = AcyclicSCC (f v)
    fmap f (CyclicSCC vs) = CyclicSCC (fmap f vs)

flattenSCCs :: [SCC a] -> [a]
flattenSCCs = concatMap flattenSCC

flattenSCC :: SCC a -> [a]
flattenSCC (AcyclicSCC v) = [v]
flattenSCC (CyclicSCC vs) = vs

instance Outputable a => Outputable (SCC a) where
   ppr (AcyclicSCC v) = text "NONREC" $$ (nest 3 (ppr v))
   ppr (CyclicSCC vs) = text "REC" $$ (nest 3 (vcat (map ppr vs)))
\end{code}

%************************************************************************
%*                                                                      *
%*      Strongly Connected Component wrappers for Graph
%*                                                                      *
%************************************************************************

Note: the components are returned topologically sorted: later components
depend on earlier ones, but not vice versa i.e. later components only have
edges going from them to earlier ones.

\begin{code}
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
\end{code}

%************************************************************************
%*                                                                      *
%*      Misc wrappers for Graph
%*                                                                      *
%************************************************************************

\begin{code}
topologicalSortG :: Graph node -> [node]
topologicalSortG graph = map (gr_vertex_to_node graph) result
  where result = {-# SCC "Digraph.topSort" #-} topSort (gr_int_graph graph)

dfsTopSortG :: Graph node -> [[node]]
dfsTopSortG graph =
  map (map (gr_vertex_to_node graph) . flattenTree) $ dfs g (topSort g)
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
transposeG graph = Graph (transpose (gr_int_graph graph)) (gr_vertex_to_node graph) (gr_node_to_vertex graph)

outdegreeG :: Graph node -> node -> Maybe Int
outdegreeG = degreeG outdegree

indegreeG :: Graph node -> node -> Maybe Int
indegreeG = degreeG indegree

degreeG :: (IntGraph -> Table Int) -> Graph node -> node -> Maybe Int
degreeG degree graph node = let table = degree (gr_int_graph graph)
                            in fmap ((!) table) $ gr_node_to_vertex graph node

vertexGroupsG :: Graph node -> [[node]]
vertexGroupsG graph = map (map (gr_vertex_to_node graph)) result
  where result = vertexGroups (gr_int_graph graph)

emptyG :: Graph node -> Bool
emptyG g = graphEmpty (gr_int_graph g)

componentsG :: Graph node -> [[node]]
componentsG graph = map (map (gr_vertex_to_node graph) . flattenTree) $ components (gr_int_graph graph)
\end{code}

%************************************************************************
%*                                                                      *
%*      Showing Graphs
%*                                                                      *
%************************************************************************

\begin{code}

instance Outputable node => Outputable (Graph node) where
    ppr graph = vcat [
                  hang (text "Vertices:") 2 (vcat (map ppr $ verticesG graph)),
                  hang (text "Edges:") 2 (vcat (map ppr $ edgesG graph))
                ]

instance Outputable node => Outputable (Edge node) where
    ppr (Edge from to) = ppr from <+> text "->" <+> ppr to

\end{code}

%************************************************************************
%*                                                                      *
%*      IntGraphs
%*                                                                      *
%************************************************************************

\begin{code}
type Vertex  = Int
type Table a = Array Vertex a
type IntGraph   = Table [Vertex]
type Bounds  = (Vertex, Vertex)
type IntEdge    = (Vertex, Vertex)
\end{code}

\begin{code}
vertices :: IntGraph -> [Vertex]
vertices  = indices

edges    :: IntGraph -> [IntEdge]
edges g   = [ (v, w) | v <- vertices g, w <- g!v ]

mapT    :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [ (v, f v (t ! v)) | v <- indices t ]

buildG :: Bounds -> [IntEdge] -> IntGraph
buildG bounds edges = accumArray (flip (:)) [] bounds edges

transpose  :: IntGraph -> IntGraph
transpose g = buildG (bounds g) (reverseE g)

reverseE    :: IntGraph -> [IntEdge]
reverseE g   = [ (w, v) | (v, w) <- edges g ]

outdegree :: IntGraph -> Table Int
outdegree  = mapT numEdges
             where numEdges _ ws = length ws

indegree :: IntGraph -> Table Int
indegree  = outdegree . transpose

graphEmpty :: IntGraph -> Bool
graphEmpty g = lo > hi
  where (lo, hi) = bounds g

\end{code}

%************************************************************************
%*                                                                      *
%*      Trees and forests
%*                                                                      *
%************************************************************************

\begin{code}
data Tree a   = Node a (Forest a)
type Forest a = [Tree a]

mapTree              :: (a -> b) -> (Tree a -> Tree b)
mapTree f (Node x ts) = Node (f x) (map (mapTree f) ts)

flattenTree :: Tree a -> [a]
flattenTree (Node x ts) = x : concatMap flattenTree ts
\end{code}

\begin{code}
instance Show a => Show (Tree a) where
  showsPrec _ t s = showTree t ++ s

showTree :: Show a => Tree a -> String
showTree  = drawTree . mapTree show

drawTree        :: Tree String -> String
drawTree         = unlines . draw

draw :: Tree String -> [String]
draw (Node x ts) = grp this (space (length this)) (stLoop ts)
 where this          = s1 ++ x ++ " "

       space n       = replicate n ' '

       stLoop []     = [""]
       stLoop [t]    = grp s2 "  " (draw t)
       stLoop (t:ts) = grp s3 s4 (draw t) ++ [s4] ++ rsLoop ts

       rsLoop []     = []
       rsLoop [t]    = grp s5 "  " (draw t)
       rsLoop (t:ts) = grp s6 s4 (draw t) ++ [s4] ++ rsLoop ts

       grp fst rst   = zipWith (++) (fst:repeat rst)

       [s1,s2,s3,s4,s5,s6] = ["- ", "--", "-+", " |", " `", " +"]
\end{code}


%************************************************************************
%*                                                                      *
%*      Depth first search
%*                                                                      *
%************************************************************************

\begin{code}
type Set s    = STArray s Vertex Bool

mkEmpty      :: Bounds -> ST s (Set s)
mkEmpty bnds  = newArray bnds False

contains     :: Set s -> Vertex -> ST s Bool
contains m v  = readArray m v

include      :: Set s -> Vertex -> ST s ()
include m v   = writeArray m v True
\end{code}

\begin{code}
dff          :: IntGraph -> Forest Vertex
dff g         = dfs g (vertices g)

dfs          :: IntGraph -> [Vertex] -> Forest Vertex
dfs g vs      = prune (bounds g) (map (generate g) vs)

generate     :: IntGraph -> Vertex -> Tree Vertex
generate g v  = Node v (map (generate g) (g!v))

prune        :: Bounds -> Forest Vertex -> Forest Vertex
prune bnds ts = runST (mkEmpty bnds  >>= \m ->
                       chop m ts)

chop         :: Set s -> Forest Vertex -> ST s (Forest Vertex)
chop _ []     = return []
chop m (Node v ts : us)
              = contains m v >>= \visited ->
                if visited then
                  chop m us
                else
                  include m v >>= \_  ->
                  chop m ts   >>= \as ->
                  chop m us   >>= \bs ->
                  return (Node v as : bs)
\end{code}


%************************************************************************
%*                                                                      *
%*      Algorithms
%*                                                                      *
%************************************************************************

------------------------------------------------------------
-- Algorithm 1: depth first search numbering
------------------------------------------------------------

\begin{code}
preorder            :: Tree a -> [a]
preorder (Node a ts) = a : preorderF ts

preorderF           :: Forest a -> [a]
preorderF ts         = concat (map preorder ts)

tabulate        :: Bounds -> [Vertex] -> Table Int
tabulate bnds vs = array bnds (zip vs [1..])

preArr          :: Bounds -> Forest Vertex -> Table Int
preArr bnds      = tabulate bnds . preorderF
\end{code}

------------------------------------------------------------
-- Algorithm 2: topological sorting
------------------------------------------------------------

\begin{code}
postorder :: Tree a -> [a] -> [a]
postorder (Node a ts) = postorderF ts . (a :)

postorderF   :: Forest a -> [a] -> [a]
postorderF ts = foldr (.) id $ map postorder ts

postOrd :: IntGraph -> [Vertex]
postOrd g = postorderF (dff g) []

topSort :: IntGraph -> [Vertex]
topSort = reverse . postOrd
\end{code}

------------------------------------------------------------
-- Algorithm 3: connected components
------------------------------------------------------------

\begin{code}
components   :: IntGraph -> Forest Vertex
components    = dff . undirected

undirected   :: IntGraph -> IntGraph
undirected g  = buildG (bounds g) (edges g ++ reverseE g)
\end{code}

------------------------------------------------------------
-- Algorithm 4: strongly connected components
------------------------------------------------------------

\begin{code}
scc  :: IntGraph -> Forest Vertex
scc g = dfs g (reverse (postOrd (transpose g)))
\end{code}

------------------------------------------------------------
-- Algorithm 5: Classifying edges
------------------------------------------------------------

\begin{code}
back              :: IntGraph -> Table Int -> IntGraph
back g post        = mapT select g
 where select v ws = [ w | w <- ws, post!v < post!w ]

cross             :: IntGraph -> Table Int -> Table Int -> IntGraph
cross g pre post   = mapT select g
 where select v ws = [ w | w <- ws, post!v > post!w, pre!v > pre!w ]

forward           :: IntGraph -> IntGraph -> Table Int -> IntGraph
forward g tree pre = mapT select g
 where select v ws = [ w | w <- ws, pre!v < pre!w ] \\ tree!v
\end{code}

------------------------------------------------------------
-- Algorithm 6: Finding reachable vertices
------------------------------------------------------------

\begin{code}
reachable    :: IntGraph -> [Vertex] -> [Vertex]
reachable g vs = preorderF (dfs g vs)

path         :: IntGraph -> Vertex -> Vertex -> Bool
path g v w    = w `elem` (reachable g [v])
\end{code}

------------------------------------------------------------
-- Algorithm 7: Biconnected components
------------------------------------------------------------

\begin{code}
bcc :: IntGraph -> Forest [Vertex]
bcc g = (concat . map bicomps . map (do_label g dnum)) forest
 where forest = dff g
       dnum   = preArr (bounds g) forest

do_label :: IntGraph -> Table Int -> Tree Vertex -> Tree (Vertex,Int,Int)
do_label g dnum (Node v ts) = Node (v,dnum!v,lv) us
 where us = map (do_label g dnum) ts
       lv = minimum ([dnum!v] ++ [dnum!w | w <- g!v]
                     ++ [lu | Node (_,_,lu) _ <- us])

bicomps :: Tree (Vertex, Int, Int) -> Forest [Vertex]
bicomps (Node (v,_,_) ts)
      = [ Node (v:vs) us | (_,Node vs us) <- map collect ts]

collect :: Tree (Vertex, Int, Int) -> (Int, Tree [Vertex])
collect (Node (v,dv,lv) ts) = (lv, Node (v:vs) cs)
 where collected = map collect ts
       vs = concat [ ws | (lw, Node ws _)  <- collected, lw<dv]
       cs = concat [ if lw<dv then us else [Node (v:ws) us]
                        | (lw, Node ws us) <- collected ]
\end{code}

------------------------------------------------------------
-- Algorithm 8: Total ordering on groups of vertices
------------------------------------------------------------

The plan here is to extract a list of groups of elements of the graph
such that each group has no dependence except on nodes in previous
groups (i.e. in particular they may not depend on nodes in their own
group) and is maximal such group.

Clearly we cannot provide a solution for cyclic graphs.

We proceed by iteratively removing elements with no outgoing edges
and their associated edges from the graph.

This probably isn't very efficient and certainly isn't very clever.

\begin{code}

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
\end{code}
