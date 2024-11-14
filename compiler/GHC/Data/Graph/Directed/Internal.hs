module GHC.Data.Graph.Directed.Internal where

import GHC.Prelude
import GHC.Utils.Outputable

import Data.Array
import qualified Data.Graph as G
import Data.Graph ( Vertex, SCC(..) ) -- Used in the underlying representation
import Data.Tree

data Graph node = Graph {
    gr_int_graph      :: IntGraph,
    gr_vertex_to_node :: Vertex -> node,
    gr_node_to_vertex :: node -> Maybe Vertex
}

data Edge node = Edge node node

------------------------------------------------------------
-- Nodes and Edges
------------------------------------------------------------

verticesG :: Graph node -> [node]
verticesG graph = map (gr_vertex_to_node graph) $ G.vertices (gr_int_graph graph)

edgesG :: Graph node -> [Edge node]
edgesG graph = map (\(v1, v2) -> Edge (v2n v1) (v2n v2)) $ G.edges (gr_int_graph graph)
  where v2n = gr_vertex_to_node graph

------------------------------------------------------------
-- Showing Graphs
------------------------------------------------------------

instance Outputable node => Outputable (Graph node) where
    ppr graph = vcat [
                  hang (text "Vertices:") 2 (vcat (map ppr $ verticesG graph)),
                  hang (text "Edges:") 2 (vcat (map ppr $ edgesG graph))
                ]

instance Outputable node => Outputable (Edge node) where
    ppr (Edge from to) = ppr from <+> text "->" <+> ppr to

{-
************************************************************************
*                                                                      *
*      IntGraphs
*                                                                      *
************************************************************************
-}

type IntGraph = G.Graph

------------------------------------------------------------
-- Depth first search numbering
------------------------------------------------------------

-- Data.Tree has flatten for Tree, but nothing for Forest
preorderF           :: Forest a -> [a]
preorderF ts         = concatMap flatten ts

------------------------------------------------------------
-- Finding reachable vertices
------------------------------------------------------------

-- This generalizes reachable which was found in Data.Graph
reachable    :: IntGraph -> [Vertex] -> [Vertex]
reachable g vs = preorderF (G.dfs g vs)

scc :: IntGraph -> [SCC Vertex]
scc graph = map decode forest
  where
    forest = {-# SCC "Digraph.scc" #-} G.scc graph

    decode (Node v []) | mentions_itself v = CyclicSCC [v]
                       | otherwise         = AcyclicSCC v
    decode other = CyclicSCC (dec other [])
      where dec (Node v ts) vs = v : foldr dec vs ts
    mentions_itself v = v `elem` (graph ! v)

