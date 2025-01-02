module GHC.Data.Graph.Directed.Internal where

import GHC.Prelude
import GHC.Utils.Outputable

import Data.Array
import qualified Data.Graph as G
import Data.Graph ( Vertex, SCC(..) ) -- Used in the underlying representation
import Data.Tree
import Data.Array.ST.Safe (STUArray)
import Control.Monad.ST
import Data.Array.ST.Safe (newArray, readArray, writeArray)
import Data.List (sort)

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
    forest = {-# SCC "Digraph.scc" #-} scc2 graph

    decode (Node v []) | mentions_itself v = CyclicSCC [v]
                       | otherwise         = AcyclicSCC v
    decode other = CyclicSCC (dec other [])
      where dec (Node v ts) vs = v : foldr dec vs ts
    mentions_itself v = v `elem` (graph ! v)



newtype SetM s a = SetM { runSetM :: STUArray s Vertex Bool -> ST s a }

instance Monad (SetM s) where
    return = pure
    {-# INLINE return #-}
    SetM v >>= f = SetM $ \s -> do { x <- v s; runSetM (f x) s }
    {-# INLINE (>>=) #-}

instance Functor (SetM s) where
    f `fmap` SetM v = SetM $ \s -> f `fmap` v s
    {-# INLINE fmap #-}

instance Applicative (SetM s) where
    pure x = SetM $ const (return x)
    {-# INLINE pure #-}
    SetM f <*> SetM v = SetM $ \s -> f s >>= (`fmap` v s)
    -- We could also use the following definition
    --   SetM f <*> SetM v = SetM $ \s -> f s <*> v s
    -- but Applicative (ST s) instance is present only in GHC 7.2+
    {-# INLINE (<*>) #-}

run          :: G.Bounds -> (forall s. SetM s a) -> a
run bnds act  = runST (newArray bnds False >>= runSetM act)

contains     :: Vertex -> SetM s Bool
contains v    = SetM $ \ m -> readArray m v

include      :: Vertex -> SetM s ()
include v     = SetM $ \ m -> writeArray m v True

scc2 :: G.Graph -> [Tree Vertex]
scc2 g = dfs g (reverse (postOrd (G.transposeG g)))

postorder :: Tree a -> [a] -> [a]
postorder (Node a ts) = postorderF ts . (a :)

postorderF   :: [Tree a] -> [a] -> [a]
postorderF ts = foldr (.) id $ map postorder ts

postOrd :: G.Graph -> [Vertex]
postOrd g = postorderF (dff g) []

dff          :: G.Graph -> [Tree Vertex]
dff g         = dfs g (G.vertices g)


-- This dfs provides stability under transposition.
dfs :: G.Graph -> [Vertex] -> [Tree Vertex]
dfs g vs0 = run (bounds g) $ go vs0
  where
    go :: [Vertex] -> SetM s [Tree Vertex]
    go [] = pure []
    go (k: is')  = do
      visited <- contains k
      if visited
        then go is'
        else do
          include k
          as <- go (sort (g!k))
          bs <- go is'
          pure $ Node k as : bs


