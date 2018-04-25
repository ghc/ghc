-- | Wrapper around Data.Graph with support for edge labels
{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Solver.Modular.LabeledGraph (
    -- * Graphs
    Graph
  , Vertex
    -- ** Building graphs
  , graphFromEdges
  , graphFromEdges'
  , buildG
  , transposeG
    -- ** Graph properties
  , vertices
  , edges
    -- ** Operations on the underlying unlabeled graph
  , forgetLabels
  , topSort
  ) where

import Data.Array
import Data.Graph (Vertex, Bounds)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Graph as G

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

type Graph e = Array Vertex [(e, Vertex)]
type Edge  e = (Vertex, e, Vertex)

{-------------------------------------------------------------------------------
  Building graphs
-------------------------------------------------------------------------------}

-- | Construct an edge-labeled graph
--
-- This is a simple adaptation of the definition in Data.Graph
graphFromEdges :: forall key node edge. Ord key
               => [ (node, key, [(edge, key)]) ]
               -> ( Graph edge
                  , Vertex -> (node, key, [(edge, key)])
                  , key -> Maybe Vertex
                  )
graphFromEdges edges0 =
    (graph, \v -> vertex_map ! v, key_vertex)
  where
    max_v        = length edges0 - 1
    bounds0      = (0, max_v) :: (Vertex, Vertex)
    sorted_edges = sortBy lt edges0
    edges1       = zip [0..] sorted_edges

    graph        = array bounds0 [(v, (mapMaybe mk_edge ks))
                                 | (v, (_, _, ks)) <- edges1]
    key_map      = array bounds0 [(v, k                    )
                                 | (v, (_, k, _ )) <- edges1]
    vertex_map   = array bounds0 edges1

    (_,k1,_) `lt` (_,k2,_) = k1 `compare` k2

    mk_edge :: (edge, key) -> Maybe (edge, Vertex)
    mk_edge (edge, key) = do v <- key_vertex key ; return (edge, v)

    --  returns Nothing for non-interesting vertices
    key_vertex :: key -> Maybe Vertex
    key_vertex k = findVertex 0 max_v
      where
        findVertex a b
          | a > b     = Nothing
          | otherwise = case compare k (key_map ! mid) of
              LT -> findVertex a (mid-1)
              EQ -> Just mid
              GT -> findVertex (mid+1) b
          where
            mid = a + (b - a) `div` 2

graphFromEdges' :: Ord key
                => [ (node, key, [(edge, key)]) ]
                -> ( Graph edge
                   , Vertex -> (node, key, [(edge, key)])
                   )
graphFromEdges' x = (a,b)
  where
    (a,b,_) = graphFromEdges x

transposeG :: Graph e -> Graph e
transposeG g = buildG (bounds g) (reverseE g)

buildG :: Bounds -> [Edge e] -> Graph e
buildG bounds0 edges0 = accumArray (flip (:)) [] bounds0 (map reassoc edges0)
  where
    reassoc (v, e, w) = (v, (e, w))

reverseE :: Graph e -> [Edge e]
reverseE g = [ (w, e, v) | (v, e, w) <- edges g ]

{-------------------------------------------------------------------------------
  Graph properties
-------------------------------------------------------------------------------}

vertices :: Graph e -> [Vertex]
vertices = indices

edges :: Graph e -> [Edge e]
edges g = [ (v, e, w) | v <- vertices g, (e, w) <- g!v ]

{-------------------------------------------------------------------------------
  Operations on the underlying unlabelled graph
-------------------------------------------------------------------------------}

forgetLabels :: Graph e -> G.Graph
forgetLabels = fmap (map snd)

topSort :: Graph e -> [Vertex]
topSort = G.topSort . forgetLabels
