{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Compat.Graph
-- Copyright   :  (c) Edward Z. Yang 2016
-- License     :  BSD3
--
-- Maintainer  :  cabal-dev@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A data type representing directed graphs, backed by "Data.Graph".
-- It is strict in the node type.
--
-- This is an alternative interface to "Data.Graph".  In this interface,
-- nodes (identified by the 'IsNode' type class) are associated with a
-- key and record the keys of their neighbors.  This interface is more
-- convenient than 'Data.Graph.Graph', which requires vertices to be
-- explicitly handled by integer indexes.
--
-- The current implementation has somewhat peculiar performance
-- characteristics.  The asymptotics of all map-like operations mirror
-- their counterparts in "Data.Map".  However, to perform a graph
-- operation, we first must build the "Data.Graph" representation, an
-- operation that takes /O(V + E log V)/.  However, this operation can
-- be amortized across all queries on that particular graph.
--
-- Some nodes may be broken, i.e., refer to neighbors which are not
-- stored in the graph.  In our graph algorithms, we transparently
-- ignore such edges; however, you can easily query for the broken
-- vertices of a graph using 'broken' (and should, e.g., to ensure that
-- a closure of a graph is well-formed.)  It's possible to take a closed
-- subset of a broken graph and get a well-formed graph.
--
-----------------------------------------------------------------------------

module Distribution.Compat.Graph (
    -- * Graph type
    Graph,
    IsNode(..),
    -- * Query
    null,
    size,
    member,
    lookup,
    -- * Construction
    empty,
    insert,
    deleteKey,
    deleteLookup,
    -- * Combine
    unionLeft,
    unionRight,
    -- * Graph algorithms
    stronglyConnComp,
    SCC(..),
    cycles,
    broken,
    neighbors,
    revNeighbors,
    closure,
    revClosure,
    topSort,
    revTopSort,
    -- * Conversions
    -- ** Maps
    toMap,
    -- ** Lists
    fromDistinctList,
    toList,
    keys,
    -- ** Sets
    keysSet,
    -- ** Graphs
    toGraph,
    -- * Node type
    Node(..),
    nodeValue,
) where

-- For bootstrapping GHC
#ifdef MIN_VERSION_containers
#if MIN_VERSION_containers(0,5,0)
#define HAVE_containers_050
#endif
#endif

import Prelude ()
import qualified Distribution.Compat.Prelude as Prelude
import Distribution.Compat.Prelude hiding (lookup, null, empty)

import Data.Graph (SCC(..))
import qualified Data.Graph as G

#ifdef HAVE_containers_050
import qualified Data.Map.Strict as Map
#else
import qualified Data.Map as Map
#endif
import qualified Data.Set as Set
import qualified Data.Array as Array
import Data.Array ((!))
import qualified Data.Tree as Tree
import Data.Either (partitionEithers)
import qualified Data.Foldable as Foldable

-- | A graph of nodes @a@.  The nodes are expected to have instance
-- of class 'IsNode'.
data Graph a
    = Graph {
        graphMap          :: !(Map (Key a) a),
        -- Lazily cached graph representation
        graphForward      :: G.Graph,
        graphAdjoint      :: G.Graph,
        graphVertexToNode :: G.Vertex -> a,
        graphKeyToVertex  :: Key a -> Maybe G.Vertex,
        graphBroken       :: [(a, [Key a])]
    }
    deriving (Typeable)

-- NB: Not a Functor! (or Traversable), because you need
-- to restrict Key a ~ Key b.  We provide our own mapping
-- functions.

-- General strategy is most operations are deferred to the
-- Map representation.

instance Show a => Show (Graph a) where
    show = show . toList

instance (IsNode a, Read a, Show (Key a)) => Read (Graph a) where
    readsPrec d s = map (\(a,r) -> (fromDistinctList a, r)) (readsPrec d s)

instance (IsNode a, Binary a, Show (Key a)) => Binary (Graph a) where
    put x = put (toList x)
    get = fmap fromDistinctList get

instance (Eq (Key a), Eq a) => Eq (Graph a) where
    g1 == g2 = graphMap g1 == graphMap g2

instance Foldable.Foldable Graph where
    fold = Foldable.fold . graphMap
    foldr f z = Foldable.foldr f z . graphMap
    foldl f z = Foldable.foldl f z . graphMap
    foldMap f = Foldable.foldMap f . graphMap
#ifdef MIN_VERSION_base
#if MIN_VERSION_base(4,6,0)
    foldl' f z = Foldable.foldl' f z . graphMap
    foldr' f z = Foldable.foldr' f z . graphMap
#endif
#if MIN_VERSION_base(4,8,0)
    length = Foldable.length . graphMap
    null   = Foldable.null   . graphMap
    toList = Foldable.toList . graphMap
    elem x = Foldable.elem x . graphMap
    maximum = Foldable.maximum . graphMap
    minimum = Foldable.minimum . graphMap
    sum     = Foldable.sum     . graphMap
    product = Foldable.product . graphMap
#endif
#endif

instance (NFData a, NFData (Key a)) => NFData (Graph a) where
    rnf Graph {
        graphMap = m,
        graphForward = gf,
        graphAdjoint = ga,
        graphVertexToNode = vtn,
        graphKeyToVertex = ktv,
        graphBroken = b
    } = gf `seq` ga `seq` vtn `seq` ktv `seq` b `seq` rnf m

-- TODO: Data instance?

-- | The 'IsNode' class is used for datatypes which represent directed
-- graph nodes.  A node of type @a@ is associated with some unique key of
-- type @'Key' a@; given a node we can determine its key ('nodeKey')
-- and the keys of its neighbors ('nodeNeighbors').
class Ord (Key a) => IsNode a where
    type Key a :: *
    nodeKey :: a -> Key a
    nodeNeighbors :: a -> [Key a]

instance (IsNode a, IsNode b, Key a ~ Key b) => IsNode (Either a b) where
    type Key (Either a b) = Key a
    nodeKey (Left x)  = nodeKey x
    nodeKey (Right x) = nodeKey x
    nodeNeighbors (Left x)  = nodeNeighbors x
    nodeNeighbors (Right x) = nodeNeighbors x

-- | A simple, trivial data type which admits an 'IsNode' instance.
data Node k a = N a k [k]
    deriving (Show, Eq)

-- | Get the value from a 'Node'.
nodeValue :: Node k a -> a
nodeValue (N a _ _) = a

instance Functor (Node k) where
    fmap f (N a k ks) = N (f a) k ks

instance Ord k => IsNode (Node k a) where
    type Key (Node k a) = k
    nodeKey (N _ k _) = k
    nodeNeighbors (N _ _ ks) = ks

-- TODO: Maybe introduce a typeclass for items which just
-- keys (so, Key associated type, and nodeKey method).  But
-- I didn't need it here, so I didn't introduce it.

-- Query

-- | /O(1)/. Is the graph empty?
null :: Graph a -> Bool
null = Map.null . toMap

-- | /O(1)/. The number of nodes in the graph.
size :: Graph a -> Int
size = Map.size . toMap

-- | /O(log V)/. Check if the key is in the graph.
member :: IsNode a => Key a -> Graph a -> Bool
member k g = Map.member k (toMap g)

-- | /O(log V)/. Lookup the node at a key in the graph.
lookup :: IsNode a => Key a -> Graph a -> Maybe a
lookup k g = Map.lookup k (toMap g)

-- Construction

-- | /O(1)/. The empty graph.
empty :: IsNode a => Graph a
empty = fromMap Map.empty

-- | /O(log V)/. Insert a node into a graph.
insert :: IsNode a => a -> Graph a -> Graph a
insert !n g = fromMap (Map.insert (nodeKey n) n (toMap g))

-- | /O(log V)/. Delete the node at a key from the graph.
deleteKey :: IsNode a => Key a -> Graph a -> Graph a
deleteKey k g = fromMap (Map.delete k (toMap g))

-- | /O(log V)/. Lookup and delete.  This function returns the deleted
-- value if it existed.
deleteLookup :: IsNode a => Key a -> Graph a -> (Maybe a, Graph a)
deleteLookup k g =
    let (r, m') = Map.updateLookupWithKey (\_ _ -> Nothing) k (toMap g)
    in (r, fromMap m')

-- Combining

-- | /O(V + V')/. Right-biased union, preferring entries
-- from the second map when conflicts occur.
-- @'nodeKey' x = 'nodeKey' (f x)@.
unionRight :: IsNode a => Graph a -> Graph a -> Graph a
unionRight g g' = fromMap (Map.union (toMap g') (toMap g))

-- | /O(V + V')/. Left-biased union, preferring entries from
-- the first map when conflicts occur.
unionLeft :: IsNode a => Graph a -> Graph a -> Graph a
unionLeft = flip unionRight

-- Graph-like operations

-- | /Ω(V + E)/. Compute the strongly connected components of a graph.
-- Requires amortized construction of graph.
stronglyConnComp :: Graph a -> [SCC a]
stronglyConnComp g = map decode forest
  where
    forest = G.scc (graphForward g)
    decode (Tree.Node v [])
        | mentions_itself v = CyclicSCC  [graphVertexToNode g v]
        | otherwise         = AcyclicSCC (graphVertexToNode g v)
    decode other = CyclicSCC (dec other [])
        where dec (Tree.Node v ts) vs
                = graphVertexToNode g v : foldr dec vs ts
    mentions_itself v = v `elem` (graphForward g ! v)
-- Implementation copied from 'stronglyConnCompR' in 'Data.Graph'.

-- | /Ω(V + E)/. Compute the cycles of a graph.
-- Requires amortized construction of graph.
cycles :: Graph a -> [[a]]
cycles g = [ vs | CyclicSCC vs <- stronglyConnComp g ]

-- | /O(1)/.  Return a list of nodes paired with their broken
-- neighbors (i.e., neighbor keys which are not in the graph).
-- Requires amortized construction of graph.
broken :: Graph a -> [(a, [Key a])]
broken g = graphBroken g

-- | Lookup the immediate neighbors from a key in the graph.
-- Requires amortized construction of graph.
neighbors :: Graph a -> Key a -> Maybe [a]
neighbors g k = do
    v <- graphKeyToVertex g k
    return (map (graphVertexToNode g) (graphForward g ! v))

-- | Lookup the immediate reverse neighbors from a key in the graph.
-- Requires amortized construction of graph.
revNeighbors :: Graph a -> Key a -> Maybe [a]
revNeighbors g k = do
    v <- graphKeyToVertex g k
    return (map (graphVertexToNode g) (graphAdjoint g ! v))

-- | Compute the subgraph which is the closure of some set of keys.
-- Returns @Nothing@ if one (or more) keys are not present in
-- the graph.
-- Requires amortized construction of graph.
closure :: Graph a -> [Key a] -> Maybe [a]
closure g ks = do
    vs <- traverse (graphKeyToVertex g) ks
    return (decodeVertexForest g (G.dfs (graphForward g) vs))

-- | Compute the reverse closure of a graph from some set
-- of keys.  Returns @Nothing@ if one (or more) keys are not present in
-- the graph.
-- Requires amortized construction of graph.
revClosure :: Graph a -> [Key a] -> Maybe [a]
revClosure g ks = do
    vs <- traverse (graphKeyToVertex g) ks
    return (decodeVertexForest g (G.dfs (graphAdjoint g) vs))

flattenForest :: Tree.Forest a -> [a]
flattenForest = concatMap Tree.flatten

decodeVertexForest :: Graph a -> Tree.Forest G.Vertex -> [a]
decodeVertexForest g = map (graphVertexToNode g) . flattenForest

-- | Topologically sort the nodes of a graph.
-- Requires amortized construction of graph.
topSort :: Graph a -> [a]
topSort g = map (graphVertexToNode g) $ G.topSort (graphForward g)

-- | Reverse topologically sort the nodes of a graph.
-- Requires amortized construction of graph.
revTopSort :: Graph a -> [a]
revTopSort g = map (graphVertexToNode g) $ G.topSort (graphAdjoint g)

-- Conversions

-- | /O(1)/. Convert a map from keys to nodes into a graph.
-- The map must satisfy the invariant that
-- @'fromMap' m == 'fromList' ('Data.Map.elems' m)@;
-- if you can't fulfill this invariant use @'fromList' ('Data.Map.elems' m)@
-- instead.  The values of the map are assumed to already
-- be in WHNF.
fromMap :: IsNode a => Map (Key a) a -> Graph a
fromMap m
    = Graph { graphMap = m
            -- These are lazily computed!
            , graphForward = g
            , graphAdjoint = G.transposeG g
            , graphVertexToNode = vertex_to_node
            , graphKeyToVertex = key_to_vertex
            , graphBroken = broke
            }
  where
    try_key_to_vertex k = maybe (Left k) Right (key_to_vertex k)

    (brokenEdges, edges)
        = unzip
        $ [ partitionEithers (map try_key_to_vertex (nodeNeighbors n))
          | n <- ns ]
    broke = filter (not . Prelude.null . snd) (zip ns brokenEdges)

    g = Array.listArray bounds edges

    ns              = Map.elems m -- sorted ascending
    vertices        = zip (map nodeKey ns) [0..]
    vertex_map      = Map.fromAscList vertices
    key_to_vertex k = Map.lookup k vertex_map

    vertex_to_node vertex = nodeTable ! vertex

    nodeTable   = Array.listArray bounds ns
    bounds = (0, Map.size m - 1)

-- | /O(V log V)/. Convert a list of nodes (with distinct keys) into a graph.
fromDistinctList :: (IsNode a, Show (Key a)) => [a] -> Graph a
fromDistinctList = fromMap
                 . Map.fromListWith (\_ -> duplicateError)
                 . map (\n -> n `seq` (nodeKey n, n))
  where
    duplicateError n = error $ "Graph.fromDistinctList: duplicate key: "
                            ++ show (nodeKey n)

-- Map-like operations

-- | /O(V)/. Convert a graph into a list of nodes.
toList :: Graph a -> [a]
toList g = Map.elems (toMap g)

-- | /O(V)/. Convert a graph into a list of keys.
keys :: Graph a -> [Key a]
keys g = Map.keys (toMap g)

-- | /O(V)/. Convert a graph into a set of keys.
keysSet :: Graph a -> Set.Set (Key a)
keysSet g = Map.keysSet (toMap g)

-- | /O(1)/. Convert a graph into a map from keys to nodes.
-- The resulting map @m@ is guaranteed to have the property that
-- @'Prelude.all' (\(k,n) -> k == 'nodeKey' n) ('Data.Map.toList' m)@.
toMap :: Graph a -> Map (Key a) a
toMap = graphMap

-- Graph-like operations

-- | /O(1)/. Convert a graph into a 'Data.Graph.Graph'.
-- Requires amortized construction of graph.
toGraph :: Graph a -> (G.Graph, G.Vertex -> a, Key a -> Maybe G.Vertex)
toGraph g = (graphForward g, graphVertexToNode g, graphKeyToVertex g)
