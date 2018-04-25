{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif
#if __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Trustworthy #-}
#endif
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- A version of the graph algorithms described in:
--
--   /Structuring Depth-First Search Algorithms in Haskell/,
--   by David King and John Launchbury.
--
-----------------------------------------------------------------------------

module Data.Graph(

        -- * External interface

        -- At present the only one with a "nice" external interface
        stronglyConnComp, stronglyConnCompR, SCC(..), flattenSCC, flattenSCCs,

        -- * Graphs

        Graph, Table, Bounds, Edge, Vertex,

        -- ** Building graphs

        graphFromEdges, graphFromEdges', buildG, transposeG,
        -- reverseE,

        -- ** Graph properties

        vertices, edges,
        outdegree, indegree,

        -- * Algorithms

        dfs, dff,
        topSort,
        components,
        scc,
        bcc,
        -- tree, back, cross, forward,
        reachable, path,

        module Data.Tree

    ) where

#if __GLASGOW_HASKELL__
# define USE_ST_MONAD 1
#endif

-- Extensions
#if USE_ST_MONAD
import Control.Monad.ST
import Data.Array.ST (STArray, newArray, readArray, writeArray)
#else
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
#endif
import Data.Tree (Tree(Node), Forest)

-- std interfaces
import Control.Applicative
#if !MIN_VERSION_base(4,8,0)
import qualified Data.Foldable as F
import Data.Traversable
#else
import Data.Foldable as F
#endif
import Control.DeepSeq (NFData(rnf))
import Data.Maybe
import Data.Array
import Data.List
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes
import Data.Semigroup (Semigroup (..))
#endif
#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic, Generic1)
#elif __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif
#ifdef __GLASGOW_HASKELL__
import Data.Data (Data)
#endif
import Data.Typeable


-------------------------------------------------------------------------
--                                                                      -
--      External interface
--                                                                      -
-------------------------------------------------------------------------

-- | Strongly connected component.
data SCC vertex = AcyclicSCC vertex     -- ^ A single vertex that is not
                                        -- in any cycle.
                | CyclicSCC  [vertex]   -- ^ A maximal set of mutually
                                        -- reachable vertices.
  deriving (Eq, Show, Read)

INSTANCE_TYPEABLE1(SCC)

#ifdef __GLASGOW_HASKELL__
deriving instance Data vertex => Data (SCC vertex)
#endif

#if __GLASGOW_HASKELL__ >= 706
deriving instance Generic1 SCC
#endif

#if __GLASGOW_HASKELL__ >= 702
deriving instance Generic (SCC vertex)
#endif

#if MIN_VERSION_base(4,9,0)
instance Eq1 SCC where
  liftEq eq (AcyclicSCC v1) (AcyclicSCC v2) = eq v1 v2
  liftEq eq (CyclicSCC vs1) (CyclicSCC vs2) = liftEq eq vs1 vs2
  liftEq _ _ _ = False
instance Show1 SCC where
  liftShowsPrec sp _sl d (AcyclicSCC v) = showsUnaryWith sp "AcyclicSCC" d v
  liftShowsPrec _sp sl d (CyclicSCC vs) = showsUnaryWith (const sl) "CyclicSCC" d vs
instance Read1 SCC where
  liftReadsPrec rp rl = readsData $
    readsUnaryWith rp "AcyclicSCC" AcyclicSCC <>
    readsUnaryWith (const rl) "CyclicSCC" CyclicSCC
#endif

instance F.Foldable SCC where
  foldr c n (AcyclicSCC v) = c v n
  foldr c n (CyclicSCC vs) = foldr c n vs

instance Traversable SCC where
  -- We treat the non-empty cyclic case specially to cut one
  -- fmap application.
  traverse f (AcyclicSCC vertex) = AcyclicSCC <$> f vertex
  traverse _f (CyclicSCC []) = pure (CyclicSCC [])
  traverse f (CyclicSCC (x : xs)) =
    liftA2 (\x' xs' -> CyclicSCC (x' : xs')) (f x) (traverse f xs)

instance NFData a => NFData (SCC a) where
    rnf (AcyclicSCC v) = rnf v
    rnf (CyclicSCC vs) = rnf vs

instance Functor SCC where
    fmap f (AcyclicSCC v) = AcyclicSCC (f v)
    fmap f (CyclicSCC vs) = CyclicSCC (fmap f vs)

-- | The vertices of a list of strongly connected components.
flattenSCCs :: [SCC a] -> [a]
flattenSCCs = concatMap flattenSCC

-- | The vertices of a strongly connected component.
flattenSCC :: SCC vertex -> [vertex]
flattenSCC (AcyclicSCC v) = [v]
flattenSCC (CyclicSCC vs) = vs

-- | The strongly connected components of a directed graph, topologically
-- sorted.
stronglyConnComp
        :: Ord key
        => [(node, key, [key])]
                -- ^ The graph: a list of nodes uniquely identified by keys,
                -- with a list of keys of nodes this node has edges to.
                -- The out-list may contain keys that don't correspond to
                -- nodes of the graph; such edges are ignored.
        -> [SCC node]

stronglyConnComp edges0
  = map get_node (stronglyConnCompR edges0)
  where
    get_node (AcyclicSCC (n, _, _)) = AcyclicSCC n
    get_node (CyclicSCC triples)     = CyclicSCC [n | (n,_,_) <- triples]

-- | The strongly connected components of a directed graph, topologically
-- sorted.  The function is the same as 'stronglyConnComp', except that
-- all the information about each node retained.
-- This interface is used when you expect to apply 'SCC' to
-- (some of) the result of 'SCC', so you don't want to lose the
-- dependency information.
stronglyConnCompR
        :: Ord key
        => [(node, key, [key])]
                -- ^ The graph: a list of nodes uniquely identified by keys,
                -- with a list of keys of nodes this node has edges to.
                -- The out-list may contain keys that don't correspond to
                -- nodes of the graph; such edges are ignored.
        -> [SCC (node, key, [key])]     -- ^ Topologically sorted

stronglyConnCompR [] = []  -- added to avoid creating empty array in graphFromEdges -- SOF
stronglyConnCompR edges0
  = map decode forest
  where
    (graph, vertex_fn,_) = graphFromEdges edges0
    forest             = scc graph
    decode (Node v []) | mentions_itself v = CyclicSCC [vertex_fn v]
                       | otherwise         = AcyclicSCC (vertex_fn v)
    decode other = CyclicSCC (dec other [])
                 where
                   dec (Node v ts) vs = vertex_fn v : foldr dec vs ts
    mentions_itself v = v `elem` (graph ! v)

-------------------------------------------------------------------------
--                                                                      -
--      Graphs
--                                                                      -
-------------------------------------------------------------------------

-- | Abstract representation of vertices.
type Vertex  = Int
-- | Table indexed by a contiguous set of vertices.
type Table a = Array Vertex a
-- | Adjacency list representation of a graph, mapping each vertex to its
-- list of successors.
type Graph   = Table [Vertex]
-- | The bounds of a 'Table'.
type Bounds  = (Vertex, Vertex)
-- | An edge from the first vertex to the second.
type Edge    = (Vertex, Vertex)

-- | All vertices of a graph.
vertices :: Graph -> [Vertex]
vertices  = indices

-- | All edges of a graph.
edges    :: Graph -> [Edge]
edges g   = [ (v, w) | v <- vertices g, w <- g!v ]

mapT    :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [ (,) v (f v (t!v)) | v <- indices t ]

-- | Build a graph from a list of edges.
buildG :: Bounds -> [Edge] -> Graph
buildG bounds0 edges0 = accumArray (flip (:)) [] bounds0 edges0

-- | The graph obtained by reversing all edges.
transposeG  :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

reverseE    :: Graph -> [Edge]
reverseE g   = [ (w, v) | (v, w) <- edges g ]

-- | A table of the count of edges from each node.
outdegree :: Graph -> Table Int
outdegree  = mapT numEdges
             where numEdges _ ws = length ws

-- | A table of the count of edges into each node.
indegree :: Graph -> Table Int
indegree  = outdegree . transposeG

-- | Identical to 'graphFromEdges', except that the return value
-- does not include the function which maps keys to vertices.  This
-- version of 'graphFromEdges' is for backwards compatibility.
graphFromEdges'
        :: Ord key
        => [(node, key, [key])]
        -> (Graph, Vertex -> (node, key, [key]))
graphFromEdges' x = (a,b) where
    (a,b,_) = graphFromEdges x

-- | Build a graph from a list of nodes uniquely identified by keys,
-- with a list of keys of nodes this node should have edges to.
-- The out-list may contain keys that don't correspond to
-- nodes of the graph; they are ignored.
graphFromEdges
        :: Ord key
        => [(node, key, [key])]
        -> (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)
graphFromEdges edges0
  = (graph, \v -> vertex_map ! v, key_vertex)
  where
    max_v           = length edges0 - 1
    bounds0         = (0,max_v) :: (Vertex, Vertex)
    sorted_edges    = sortBy lt edges0
    edges1          = zipWith (,) [0..] sorted_edges

    graph           = array bounds0 [(,) v (mapMaybe key_vertex ks) | (,) v (_,    _, ks) <- edges1]
    key_map         = array bounds0 [(,) v k                       | (,) v (_,    k, _ ) <- edges1]
    vertex_map      = array bounds0 edges1

    (_,k1,_) `lt` (_,k2,_) = k1 `compare` k2

    -- key_vertex :: key -> Maybe Vertex
    --  returns Nothing for non-interesting vertices
    key_vertex k   = findVertex 0 max_v
                   where
                     findVertex a b | a > b
                              = Nothing
                     findVertex a b = case compare k (key_map ! mid) of
                                   LT -> findVertex a (mid-1)
                                   EQ -> Just mid
                                   GT -> findVertex (mid+1) b
                              where
                                mid = a + (b - a) `div` 2

-------------------------------------------------------------------------
--                                                                      -
--      Depth first search
--                                                                      -
-------------------------------------------------------------------------

-- | A spanning forest of the graph, obtained from a depth-first search of
-- the graph starting from each vertex in an unspecified order.
dff          :: Graph -> Forest Vertex
dff g         = dfs g (vertices g)

-- | A spanning forest of the part of the graph reachable from the listed
-- vertices, obtained from a depth-first search of the graph starting at
-- each of the listed vertices in order.
dfs          :: Graph -> [Vertex] -> Forest Vertex
dfs g vs      = prune (bounds g) (map (generate g) vs)

generate     :: Graph -> Vertex -> Tree Vertex
generate g v  = Node v (map (generate g) (g!v))

prune        :: Bounds -> Forest Vertex -> Forest Vertex
prune bnds ts = run bnds (chop ts)

chop         :: Forest Vertex -> SetM s (Forest Vertex)
chop []       = return []
chop (Node v ts : us)
              = do
                visited <- contains v
                if visited then
                  chop us
                 else do
                  include v
                  as <- chop ts
                  bs <- chop us
                  return (Node v as : bs)

-- A monad holding a set of vertices visited so far.
#if USE_ST_MONAD

-- Use the ST monad if available, for constant-time primitives.

newtype SetM s a = SetM { runSetM :: STArray s Vertex Bool -> ST s a }

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

run          :: Bounds -> (forall s. SetM s a) -> a
run bnds act  = runST (newArray bnds False >>= runSetM act)

contains     :: Vertex -> SetM s Bool
contains v    = SetM $ \ m -> readArray m v

include      :: Vertex -> SetM s ()
include v     = SetM $ \ m -> writeArray m v True

#else /* !USE_ST_MONAD */

-- Portable implementation using IntSet.

newtype SetM s a = SetM { runSetM :: IntSet -> (a, IntSet) }

instance Monad (SetM s) where
    return x     = SetM $ \s -> (x, s)
    SetM v >>= f = SetM $ \s -> case v s of (x, s') -> runSetM (f x) s'

instance Functor (SetM s) where
    f `fmap` SetM v = SetM $ \s -> case v s of (x, s') -> (f x, s')
    {-# INLINE fmap #-}

instance Applicative (SetM s) where
    pure x = SetM $ \s -> (x, s)
    {-# INLINE pure #-}
    SetM f <*> SetM v = SetM $ \s -> case f s of (k, s') -> case v s' of (x, s'') -> (k x, s'')
    {-# INLINE (<*>) #-}

run          :: Bounds -> SetM s a -> a
run _ act     = fst (runSetM act Set.empty)

contains     :: Vertex -> SetM s Bool
contains v    = SetM $ \ m -> (Set.member v m, m)

include      :: Vertex -> SetM s ()
include v     = SetM $ \ m -> ((), Set.insert v m)

#endif /* !USE_ST_MONAD */

-------------------------------------------------------------------------
--                                                                      -
--      Algorithms
--                                                                      -
-------------------------------------------------------------------------

------------------------------------------------------------
-- Algorithm 1: depth first search numbering
------------------------------------------------------------

preorder' :: Tree a -> [a] -> [a]
preorder' (Node a ts) = (a :) . preorderF' ts

preorderF' :: Forest a -> [a] -> [a]
preorderF' ts = foldr (.) id $ map preorder' ts

preorderF :: Forest a -> [a]
preorderF ts = preorderF' ts []

tabulate        :: Bounds -> [Vertex] -> Table Int
tabulate bnds vs = array bnds (zipWith (,) vs [1..])

preArr          :: Bounds -> Forest Vertex -> Table Int
preArr bnds      = tabulate bnds . preorderF

------------------------------------------------------------
-- Algorithm 2: topological sorting
------------------------------------------------------------

postorder :: Tree a -> [a] -> [a]
postorder (Node a ts) = postorderF ts . (a :)

postorderF   :: Forest a -> [a] -> [a]
postorderF ts = foldr (.) id $ map postorder ts

postOrd :: Graph -> [Vertex]
postOrd g = postorderF (dff g) []

-- | A topological sort of the graph.
-- The order is partially specified by the condition that a vertex /i/
-- precedes /j/ whenever /j/ is reachable from /i/ but not vice versa.
topSort      :: Graph -> [Vertex]
topSort       = reverse . postOrd

------------------------------------------------------------
-- Algorithm 3: connected components
------------------------------------------------------------

-- | The connected components of a graph.
-- Two vertices are connected if there is a path between them, traversing
-- edges in either direction.
components   :: Graph -> Forest Vertex
components    = dff . undirected

undirected   :: Graph -> Graph
undirected g  = buildG (bounds g) (edges g ++ reverseE g)

-- Algorithm 4: strongly connected components

-- | The strongly connected components of a graph.
scc  :: Graph -> Forest Vertex
scc g = dfs g (reverse (postOrd (transposeG g)))

------------------------------------------------------------
-- Algorithm 5: Classifying edges
------------------------------------------------------------

{-
XXX unused code

tree              :: Bounds -> Forest Vertex -> Graph
tree bnds ts       = buildG bnds (concat (map flat ts))
 where flat (Node v ts') = [ (v, w) | Node w _us <- ts' ]
                        ++ concat (map flat ts')

back              :: Graph -> Table Int -> Graph
back g post        = mapT select g
 where select v ws = [ w | w <- ws, post!v < post!w ]

cross             :: Graph -> Table Int -> Table Int -> Graph
cross g pre post   = mapT select g
 where select v ws = [ w | w <- ws, post!v > post!w, pre!v > pre!w ]

forward           :: Graph -> Graph -> Table Int -> Graph
forward g tree' pre = mapT select g
 where select v ws = [ w | w <- ws, pre!v < pre!w ] \\ tree' ! v
-}

------------------------------------------------------------
-- Algorithm 6: Finding reachable vertices
------------------------------------------------------------

-- | A list of vertices reachable from a given vertex.
reachable    :: Graph -> Vertex -> [Vertex]
reachable g v = preorderF (dfs g [v])

-- | Is the second vertex reachable from the first?
path         :: Graph -> Vertex -> Vertex -> Bool
path g v w    = w `elem` (reachable g v)

------------------------------------------------------------
-- Algorithm 7: Biconnected components
------------------------------------------------------------

-- | The biconnected components of a graph.
-- An undirected graph is biconnected if the deletion of any vertex
-- leaves it connected.
bcc :: Graph -> Forest [Vertex]
bcc g = (concat . map bicomps . map (do_label g dnum)) forest
 where forest = dff g
       dnum   = preArr (bounds g) forest

do_label :: Graph -> Table Int -> Tree Vertex -> Tree (Vertex,Int,Int)
do_label g dnum (Node v ts) = Node (v,dnum!v,lv) us
 where us = map (do_label g dnum) ts
       lv = minimum ([dnum!v] ++ [dnum!w | w <- g!v]
                     ++ [lu | Node (_,_,lu) _ <- us])

bicomps :: Tree (Vertex,Int,Int) -> Forest [Vertex]
bicomps (Node (v,_,_) ts)
      = [ Node (v:vs) us | (_,Node vs us) <- map collect ts]

collect :: Tree (Vertex,Int,Int) -> (Int, Tree [Vertex])
collect (Node (v,dv,lv) ts) = (lv, Node (v:vs) cs)
 where collected = map collect ts
       vs = concat [ ws | (lw, Node ws _) <- collected, lw<dv]
       cs = concat [ if lw<dv then us else [Node (v:ws) us]
                        | (lw, Node ws us) <- collected ]
