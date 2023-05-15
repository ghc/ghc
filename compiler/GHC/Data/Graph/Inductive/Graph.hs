-- (c) 1999-2005 by Martin Erwig (see copyright at bottom)
-- | Static and Dynamic Inductive Graphs
--
-- Code is from Hackage `fgl` package version 5.7.0.3
--
module GHC.Data.Graph.Inductive.Graph (
    -- * General Type Defintions
    -- ** Node and Edge Types
    Node,LNode,UNode,
    Edge,LEdge,UEdge,
    -- ** Types Supporting Inductive Graph View
    Adj,Context,MContext,Decomp,GDecomp,UContext,UDecomp,
    Path,LPath(..),UPath,
    -- * Graph Type Classes
    -- | We define two graph classes:
    --
    --   Graph: static, decomposable graphs.
    --    Static means that a graph itself cannot be changed
    --
    --   DynGraph: dynamic, extensible graphs.
    --             Dynamic graphs inherit all operations from static graphs
    --             but also offer operations to extend and change graphs.
    --
    -- Each class contains in addition to its essential operations those
    -- derived operations that might be overwritten by a more efficient
    -- implementation in an instance definition.
    --
    -- Note that labNodes is essentially needed because the default definition
    -- for matchAny is based on it: we need some node from the graph to define
    -- matchAny in terms of match. Alternatively, we could have made matchAny
    -- essential and have labNodes defined in terms of ufold and matchAny.
    -- However, in general, labNodes seems to be (at least) as easy to define
    -- as matchAny. We have chosen labNodes instead of the function nodes since
    -- nodes can be easily derived from labNodes, but not vice versa.
    Graph(..),
    DynGraph(..),
    -- * Operations
    order,
    size,
    -- ** Graph Folds and Maps
    ufold,gmap,nmap,emap,nemap,
    -- ** Graph Projection
    nodes,edges,toEdge,edgeLabel,toLEdge,newNodes,gelem,
    -- ** Graph Construction and Destruction
    insNode,insEdge,delNode,delEdge,delLEdge,delAllLEdge,
    insNodes,insEdges,delNodes,delEdges,
    buildGr,mkUGraph,
    -- ** Subgraphs
    gfiltermap,nfilter,labnfilter,labfilter,subgraph,
    -- ** Graph Inspection
    context,lab,neighbors,lneighbors,
    suc,pre,lsuc,lpre,
    out,inn,outdeg,indeg,deg,
    hasEdge,hasNeighbor,hasLEdge,hasNeighborAdj,
    equal,
    -- ** Context Inspection
    node',lab',labNode',neighbors',lneighbors',
    suc',pre',lpre',lsuc',
    out',inn',outdeg',indeg',deg',
    -- * Pretty-printing
    prettify,
    prettyPrint,
    -- * Ordering of Graphs
    OrdGr(..)
) where

import GHC.Prelude

import           Control.Arrow (first)
import           Data.Function (on)
import qualified Data.IntSet   as IntSet
import           Data.List     (delete, groupBy, sort, sortBy, (\\))
import           Data.List.NonEmpty (nonEmpty)
import           Data.Maybe    (fromMaybe, isJust)

import GHC.Utils.Panic

-- | Unlabeled node
type  Node   = Int
-- | Labeled node
type LNode a = (Node,a)
-- | Quasi-unlabeled node
type UNode   = LNode ()

-- | Unlabeled edge
type  Edge   = (Node,Node)
-- | Labeled edge
type LEdge b = (Node,Node,b)
-- | Quasi-unlabeled edge
type UEdge   = LEdge ()

-- | Unlabeled path
type Path    = [Node]
-- | Labeled path
newtype LPath a = LP { unLPath :: [LNode a] }

instance (Show a) => Show (LPath a) where
  show (LP xs) = show xs

instance (Eq a) => Eq (LPath a) where
  (LP [])        == (LP [])        = True
  (LP ((_,x):_)) == (LP ((_,y):_)) = x==y
  (LP _)         == (LP _)         = False

instance (Ord a) => Ord (LPath a) where
  compare (LP [])        (LP [])        = EQ
  compare (LP ((_,x):_)) (LP ((_,y):_)) = compare x y
  compare _ _ = panic "LPath: cannot compare two empty paths"

-- | Quasi-unlabeled path
type UPath   = [UNode]

-- | Labeled links to or from a 'Node'.
type Adj b        = [(b,Node)]
-- | Links to the 'Node', the 'Node' itself, a label, links from the 'Node'.
--
--   In other words, this captures all information regarding the
--   specified 'Node' within a graph.
type Context a b  = (Adj b,Node,a,Adj b) -- Context a b "=" Context' a b "+" Node
type MContext a b = Maybe (Context a b)
-- | 'Graph' decomposition - the context removed from a 'Graph', and the rest
-- of the 'Graph'.
type Decomp g a b = (MContext a b,g a b)
-- | The same as 'Decomp', only more sure of itself.
type GDecomp g a b  = (Context a b,g a b)

-- | Unlabeled context.
type UContext     = ([Node],Node,[Node])
-- | Unlabeled decomposition.
type UDecomp g    = (Maybe UContext,g)

-- | Minimum implementation: 'empty', 'isEmpty', 'match', 'mkGraph', 'labNodes'
class Graph gr where
  {-# MINIMAL empty, isEmpty, match, mkGraph, labNodes #-}

  -- | An empty 'Graph'.
  empty     :: gr a b

  -- | True if the given 'Graph' is empty.
  isEmpty   :: gr a b -> Bool

  -- | Decompose a 'Graph' into the 'MContext' found for the given node and the
  -- remaining 'Graph'.
  match     :: Node -> gr a b -> Decomp gr a b

  -- | Create a 'Graph' from the list of 'LNode's and 'LEdge's.
  --
  --   For graphs that are also instances of 'DynGraph', @mkGraph ns
  --   es@ should be equivalent to @('insEdges' es . 'insNodes' ns)
  --   'empty'@.
  mkGraph   :: [LNode a] -> [LEdge b] -> gr a b

  -- | A list of all 'LNode's in the 'Graph'.
  labNodes  :: gr a b -> [LNode a]

  -- | Decompose a graph into the 'Context' for an arbitrarily-chosen 'Node'
  -- and the remaining 'Graph'.
  matchAny  :: gr a b -> GDecomp gr a b
  matchAny g = case labNodes g of
                 []      -> panic "Match Exception, Empty Graph"
                 (v,_):_ | (Just c,g') <- match v g -> (c,g')
                 _       -> panic "This can't happen: failed to match node in graph"


  -- | The number of 'Node's in a 'Graph'.
  noNodes   :: gr a b -> Int
  noNodes = length . labNodes

  -- | The minimum and maximum 'Node' in a 'Graph'.
  nodeRange :: gr a b -> (Node,Node)
  nodeRange g = case nonEmpty (nodes g) of
      Nothing -> panic "nodeRange of empty graph"
      Just vs -> (minimum vs, maximum vs)

  -- | A list of all 'LEdge's in the 'Graph'.
  labEdges  :: gr a b -> [LEdge b]
  labEdges = ufold (\(_,v,_,s)->(map (\(l,w)->(v,w,l)) s ++)) []

class (Graph gr) => DynGraph gr where
  -- | Merge the 'Context' into the 'DynGraph'.
  --
  --   Context adjacencies should only refer to either a Node already
  --   in a graph or the node in the Context itself (for loops).
  --
  --   Behaviour is undefined if the specified 'Node' already exists
  --   in the graph.
  (&) :: Context a b -> gr a b -> gr a b


-- | The number of nodes in the graph.  An alias for 'noNodes'.
order :: (Graph gr) => gr a b -> Int
order = noNodes

-- | The number of edges in the graph.
--
--   Note that this counts every edge found, so if you are
--   representing an unordered graph by having each edge mirrored this
--   will be incorrect.
--
--   If you created an unordered graph by either mirroring every edge
--   (including loops!) or using the @undir@ function in
--   "Data.Graph.Inductive.Basic" then you can safely halve the value
--   returned by this.
size :: (Graph gr) => gr a b -> Int
size = length . labEdges

-- | Fold a function over the graph by recursively calling 'match'.
ufold :: (Graph gr) => (Context a b -> c -> c) -> c -> gr a b -> c
ufold f u g
  | isEmpty g = u
  | otherwise = f c (ufold f u g')
  where
    (c,g') = matchAny g

-- | Map a function over the graph by recursively calling 'match'.
gmap :: (DynGraph gr) => (Context a b -> Context c d) -> gr a b -> gr c d
gmap f = ufold (\c->(f c&)) empty
{-# NOINLINE [0] gmap #-}

-- | Map a function over the 'Node' labels in a graph.
nmap :: (DynGraph gr) => (a -> c) -> gr a b -> gr c b
nmap f = gmap (\(p,v,l,s)->(p,v,f l,s))
{-# NOINLINE [0] nmap #-}

-- | Map a function over the 'Edge' labels in a graph.
emap :: (DynGraph gr) => (b -> c) -> gr a b -> gr a c
emap f = gmap (\(p,v,l,s)->(map1 f p,v,l,map1 f s))
  where
    map1 g = map (first g)
{-# NOINLINE [0] emap #-}

-- | Map functions over both the 'Node' and 'Edge' labels in a graph.
nemap :: (DynGraph gr) => (a -> c) -> (b -> d) -> gr a b -> gr c d
nemap fn fe = gmap (\(p,v,l,s) -> (fe' p,v,fn l,fe' s))
  where
    fe' = map (first fe)
{-# NOINLINE [0] nemap #-}

-- | List all 'Node's in the 'Graph'.
nodes :: (Graph gr) => gr a b -> [Node]
nodes = map fst . labNodes

-- | List all 'Edge's in the 'Graph'.
edges :: (Graph gr) => gr a b -> [Edge]
edges = map toEdge . labEdges

-- | Drop the label component of an edge.
toEdge :: LEdge b -> Edge
toEdge (v,w,_) = (v,w)

-- | Add a label to an edge.
toLEdge :: Edge -> b -> LEdge b
toLEdge (v,w) l = (v,w,l)

-- | The label in an edge.
edgeLabel :: LEdge b -> b
edgeLabel (_,_,l) = l

-- | List N available 'Node's, i.e. 'Node's that are not used in the 'Graph'.
newNodes :: (Graph gr) => Int -> gr a b -> [Node]
newNodes i g
  | isEmpty g = [0..i-1]
  | otherwise = [n+1..n+i]
  where
    (_,n) = nodeRange g

-- | 'True' if the 'Node' is present in the 'Graph'.
gelem :: (Graph gr) => Node -> gr a b -> Bool
gelem v = isJust . fst . match v

-- | Insert a 'LNode' into the 'Graph'.
insNode :: (DynGraph gr) => LNode a -> gr a b -> gr a b
insNode (v,l) = (([],v,l,[])&)
{-# NOINLINE [0] insNode #-}

-- | Insert a 'LEdge' into the 'Graph'.
insEdge :: (DynGraph gr) => LEdge b -> gr a b -> gr a b
insEdge (v,w,l) g = (pr,v,la,(l,w):su) & g'
  where
    (mcxt,g') = match v g
    (pr,_,la,su) = fromMaybe
                     (panic ("insEdge: cannot add edge from non-existent vertex " ++ show v))
                     mcxt
{-# NOINLINE [0] insEdge #-}

-- | Remove a 'Node' from the 'Graph'.
delNode :: (Graph gr) => Node -> gr a b -> gr a b
delNode v = delNodes [v]

-- | Remove an 'Edge' from the 'Graph'.
--
--   NOTE: in the case of multiple edges, this will delete /all/ such
--   edges from the graph as there is no way to distinguish between
--   them.  If you need to delete only a single such edge, please use
--   'delLEdge'.
delEdge :: (DynGraph gr) => Edge -> gr a b -> gr a b
delEdge (v,w) g = case match v g of
                    (Nothing,_)          -> g
                    (Just (p,v',l,s),g') -> (p,v',l,filter ((/=w).snd) s) & g'

-- | Remove an 'LEdge' from the 'Graph'.
--
--   NOTE: in the case of multiple edges with the same label, this
--   will only delete the /first/ such edge.  To delete all such
--   edges, please use 'delAllLedge'.
delLEdge :: (DynGraph gr, Eq b) => LEdge b -> gr a b -> gr a b
delLEdge = delLEdgeBy delete

-- | Remove all edges equal to the one specified.
delAllLEdge :: (DynGraph gr, Eq b) => LEdge b -> gr a b -> gr a b
delAllLEdge = delLEdgeBy (filter . (/=))

delLEdgeBy :: (DynGraph gr) => ((b,Node) -> Adj b -> Adj b)
              -> LEdge b -> gr a b -> gr a b
delLEdgeBy f (v,w,b) g = case match v g of
                           (Nothing,_)          -> g
                           (Just (p,v',l,s),g') -> (p,v',l,f (b,w) s) & g'

-- | Insert multiple 'LNode's into the 'Graph'.
insNodes   :: (DynGraph gr) => [LNode a] -> gr a b -> gr a b
insNodes vs g = foldl' (flip insNode) g vs
{-# INLINABLE insNodes #-}

-- | Insert multiple 'LEdge's into the 'Graph'.
insEdges :: (DynGraph gr) => [LEdge b] -> gr a b -> gr a b
insEdges es g = foldl' (flip insEdge) g es
{-# INLINABLE insEdges #-}

-- | Remove multiple 'Node's from the 'Graph'.
delNodes :: (Graph gr) => [Node] -> gr a b -> gr a b
delNodes vs g = foldl' (snd .: flip match) g vs

-- | Remove multiple 'Edge's from the 'Graph'.
delEdges :: (DynGraph gr) => [Edge] -> gr a b -> gr a b
delEdges es g = foldl' (flip delEdge) g es

-- | Build a 'Graph' from a list of 'Context's.
--
--   The list should be in the order such that earlier 'Context's
--   depend upon later ones (i.e. as produced by @'ufold' (:) []@).
buildGr :: (DynGraph gr) => [Context a b] -> gr a b
buildGr = foldr (&) empty

-- | Build a quasi-unlabeled 'Graph'.
mkUGraph :: (Graph gr) => [Node] -> [Edge] -> gr () ()
mkUGraph vs es = mkGraph (labUNodes vs) (labUEdges es)
   where
     labUEdges = map (`toLEdge` ())
     labUNodes = map (flip (,) ())

-- | Build a graph out of the contexts for which the predicate is
-- satisfied by recursively calling 'match'.
gfiltermap :: DynGraph gr => (Context a b -> MContext c d) -> gr a b -> gr c d
gfiltermap f = ufold (maybe id (&) . f) empty

-- | Returns the subgraph only containing the labelled nodes which
-- satisfy the given predicate.
labnfilter :: Graph gr => (LNode a -> Bool) -> gr a b -> gr a b
labnfilter p gr = delNodes (map fst . filter (not . p) $ labNodes gr) gr

-- | Returns the subgraph only containing the nodes which satisfy the
-- given predicate.
nfilter :: DynGraph gr => (Node -> Bool) -> gr a b -> gr a b
nfilter f = labnfilter (f . fst)

-- | Returns the subgraph only containing the nodes whose labels
-- satisfy the given predicate.
labfilter :: DynGraph gr => (a -> Bool) -> gr a b -> gr a b
labfilter f = labnfilter (f . snd)

-- | Returns the subgraph induced by the supplied nodes.
subgraph :: DynGraph gr => [Node] -> gr a b -> gr a b
subgraph vs = let vs' = IntSet.fromList vs
              in nfilter (`IntSet.member` vs')

-- | Find the context for the given 'Node'.  Causes an error if the 'Node' is
-- not present in the 'Graph'.
context :: (Graph gr) => gr a b -> Node -> Context a b
context g v = fromMaybe (panic ("Match Exception, Node: "++show v))
                        (fst (match v g))

-- | Find the label for a 'Node'.
lab :: (Graph gr) => gr a b -> Node -> Maybe a
lab g v = fmap lab' . fst $ match v g

-- | Find the neighbors for a 'Node'.
neighbors :: (Graph gr) => gr a b -> Node -> [Node]
neighbors = map snd .: lneighbors

-- | Find the labelled links coming into or going from a 'Context'.
lneighbors :: (Graph gr) => gr a b -> Node -> Adj b
lneighbors = maybe [] lneighbors' .: mcontext

-- | Find all 'Node's that have a link from the given 'Node'.
suc :: (Graph gr) => gr a b -> Node -> [Node]
suc = map snd .: context4l

-- | Find all 'Node's that link to to the given 'Node'.
pre :: (Graph gr) => gr a b -> Node -> [Node]
pre = map snd .: context1l

-- | Find all 'Node's that are linked from the given 'Node' and the label of
-- each link.
lsuc :: (Graph gr) => gr a b -> Node -> [(Node,b)]
lsuc = map flip2 .: context4l

-- | Find all 'Node's that link to the given 'Node' and the label of each link.
lpre :: (Graph gr) => gr a b -> Node -> [(Node,b)]
lpre = map flip2 .: context1l

-- | Find all outward-bound 'LEdge's for the given 'Node'.
out :: (Graph gr) => gr a b -> Node -> [LEdge b]
out g v = map (\(l,w)->(v,w,l)) (context4l g v)

-- | Find all inward-bound 'LEdge's for the given 'Node'.
inn :: (Graph gr) => gr a b -> Node -> [LEdge b]
inn g v = map (\(l,w)->(w,v,l)) (context1l g v)

-- | The outward-bound degree of the 'Node'.
outdeg :: (Graph gr) => gr a b -> Node -> Int
outdeg = length .: context4l

-- | The inward-bound degree of the 'Node'.
indeg :: (Graph gr) => gr a b -> Node -> Int
indeg  = length .: context1l

-- | The degree of the 'Node'.
deg :: (Graph gr) => gr a b -> Node -> Int
deg = deg' .: context

-- | The 'Node' in a 'Context'.
node' :: Context a b -> Node
node' (_,v,_,_) = v

-- | The label in a 'Context'.
lab' :: Context a b -> a
lab' (_,_,l,_) = l

-- | The 'LNode' from a 'Context'.
labNode' :: Context a b -> LNode a
labNode' (_,v,l,_) = (v,l)

-- | All 'Node's linked to or from in a 'Context'.
neighbors' :: Context a b -> [Node]
neighbors' (p,_,_,s) = map snd p++map snd s

-- | All labelled links coming into or going from a 'Context'.
lneighbors' :: Context a b -> Adj b
lneighbors' (p,_,_,s) = p ++ s

-- | All 'Node's linked to in a 'Context'.
suc' :: Context a b -> [Node]
suc' = map snd . context4l'

-- | All 'Node's linked from in a 'Context'.
pre' :: Context a b -> [Node]
pre' = map snd . context1l'

-- | All 'Node's linked from in a 'Context', and the label of the links.
lsuc' :: Context a b -> [(Node,b)]
lsuc' = map flip2 . context4l'

-- | All 'Node's linked from in a 'Context', and the label of the links.
lpre' :: Context a b -> [(Node,b)]
lpre' = map flip2 . context1l'

-- | All outward-directed 'LEdge's in a 'Context'.
out' :: Context a b -> [LEdge b]
out' c@(_,v,_,_) = map (\(l,w)->(v,w,l)) (context4l' c)

-- | All inward-directed 'LEdge's in a 'Context'.
inn' :: Context a b -> [LEdge b]
inn' c@(_,v,_,_) = map (\(l,w)->(w,v,l)) (context1l' c)

-- | The outward degree of a 'Context'.
outdeg' :: Context a b -> Int
outdeg' = length . context4l'

-- | The inward degree of a 'Context'.
indeg' :: Context a b -> Int
indeg' = length . context1l'

-- | The degree of a 'Context'.
deg' :: Context a b -> Int
deg' (p,_,_,s) = length p+length s

-- | Checks if there is a directed edge between two nodes.
hasEdge :: Graph gr => gr a b -> Edge -> Bool
hasEdge gr (v,w) = w `elem` suc gr v

-- | Checks if there is an undirected edge between two nodes.
hasNeighbor :: Graph gr => gr a b -> Node -> Node -> Bool
hasNeighbor gr v w = w `elem` neighbors gr v

-- | Checks if there is a labelled edge between two nodes.
hasLEdge :: (Graph gr, Eq b) => gr a b -> LEdge b -> Bool
hasLEdge gr (v,w,l) = (w,l) `elem` lsuc gr v

-- | Checks if there is an undirected labelled edge between two nodes.
hasNeighborAdj :: (Graph gr, Eq b) => gr a b -> Node -> (b,Node) -> Bool
hasNeighborAdj gr v a = a `elem` lneighbors gr v

----------------------------------------------------------------------
-- GRAPH EQUALITY
----------------------------------------------------------------------

slabNodes :: (Graph gr) => gr a b -> [LNode a]
slabNodes = sortBy (compare `on` fst) . labNodes

glabEdges :: (Graph gr) => gr a b -> [GroupEdges b]
glabEdges = map (GEs . groupLabels)
            . groupBy ((==) `on` toEdge)
            . sortBy (compare `on` toEdge)
            . labEdges
  where
    groupLabels les = toLEdge (toEdge (head les)) (map edgeLabel les)

equal :: (Eq a,Eq b,Graph gr) => gr a b -> gr a b -> Bool
equal g g' = slabNodes g == slabNodes g' && glabEdges g == glabEdges g'
-- This assumes that nodes aren't repeated (which shouldn't happen for
-- sane graph instances).  If node IDs are repeated, then the usage of
-- slabNodes cannot guarantee stable ordering.

-- Newtype wrapper just to test for equality of multiple edges.  This
-- is needed because without an Ord constraint on `b' it is not
-- possible to guarantee a stable ordering on edge labels.
newtype GroupEdges b = GEs (LEdge [b])
  deriving (Show, Read)

instance (Eq b) => Eq (GroupEdges b) where
  (GEs (v1,w1,bs1)) == (GEs (v2,w2,bs2)) = v1 == v2
                                           && w1 == w2
                                           && eqLists bs1 bs2

eqLists :: (Eq a) => [a] -> [a] -> Bool
eqLists xs ys = null (xs \\ ys) && null (ys \\ xs)
-- OK to use \\ here as we want each value in xs to cancel a *single*
-- value in ys.

----------------------------------------------------------------------
-- UTILITIES
----------------------------------------------------------------------

-- auxiliary functions used in the implementation of the
-- derived class members
--
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
-- f .: g = \x y->f (g x y)
-- f .: g = (f .) . g
-- (.:) f = ((f .) .)
-- (.:) = (.) (.) (.)
(.:) = (.) . (.)

flip2 :: (a,b) -> (b,a)
flip2 (x,y) = (y,x)

-- projecting on context elements
--
context1l :: (Graph gr) => gr a b -> Node -> Adj b
context1l = maybe [] context1l' .: mcontext

context4l :: (Graph gr) => gr a b -> Node -> Adj b
context4l = maybe [] context4l' .: mcontext

mcontext :: (Graph gr) => gr a b -> Node -> MContext a b
mcontext = fst .: flip match

context1l' :: Context a b -> Adj b
context1l' (p,v,_,s) = p++filter ((==v).snd) s

context4l' :: Context a b -> Adj b
context4l' (p,v,_,s) = s++filter ((==v).snd) p

----------------------------------------------------------------------
-- PRETTY PRINTING
----------------------------------------------------------------------

-- | Pretty-print the graph.  Note that this loses a lot of
--   information, such as edge inverses, etc.
prettify :: (DynGraph gr, Show a, Show b) => gr a b -> String
prettify g = foldr (showsContext . context g) id (nodes g) ""
  where
    showsContext (_,n,l,s) sg = shows n . (':':) . shows l
                                . showString "->" . shows s
                                . ('\n':) . sg

-- | Pretty-print the graph to stdout.
prettyPrint :: (DynGraph gr, Show a, Show b) => gr a b -> IO ()
prettyPrint = putStr . prettify

----------------------------------------------------------------------
-- Ordered Graph
----------------------------------------------------------------------

-- | OrdGr comes equipped with an Ord instance, so that graphs can be
--   used as e.g. Map keys.
newtype OrdGr gr a b = OrdGr { unOrdGr :: gr a b }
  deriving (Read,Show)

instance (Graph gr, Ord a, Ord b) => Eq (OrdGr gr a b) where
  g1 == g2 = compare g1 g2 == EQ

instance (Graph gr, Ord a, Ord b) => Ord (OrdGr gr a b) where
  compare (OrdGr g1) (OrdGr g2) =
    (compare `on` sort . labNodes) g1 g2
    `mappend` (compare `on` sort . labEdges) g1 g2


{-----------------------------------------------------------------

Copyright (c) 1999-2008, Martin Erwig
              2010, Ivan Lazar Miljenovic
              2022, Norman Ramsey
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Neither the name of the author nor the names of its contributors may be
   used to endorse or promote products derived from this software without
   specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

----------------------------------------------------------------}
