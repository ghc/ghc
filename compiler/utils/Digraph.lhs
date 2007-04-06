%
% (c) The University of Glasgow 2006
%

\begin{code}
module Digraph(

	-- At present the only one with a "nice" external interface
	stronglyConnComp, stronglyConnCompR, SCC(..), flattenSCC, flattenSCCs,

	Graph, Vertex, 
	graphFromEdges, graphFromEdges', 
	buildG, transposeG, reverseE, outdegree, indegree,

	Tree(..), Forest,
	showTree, showForest,

	dfs, dff,
	topSort,
	components,
	scc,
	back, cross, forward,
	reachable, path,
	bcc

    ) where

# include "HsVersions.h"

------------------------------------------------------------------------------
-- A version of the graph algorithms described in:
-- 
-- ``Lazy Depth-First Search and Linear Graph Algorithms in Haskell''
--   by David King and John Launchbury
-- 
-- Also included is some additional code for printing tree structures ...
------------------------------------------------------------------------------


import Util	( sortLe )
import Outputable

-- Extensions
import Control.Monad.ST

-- std interfaces
import Data.Maybe
import Data.Array
import Data.List

#if __GLASGOW_HASKELL__ > 604
import Data.Array.ST
#else
import Data.Array.ST  hiding ( indices, bounds )
#endif
\end{code}


%************************************************************************
%*									*
%*	External interface
%*									*
%************************************************************************

\begin{code}
data SCC vertex = AcyclicSCC vertex
	        | CyclicSCC  [vertex]

flattenSCCs :: [SCC a] -> [a]
flattenSCCs = concatMap flattenSCC

flattenSCC (AcyclicSCC v) = [v]
flattenSCC (CyclicSCC vs) = vs

instance Outputable a => Outputable (SCC a) where
   ppr (AcyclicSCC v) = text "NONREC" $$ (nest 3 (ppr v))
   ppr (CyclicSCC vs) = text "REC" $$ (nest 3 (vcat (map ppr vs)))
\end{code}

\begin{code}
stronglyConnComp
	:: Ord key
	=> [(node, key, [key])]		-- The graph; its ok for the
					-- out-list to contain keys which arent
					-- a vertex key, they are ignored
	-> [SCC node]	-- Returned in topologically sorted order
			-- Later components depend on earlier ones, but not vice versa

stronglyConnComp edges
  = map get_node (stronglyConnCompR edges)
  where
    get_node (AcyclicSCC (n, _, _)) = AcyclicSCC n
    get_node (CyclicSCC triples)     = CyclicSCC [n | (n,_,_) <- triples]

-- The "R" interface is used when you expect to apply SCC to
-- the (some of) the result of SCC, so you dont want to lose the dependency info
stronglyConnCompR
	:: Ord key
	=> [(node, key, [key])]		-- The graph; its ok for the
					-- out-list to contain keys which arent
					-- a vertex key, they are ignored
	-> [SCC (node, key, [key])]	-- Topologically sorted

stronglyConnCompR [] = []  -- added to avoid creating empty array in graphFromEdges -- SOF
stronglyConnCompR edges
  = map decode forest
  where
    (graph, vertex_fn) = _scc_ "graphFromEdges" graphFromEdges edges
    forest	       = _scc_ "Digraph.scc" scc graph
    decode (Node v []) | mentions_itself v = CyclicSCC [vertex_fn v]
		       | otherwise	   = AcyclicSCC (vertex_fn v)
    decode other = CyclicSCC (dec other [])
		 where
		   dec (Node v ts) vs = vertex_fn v : foldr dec vs ts
    mentions_itself v = v `elem` (graph ! v)
\end{code}

%************************************************************************
%*									*
%*	Graphs
%*									*
%************************************************************************


\begin{code}
type Vertex  = Int
type Table a = Array Vertex a
type Graph   = Table [Vertex]
type Bounds  = (Vertex, Vertex)
type Edge    = (Vertex, Vertex)
\end{code}

\begin{code}
vertices :: Graph -> [Vertex]
vertices  = indices

edges    :: Graph -> [Edge]
edges g   = [ (v, w) | v <- vertices g, w <- g!v ]

mapT    :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [ (,) v (f v (t!v)) | v <- indices t ]

buildG :: Bounds -> [Edge] -> Graph
buildG bounds edges = accumArray (flip (:)) [] bounds edges

transposeG  :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

reverseE    :: Graph -> [Edge]
reverseE g   = [ (w, v) | (v, w) <- edges g ]

outdegree :: Graph -> Table Int
outdegree  = mapT numEdges
             where numEdges v ws = length ws

indegree :: Graph -> Table Int
indegree  = outdegree . transposeG
\end{code}


\begin{code}
graphFromEdges 
	:: Ord key
	=> [(node, key, [key])]
	-> (Graph, Vertex -> (node, key, [key]))
graphFromEdges edges = 
  case graphFromEdges' edges of (graph, vertex_fn, _) -> (graph, vertex_fn) 

graphFromEdges'
	:: Ord key
	=> [(node, key, [key])]
	-> (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)
graphFromEdges' edges
  = (graph, \v -> vertex_map ! v, key_vertex)
  where
    max_v      	    = length edges - 1
    bounds          = (0,max_v) :: (Vertex, Vertex)
    sorted_edges    = let
			 (_,k1,_) `le` (_,k2,_) = case k1 `compare` k2 of { GT -> False; other -> True }
		      in
			sortLe le edges
    edges1	    = zipWith (,) [0..] sorted_edges

    graph	    = array bounds [(,) v (mapMaybe key_vertex ks) | (,) v (_,    _, ks) <- edges1]
    key_map	    = array bounds [(,) v k			   | (,) v (_,    k, _ ) <- edges1]
    vertex_map	    = array bounds edges1


    -- key_vertex :: key -> Maybe Vertex
    -- 	returns Nothing for non-interesting vertices
    key_vertex k   = find 0 max_v 
		   where
		     find a b | a > b 
			      = Nothing
		     find a b = case compare k (key_map ! mid) of
				   LT -> find a (mid-1)
				   EQ -> Just mid
				   GT -> find (mid+1) b
			      where
			 	mid = (a + b) `div` 2
\end{code}

%************************************************************************
%*									*
%*	Trees and forests
%*									*
%************************************************************************

\begin{code}
data Tree a   = Node a (Forest a)
type Forest a = [Tree a]

mapTree              :: (a -> b) -> (Tree a -> Tree b)
mapTree f (Node x ts) = Node (f x) (map (mapTree f) ts)
\end{code}

\begin{code}
instance Show a => Show (Tree a) where
  showsPrec p t s = showTree t ++ s

showTree :: Show a => Tree a -> String
showTree  = drawTree . mapTree show

showForest :: Show a => Forest a -> String
showForest  = unlines . map showTree

drawTree        :: Tree String -> String
drawTree         = unlines . draw

draw (Node x ts) = grp this (space (length this)) (stLoop ts)
 where this          = s1 ++ x ++ " "

       space n       = replicate n ' '

       stLoop []     = [""]
       stLoop [t]    = grp s2 "  " (draw t)
       stLoop (t:ts) = grp s3 s4 (draw t) ++ [s4] ++ rsLoop ts

       rsLoop [t]    = grp s5 "  " (draw t)
       rsLoop (t:ts) = grp s6 s4 (draw t) ++ [s4] ++ rsLoop ts

       grp fst rst   = zipWith (++) (fst:repeat rst)

       [s1,s2,s3,s4,s5,s6] = ["- ", "--", "-+", " |", " `", " +"]
\end{code}


%************************************************************************
%*									*
%*	Depth first search
%*									*
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
dff          :: Graph -> Forest Vertex
dff g         = dfs g (vertices g)

dfs          :: Graph -> [Vertex] -> Forest Vertex
dfs g vs      = prune (bounds g) (map (generate g) vs)

generate     :: Graph -> Vertex -> Tree Vertex
generate g v  = Node v (map (generate g) (g!v))

prune        :: Bounds -> Forest Vertex -> Forest Vertex
prune bnds ts = runST (mkEmpty bnds  >>= \m ->
                       chop m ts)

chop         :: Set s -> Forest Vertex -> ST s (Forest Vertex)
chop m []     = return []
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
%*									*
%*	Algorithms
%*									*
%************************************************************************

------------------------------------------------------------
-- Algorithm 1: depth first search numbering
------------------------------------------------------------

\begin{code}
--preorder            :: Tree a -> [a]
preorder (Node a ts) = a : preorderF ts

preorderF           :: Forest a -> [a]
preorderF ts         = concat (map preorder ts)

tabulate        :: Bounds -> [Vertex] -> Table Int
tabulate bnds vs = array bnds (zipWith (,) vs [1..])

preArr          :: Bounds -> Forest Vertex -> Table Int
preArr bnds      = tabulate bnds . preorderF
\end{code}


------------------------------------------------------------
-- Algorithm 2: topological sorting
------------------------------------------------------------

\begin{code}
--postorder :: Tree a -> [a]
postorder (Node a ts) = postorderF ts ++ [a]

postorderF   :: Forest a -> [a]
postorderF ts = concat (map postorder ts)

postOrd      :: Graph -> [Vertex]
postOrd       = postorderF . dff

topSort      :: Graph -> [Vertex]
topSort       = reverse . postOrd
\end{code}


------------------------------------------------------------
-- Algorithm 3: connected components
------------------------------------------------------------

\begin{code}
components   :: Graph -> Forest Vertex
components    = dff . undirected

undirected   :: Graph -> Graph
undirected g  = buildG (bounds g) (edges g ++ reverseE g)
\end{code}


-- Algorithm 4: strongly connected components

\begin{code}
scc  :: Graph -> Forest Vertex
scc g = dfs g (reverse (postOrd (transposeG g)))
\end{code}


------------------------------------------------------------
-- Algorithm 5: Classifying edges
------------------------------------------------------------

\begin{code}
back              :: Graph -> Table Int -> Graph
back g post        = mapT select g
 where select v ws = [ w | w <- ws, post!v < post!w ]

cross             :: Graph -> Table Int -> Table Int -> Graph
cross g pre post   = mapT select g
 where select v ws = [ w | w <- ws, post!v > post!w, pre!v > pre!w ]

forward           :: Graph -> Graph -> Table Int -> Graph
forward g tree pre = mapT select g
 where select v ws = [ w | w <- ws, pre!v < pre!w ] \\ tree!v
\end{code}


------------------------------------------------------------
-- Algorithm 6: Finding reachable vertices
------------------------------------------------------------

\begin{code}
reachable    :: Graph -> Vertex -> [Vertex]
reachable g v = preorderF (dfs g [v])

path         :: Graph -> Vertex -> Vertex -> Bool
path g v w    = w `elem` (reachable g v)
\end{code}


------------------------------------------------------------
-- Algorithm 7: Biconnected components
------------------------------------------------------------

\begin{code}
bcc :: Graph -> Forest [Vertex]
bcc g = (concat . map bicomps . map (do_label g dnum)) forest
 where forest = dff g
       dnum   = preArr (bounds g) forest

do_label :: Graph -> Table Int -> Tree Vertex -> Tree (Vertex,Int,Int)
do_label g dnum (Node v ts) = Node (v,dnum!v,lv) us
 where us = map (do_label g dnum) ts
       lv = minimum ([dnum!v] ++ [dnum!w | w <- g!v]
                     ++ [lu | Node (u,du,lu) xs <- us])

bicomps :: Tree (Vertex,Int,Int) -> Forest [Vertex]
bicomps (Node (v,dv,lv) ts)
      = [ Node (v:vs) us | (l,Node vs us) <- map collect ts]

collect :: Tree (Vertex,Int,Int) -> (Int, Tree [Vertex])
collect (Node (v,dv,lv) ts) = (lv, Node (v:vs) cs)
 where collected = map collect ts
       vs = concat [ ws | (lw, Node ws us) <- collected, lw<dv]
       cs = concat [ if lw<dv then us else [Node (v:ws) us]
                        | (lw, Node ws us) <- collected ]
\end{code}

