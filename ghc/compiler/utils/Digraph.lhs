%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Digraph]{An implementation of directed graphs}

\begin{code}
#include "HsVersions.h"

module Digraph (
	 stronglyConnComp,
	 topologicalSort,
	 dfs,
	 MaybeErr,

	 -- alternative interface
	 findSCCs, SCC(..), Bag
    ) where

CHK_Ubiq() -- debugging consistency check
IMPORT_1_3(List(partition))

import Maybes		( MaybeErr(..), maybeToBool )
import Bag		( Bag, filterBag, bagToList, listToBag )
import FiniteMap	( FiniteMap, listToFM, lookupFM, lookupWithDefaultFM )
import Unique		( Unique )
import Util
\end{code}

This module implements at least part of an abstract data type for
directed graphs.  The part implemented is what we need for doing
dependency analyses.

>type Edge  vertex = (vertex, vertex)
>type Cycle vertex = [vertex]

%************************************************************************
%*									*
%*	Strongly connected components					*
%*									*
%************************************************************************

John Launchbury provided the basic code for doing strongly-connected
components.

The result is a list of cycles (each of which is a list of vertices),
and these cycles are topologically sorted, so that if there is an edge from
cycle A to cycle B, then A occurs after B in the result list.

\begin{code}
stronglyConnComp :: (vertex->vertex->Bool) -> [Edge vertex] -> [vertex] -> [[vertex]]

stronglyConnComp eq edges vertices
  = snd (span_tree (new_range reversed_edges)
		    ([],[])
		   ( snd (dfs (new_range edges) ([],[]) vertices) )
	)
  where
    reversed_edges = map swap edges

    swap (x,y) = (y,x)

    -- new_range :: Eq v => [Edge v] -> v -> [v]

    new_range    []       w = []
    new_range ((x,y):xys) w
	 = if x `eq` w
	   then (y : (new_range xys w))
	   else (new_range xys w)

    elem x []	  = False
    elem x (y:ys) = x `eq` y || x `elem` ys

{-  span_tree :: Eq v => (v -> [v])
		      -> ([v], [[v]])
		      -> [v]
		      -> ([v], [[v]])
-}
    span_tree r (vs,ns) []   = (vs,ns)
    span_tree r (vs,ns) (x:xs)
	 | x `elem` vs = span_tree r (vs,ns) xs
	 | True	       = case (dfs r (x:vs,[]) (r x)) of { (vs',ns') ->
			 span_tree r (vs',(x:ns'):ns) xs }

{-  dfs :: Eq v => (v -> [v])
		-> ([v], [v])
		-> [v]
		-> ([v], [v])
-}
    dfs r (vs,ns)   []   = (vs,ns)
    dfs r (vs,ns) (x:xs) | x `elem` vs = dfs r (vs,ns) xs
			 | True        = case (dfs r (x:vs,[]) (r x)) of { (vs',ns') ->
					 dfs r (vs',(x:ns')++ns) xs }
\end{code}

\begin{code}
dfs :: (v -> v -> Bool)
    -> (v -> [v])
    -> ([v], [v])
    -> [v]
    -> ([v], [v])

dfs eq r (vs,ns)   []   = (vs,ns)
dfs eq r (vs,ns) (x:xs)
    	| any (eq x) vs = dfs eq r (vs,ns) xs
	| True          = case (dfs eq r (x:vs,[]) (r x)) of
				(vs',ns') -> dfs eq r (vs',(x:ns')++ns) xs
\end{code}

\begin{code}
{-# SPECIALIZE findSCCs :: (a -> (Unique, Bag Unique)) -> Bag a -> [SCC a] #-}

findSCCs :: Ord key
	 => (vertex -> (key, Bag key))	-- Give key of vertex, and keys of thing's
					-- immediate neighbours.  It's ok for the
					-- list to contain keys which don't correspond
					-- to any vertex; they are ignored.
	 -> Bag vertex		-- Stuff to be SCC'd
	 -> [SCC vertex]	-- The union of all these is the original bag

data SCC thing = AcyclicSCC thing
	       | CyclicSCC  (Bag thing)

findSCCs v_info vs
  = let
        (keys, keys_of, edgess) = unzip3 (map do_vertex (bagToList vs))
	key_map = listToFM keys_of
	edges   = concat edgess

	do_vertex v = (k, (k, (v, ok_ns)), ok_edges)
	  where
	    (k, ns)  = v_info v
	    ok_ns    = filter key_in_graph (bagToList ns)
	    ok_edges = map (\n->(k,n)) ok_ns

	key_in_graph n = maybeToBool (lookupFM key_map n)

	the_sccs = stronglyConnComp (==) edges keys 

	cnv_sccs = map cnv_scc the_sccs 

	cnv_scc []  = panic "findSCCs: empty component"
	cnv_scc [k] | singlecycle k
		    = AcyclicSCC (get_vertex k)
        cnv_scc ks  = CyclicSCC (listToBag (map get_vertex ks))

	singlecycle k = not (isIn "cycle" k (get_neighs k))

	get_vertex k = fst (lookupWithDefaultFM key_map vpanic k)
	get_neighs k = snd (lookupWithDefaultFM key_map vpanic k)

	vpanic = panic "Digraph: vertix not found from key"
    in
    cnv_sccs
\end{code}

%************************************************************************
%*									*
%*	Topological sort						*
%*									*
%************************************************************************

Topological sort fails if it finds any cycles, returning the offending cycles.

If it succeeds, the result is a list of vertices, such that if there is
an edge from vertex A to vertex B then A occurs after B in the result list.

\begin{code}
topologicalSort :: (vertex->vertex->Bool) -> [Edge vertex] -> [vertex]
	-> MaybeErr [vertex] 	-- Success: the sorted list
		    [[vertex]]	-- Failure: the cycles

topologicalSort eq edges vertices
  = case (stronglyConnComp eq edges vertices) of { sccs ->
    case (partition (is_cyclic edges) sccs)   of { (cycles, singletons) ->
    if null cycles
    then Succeeded [ v | [v] <- singletons ]
    else Failed cycles
    }}
  where
    is_cyclic es []  = panic "is_cyclic: empty component"
    is_cyclic es [v] = (v,v) `elem` es
    is_cyclic es vs  = True

    elem (x,y)   []	    = False
    elem z@(x,y) ((a,b):cs) = (x `eq` a && y `eq` b) || z `elem` cs
\end{code}
