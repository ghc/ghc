%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Digraph]{An implementation of directed graphs}

\begin{code}
module Digraph (
	 stronglyConnComp,
--OLD:	 whichCycle, -- MOVED: isCyclic,
	 topologicalSort,
	 dfs, -- deforester
	 MaybeErr
    ) where

import Maybes		( MaybeErr(..) )
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

    swap (x,y) = (y, x)

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
  

@isCyclic@ expects to be applied to an element of the result of a
stronglyConnComp; it tells whether such an element is a cycle.  The
answer is True if it is not a singleton, of course, but if it is a
singleton we have to look up in the edges to see if it refers to
itself.

\begin{code}
{- MOVED TO POINT OF SINGLE USE: RenameBinds4 (WDP 95/02)

isCyclic :: Eq vertex => [Edge vertex] -> [vertex] -> Bool

isCyclic edges [] = panic "isCyclic: empty component"
isCyclic edges [v] = (v,v) `is_elem` edges where { is_elem = isIn "isCyclic" }
isCyclic edges vs = True
-}
\end{code}

OLD: The following @whichCycle@ should be called only when the given
@vertex@ is known to be in one of the cycles. This isn't difficult to
achieve if the call follows the creation of the list of components by
@cycles@ (NB: strictness analyser) with all vertices of interest in
them.

>{- UNUSED:
>whichCycle :: Eq vertex => [Cycle vertex] -> vertex -> (Cycle vertex)
>whichCycle vss v = head [vs | vs <-vss, v `is_elem` vs] where { is_elem = isIn "whichCycle" }
>-}

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
