module ShouldSucceed where

-- import TheUtils
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (partition )

data Digraph vertex = MkDigraph [vertex]

type Edge  vertex = (vertex, vertex)
type Cycle vertex = [vertex]

mkDigraph = MkDigraph

stronglyConnComp :: Eq vertex => [Edge vertex] -> [vertex] -> [[vertex]]
stronglyConnComp es vs
  = snd (span_tree (new_range reversed_edges)
		    ([],[])
                   ( snd (dfs (new_range es) ([],[]) vs) )
	 )
 where
   reversed_edges = map swap es

   swap :: Edge v -> Edge v
   swap (x,y) = (y, x)

   new_range    []       w = []
   new_range ((x,y):xys) w
	= if x==w
	  then (y : (new_range xys w))
	  else (new_range xys w)

   span_tree r (vs,ns) []   = (vs,ns)
   span_tree r (vs,ns) (x:xs)
	| x `elem` vs = span_tree r (vs,ns) xs
	| otherwise = span_tree r (vs',(x:ns'):ns) xs
	  where
	    (vs',ns') = dfs r (x:vs,[]) (r x)

dfs r (vs,ns)   []   = (vs,ns)
dfs r (vs,ns) (x:xs) | x `elem` vs = dfs r (vs,ns) xs
                     | otherwise = dfs r (vs',(x:ns')++ns) xs
                                   where
                                     (vs',ns') = dfs r (x:vs,[]) (r x)


isCyclic :: Eq vertex => [Edge vertex] -> [vertex] -> Bool
isCyclic edges [v] = (v,v) `elem` edges
isCyclic edges vs = True


topSort :: (Eq vertex) => [Edge vertex] -> [vertex]
              -> MaybeErr [vertex] [[vertex]]


topSort edges vertices
 = case cycles of
	[] -> Succeeded [v | [v] <- singletons]
	_  -> Failed cycles
   where
   sccs = stronglyConnComp edges vertices
   (cycles, singletons) = partition (isCyclic edges) sccs


type FlattenedDependencyInfo vertex name code
   = [(vertex, Set name, Set name, code)]

mkVertices :: FlattenedDependencyInfo vertex name code -> [vertex]
mkVertices info = [ vertex | (vertex,_,_,_) <- info]

mkEdges :: (Eq vertex, Ord name) =>
	    [vertex]
	 -> FlattenedDependencyInfo vertex name code
	 -> [Edge vertex]

mkEdges vertices flat_info
 = [ (source_vertex, target_vertex)
   | (source_vertex, _, used_names, _) <- flat_info,
     target_name   <- Set.toList used_names,
     target_vertex <- vertices_defining target_name flat_info
   ]
 where
   vertices_defining name flat_info
    = [ vertex |  (vertex, names_defined, _, _) <- flat_info,
   		name `Set.member` names_defined
      ]

lookupVertex :: (Eq vertex, Ord name) =>
	    	 FlattenedDependencyInfo vertex name code
	      -> vertex
	      -> code

lookupVertex flat_info vertex
 = head code_list
 where
   code_list = [ code | (vertex',_,_,code) <- flat_info, vertex == vertex']


isRecursiveCycle :: (Eq vertex) => Cycle vertex -> [Edge vertex] -> Bool
isRecursiveCycle [vertex] edges = (vertex, vertex) `elem` edges
isRecursiveCycle cycle    edges = True



-- may go to TheUtils

data MaybeErr a b = Succeeded a | Failed b

