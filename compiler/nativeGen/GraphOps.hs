
-- | Basic operations on graphs.
--
module GraphOps (
	addNode, 	delNode,	getNode,	lookupNode,	modNode,
	size,
	union,
	addConflict,	delConflict,	addConflicts,
	addCoalesce,	delCoalesce,	
	addExclusion,	
	addPreference,
	setColor,
	verify
)
where

import GraphBase

import Outputable
import Unique
import UniqSet
import UniqFM

import Data.List	hiding (union)
import Data.Maybe


-- | Lookup a node from the graph.
lookupNode 
	:: Uniquable k
	=> Graph k cls color
	-> k -> Maybe (Node  k cls color)

lookupNode graph k	
	= lookupUFM (graphMap graph) k


-- | Get a node from the graph, throwing an error if it's not there
getNode
	:: Uniquable k
	=> Graph k cls color
	-> k -> Node k cls color

getNode graph k
 = case lookupUFM (graphMap graph) k of
	Just node	-> node
	Nothing		-> panic "ColorOps.getNode: not found" 


-- | Add a node to the graph, linking up its edges
addNode :: Uniquable k
	=> k -> Node k cls color 
	-> Graph k cls color -> Graph k cls color
	
addNode k node graph
 = let	
 	-- add back conflict edges from other nodes to this one
 	map_conflict	
		= foldUniqSet 
			(adjustUFM (\n -> n { nodeConflicts = addOneToUniqSet (nodeConflicts n) k}))
			(graphMap graph)
			(nodeConflicts node)
			
	-- add back coalesce edges from other nodes to this one
	map_coalesce
		= foldUniqSet
			(adjustUFM (\n -> n { nodeCoalesce = addOneToUniqSet (nodeCoalesce n) k}))
			map_conflict
			(nodeCoalesce node)
	
  in	graph
  	{ graphMap	= addToUFM map_coalesce k node}
		


-- | Delete a node and all its edges from the graph.
--	Throws an error if it's not there.
delNode :: Uniquable k
	=> k -> Graph k cls color -> Graph k cls color

delNode k graph
 = let	Just node	= lookupNode graph k

	-- delete conflict edges from other nodes to this one.
	graph1		= foldl' (\g k1 -> delConflict k1 k g) graph 
			$ uniqSetToList (nodeConflicts node)
	
	-- delete coalesce edge from other nodes to this one.
	graph2		= foldl' (\g k1 -> delCoalesce k1 k g) graph1 
			$ uniqSetToList (nodeCoalesce node)
	
	-- delete the node
	graph3		= graphMapModify (\fm -> delFromUFM fm k) graph2
	
  in	graph3
		

-- | Modify a node in the graph
modNode :: Uniquable k
	=> (Node k cls color -> Node k cls color) 
	-> k -> Graph k cls color -> Graph k cls color

modNode f k graph
 = case getNode graph k of
 	Node{} -> graphMapModify
		 (\fm	-> let	Just node	= lookupUFM fm k
			   	node'		= f node
			   in	addToUFM fm k node') 
		graph


-- | Get the size of the graph, O(n)
size	:: Uniquable k 
	=> Graph k cls color -> Int
	
size graph	
	= sizeUFM $ graphMap graph
	

-- | Union two graphs together.
union	:: Uniquable k
	=> Graph k cls color -> Graph k cls color -> Graph k cls color
	
union	graph1 graph2
	= Graph 
	{ graphMap		= plusUFM (graphMap graph1) (graphMap graph2) }
	 
	


-- | Add a conflict between nodes to the graph, creating the nodes required.
--	Conflicts are virtual regs which need to be colored differently.
addConflict
	:: Uniquable k
	=> (k, cls) -> (k, cls) 
	-> Graph k cls color -> Graph k cls color

addConflict (u1, c1) (u2, c2)
 = let	addNeighbor u c u'
	 	= adjustWithDefaultUFM
			(\node -> node { nodeConflicts = addOneToUniqSet (nodeConflicts node) u' })
			(newNode u c)  { nodeConflicts = unitUniqSet u' }
			u
	
   in	graphMapModify
   	( addNeighbor u1 c1 u2 
	. addNeighbor u2 c2 u1)

 
-- | Delete a conflict edge. k1 -> k2
delConflict 
	:: Uniquable k
	=> k -> k
	-> Graph k cls color -> Graph k cls color
	
delConflict k1 k2
	= modNode
		(\node -> node { nodeConflicts = delOneFromUniqSet (nodeConflicts node) k2 })
		k1


-- | Add some conflicts to the graph, creating nodes if required.
--	All the nodes in the set are taken to conflict with each other.
addConflicts
	:: Uniquable k
	=> UniqSet k -> (k -> cls)
	-> Graph k cls color -> Graph k cls color
	
addConflicts conflicts getClass

	-- just a single node, but no conflicts, create the node anyway.
	| (u : [])	<- uniqSetToList conflicts
	= graphMapModify 
	$ adjustWithDefaultUFM 
		id
		(newNode u (getClass u)) 
		u

	| otherwise
	= graphMapModify
	$ (\fm -> foldr	(\u -> addConflictSet1 u getClass conflicts) fm
		$ uniqSetToList conflicts)


addConflictSet1 u getClass set 
 = let	set'	= delOneFromUniqSet set u
   in	adjustWithDefaultUFM 
		(\node -> node 			{ nodeConflicts = unionUniqSets set' (nodeConflicts node) } )
		(newNode u (getClass u))	{ nodeConflicts = set' }
		u


-- | Add an exclusion to the graph, creating nodes if required.
--	These are extra colors that the node cannot use.
addExclusion
	:: (Uniquable k, Uniquable color)
	=> k -> (k -> cls) -> color 
	-> Graph k cls color -> Graph k cls color
	
addExclusion u getClass color 
 	= graphMapModify
	$ adjustWithDefaultUFM 
		(\node -> node 			{ nodeExclusions = addOneToUniqSet (nodeExclusions node) color })
		(newNode u (getClass u))  	{ nodeExclusions = unitUniqSet color }
		u


-- | Add a coalescence edge to the graph, creating nodes if requried.
--	It is considered adventageous to assign the same color to nodes in a coalesence.
addCoalesce 
	:: Uniquable k
	=> (k, cls) -> (k, cls) 
	-> Graph k cls color -> Graph k cls color
	
addCoalesce (u1, c1) (u2, c2) 
 = let	addCoalesce u c u'
 	 = 	adjustWithDefaultUFM
	 		(\node -> node { nodeCoalesce = addOneToUniqSet (nodeCoalesce node) u' })
			(newNode u c)  { nodeCoalesce = unitUniqSet u' }
			u
			
   in	graphMapModify
     	( addCoalesce u1 c1 u2
        . addCoalesce u2 c2 u1)


-- | Delete a coalescence edge (k1 -> k2) from the graph.
delCoalesce
	:: Uniquable k
	=> k -> k 
	-> Graph k cls color	-> Graph k cls color

delCoalesce k1 k2
	= modNode (\node -> node { nodeCoalesce = delOneFromUniqSet (nodeCoalesce node) k2 })
		k1


-- | Add a color preference to the graph, creating nodes if required.
--	The most recently added preference is the most prefered.
--	The algorithm tries to assign a node it's prefered color if possible.
--
addPreference 
	:: Uniquable k
	=> (k, cls) -> color
	-> Graph k cls color -> Graph k cls color
	
addPreference (u, c) color 
 	= graphMapModify
	$ adjustWithDefaultUFM 
		(\node -> node { nodePreference = color : (nodePreference node) })
		(newNode u c)  { nodePreference = [color] }
		u

		
-- | Verify the internal structure of a graph
--	all its edges should point to valid nodes
--
verify 	:: Uniquable k 
	=> Graph k cls color
	-> Bool

verify graph
 = let	edges	= unionUniqSets
 			(unionManyUniqSets
 				(map nodeConflicts $ eltsUFM $ graphMap graph))
			(unionManyUniqSets
				(map nodeCoalesce  $ eltsUFM $ graphMap graph))
				
	nodes	= mkUniqSet $ map nodeId $ eltsUFM $ graphMap graph
	
	badEdges = minusUniqSet edges nodes
	
  in	if isEmptyUniqSet badEdges 
  	 then 	True
	 else	False


-- | Set the color of a certain node
setColor 
	:: Uniquable k
	=> k -> color
	-> Graph k cls color -> Graph k cls color
	
setColor u color
 	= graphMapModify
 	$ adjustUFM
		(\n -> n { nodeColor = Just color })
		u 
	

adjustWithDefaultUFM 
	:: Uniquable k 
	=> (a -> a) -> a -> k 
	-> UniqFM a -> UniqFM a

adjustWithDefaultUFM f def k map
	= addToUFM_C 
		(\old new -> f old)
		map
		k def
		

adjustUFM 
	:: Uniquable k
	=> (a -> a)
	-> k -> UniqFM a -> UniqFM a

adjustUFM f k map
 = case lookupUFM map k of
 	Nothing	-> map
	Just a	-> addToUFM map k (f a)
	

