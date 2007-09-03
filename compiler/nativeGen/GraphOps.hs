
-- | Basic operations on graphs.
--

{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module GraphOps (
	addNode, 	delNode,	getNode,	lookupNode,	modNode,
	size,
	union,
	addConflict,	delConflict,	addConflicts,
	addCoalesce,	delCoalesce,	
	addExclusion,	
	addPreference,
	coalesceGraph,
	coalesceNodes,
	setColor,
	validateGraph,
	slurpNodeConflictCount
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
	graph1		= foldl' (\g k1 -> let Just g' = delConflict k1 k g in g') graph
			$ uniqSetToList (nodeConflicts node)
	
	-- delete coalesce edge from other nodes to this one.
	graph2		= foldl' (\g k1 -> let Just g' = delCoalesce k1 k g in g') graph1
			$ uniqSetToList (nodeCoalesce node)
	
	-- delete the node
	graph3		= graphMapModify (\fm -> delFromUFM fm k) graph2
	
  in	graph3
		

-- | Modify a node in the graph.
--	returns Nothing if the node isn't present.
--
modNode :: Uniquable k
	=> (Node k cls color -> Node k cls color) 
	-> k -> Graph k cls color -> Maybe (Graph k cls color)

modNode f k graph
 = case lookupNode graph k of
 	Just Node{}
	 -> Just
	 $  graphMapModify
		 (\fm	-> let	Just node	= lookupUFM fm k
			   	node'		= f node
			   in	addToUFM fm k node') 
		graph

	Nothing	-> Nothing

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
--	returns Nothing if the node isn't in the graph
delConflict 
	:: Uniquable k
	=> k -> k
	-> Graph k cls color -> Maybe (Graph k cls color)
	
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
	-> Graph k cls color	-> Maybe (Graph k cls color)

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


-- | Do agressive coalescing on this graph.
--	returns	the new graph and the list of pairs of nodes that got coaleced together.
--	for each pair, the resulting node will have the least key and be second in the pair.
--
coalesceGraph
	:: (Uniquable k, Ord k, Eq cls, Outputable k)
	=> Triv k cls color
	-> Graph k cls color
	-> (Graph k cls color, [(k, k)])

coalesceGraph triv graph
 = let
 	-- find all the nodes that have coalescence edges
	cNodes	= filter (\node -> not $ isEmptyUniqSet (nodeCoalesce node))
		$ eltsUFM $ graphMap graph

	-- build a list of pairs of keys for node's we'll try and coalesce
	--	every pair of nodes will appear twice in this list
	--	ie [(k1, k2), (k2, k1) ... ]
	--	This is ok, GrapOps.coalesceNodes handles this and it's convenient for
	--	build a list of what nodes get coalesced together for later on.
	--
	cList	= [ (nodeId node1, k2)
			| node1	<- cNodes
			, k2	<- uniqSetToList $ nodeCoalesce node1 ]

	-- do the coalescing, returning the new graph and a list of pairs of keys
	--	that got coalesced together.
	(graph', mPairs)
		= mapAccumL (coalesceNodes False triv) graph cList

   in	(graph', catMaybes mPairs)


-- | Coalesce this pair of nodes unconditionally / agressively.
--	The resulting node is the one with the least key.
--
--	returns: Just 	 the pair of keys if the nodes were coalesced
--			 the second element of the pair being the least one
--
--		 Nothing if either of the nodes weren't in the graph

coalesceNodes
	:: (Uniquable k, Ord k, Eq cls, Outputable k)
	=> Bool			-- ^ If True, coalesce nodes even if this might make the graph
				--	less colorable (aggressive coalescing)
	-> Triv  k cls color
	-> Graph k cls color
	-> (k, k)		-- ^ keys of the nodes to be coalesced
	-> (Graph k cls color, Maybe (k, k))

coalesceNodes aggressive triv graph (k1, k2)
	| (kMin, kMax)	<- if k1 < k2
				then (k1, k2)
				else (k2, k1)

	-- the nodes being coalesced must be in the graph
	, Just nMin		<- lookupNode graph kMin
	, Just nMax		<- lookupNode graph kMax

	-- can't coalesce conflicting modes
	, not $ elementOfUniqSet kMin (nodeConflicts nMax)
	, not $ elementOfUniqSet kMax (nodeConflicts nMin)

	= coalesceNodes_merge aggressive triv graph kMin kMax nMin nMax

	-- don't do the coalescing after all
	| otherwise
	= (graph, Nothing)

coalesceNodes_merge aggressive triv graph kMin kMax nMin nMax

	-- sanity checks
	| nodeClass nMin /= nodeClass nMax
	= error "GraphOps.coalesceNodes: can't coalesce nodes of different classes."

	| not (isNothing (nodeColor nMin) && isNothing (nodeColor nMax))
	= error "GraphOps.coalesceNodes: can't coalesce colored nodes."

	---
	| otherwise
	= let
		-- the new node gets all the edges from its two components
		node	=
		 Node	{ nodeId		= kMin
			, nodeClass		= nodeClass nMin
			, nodeColor		= Nothing

			-- nodes don't conflict with themselves..
			, nodeConflicts
				= (unionUniqSets (nodeConflicts nMin) (nodeConflicts nMax))
					`delOneFromUniqSet` kMin
					`delOneFromUniqSet` kMax

			, nodeExclusions	= unionUniqSets (nodeExclusions nMin) (nodeExclusions nMax)
			, nodePreference	= nodePreference nMin ++ nodePreference nMax

			-- nodes don't coalesce with themselves..
			, nodeCoalesce
				= (unionUniqSets (nodeCoalesce nMin) (nodeCoalesce nMax))
					`delOneFromUniqSet` kMin
					`delOneFromUniqSet` kMax
			}

	  in	coalesceNodes_check aggressive triv graph kMin kMax node

coalesceNodes_check aggressive triv graph kMin kMax node

	-- Unless we're coalescing aggressively, if the result node is not trivially
	--	colorable then don't do the coalescing.
	| not aggressive
	, not $ triv (nodeClass node) (nodeConflicts node) (nodeExclusions node)
	= (graph, Nothing)

	| otherwise
	= let -- delete the old nodes from the graph and add the new one
		graph'	= addNode kMin node
			$ delNode kMin
			$ delNode kMax
			$ graph

	  in	(graph', Just (kMax, kMin))

		
-- | validate the internal structure of a graph
--	all its edges should point to valid nodes
--	if they don't then throw an error
--
validateGraph
	:: (Uniquable k, Outputable k)
	=> SDoc
	-> Graph k cls color
	-> Graph k cls color

validateGraph doc graph
 = let	edges	= unionUniqSets
 			(unionManyUniqSets
 				(map nodeConflicts $ eltsUFM $ graphMap graph))
			(unionManyUniqSets
				(map nodeCoalesce  $ eltsUFM $ graphMap graph))
				
	nodes	= mkUniqSet $ map nodeId $ eltsUFM $ graphMap graph
	
	badEdges = minusUniqSet edges nodes
	
  in	if isEmptyUniqSet badEdges 
  	 then 	graph
	 else	pprPanic "GraphOps.validateGraph"
	 	( text  "-- bad edges"
		$$ vcat (map ppr $ uniqSetToList badEdges)
		$$ text "----------------------------"
		$$ doc)


-- | Slurp out a map of how many nodes had a certain number of conflict neighbours

slurpNodeConflictCount
	:: Uniquable k
	=> Graph k cls color
	-> UniqFM (Int, Int)	-- ^ (conflict neighbours, num nodes with that many conflicts)

slurpNodeConflictCount graph
	= addListToUFM_C
		(\(c1, n1) (c2, n2) -> (c1, n1 + n2))
		emptyUFM
	$ map 	(\node
	 	  -> let count	= sizeUniqSet $ nodeConflicts node
		     in  (count, (count, 1)))
	$ eltsUFM
	$ graphMap graph


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
	

