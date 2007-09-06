
-- | Graph Coloring.
--	This is a generic graph coloring library, abstracted over the type of
--	the node keys, nodes and colors.
--
{-# OPTIONS -fno-warn-missing-signatures #-}

module GraphColor ( 
	module GraphBase,
	module GraphOps,
	module GraphPpr,
	colorGraph
)

where

import GraphBase
import GraphOps
import GraphPpr

import Unique
import UniqFM
import UniqSet
import Outputable	

import Data.Maybe
import Data.List
	

-- | Try to color a graph with this set of colors.
--	Uses Chaitin's algorithm to color the graph.
--	The graph is scanned for nodes which are deamed 'trivially colorable'. These nodes
--	are pushed onto a stack and removed from the graph.
--	Once this process is complete the graph can be colored by removing nodes from
--	the stack (ie in reverse order) and assigning them colors different to their neighbors.
--
colorGraph
	:: ( Uniquable  k, Uniquable cls,  Uniquable  color
	   , Eq color, Eq cls, Ord k
	   , Outputable k, Outputable cls, Outputable color)
	=> UniqFM (UniqSet color)	-- ^ map of (node class -> set of colors available for this class).
	-> Triv   k cls color 		-- ^ fn to decide whether a node is trivially colorable.
	-> (Graph k cls color -> k)	-- ^ fn to choose a node to potentially leave uncolored if nothing is trivially colorable.
	-> Graph  k cls color 		-- ^ the graph to color.

	-> ( Graph k cls color 		-- the colored graph.
	   , UniqSet k			-- the set of nodes that we couldn't find a color for.
	   , UniqFM  k )		-- map of regs (r1 -> r2) that were coaleced
	   				--	 r1 should be replaced by r2 in the source

colorGraph colors triv spill graph0
 = let
 	-- do aggressive coalesing on the graph
 	(graph_coalesced, rsCoalesce)
		= coalesceGraph triv graph0

 	-- run the scanner to slurp out all the trivially colorable nodes
  	(ksTriv, ksProblems)
		= colorScan triv spill graph_coalesced
 
	-- color the trivially colorable nodes
	--	as the keys were added to the front of the list while they were scanned,
	--	this colors them in the reverse order they were found, as required by the algorithm.
	(graph_triv, ksNoTriv)
		= assignColors colors graph_coalesced ksTriv

 	-- try and color the problem nodes
	(graph_prob, ksNoColor)	= assignColors colors graph_triv ksProblems

	-- if the trivially colorable nodes didn't color then something is wrong
	--	with the provided triv function.
   in	if not $ null ksNoTriv
   	 then	pprPanic "colorGraph: trivially colorable nodes didn't color!" empty
{-	 		(  empty
			$$ text "ksTriv    = " <> ppr ksTriv
			$$ text "ksNoTriv  = " <> ppr ksNoTriv
			$$ empty
			$$ dotGraph (\x -> text "white") triv graph1) -}

	 else	( graph_prob
		, mkUniqSet ksNoColor
		, listToUFM rsCoalesce)
	

-- | Scan through the conflict graph separating out trivially colorable and
--	potentially uncolorable (problem) nodes.
--
--	Checking whether a node is trivially colorable or not is a resonably expensive operation,
--	so after a triv node is found and removed from the graph it's no good to return to the 'start'
--	of the graph and recheck a bunch of nodes that will probably still be non-trivially colorable.
--
--	To ward against this, during each pass through the graph we collect up a list of triv nodes
--	that were found, and only remove them once we've finished the pass. The more nodes we can delete
--	at once the more likely it is that nodes we've already checked will become trivially colorable
--	for the next pass.
--
colorScan
	:: ( Uniquable k, Uniquable cls, Uniquable color)
	=> Triv k cls color		-- ^ fn to decide whether a node is trivially colorable
	-> (Graph k cls color -> k)	-- ^ fn to choose a node to potentially leave uncolored if nothing is trivially colorable.
	-> Graph k cls color		-- ^ the graph to scan
	-> ([k], [k])			--  triv colorable, problem nodes


colorScan triv spill graph
	= colorScan' triv spill graph
		[]	[]
		[]
		(eltsUFM $ graphMap graph)

-- we've reached the end of the candidates list
colorScan' triv spill graph
	ksTriv 	ksTrivFound
	ksSpill
	[]

	-- if the graph is empty then we're done
	| isNullUFM $ graphMap graph
	= (ksTrivFound ++ ksTriv, ksSpill)

	-- if we haven't found a trivially colorable node then we'll have to
	--	choose a spill candidate and leave it uncolored
	| []		<- ksTrivFound
	, kSpill	<- spill graph			-- choose a spill candiate
	, graph'	<- delNode kSpill graph		-- remove it from the graph
	, nsRest'	<- eltsUFM $ graphMap graph'	-- graph has changed, so get new node list

	= colorScan' triv spill graph'
		ksTriv ksTrivFound
		(kSpill : ksSpill)
		nsRest'

	-- we're at the end of the candidates list but we've found some triv nodes
	--	along the way. We can delete them from the graph and go back for more.
	| graph'	<- foldr delNode graph ksTrivFound
	, nsRest'	<- eltsUFM $ graphMap graph'

	= colorScan' triv spill graph'
		(ksTrivFound ++ ksTriv) []
		ksSpill
		nsRest'

-- check if the current node is triv colorable
colorScan' triv spill graph
	ksTriv	ksTrivFound
	ksSpill
	(node : nsRest)

	-- node is trivially colorable
	--	add it to the found nodes list and carry on.
	| k	<- nodeId node
	, triv (nodeClass node) (nodeConflicts node) (nodeExclusions node)

	= colorScan' triv spill graph
		ksTriv 	(k : ksTrivFound)
		ksSpill
		nsRest

	-- node wasn't trivially colorable, skip over it and look in the rest of the list
	| otherwise
	= colorScan' triv spill graph
		ksTriv ksTrivFound
		ksSpill
		nsRest

{- -- This is cute and easy to understand, but too slow.. BL 2007/09

colorScan colors triv spill safe prob graph

	-- empty graphs are easy to color.
	| isNullUFM $ graphMap graph
 	= (safe, prob)
	
	-- Try and find a trivially colorable node.
	| Just node	<- find (\node -> triv 	(nodeClass node) 
						(nodeConflicts node)
						(nodeExclusions node))
				$ eltsUFM $ graphMap graph
	, k		<- nodeId node
	= colorScan colors triv spill
		(k : safe) prob (delNode k graph)
	
	-- There was no trivially colorable node,
	--	Choose one to potentially leave uncolored. We /might/ be able to find
	--	a color for this later on, but no guarantees.
	| k		<- spill graph
	= colorScan colors triv spill
		safe (addOneToUniqSet prob k) (delNode k graph)
-}


-- | Try to assign a color to all these nodes.

assignColors 
	:: ( Uniquable k, Uniquable cls, Uniquable color, Eq color )
	=> UniqFM (UniqSet color)	-- ^ map of (node class -> set of colors available for this class).
	-> Graph k cls color		-- ^ the graph
	-> [k]				-- ^ nodes to assign a color to.
	-> ( Graph k cls color		-- the colored graph
	   , [k])			-- the nodes that didn't color.

assignColors colors graph ks 
 	= assignColors' colors graph [] ks

 where	assignColors' _ graph prob []
		= (graph, prob)

	assignColors' colors graph prob (k:ks)
	 = case assignColor colors k graph of

		-- couldn't color this node
	 	Nothing		-> assignColors' colors graph (k : prob) ks

		-- this node colored ok, so do the rest
		Just graph'	-> assignColors' colors graph' prob ks


	assignColor colors u graph
		| Just c	<- selectColor colors graph u
		= Just (setColor u c graph)

		| otherwise
		= Nothing

	
	
-- | Select a color for a certain node
--	taking into account preferences, neighbors and exclusions.
--	returns Nothing if no color can be assigned to this node.
--
selectColor
	:: ( Uniquable k, Uniquable cls, Uniquable color, Eq color)
	=> UniqFM (UniqSet color)	-- ^ map of (node class -> set of colors available for this class).
	-> Graph k cls color		-- ^ the graph
	-> k				-- ^ key of the node to select a color for.
	-> Maybe color
	
selectColor colors graph u 
 = let	-- lookup the node
 	Just node	= lookupNode graph u

	-- lookup the available colors for the class of this node.
	Just colors_avail
			= lookupUFM colors (nodeClass node)

	-- find colors we can't use because they're already being used
	--	by a node that conflicts with this one.
	Just nsConflicts 	
			= sequence
			$ map (lookupNode graph)
			$ uniqSetToList 
			$ nodeConflicts node
		
	colors_conflict	= mkUniqSet 
			$ catMaybes 
			$ map nodeColor nsConflicts
	
	-- the prefs of our neighbors
	colors_neighbor_prefs
			= mkUniqSet
			$ concat $ map nodePreference nsConflicts

	-- colors that are still valid for us
	colors_ok_ex	= minusUniqSet colors_avail (nodeExclusions node)
	colors_ok	= minusUniqSet colors_ok_ex colors_conflict
				
	-- the colors that we prefer, and are still ok
	colors_ok_pref	= intersectUniqSets
				(mkUniqSet $ nodePreference node) colors_ok

	-- the colors that we could choose while being nice to our neighbors
	colors_ok_nice	= minusUniqSet
				colors_ok colors_neighbor_prefs

	-- the best of all possible worlds..
	colors_ok_pref_nice
			= intersectUniqSets
				colors_ok_nice colors_ok_pref

	-- make the decision
	chooseColor

		-- everyone is happy, yay!
		| not $ isEmptyUniqSet colors_ok_pref_nice
		, c : _		<- filter (\x -> elementOfUniqSet x colors_ok_pref_nice)
					(nodePreference node)
		= Just c

		-- we've got one of our preferences
		| not $ isEmptyUniqSet colors_ok_pref	
		, c : _		<- filter (\x -> elementOfUniqSet x colors_ok_pref)
					(nodePreference node)
		= Just c
		
		-- it wasn't a preference, but it was still ok
		| not $ isEmptyUniqSet colors_ok
		, c : _		<- uniqSetToList colors_ok
		= Just c
		
		-- no colors were available for us this time.
		--	looks like we're going around the loop again..
		| otherwise
		= Nothing
		
   in	chooseColor 



