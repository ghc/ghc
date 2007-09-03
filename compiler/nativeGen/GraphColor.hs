
-- | Graph Coloring.
--	This is a generic graph coloring library, abstracted over the type of
--	the node keys, nodes and colors.
--

{-# OPTIONS_GHC -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/WorkingConventions#Warnings
-- for details

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
		= coalesceGraph graph0

 	-- run the scanner to slurp out all the trivially colorable nodes
  	(ksTriv, ksProblems)
		= colorScan colors triv spill [] emptyUniqSet graph_coalesced
 
	-- color the trivially colorable nodes
	(graph_triv, ksNoTriv)
		= assignColors colors graph_coalesced ksTriv

 	-- try and color the problem nodes
	(graph_prob, ksNoColor)	= assignColors colors graph_triv (uniqSetToList ksProblems)

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

 where	assignColors' colors graph prob []
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
--	TODO: avoid using the prefs of the neighbors, if at all possible.
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
	
	-- colors that are still ok
	colors_ok_ex	= minusUniqSet colors_avail (nodeExclusions node)
	colors_ok	= minusUniqSet colors_ok_ex colors_conflict
				
	-- the colors that we prefer, and are still ok
	colors_ok_pref	= intersectUniqSets
				(mkUniqSet $ nodePreference node) colors_ok
				
	-- make the decision
	chooseColor

		-- we got one of our preferences, score!
		| not $ isEmptyUniqSet colors_ok_pref	
		, c : rest	<- uniqSetToList colors_ok_pref
		= Just c
		
		-- it wasn't a preference, but it was still ok
		| not $ isEmptyUniqSet colors_ok
		, c : rest	<- uniqSetToList colors_ok
		= Just c
		
		-- leave this node uncolored
		| otherwise
		= Nothing
		
   in	chooseColor 



