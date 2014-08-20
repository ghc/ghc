
-- | Pretty printing of graphs.

{-# OPTIONS_GHC -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module GraphPpr (
	dumpGraph,
	dotGraph
)
where

import GraphBase

import Outputable
import Unique
import UniqSet
import UniqFM

import Data.List
import Data.Maybe


-- | Pretty print a graph in a somewhat human readable format.
dumpGraph 
	:: (Outputable k, Outputable cls, Outputable color)
	=> Graph k cls color -> SDoc

dumpGraph graph
	=  text "Graph"
	$$ (vcat $ map dumpNode $ eltsUFM $ graphMap graph)
	
dumpNode 
	:: (Outputable k, Outputable cls, Outputable color)
	=> Node k cls color -> SDoc

dumpNode node
 	=  text "Node " <> ppr (nodeId node)
	$$ text "conflicts " 
		<> parens (int (sizeUniqSet $ nodeConflicts node)) 
		<> text " = "
		<> ppr (nodeConflicts node) 

	$$ text "exclusions "
		<> parens (int (sizeUniqSet $ nodeExclusions node))
		<> text " = " 
		<> ppr (nodeExclusions node)

	$$ text "coalesce "
		<> parens (int (sizeUniqSet $ nodeCoalesce node))
		<> text " = "
		<> ppr (nodeCoalesce node)
		
	$$ space



-- | Pretty print a graph in graphviz .dot format.
--	Conflicts get solid edges.
--	Coalescences get dashed edges.
dotGraph 
	:: ( Uniquable k
	   , Outputable k, Outputable cls, Outputable color)
	=> (color -> SDoc)  -- ^ What graphviz color to use for each node color
			    --	It's usually safe to return X11 style colors here,
			    --	ie "red", "green" etc or a hex triplet #aaff55 etc
	-> Triv k cls color
	-> Graph k cls color -> SDoc

dotGraph colorMap triv graph
 = let	nodes	= eltsUFM $ graphMap graph
   in	vcat 
		(  [ text "graph G {" ]
		++ map (dotNode colorMap triv) nodes
		++ (catMaybes $ snd $ mapAccumL dotNodeEdges emptyUniqSet nodes)
		++ [ text "}"
		   , space ])
	

dotNode :: ( Uniquable k
	   , Outputable k, Outputable cls, Outputable color)
	=> (color -> SDoc)
	-> Triv k cls color
	-> Node k cls color -> SDoc
	
dotNode colorMap triv node
 = let	name	= ppr $ nodeId node
	cls	= ppr $ nodeClass node

	excludes
		= hcat $ punctuate space 
		$ map (\n -> text "-" <> ppr n)
		$ uniqSetToList $ nodeExclusions node
		
	preferences
		= hcat $ punctuate space
		$ map (\n -> text "+" <> ppr n)
		$ nodePreference node
	
	expref	= if and [isEmptyUniqSet (nodeExclusions node), null (nodePreference node)]
			then empty
			else text "\\n" <> (excludes <+> preferences)

	-- if the node has been colored then show that,
	--	otherwise indicate whether it looks trivially colorable.
	color
		| Just c	<- nodeColor node 
		= text "\\n(" <> ppr c <> text ")"

		| triv (nodeClass node) (nodeConflicts node) (nodeExclusions node)
		= text "\\n(" <> text "triv" <> text ")"

		| otherwise
		= text "\\n(" <> text "spill?" <> text ")"

	label	=  name <> text " :: " <> cls
		<> expref
		<> color

	pcolorC	= case nodeColor node of
			Nothing	-> text "style=filled fillcolor=white"
			Just c	-> text "style=filled fillcolor=" <> doubleQuotes (colorMap c)
		

	pout	= text "node [label=" <> doubleQuotes label <> space <> pcolorC <> text "]" 
		<> space <> doubleQuotes name 
		<> text ";"

 in	pout


-- | Nodes in the graph are doubly linked, but we only want one edge for each
--	conflict if the graphviz graph. Traverse over the graph, but make sure
--	to only print the edges for each node once.

dotNodeEdges 
	:: ( Uniquable k
	   , Outputable k, Outputable cls, Outputable color)
	=> UniqSet k
	-> Node k cls color
	-> (UniqSet k, Maybe SDoc)

dotNodeEdges visited node
	| elementOfUniqSet (nodeId node) visited
	= ( visited
	  , Nothing)
	
	| otherwise
	= let	dconflicts
			= map (dotEdgeConflict (nodeId node)) 
			$ uniqSetToList
			$ minusUniqSet (nodeConflicts node) visited
				
		dcoalesces
			= map (dotEdgeCoalesce (nodeId node))
			$ uniqSetToList
			$ minusUniqSet (nodeCoalesce node) visited
	
		out	=  vcat dconflicts
			$$ vcat dcoalesces

	  in	( addOneToUniqSet visited (nodeId node)
		, Just out)

	where	dotEdgeConflict u1 u2
			= doubleQuotes (ppr u1) <> text " -- " <> doubleQuotes (ppr u2) 
			<> text ";"

		dotEdgeCoalesce u1 u2
			= doubleQuotes (ppr u1) <> text " -- " <> doubleQuotes (ppr u2) 
			<> space <> text "[ style = dashed ];"

