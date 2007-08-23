-- | Graph coloring register allocator.
--
-- TODO:
--	Live range splitting:
--		At the moment regs that are spilled are spilled for all time, even though
--		we might be able to allocate them a hardreg in different parts of the code.
--
--	As we're aggressively coalescing before register allocation proper we're not currently
--	using the coalescence information present in the graph.
--
--	The function that choosing the potential spills could be a bit cleverer.
--
--	Colors in graphviz graphs could be nicer.
--

module RegAllocColor ( 
	regAlloc,
	regDotColor
) 

where

import qualified GraphColor	as Color
import RegLiveness
import RegSpill
import RegAllocStats
import MachRegs
import MachInstrs
import RegCoalesce
import PprMach

import UniqSupply
import UniqSet
import UniqFM
import Bag
import Outputable

import Data.List
import Data.Maybe
import Control.Monad

-- | The maximum number of build/spill cycles we'll allow.
--	We should only need 3 or 4 cycles tops.
--	If we run for any longer than this we're probably in an infinite loop,
--	It's probably better just to bail out and report a bug at this stage.
maxSpinCount	:: Int
maxSpinCount	= 10


-- | The top level of the graph coloring register allocator.
--	
regAlloc
	:: UniqFM (UniqSet Reg)		-- ^ the registers we can use for allocation
	-> UniqSet Int			-- ^ the set of available spill slots.
	-> [LiveCmmTop]			-- ^ code annotated with liveness information.
	-> UniqSM 
		( [NatCmmTop]		-- ^ code with registers allocated.
		, [RegAllocStats] )	-- ^ stats for each stage of allocation
		
regAlloc regsFree slotsFree code
 = do
 	(code_final, debug_codeGraphs, graph_final)
		<- regAlloc_spin 0 trivColorable regsFree slotsFree [] code
	
	return	( code_final
		, reverse debug_codeGraphs )

regAlloc_spin (spinCount :: Int) triv regsFree slotsFree debug_codeGraphs code 
 = do
	-- check that we're not running off down the garden path.
	when (spinCount > maxSpinCount)
	 $ pprPanic "regAlloc_spin: max build/spill cycle count exceeded."
	 	(  text "It looks like the register allocator is stuck in an infinite loop."
		$$ text "max cycles  = " <> int maxSpinCount
	 	$$ text "regsFree    = " <> (hcat	$ punctuate space $ map (docToSDoc . pprUserReg)
						$ uniqSetToList $ unionManyUniqSets $ eltsUFM regsFree)
		$$ text "slotsFree   = " <> ppr (sizeUniqSet slotsFree))

 	-- build a conflict graph from the code.
	graph		<- buildGraph code

	-- build a map of how many instructions each reg lives for.
	--	this is lazy, it won't be computed unless we need to spill
	let fmLife	= plusUFMs_C (\(r1, l1) (r2, l2) -> (r1, l1 + l2))
			$ map lifetimeCount code

	-- record startup state
	let stat1	=
		if spinCount == 0
		 then	Just $ RegAllocStatsStart
		 	{ raLiveCmm	= code
			, raGraph	= graph
			, raLifetimes	= fmLife }
		 else	Nothing


	-- the function to choose regs to leave uncolored
	let spill	= chooseSpill_maxLife fmLife
	
	-- try and color the graph 
	let (graph_colored, rsSpill)	
			= Color.colorGraph regsFree triv spill graph

	-- see if we've found a coloring
	if isEmptyUniqSet rsSpill
	 then do
		-- patch the registers using the info in the graph
	 	let code_patched	= map (patchRegsFromGraph graph_colored) code
		let code_nat		= map stripLive code_patched
		
		-- record what happened in this stage for debugging
		let stat		=
			RegAllocStatsColored
			{ raGraph	= graph_colored
			, raPatchedCmm	= code_patched }

		return	( code_nat
			, [stat] ++ maybeToList stat1 ++ debug_codeGraphs
			, graph_colored)

	 else do
	 	-- spill the uncolored regs
		(code_spilled, slotsFree', spillStats)
			<- regSpill code slotsFree rsSpill
			
		-- recalculate liveness
		let code_nat	= map stripLive code_spilled
		code_relive	<- mapM regLiveness code_nat

		-- record what happened in this stage for debugging
		let stat	=
			RegAllocStatsSpill
			{ raGraph	= graph_colored
			, raSpillStats	= spillStats
			, raLifetimes	= fmLife
			, raSpilled	= code_spilled }
			    	
		-- try again
		regAlloc_spin (spinCount + 1) triv regsFree slotsFree' 
			([stat] ++ maybeToList stat1 ++ debug_codeGraphs)
			code_relive

 
-----
-- Simple maxconflicts isn't always good, because we
--	can naievely end up spilling vregs that only live for one or two instrs.
--	
{-
chooseSpill_maxConflicts
	:: Color.Graph Reg RegClass Reg
	-> Reg
	
chooseSpill_maxConflicts graph
 = let	node	= maximumBy 
 			(\n1 n2 -> compare 
 				(sizeUniqSet $ Color.nodeConflicts n1) 
				(sizeUniqSet $ Color.nodeConflicts n2))
 		$ eltsUFM $ Color.graphMap graph
		
   in	Color.nodeId node
-} 
   
-----
chooseSpill_maxLife
	:: UniqFM (Reg, Int)
	-> Color.Graph Reg RegClass Reg
	-> Reg

chooseSpill_maxLife life graph
 = let	node	= maximumBy (\n1 n2 -> compare (getLife n1) (getLife n2))
		$ eltsUFM $ Color.graphMap graph

	-- Orphan vregs die in the same instruction they are born in.
	--	They will be in the graph, but not in the liveness map.
	--	Their liveness is 0.
	getLife n
	 = case lookupUFM life (Color.nodeId n) of
	 	Just (_, l)	-> l
		Nothing		-> 0

   in	Color.nodeId node
   

-- | Build a graph from the liveness and coalesce information in this code.

buildGraph 
	:: [LiveCmmTop]
	-> UniqSM (Color.Graph Reg RegClass Reg)
	
buildGraph code
 = do
 	-- Add the reg-reg conflicts to the graph
	let conflictSets	= unionManyBags (map slurpConflicts code)
	let graph_conflict	= foldrBag graphAddConflictSet Color.initGraph conflictSets


	-- Add the coalescences edges to the graph.
	let coalesce		= unionManyBags (map slurpJoinMovs code)
	let graph_coalesce	= foldrBag graphAddCoalesce graph_conflict coalesce
			
	return	$ graph_coalesce


-- | Add some conflict edges to the graph.
--	Conflicts between virtual and real regs are recorded as exclusions.
--
graphAddConflictSet 
	:: UniqSet Reg
	-> Color.Graph Reg RegClass Reg
	-> Color.Graph Reg RegClass Reg
	
graphAddConflictSet set graph
 = let	reals		= filterUFM isRealReg set
 	virtuals	= filterUFM (not . isRealReg) set
 
	graph1	= Color.addConflicts virtuals regClass graph
	graph2	= foldr (\(r1, r2) -> Color.addExclusion r1 regClass r2)
			graph1
			[ (a, b) 
				| a <- uniqSetToList virtuals
				, b <- uniqSetToList reals]

   in	graph2
	

-- | Add some coalesence edges to the graph
--	Coalesences between virtual and real regs are recorded as preferences.
--
graphAddCoalesce 
	:: (Reg, Reg) 
	-> Color.Graph Reg RegClass Reg
	-> Color.Graph Reg RegClass Reg
	
graphAddCoalesce (r1, r2) graph
	| RealReg regno <- r1
	= Color.addPreference (regWithClass r2) r1 graph
	
	| RealReg regno <- r2
	= Color.addPreference (regWithClass r1) r2 graph
	
	| otherwise
	= Color.addCoalesce (regWithClass r1) (regWithClass r2) graph

	where 	regWithClass r	= (r, regClass r)


-- | Patch registers in code using the reg -> reg mapping in this graph.
patchRegsFromGraph 
	:: Color.Graph Reg RegClass Reg
	-> LiveCmmTop -> LiveCmmTop

patchRegsFromGraph graph code
 = let
 	-- a function to lookup the hardreg for a virtual reg from the graph.
 	patchF reg
		-- leave real regs alone.
		| isRealReg reg
		= reg

		-- this virtual has a regular node in the graph.
 		| Just node	<- Color.lookupNode graph reg
		= case Color.nodeColor node of
			Just color	-> color
			Nothing		-> reg
			
		-- no node in the graph for this virtual, bad news.
		| otherwise
		= pprPanic "patchRegsFromGraph: register mapping failed." 
			(  text "There is no node in the graph for register " <> ppr reg
			$$ ppr code
			$$ Color.dotGraph (\x -> text "white") trivColorable graph)
	
   in	patchEraseLive patchF code
   

plusUFMs_C  :: (elt -> elt -> elt) -> [UniqFM elt] -> UniqFM elt
plusUFMs_C f maps
	= foldl (plusUFM_C f) emptyUFM maps
	
