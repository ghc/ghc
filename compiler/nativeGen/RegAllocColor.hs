{-# OPTIONS -fno-warn-missing-signatures #-}
-- | Graph coloring register allocator.
--
-- TODO: The colors in graphviz graphs for x86_64 and ppc could be nicer.
--

module RegAllocColor ( 
	regAlloc,
	regDotColor
) 

where

import qualified GraphColor	as Color
import RegLiveness
import RegSpill
import RegSpillClean
import RegSpillCost
import RegAllocStats
-- import RegCoalesce
import MachRegs
import MachInstrs
import PprMach

import UniqSupply
import UniqSet
import UniqFM
import Bag
import Outputable
import DynFlags

import Data.List
import Data.Maybe
import Control.Monad

-- | The maximum number of build\/spill cycles we'll allow.
--	We should only need 3 or 4 cycles tops.
--	If we run for any longer than this we're probably in an infinite loop,
--	It's probably better just to bail out and report a bug at this stage.
maxSpinCount	:: Int
maxSpinCount	= 10


-- | The top level of the graph coloring register allocator.
--	
regAlloc
	:: DynFlags
	-> UniqFM (UniqSet Reg)		-- ^ the registers we can use for allocation
	-> UniqSet Int			-- ^ the set of available spill slots.
	-> [LiveCmmTop]			-- ^ code annotated with liveness information.
	-> UniqSM ( [NatCmmTop], [RegAllocStats] )
           -- ^ code with registers allocated and stats for each stage of
           -- allocation
		
regAlloc dflags regsFree slotsFree code
 = do
 	(code_final, debug_codeGraphs, _)
		<- regAlloc_spin dflags 0 trivColorable regsFree slotsFree [] code
	
	return	( code_final
		, reverse debug_codeGraphs )

regAlloc_spin dflags spinCount triv regsFree slotsFree debug_codeGraphs code
 = do
 	-- if any of these dump flags are turned on we want to hang on to
	--	intermediate structures in the allocator - otherwise tell the
	--	allocator to ditch them early so we don't end up creating space leaks.
	let dump = or
		[ dopt Opt_D_dump_asm_regalloc_stages dflags
		, dopt Opt_D_dump_asm_stats dflags
		, dopt Opt_D_dump_asm_conflicts dflags ]

	-- check that we're not running off down the garden path.
	when (spinCount > maxSpinCount)
	 $ pprPanic "regAlloc_spin: max build/spill cycle count exceeded."
	 	(  text "It looks like the register allocator is stuck in an infinite loop."
		$$ text "max cycles  = " <> int maxSpinCount
	 	$$ text "regsFree    = " <> (hcat	$ punctuate space $ map (docToSDoc . pprUserReg)
						$ uniqSetToList $ unionManyUniqSets $ eltsUFM regsFree)
		$$ text "slotsFree   = " <> ppr (sizeUniqSet slotsFree))

 	-- build a conflict graph from the code.
	graph		<- {-# SCC "BuildGraph" #-} buildGraph code

	-- VERY IMPORTANT:
	--	We really do want the graph to be fully evaluated _before_ we start coloring.
	--	If we don't do this now then when the call to Color.colorGraph forces bits of it,
	--	the heap will be filled with half evaluated pieces of graph and zillions of apply thunks.
	--
	seqGraph graph `seq` return ()


	-- build a map of the cost of spilling each instruction
	--	this will only actually be computed if we have to spill something.
	let spillCosts	= foldl' plusSpillCostInfo zeroSpillCostInfo
			$ map slurpSpillCostInfo code

	-- the function to choose regs to leave uncolored
	let spill	= chooseSpill spillCosts

	-- record startup state
	let stat1	=
		if spinCount == 0
		 then	Just $ RegAllocStatsStart
		 	{ raLiveCmm	= code
			, raGraph	= graph
			, raSpillCosts	= spillCosts }
		 else	Nothing
	
	-- try and color the graph 
	let (graph_colored, rsSpill, rmCoalesce)
			= {-# SCC "ColorGraph" #-}
			   Color.colorGraph
			    	(dopt Opt_RegsIterative dflags)
				spinCount
			    	regsFree triv spill graph

	-- rewrite regs in the code that have been coalesced
	let patchF reg	= case lookupUFM rmCoalesce reg of
				Just reg'	-> patchF reg'
				Nothing		-> reg
	let code_coalesced
			= map (patchEraseLive patchF) code


	-- see if we've found a coloring
	if isEmptyUniqSet rsSpill
	 then do
		-- if -fasm-lint is turned on then validate the graph
		let graph_colored_lint	=
			if dopt Opt_DoAsmLinting dflags
				then Color.validateGraph (text "")
					True 	-- require all nodes to be colored
					graph_colored
				else graph_colored

		-- patch the registers using the info in the graph
	 	let code_patched	= map (patchRegsFromGraph graph_colored_lint) code_coalesced

		-- clean out unneeded SPILL/RELOADs
		let code_spillclean	= map cleanSpills code_patched

		-- strip off liveness information
		let code_nat		= map stripLive code_spillclean

		-- rewrite SPILL/RELOAD pseudos into real instructions
		let spillNatTop		= mapGenBlockTop spillNatBlock
		let code_final		= map spillNatTop code_nat
		
		-- record what happened in this stage for debugging
		let stat		=
			RegAllocStatsColored
			{ raGraph		= graph
			, raGraphColored	= graph_colored_lint
			, raCoalesced		= rmCoalesce
			, raPatched		= code_patched
			, raSpillClean		= code_spillclean
			, raFinal		= code_final
			, raSRMs		= foldl' addSRM (0, 0, 0) $ map countSRMs code_spillclean }


		let statList =
			if dump	then [stat] ++ maybeToList stat1 ++ debug_codeGraphs
				else []

		-- space leak avoidance
		seqList statList `seq` return ()

		return	( code_final
			, statList
			, graph_colored_lint)

	 -- we couldn't find a coloring, time to spill something
	 else do
		-- if -fasm-lint is turned on then validate the graph
		let graph_colored_lint	=
			if dopt Opt_DoAsmLinting dflags
				then Color.validateGraph (text "")
					False 	-- don't require nodes to be colored
					graph_colored
				else graph_colored

	 	-- spill the uncolored regs
		(code_spilled, slotsFree', spillStats)
			<- regSpill code_coalesced slotsFree rsSpill

		-- recalculate liveness
		let code_nat	= map stripLive code_spilled
		code_relive	<- mapM regLiveness code_nat

		-- record what happened in this stage for debugging
		let stat	=
			RegAllocStatsSpill
			{ raGraph	= graph_colored_lint
			, raCoalesced	= rmCoalesce
			, raSpillStats	= spillStats
			, raSpillCosts	= spillCosts
			, raSpilled	= code_spilled }
			    	
		let statList =
			if dump
				then [stat] ++ maybeToList stat1 ++ debug_codeGraphs
				else []

		-- space leak avoidance
		seqList statList `seq` return ()

		regAlloc_spin dflags (spinCount + 1) triv regsFree slotsFree'
			statList
			code_relive



-- | Build a graph from the liveness and coalesce information in this code.

buildGraph 
	:: [LiveCmmTop]
	-> UniqSM (Color.Graph Reg RegClass Reg)
	
buildGraph code
 = do
	-- Slurp out the conflicts and reg->reg moves from this code
	let (conflictList, moveList) =
		unzip $ map slurpConflicts code

	-- Slurp out the spill/reload coalesces
	let moveList2		= map slurpReloadCoalesce code

 	-- Add the reg-reg conflicts to the graph
	let conflictBag		= unionManyBags conflictList
	let graph_conflict	= foldrBag graphAddConflictSet Color.initGraph conflictBag

	-- Add the coalescences edges to the graph.
	let moveBag		= unionBags (unionManyBags moveList2) (unionManyBags moveList)
	let graph_coalesce	= foldrBag graphAddCoalesce graph_conflict moveBag
			
	return	graph_coalesce


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
	| RealReg _ <- r1
	= Color.addPreference (regWithClass r2) r1 graph
	
	| RealReg _ <- r2
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
			$$ Color.dotGraph (\_ -> text "white") trivColorable graph)

   in	patchEraseLive patchF code
   

-----
-- for when laziness just isn't what you wanted...
--
seqGraph :: Color.Graph Reg RegClass Reg -> ()
seqGraph graph		= seqNodes (eltsUFM (Color.graphMap graph))

seqNodes :: [Color.Node Reg RegClass Reg] -> ()
seqNodes ns
 = case ns of
 	[]		-> ()
	(n : ns)	-> seqNode n `seq` seqNodes ns

seqNode :: Color.Node Reg RegClass Reg -> ()
seqNode node
	=     seqReg      (Color.nodeId node)
	`seq` seqRegClass (Color.nodeClass node)
	`seq` seqMaybeReg (Color.nodeColor node)
	`seq` (seqRegList (uniqSetToList (Color.nodeConflicts node)))
	`seq` (seqRegList (uniqSetToList (Color.nodeExclusions node)))
	`seq` (seqRegList (Color.nodePreference node))
	`seq` (seqRegList (uniqSetToList (Color.nodeCoalesce node)))

seqReg :: Reg -> ()
seqReg reg
 = case reg of
 	RealReg _	-> ()
	VirtualRegI _	-> ()
	VirtualRegHi _	-> ()
	VirtualRegF _	-> ()
	VirtualRegD _	-> ()

seqRegClass :: RegClass -> ()
seqRegClass c
 = case c of
 	RcInteger	-> ()
	RcFloat		-> ()
	RcDouble	-> ()

seqMaybeReg :: Maybe Reg -> ()
seqMaybeReg mr
 = case mr of
 	Nothing		-> ()
	Just r		-> seqReg r

seqRegList :: [Reg] -> ()
seqRegList rs
 = case rs of
 	[]		-> ()
	(r : rs)	-> seqReg r `seq` seqRegList rs

seqList :: [a] -> ()
seqList ls
 = case ls of
 	[]		-> ()
	(r : rs)	-> r `seq` seqList rs


