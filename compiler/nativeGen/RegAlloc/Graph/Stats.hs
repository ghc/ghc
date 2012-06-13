{-# OPTIONS -fno-warn-missing-signatures #-}
-- | Carries interesting info for debugging / profiling of the 
--	graph coloring register allocator.
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module RegAlloc.Graph.Stats (
	RegAllocStats (..),

	pprStats,
	pprStatsSpills,
	pprStatsLifetimes,
	pprStatsConflict,
	pprStatsLifeConflict,

	countSRMs, addSRM
)

where

#include "nativeGen/NCG.h"

import qualified GraphColor as Color
import RegAlloc.Liveness
import RegAlloc.Graph.Spill
import RegAlloc.Graph.SpillCost
import RegAlloc.Graph.TrivColorable
import Instruction
import RegClass
import Reg
import TargetReg

import OldCmm
import OldPprCmm()
import Outputable
import UniqFM
import UniqSet
import State

import Data.List

data RegAllocStats statics instr

	-- initial graph
	= RegAllocStatsStart
	{ raLiveCmm	:: [LiveCmmDecl statics instr]		  	-- ^ initial code, with liveness
	, raGraph	:: Color.Graph VirtualReg RegClass RealReg   	-- ^ the initial, uncolored graph
	, raSpillCosts	:: SpillCostInfo } 		 		-- ^ information to help choose which regs to spill

	-- a spill stage
	| RegAllocStatsSpill
	{ raCode	:: [LiveCmmDecl statics instr]			-- ^ the code we tried to allocate registers for
	, raGraph	:: Color.Graph VirtualReg RegClass RealReg	-- ^ the partially colored graph
	, raCoalesced	:: UniqFM VirtualReg				-- ^ the regs that were coaleced
	, raSpillStats	:: SpillStats 					-- ^ spiller stats
	, raSpillCosts	:: SpillCostInfo 				-- ^ number of instrs each reg lives for
	, raSpilled	:: [LiveCmmDecl statics instr] }			-- ^ code with spill instructions added

	-- a successful coloring
	| RegAllocStatsColored
	{ raCode	  :: [LiveCmmDecl statics instr]			-- ^ the code we tried to allocate registers for
	, raGraph	  :: Color.Graph VirtualReg RegClass RealReg	-- ^ the uncolored graph
	, raGraphColored  :: Color.Graph VirtualReg RegClass RealReg 	-- ^ the coalesced and colored graph
	, raCoalesced	  :: UniqFM VirtualReg				-- ^ the regs that were coaleced
	, raCodeCoalesced :: [LiveCmmDecl statics instr]			-- ^ code with coalescings applied 
	, raPatched	  :: [LiveCmmDecl statics instr] 		-- ^ code with vregs replaced by hregs
	, raSpillClean    :: [LiveCmmDecl statics instr]			-- ^ code with unneeded spill\/reloads cleaned out
	, raFinal	  :: [NatCmmDecl statics instr] 			-- ^ final code
	, raSRMs	  :: (Int, Int, Int) }				-- ^ spill\/reload\/reg-reg moves present in this code

instance (Outputable statics, Outputable instr) => Outputable (RegAllocStats statics instr) where

 ppr (s@RegAllocStatsStart{}) = sdocWithPlatform $ \platform ->
 	   text "#  Start"
	$$ text "#  Native code with liveness information."
	$$ ppr (raLiveCmm s)
	$$ text ""
	$$ text "#  Initial register conflict graph."
	$$ Color.dotGraph 
		(targetRegDotColor platform)
		(trivColorable platform
			(targetVirtualRegSqueeze platform)
			(targetRealRegSqueeze platform))
		(raGraph s)


 ppr (s@RegAllocStatsSpill{}) =
 	   text "#  Spill"

	$$ text "#  Code with liveness information."
	$$ ppr (raCode s)
	$$ text ""

	$$ (if (not $ isNullUFM $ raCoalesced s)
		then 	text "#  Registers coalesced."
			$$ (vcat $ map ppr $ ufmToList $ raCoalesced s)
			$$ text ""
		else empty)

	$$ text "#  Spills inserted."
	$$ ppr (raSpillStats s)
	$$ text ""

	$$ text "#  Code with spills inserted."
	$$ ppr (raSpilled s)


 ppr (s@RegAllocStatsColored { raSRMs = (spills, reloads, moves) }) = sdocWithPlatform $ \platform ->
 	   text "#  Colored"

	$$ text "#  Code with liveness information."
	$$ ppr (raCode s)
	$$ text ""

	$$ text "#  Register conflict graph (colored)."
	$$ Color.dotGraph 
		(targetRegDotColor platform)
		(trivColorable platform
			(targetVirtualRegSqueeze platform)
			(targetRealRegSqueeze platform))
		(raGraphColored s)
	$$ text ""

	$$ (if (not $ isNullUFM $ raCoalesced s)
		then 	text "#  Registers coalesced."
			$$ (vcat $ map ppr $ ufmToList $ raCoalesced s)
			$$ text ""
		else empty)

	$$ text "#  Native code after coalescings applied."
	$$ ppr (raCodeCoalesced s)
	$$ text ""

	$$ text "#  Native code after register allocation."
	$$ ppr (raPatched s)
	$$ text ""

	$$ text "#  Clean out unneeded spill/reloads."
	$$ ppr (raSpillClean s)
	$$ text ""

	$$ text "#  Final code, after rewriting spill/rewrite pseudo instrs."
	$$ ppr (raFinal s)
	$$ text ""
	$$  text "#  Score:"
	$$ (text "#          spills  inserted: " <> int spills)
	$$ (text "#          reloads inserted: " <> int reloads)
	$$ (text "#   reg-reg moves remaining: " <> int moves)
	$$ text ""

-- | Do all the different analysis on this list of RegAllocStats
pprStats 
	:: [RegAllocStats statics instr] 
	-> Color.Graph VirtualReg RegClass RealReg 
	-> SDoc
	
pprStats stats graph
 = let 	outSpills	= pprStatsSpills    stats
	outLife		= pprStatsLifetimes stats
	outConflict	= pprStatsConflict  stats
	outScatter	= pprStatsLifeConflict stats graph

  in	vcat [outSpills, outLife, outConflict, outScatter]


-- | Dump a table of how many spill loads \/ stores were inserted for each vreg.
pprStatsSpills
	:: [RegAllocStats statics instr] -> SDoc

pprStatsSpills stats
 = let
	finals	= [ s	| s@RegAllocStatsColored{} <- stats]

	-- sum up how many stores\/loads\/reg-reg-moves were left in the code
	total	= foldl' addSRM (0, 0, 0)
		$ map raSRMs finals

    in	(  text "-- spills-added-total"
	$$ text "--    (stores, loads, reg_reg_moves_remaining)"
	$$ ppr total
	$$ text "")


-- | Dump a table of how long vregs tend to live for in the initial code.
pprStatsLifetimes
	:: [RegAllocStats statics instr] -> SDoc

pprStatsLifetimes stats
 = let	info		= foldl' plusSpillCostInfo zeroSpillCostInfo
 				[ raSpillCosts s
					| s@RegAllocStatsStart{} <- stats ]

	lifeBins	= binLifetimeCount $ lifeMapFromSpillCostInfo info

   in	(  text "-- vreg-population-lifetimes"
	$$ text "--   (instruction_count, number_of_vregs_that_lived_that_long)"
	$$ (vcat $ map ppr $ eltsUFM lifeBins)
	$$ text "\n")

binLifetimeCount :: UniqFM (VirtualReg, Int) -> UniqFM (Int, Int)
binLifetimeCount fm
 = let	lifes	= map (\l -> (l, (l, 1)))
 		$ map snd
		$ eltsUFM fm

   in	addListToUFM_C
		(\(l1, c1) (_, c2) -> (l1, c1 + c2))
		emptyUFM
		lifes


-- | Dump a table of how many conflicts vregs tend to have in the initial code.
pprStatsConflict
	:: [RegAllocStats statics instr] -> SDoc

pprStatsConflict stats
 = let	confMap	= foldl' (plusUFM_C (\(c1, n1) (_, n2) -> (c1, n1 + n2)))
			emptyUFM
		$ map Color.slurpNodeConflictCount
			[ raGraph s | s@RegAllocStatsStart{} <- stats ]

   in	(  text "-- vreg-conflicts"
	$$ text "--   (conflict_count, number_of_vregs_that_had_that_many_conflicts)"
	$$ (vcat $ map ppr $ eltsUFM confMap)
	$$ text "\n")


-- | For every vreg, dump it's how many conflicts it has and its lifetime
--	good for making a scatter plot.
pprStatsLifeConflict
	:: [RegAllocStats statics instr]
	-> Color.Graph VirtualReg RegClass RealReg 	-- ^ global register conflict graph
	-> SDoc

pprStatsLifeConflict stats graph
 = let	lifeMap	= lifeMapFromSpillCostInfo
 		$ foldl' plusSpillCostInfo zeroSpillCostInfo
		$ [ raSpillCosts s | s@RegAllocStatsStart{} <- stats ]

 	scatter	= map	(\r ->  let lifetime	= case lookupUFM lifeMap r of
							Just (_, l)	-> l
							Nothing		-> 0
				    Just node	= Color.lookupNode graph r
				in parens $ hcat $ punctuate (text ", ")
					[ doubleQuotes $ ppr $ Color.nodeId node
					, ppr $ sizeUniqSet (Color.nodeConflicts node)
					, ppr $ lifetime ])
		$ map Color.nodeId
		$ eltsUFM
		$ Color.graphMap graph

   in 	(  text "-- vreg-conflict-lifetime"
	$$ text "--   (vreg, vreg_conflicts, vreg_lifetime)"
	$$ (vcat scatter)
	$$ text "\n")


-- | Count spill/reload/reg-reg moves.
--	Lets us see how well the register allocator has done.
countSRMs 
	:: Instruction instr
	=> LiveCmmDecl statics instr -> (Int, Int, Int)

countSRMs cmm
	= execState (mapBlockTopM countSRM_block cmm) (0, 0, 0)

countSRM_block (BasicBlock i instrs)
 = do	instrs'	<- mapM countSRM_instr instrs
 	return	$ BasicBlock i instrs'

countSRM_instr li
	| LiveInstr SPILL{} _	 <- li
	= do	modify  $ \(s, r, m)	-> (s + 1, r, m)
		return li

	| LiveInstr RELOAD{} _ 	<- li
	= do	modify  $ \(s, r, m)	-> (s, r + 1, m)
		return li
	
	| LiveInstr instr _	<- li
	, Just _	<- takeRegRegMoveInstr instr
	= do	modify	$ \(s, r, m)	-> (s, r, m + 1)
		return li

	| otherwise
	=	return li

-- sigh..
addSRM (s1, r1, m1) (s2, r2, m2)
	= (s1+s2, r1+r2, m1+m2)

