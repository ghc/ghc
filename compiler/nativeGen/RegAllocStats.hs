
-- Carries interesting info for debugging / profiling of the 
--	graph coloring register allocator.

module RegAllocStats (
	RegAllocStats (..),
	regDotColor,

	pprStatsSpills,
	pprStatsLifetimes,
	pprStatsConflict,
	pprStatsLifeConflict
)

where

#include "nativeGen/NCG.h"

import qualified GraphColor as Color
import RegLiveness
import RegSpill
import MachRegs

import Outputable
import UniqFM
import UniqSet

import Data.List

data RegAllocStats

	-- a spill stage
	= RegAllocStatsSpill
	{ raLiveCmm	:: [LiveCmmTop]			-- ^ code we tried to allocate regs for
	, raGraph	:: Color.Graph Reg RegClass Reg	-- ^ the partially colored graph
	, raSpillStats	:: SpillStats 			-- ^ spiller stats
	, raLifetimes	:: UniqFM (Reg, Int) }		-- ^ number of instrs each reg lives for

	-- a successful coloring
	| RegAllocStatsColored
	{ raLiveCmm	:: [LiveCmmTop]			-- ^ the code we allocated regs for
	, raGraph	:: Color.Graph Reg RegClass Reg -- ^ the colored graph
	, raPatchedCmm	:: [LiveCmmTop] 		-- ^ code with register allocation
	, raLifetimes	:: UniqFM (Reg, Int) }		-- ^ number of instrs each reg lives for


instance Outputable RegAllocStats where

 ppr (s@RegAllocStatsSpill{})
 	= text "-- Spill"

	$$ text "-- Native code with liveness information."
	$$ ppr (raLiveCmm s)
	$$ text " "

	$$ text "-- Register conflict graph."
	$$ Color.dotGraph regDotColor trivColorable (raGraph s)

	$$ text "-- Spill statistics."
	$$ ppr (raSpillStats s)


 ppr (s@RegAllocStatsColored{})
 	= text "-- Colored"

	$$ text "-- Native code with liveness information."
	$$ ppr (raLiveCmm s)
	$$ text " "

	$$ text "-- Register conflict graph."
	$$ Color.dotGraph regDotColor trivColorable (raGraph s)

	$$ text "-- Native code after register allocation."
	$$ ppr (raPatchedCmm s)


-- | Dump a table of how many spill loads / stores were inserted for each vreg.
pprStatsSpills
	:: [RegAllocStats] -> SDoc

pprStatsSpills stats
 = let	-- slurp out the stats from all the spiller stages
	spillStats	= [ s	| s@RegAllocStatsSpill{} <- stats]

	-- build a map of how many spill load/stores were inserted for each vreg
	spillLS		= foldl' (plusUFM_C accSpillLS) emptyUFM
			$ map (spillLoadStore . raSpillStats) spillStats

	-- print the count of load/spills as a tuple so we can read back from the file easilly
	pprSpillLS (r, loads, stores)
	 = (parens $ (hcat $ punctuate (text ", ") [doubleQuotes (ppr r), int loads, int stores]))


    in	(  text "-- spills-added"
	$$ text "--    (reg_name, spill_loads_added, spill_stores_added)."
	$$ (vcat $ map pprSpillLS $ eltsUFM spillLS)
	$$ text "\n")



-- | Dump a table of how long vregs tend to live for.
pprStatsLifetimes
	:: [RegAllocStats] -> SDoc

pprStatsLifetimes stats
 = let	lifeMap		= foldl' plusUFM emptyUFM $ map raLifetimes stats
	lifeBins	= binLifetimeCount lifeMap

   in	(  text "-- vreg-population-lifetimes"
	$$ text "--   (instruction_count, number_of_vregs_that_lived_that_long)"
	$$ (vcat $ map ppr $ eltsUFM lifeBins)
	$$ text "\n")

binLifetimeCount :: UniqFM (Reg, Int) -> UniqFM (Int, Int)
binLifetimeCount fm
 = let	lifes	= map (\l -> (l, (l, 1)))
 		$ map snd
		$ eltsUFM fm

   in	addListToUFM_C
		(\(l1, c1) (l2, c2) -> (l1, c1 + c2))
		emptyUFM
		lifes


-- | Dump a table of how many conflicts vregs tend to have.
pprStatsConflict
	:: [RegAllocStats] -> SDoc

pprStatsConflict stats
 = let	confMap	= foldl' (plusUFM_C (\(c1, n1) (c2, n2) -> (c1, n1 + n2)))
			emptyUFM
		$ map Color.slurpNodeConflictCount
		$ map raGraph stats

   in	(  text "-- vreg-conflicts"
	$$ text "--   (conflict_count, number_of_vregs_that_had_that_many_conflicts)"
	$$ (vcat $ map ppr $ eltsUFM confMap)
	$$ text "\n")


-- | For every vreg, dump it's how many conflicts it has and its lifetime
--	good for making a scatter plot.
pprStatsLifeConflict
	:: [RegAllocStats] -> Color.Graph Reg RegClass Reg -> SDoc

pprStatsLifeConflict stats graph
 = let	lifeMap	= foldl' plusUFM emptyUFM $ map raLifetimes stats
 	scatter	= map	(\r ->  let Just (_, lifetime)	= lookupUFM lifeMap r
				    Just node		= Color.lookupNode graph r
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


-----
-- Register colors for drawing conflict graphs
--	Keep this out of MachRegs.hs because it's specific to the graph coloring allocator.


-- reg colors for x86
#if i386_TARGET_ARCH
regDotColor :: Reg -> SDoc
regDotColor reg
 = let	Just	str	= lookupUFM regColors reg
   in	text str

regColors
 = listToUFM
 $  	[ (eax,	"#00ff00")
	, (ebx,	"#0000ff")
	, (ecx,	"#00ffff")
	, (edx,	"#0080ff")

	, (fake0, "#ff00ff")
	, (fake1, "#ff00aa")
	, (fake2, "#aa00ff")
	, (fake3, "#aa00aa")
	, (fake4, "#ff0055")
	, (fake5, "#5500ff") ]
#endif


-- reg colors for x86_64
#if x86_64_TARGET_ARCH
regDotColor :: Reg -> SDoc
regDotColor reg
 = let	Just	str	= lookupUFM regColors reg
   in	text str

regColors
 = listToUFM
 $	[ (rax, "#00ff00"), (eax, "#00ff00")
	, (rbx,	"#0000ff"), (ebx, "#0000ff")
	, (rcx,	"#00ffff"), (ecx, "#00ffff")
	, (rdx,	"#0080ff"), (edx, "#00ffff")
	, (r8,  "#00ff80")
	, (r9,  "#008080")
	, (r10, "#0040ff")
	, (r11, "#00ff40")
	, (r12, "#008040")
	, (r13, "#004080")
	, (r14, "#004040")
	, (r15, "#002080") ]

	++ zip (map RealReg [16..31]) (repeat "red")
#endif


-- reg colors for ppc
#if powerpc_TARGET_ARCH
regDotColor :: Reg -> SDoc
regDotColor reg
 = case regClass reg of
 	RcInteger	-> text "blue"
	RcFloat		-> text "red"
#endif


{-
toX11Color (r, g, b)
 = let	rs	= padL 2 '0' (showHex r "")
 	gs	= padL 2 '0' (showHex r "")
	bs	= padL 2 '0' (showHex r "")

	padL n c s
		= replicate (n - length s) c ++ s
  in	"#" ++ rs ++ gs ++ bs
-}
