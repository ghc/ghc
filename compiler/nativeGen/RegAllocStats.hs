
-- Carries interesting info for debugging / profiling of the 
--	graph coloring register allocator.

module RegAllocStats (
	RegAllocStats (..),
	regDotColor
)

where

#include "nativeGen/NCG.h"

import qualified GraphColor as Color
import RegLiveness
import RegSpill
import MachRegs

import Outputable
import UniqFM


data RegAllocStats

	-- a spill stage
	= RegAllocStatsSpill
	{ raLiveCmm	:: [LiveCmmTop]			-- ^ code we tried to allocate regs for
	, raGraph	:: Color.Graph Reg RegClass Reg	-- ^ the partially colored graph
	, raSpillStats	:: SpillStats }			-- ^ spiller stats

	-- a successful coloring
	| RegAllocStatsColored
	{ raLiveCmm	:: [LiveCmmTop]			-- ^ the code we allocated regs for
	, raGraph	:: Color.Graph Reg RegClass Reg -- ^ the colored graph
	, raPatchedCmm	:: [LiveCmmTop] }		-- ^ code with register allocation


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
