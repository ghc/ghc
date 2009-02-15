
module RegAlloc.Linear.FreeRegs (
	FreeRegs(),
	noFreeRegs,
	releaseReg,
	initFreeRegs,
	getFreeRegs,
	allocateReg,
	maxSpillSlots
)

#include "HsVersions.h"

where

-- -----------------------------------------------------------------------------
-- The free register set
-- This needs to be *efficient*
-- Here's an inefficient 'executable specification' of the FreeRegs data type:
--
--	type FreeRegs = [RegNo]
--	noFreeRegs = 0
--	releaseReg n f = if n `elem` f then f else (n : f)
--	initFreeRegs = allocatableRegs
--	getFreeRegs cls f = filter ( (==cls) . regClass . RealReg ) f
--	allocateReg f r = filter (/= r) f


#if   defined(powerpc_TARGET_ARCH) 
import RegAlloc.Linear.PPC.FreeRegs
import PPC.Instr	(maxSpillSlots)

#elif defined(sparc_TARGET_ARCH)
import RegAlloc.Linear.SPARC.FreeRegs
import SPARC.Instr	(maxSpillSlots)

#elif defined(i386_TARGET_ARCH) || defined(x86_64_TARGET_ARCH)
import RegAlloc.Linear.X86.FreeRegs
import X86.Instr	(maxSpillSlots)

#else
#error "RegAlloc.Linear.FreeRegs not defined for this architecture."

#endif

