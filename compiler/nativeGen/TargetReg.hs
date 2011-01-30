
-- | Hard wired things related to registers.
--	This is module is preventing the native code generator being able to 
--	emit code for non-host architectures.
--
--	TODO: Do a better job of the overloading, and eliminate this module.
--	We'd probably do better with a Register type class, and hook this to 
--	Instruction somehow.
--
--	TODO: We should also make arch specific versions of RegAlloc.Graph.TrivColorable

module TargetReg (
	targetVirtualRegSqueeze,
	targetRealRegSqueeze,
	targetClassOfRealReg,
	targetMkVirtualReg,
	targetWordSize,
	targetRegDotColor,
	targetClassOfReg
)

where

#include "HsVersions.h"

import Reg
import RegClass
import Size

import CmmType	(wordWidth)
import Outputable
import Unique
import FastTypes


#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
import qualified X86.Regs	as X86
import qualified X86.RegInfo	as X86

#elif powerpc_TARGET_ARCH
import qualified PPC.Regs	as PPC

#elif sparc_TARGET_ARCH	
import qualified SPARC.Regs	as SPARC

#else
#error "RegAlloc.Graph.TargetReg: not defined"
#endif

targetVirtualRegSqueeze :: RegClass -> VirtualReg -> FastInt
targetRealRegSqueeze 	:: RegClass -> RealReg -> FastInt
targetClassOfRealReg 	:: RealReg -> RegClass
targetWordSize 		:: Size
targetMkVirtualReg 	:: Unique -> Size -> VirtualReg
targetRegDotColor 	:: RealReg -> SDoc

-- x86 -------------------------------------------------------------------------
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
targetVirtualRegSqueeze = X86.virtualRegSqueeze
targetRealRegSqueeze 	= X86.realRegSqueeze
targetClassOfRealReg 	= X86.classOfRealReg
targetWordSize 		= intSize wordWidth
targetMkVirtualReg 	= X86.mkVirtualReg
targetRegDotColor 	= X86.regDotColor

-- ppc -------------------------------------------------------------------------
#elif powerpc_TARGET_ARCH
targetVirtualRegSqueeze = PPC.virtualRegSqueeze
targetRealRegSqueeze 	= PPC.realRegSqueeze
targetClassOfRealReg 	= PPC.classOfRealReg
targetWordSize 		= intSize wordWidth
targetMkVirtualReg 	= PPC.mkVirtualReg
targetRegDotColor 	= PPC.regDotColor

-- sparc -----------------------------------------------------------------------
#elif sparc_TARGET_ARCH
targetVirtualRegSqueeze = SPARC.virtualRegSqueeze
targetRealRegSqueeze 	= SPARC.realRegSqueeze
targetClassOfRealReg 	= SPARC.classOfRealReg
targetWordSize 		= intSize wordWidth
targetMkVirtualReg 	= SPARC.mkVirtualReg
targetRegDotColor 	= SPARC.regDotColor

--------------------------------------------------------------------------------
#else
#error "RegAlloc.Graph.TargetReg: not defined"
#endif


targetClassOfReg :: Reg -> RegClass
targetClassOfReg reg
 = case reg of
 	RegVirtual vr	-> classOfVirtualReg vr
	RegReal rr	-> targetClassOfRealReg rr


