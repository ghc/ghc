-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1994-2004
-- 
-- Machine-specific info about registers.
-- 
-- Also includes stuff about immediate operands, which are
-- often/usually quite entangled with registers.
-- 
-- -----------------------------------------------------------------------------

#include "nativeGen/NCG.h"

module Regs (
	--------------------------------
	-- Generic things, shared by all architectures.
	module RegsBase,	
	getHiVRegFromLo,
	get_GlobalReg_reg_or_addr,
	allocatableRegs,
	allocatableRegsInClass,
	trivColorable,

	--------------------------------
	-- Things that are defined by the arch specific module.
	--

	-- sizes
	Size(..),
	intSize, 
	floatSize, 
	isFloatSize, 
	wordSize, 
	cmmTypeSize, 
	sizeToWidth,
	mkVReg,

	-- immediates
	Imm(..), 
	strImmLit, 
	litToImm,

	-- addressing modes
	AddrMode(..),
	addrOffset,

	-- registers
	spRel,
	argRegs, 
	allArgRegs, 
	callClobberedRegs,
	allMachRegNos,
	regClass,
	showReg,

	-- machine specific things
#if   powerpc_TARGET_ARCH
	allFPArgRegs,
	fits16Bits,
	makeImmediate,
	freg,
	sp, r3, r4, r27, r28, f1, f20, f21,

#elif i386_TARGET_ARCH || i386_64_TARGET_ARCH
	EABase(..), EAIndex(..), addrModeRegs,
	
	eax, ebx, ecx, edx, esi, edi, ebp, esp,
	fake0, fake1, fake2, fake3, fake4, fake5,
	rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp,
	r8, r9, r10, r11, r12, r13, r14, r15,
  	xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7,
  	xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15,
	xmm,

	ripRel,
	allFPArgRegs,

#elif sparc_TARGET_ARCH
	fpRel,
	fits13Bits, 
	largeOffsetError,
	gReg, iReg, lReg, oReg, fReg,
	fp, sp, g0, g1, g2, o0, o1, f0, f6, f8, f26, f27,
	nCG_FirstFloatReg,
#endif
	-- horror show
	freeReg,
	globalRegMaybe	
) 

where

#include "HsVersions.h"
#include "../includes/MachRegs.h"

import Cmm
import CgUtils          ( get_GlobalReg_addr )
import Outputable	( Outputable(..), pprPanic )
import qualified Outputable
import Panic
import Unique
import UniqSet
import FastTypes
import FastBool
import UniqFM


import RegsBase

#if   alpha_TARGET_ARCH
import Alpha.Regs
#elif powerpc_TARGET_ARCH
import PPC.Regs
#elif i386_TARGET_ARCH || x86_64_TARGET_ARCH
import X86.Regs
#elif sparc_TARGET_ARCH
import SPARC.Regs
#else
#error "Regs: not defined for this architecture"
#endif



instance Show Reg where
	show (RealReg i)      = showReg i
	show (VirtualRegI u)  = "%vI_" ++ show u
	show (VirtualRegHi u) = "%vHi_" ++ show u
	show (VirtualRegF u)  = "%vF_" ++ show u
	show (VirtualRegD u)  = "%vD_" ++ show u

instance Outputable Reg where
	ppr r = Outputable.text (show r)


-- Determine the upper-half vreg for a 64-bit quantity on a 32-bit platform
-- when supplied with the vreg for the lower-half of the quantity.
-- (NB. Not reversible).
getHiVRegFromLo :: Reg -> Reg
getHiVRegFromLo (VirtualRegI u) 
   = VirtualRegHi (newTagUnique u 'H') -- makes a pseudo-unique with tag 'H'

getHiVRegFromLo other 
   = pprPanic "getHiVRegFromLo" (ppr other)

-- -----------------------------------------------------------------------------
-- Global registers

-- We map STG registers onto appropriate CmmExprs.  Either they map
-- to real machine registers or stored as offsets from BaseReg.  Given
-- a GlobalReg, get_GlobalReg_reg_or_addr produces either the real
-- register it is in, on this platform, or a CmmExpr denoting the
-- address in the register table holding it.
-- (See also get_GlobalReg_addr in CgUtils.)

get_GlobalReg_reg_or_addr       :: GlobalReg -> Either Reg CmmExpr
get_GlobalReg_reg_or_addr mid
   = case globalRegMaybe mid of
        Just rr -> Left rr
        Nothing -> Right (get_GlobalReg_addr mid)


-- allocatableRegs is allMachRegNos with the fixed-use regs removed.
-- i.e., these are the regs for which we are prepared to allow the
-- register allocator to attempt to map VRegs to.
allocatableRegs :: [RegNo]
allocatableRegs
   = let isFree i = isFastTrue (freeReg i)
     in  filter isFree allMachRegNos


-- | The number of regs in each class.
--	We go via top level CAFs to ensure that we're not recomputing
--	the length of these lists each time the fn is called.
allocatableRegsInClass :: RegClass -> Int
allocatableRegsInClass cls
 = case cls of
 	RcInteger	-> allocatableRegsInteger
	RcDouble	-> allocatableRegsDouble
	RcFloat		-> panic "Regs.allocatableRegsInClass: no match\n"

allocatableRegsInteger :: Int
allocatableRegsInteger	
	= length $ filter (\r -> regClass r == RcInteger) 
		 $ map RealReg allocatableRegs

allocatableRegsDouble :: Int
allocatableRegsDouble
	= length $ filter (\r -> regClass r == RcDouble) 
		 $ map RealReg allocatableRegs



-- trivColorable ---------------------------------------------------------------

-- trivColorable function for the graph coloring allocator
--	This gets hammered by scanGraph during register allocation,
--	so needs to be fairly efficient.
--
--	NOTE: 	This only works for arcitectures with just RcInteger and RcDouble
--		(which are disjoint) ie. x86, x86_64 and ppc
--

--	BL 2007/09
--	Doing a nice fold over the UniqSet makes trivColorable use
--	32% of total compile time and 42% of total alloc when compiling SHA1.lhs from darcs.
{-
trivColorable :: RegClass -> UniqSet Reg -> UniqSet Reg -> Bool
trivColorable classN conflicts exclusions
 = let

	acc :: Reg -> (Int, Int) -> (Int, Int)
	acc r (cd, cf)	
	 = case regClass r of
		RcInteger	-> (cd+1, cf)
		RcDouble	-> (cd,   cf+1)
		_		-> panic "Regs.trivColorable: reg class not handled"

	tmp			= foldUniqSet acc (0, 0) conflicts
	(countInt,  countFloat)	= foldUniqSet acc tmp    exclusions

	squeese		= worst countInt   classN RcInteger
			+ worst countFloat classN RcDouble

   in	squeese < allocatableRegsInClass classN

-- | Worst case displacement
--	node N of classN has n neighbors of class C.
--
--	We currently only have RcInteger and RcDouble, which don't conflict at all.
--	This is a bit boring compared to what's in RegArchX86.
--
worst :: Int -> RegClass -> RegClass -> Int
worst n classN classC
 = case classN of
 	RcInteger
	 -> case classC of
	 	RcInteger	-> min n (allocatableRegsInClass RcInteger)
		RcDouble	-> 0
		
	RcDouble
	 -> case classC of
	 	RcDouble	-> min n (allocatableRegsInClass RcDouble)
		RcInteger	-> 0
-}


-- The number of allocatable regs is hard coded here so we can do a fast comparision
-- in trivColorable. It's ok if these numbers are _less_ than the actual number of
-- free regs, but they can't be more or the register conflict graph won't color.
--
-- There is an allocatableRegsInClass :: RegClass -> Int, but doing the unboxing
-- is too slow for us here.
--
-- Compare Regs.freeRegs  and MachRegs.h to get these numbers.
--
#if i386_TARGET_ARCH
#define ALLOCATABLE_REGS_INTEGER (_ILIT(3))
#define ALLOCATABLE_REGS_DOUBLE  (_ILIT(6))
#define ALLOCATABLE_REGS_FLOAT   (_ILIT(0))

#elif x86_64_TARGET_ARCH
#define ALLOCATABLE_REGS_INTEGER (_ILIT(5))
#define ALLOCATABLE_REGS_DOUBLE  (_ILIT(2))
#define ALLOCATABLE_REGS_FLOAT   (_ILIT(0))

#elif powerpc_TARGET_ARCH
#define ALLOCATABLE_REGS_INTEGER (_ILIT(16))
#define ALLOCATABLE_REGS_DOUBLE  (_ILIT(26))
#define ALLOCATABLE_REGS_FLOAT   (_ILIT(0))

#elif sparc_TARGET_ARCH
#define ALLOCATABLE_REGS_INTEGER (_ILIT(3))
#define ALLOCATABLE_REGS_DOUBLE  (_ILIT(6))
#define ALLOCATABLE_REGS_FLOAT   (_ILIT(0))

#else
#error ToDo: define ALLOCATABLE_REGS_INTEGER and ALLOCATABLE_REGS_DOUBLE
#endif

trivColorable :: RegClass -> UniqSet Reg -> UniqSet Reg -> Bool
trivColorable _ conflicts exclusions
 = {-# SCC "trivColorable" #-}
   let
	isSqueesed cI cF ufm
	  = case ufm of
		NodeUFM _ _ left right
		 -> case isSqueesed cI cF right of
		 	(# s, cI', cF' #)
			 -> case s of
			 	False	-> isSqueesed cI' cF' left
				True	-> (# True, cI', cF' #)

		LeafUFM _ reg
		 -> case regClass reg of
		 	RcInteger
			 -> case cI +# _ILIT(1) of
			  	cI' -> (# cI' >=# ALLOCATABLE_REGS_INTEGER, cI', cF #)

			RcDouble
			 -> case cF +# _ILIT(1) of
			 	cF' -> (# cF' >=# ALLOCATABLE_REGS_DOUBLE,  cI, cF' #)

			RcFloat 
			 -> case cF +# _ILIT(1) of
			 	cF' -> (# cF' >=# ALLOCATABLE_REGS_FLOAT,   cI, cF' #)

		EmptyUFM
		 ->	(# False, cI, cF #)

   in case isSqueesed (_ILIT(0)) (_ILIT(0)) conflicts of
	(# False, cI', cF' #)
	 -> case isSqueesed cI' cF' exclusions of
		(# s, _, _ #)	-> not s

	(# True, _, _ #)
	 -> False


