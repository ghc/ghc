
module RegAlloc.Graph.TrivColorable (
	trivColorable,
)

where

#include "HsVersions.h"

import RegClass
import Reg

import GraphBase

import UniqFM
import FastTypes

{-
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
-}


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

trivColorable 
	:: (Reg -> RegClass) 
	-> Triv Reg RegClass Reg
	
trivColorable regClass _ conflicts exclusions
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
