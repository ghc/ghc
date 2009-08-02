{-# OPTIONS -fno-warn-unused-binds #-}

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


-- trivColorable ---------------------------------------------------------------

-- trivColorable function for the graph coloring allocator
--
--	This gets hammered by scanGraph during register allocation,
--	so needs to be fairly efficient.
--
--	NOTE: 	This only works for arcitectures with just RcInteger and RcDouble
--		(which are disjoint) ie. x86, x86_64 and ppc
--
--	BL 2007/09
--	Doing a nice fold over the UniqSet makes trivColorable use
--	32% of total compile time and 42% of total alloc when compiling SHA1.lhs from darcs.
--
-- 	The number of allocatable regs is hard coded here so we can do a fast
-- 		comparision in trivColorable. 
--
-- 	It's ok if these numbers are _less_ than the actual number of free regs, 
--		but they can't be more or the register conflict graph won't color.
--
-- 	If the graph doesn't color then the allocator will panic, but it won't 
--		generate bad object code or anything nasty like that.
--
-- 	There is an allocatableRegsInClass :: RegClass -> Int, but doing the unboxing
-- 	is too slow for us here.
--
-- 	Look at includes/stg/MachRegs.h to get these numbers.
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
#define ALLOCATABLE_REGS_INTEGER (_ILIT(14))
#define ALLOCATABLE_REGS_DOUBLE  (_ILIT(11))
#define ALLOCATABLE_REGS_FLOAT   (_ILIT(22))


#else
#error ToDo: choose which trivColorable function to use for this architecture.
#endif



-- Disjoint registers ----------------------------------------------------------
--	
--	The definition has been unfolded into individual cases for speed.
-- 	Each architecture has a different register setup, so we use a
--	different regSqueeze function for each.
--
accSqueeze 
	:: FastInt 
	-> FastInt
	-> (reg -> FastInt) 
	-> UniqFM reg
	-> FastInt

accSqueeze count maxCount squeeze ufm 
 = case ufm of
 	NodeUFM _ _ left right
	 -> case accSqueeze count maxCount squeeze right of
	 	count' -> case count' >=# maxCount of
				False -> accSqueeze count' maxCount squeeze left
				True  -> count'
				
	LeafUFM _ reg	-> count +# squeeze reg
 	EmptyUFM	-> count


trivColorable
	:: (RegClass -> VirtualReg -> FastInt)
	-> (RegClass -> RealReg    -> FastInt)
	-> Triv VirtualReg RegClass RealReg

trivColorable virtualRegSqueeze realRegSqueeze RcInteger conflicts exclusions
	| count2	<- accSqueeze (_ILIT(0)) ALLOCATABLE_REGS_INTEGER 
				(virtualRegSqueeze RcInteger)
				conflicts
				
	, count3	<- accSqueeze  count2    ALLOCATABLE_REGS_INTEGER
				(realRegSqueeze   RcInteger)
				exclusions

	= count3 <# ALLOCATABLE_REGS_INTEGER

trivColorable virtualRegSqueeze realRegSqueeze RcFloat conflicts exclusions
	| count2	<- accSqueeze (_ILIT(0)) ALLOCATABLE_REGS_FLOAT
				(virtualRegSqueeze RcFloat)
				conflicts
				
	, count3	<- accSqueeze  count2    ALLOCATABLE_REGS_FLOAT
				(realRegSqueeze   RcFloat)
				exclusions

	= count3 <# ALLOCATABLE_REGS_FLOAT

trivColorable virtualRegSqueeze realRegSqueeze RcDouble conflicts exclusions
	| count2	<- accSqueeze (_ILIT(0)) ALLOCATABLE_REGS_DOUBLE
				(virtualRegSqueeze RcDouble)
				conflicts
				
	, count3	<- accSqueeze  count2    ALLOCATABLE_REGS_DOUBLE
				(realRegSqueeze   RcDouble)
				exclusions

	= count3 <# ALLOCATABLE_REGS_DOUBLE


-- Specification Code ----------------------------------------------------------
--
--	The trivColorable function for each particular architecture should
--	implement the following function, but faster.
--

{-
trivColorable :: RegClass -> UniqSet Reg -> UniqSet Reg -> Bool
trivColorable classN conflicts exclusions
 = let

	acc :: Reg -> (Int, Int) -> (Int, Int)
	acc r (cd, cf)	
	 = case regClass r of
		RcInteger	-> (cd+1, cf)
		RcFloat		-> (cd,   cf+1)
		_		-> panic "Regs.trivColorable: reg class not handled"

	tmp			= foldUniqSet acc (0, 0) conflicts
	(countInt,  countFloat)	= foldUniqSet acc tmp    exclusions

	squeese		= worst countInt   classN RcInteger
			+ worst countFloat classN RcFloat

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
		RcFloat		-> 0
		
	RcDouble
	 -> case classC of
	 	RcFloat		-> min n (allocatableRegsInClass RcFloat)
		RcInteger	-> 0

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
	RcFloat		-> allocatableRegsDouble

allocatableRegsInteger :: Int
allocatableRegsInteger	
	= length $ filter (\r -> regClass r == RcInteger) 
		 $ map RealReg allocatableRegs

allocatableRegsFloat :: Int
allocatableRegsFloat
	= length $ filter (\r -> regClass r == RcFloat 
		 $ map RealReg allocatableRegs
-}
