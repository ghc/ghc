-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1994-2004
-- 
-- -----------------------------------------------------------------------------

module SPARC.Regs (
	-- registers
	showReg,
	regClass,
	allMachRegNos,

	-- machine specific info
	gReg, iReg, lReg, oReg, fReg,
	fp, sp, g0, g1, g2, o0, o1, f0, f6, f8, f22, f26, f27,
	nCG_FirstFloatReg,
	fPair,

	-- allocatable
	allocatableRegs,
	get_GlobalReg_reg_or_addr,

	-- args
	argRegs, 
	allArgRegs, 
	callClobberedRegs,

	-- 
	mkVReg,
	regDotColor
)

where


import SPARC.RegPlate
import Reg
import RegClass
import Size

import Cmm
import PprCmm		()
import CgUtils          ( get_GlobalReg_addr )

import Unique
import Outputable
import FastBool


{-
	The SPARC has 64 registers of interest; 32 integer registers and 32
	floating point registers.  The mapping of STG registers to SPARC
	machine registers is defined in StgRegs.h.  We are, of course,
	prepared for any eventuality.

	The whole fp-register pairing thing on sparcs is a huge nuisance.  See
	fptools/ghc/includes/MachRegs.h for a description of what's going on
	here.
-}


-- | Get the standard name for the register with this number.
showReg :: RegNo -> String
showReg n
	| n >= 0  && n < 8   = "%g" ++ show n
	| n >= 8  && n < 16  = "%o" ++ show (n-8)
	| n >= 16 && n < 24  = "%l" ++ show (n-16)
	| n >= 24 && n < 32  = "%i" ++ show (n-24)
	| n >= 32 && n < 64  = "%f" ++ show (n-32)
	| otherwise          = panic "SPARC.Regs.showReg: unknown sparc register"


-- | Get the class of a register.
{-# INLINE regClass      #-}
regClass :: Reg -> RegClass
regClass reg
 = case reg of
 	VirtualRegI  _	-> RcInteger
	VirtualRegHi _	-> RcInteger
	VirtualRegF  _	-> RcFloat
	VirtualRegD  _	-> RcDouble
	RealReg i
	  | i < 32			-> RcInteger 
	  | i < nCG_FirstFloatReg	-> RcDouble
	  | otherwise			-> RcFloat


-- | The RegNos corresponding to all the registers in the machine.
--	For SPARC we use f0-f22 as doubles, so pretend that the high halves
--	of these, ie f23, f25 .. don't exist.
--
allMachRegNos :: [RegNo]
allMachRegNos	
	= ([0..31]
               ++ [32,34 .. nCG_FirstFloatReg-1]
               ++ [nCG_FirstFloatReg .. 63])	


-- | Get the regno for this sort of reg
gReg, lReg, iReg, oReg, fReg :: Int -> RegNo

gReg x	= x		-- global regs
oReg x	= (8 + x)	-- output regs
lReg x	= (16 + x)	-- local regs
iReg x	= (24 + x)	-- input regs
fReg x	= (32 + x)	-- float regs


-- | Some specific regs used by the code generator.
g0, g1, g2, fp, sp, o0, o1, f0, f6, f8, f22, f26, f27 :: Reg

f6  = RealReg (fReg 6)
f8  = RealReg (fReg 8)
f22 = RealReg (fReg 22)
f26 = RealReg (fReg 26)
f27 = RealReg (fReg 27)

g0  = RealReg (gReg 0)	-- g0 is always zero, and writes to it vanish.
g1  = RealReg (gReg 1)
g2  = RealReg (gReg 2)

-- FP, SP, int and float return (from C) regs.
fp  = RealReg (iReg 6)
sp  = RealReg (oReg 6)
o0  = RealReg (oReg 0)
o1  = RealReg (oReg 1)
f0  = RealReg (fReg 0)


-- | We use he first few float regs as double precision. 
--	This is the RegNo of the first float regs we use as single precision.
--
nCG_FirstFloatReg :: RegNo
nCG_FirstFloatReg = 54


-- | Produce the second-half-of-a-double register given the first half.
fPair :: Reg -> Maybe Reg
fPair (RealReg n) 
	| n >= 32 && n `mod` 2 == 0  = Just (RealReg (n+1))

fPair (VirtualRegD u)
	= Just (VirtualRegHi u)

fPair reg
	= trace ("MachInstrs.fPair: can't get high half of supposed double reg " ++ showPpr reg)
		Nothing



-- allocatableRegs is allMachRegNos with the fixed-use regs removed.
-- i.e., these are the regs for which we are prepared to allow the
-- register allocator to attempt to map VRegs to.
allocatableRegs :: [RegNo]
allocatableRegs
   = let isFree i = isFastTrue (freeReg i)
     in  filter isFree allMachRegNos



-- We map STG registers onto appropriate CmmExprs.  Either they map
-- to real machine registers or stored as offsets from BaseReg.  Given
-- a GlobalReg, get_GlobalReg_reg_or_addr produces either the real
-- register it is in, on this platform, or a CmmExpr denoting the
-- address in the register table holding it.
-- (See also get_GlobalReg_addr in CgUtils.)

get_GlobalReg_reg_or_addr :: GlobalReg -> Either Reg CmmExpr
get_GlobalReg_reg_or_addr mid
   = case globalRegMaybe mid of
        Just rr -> Left rr
        Nothing -> Right (get_GlobalReg_addr mid)


-- | The registers to place arguments for function calls, 
--	for some number of arguments.
--
argRegs :: RegNo -> [Reg]
argRegs r
 = case r of
 	0	-> []
	1	-> map (RealReg . oReg) [0]
	2	-> map (RealReg . oReg) [0,1]
	3	-> map (RealReg . oReg) [0,1,2]
	4	-> map (RealReg . oReg) [0,1,2,3]
	5	-> map (RealReg . oReg) [0,1,2,3,4]
	6	-> map (RealReg . oReg) [0,1,2,3,4,5]
	_	-> panic "MachRegs.argRegs(sparc): don't know about >6 arguments!"


-- | All all the regs that could possibly be returned by argRegs
--
allArgRegs :: [Reg]
allArgRegs 
	= map RealReg [oReg i | i <- [0..5]]


-- These are the regs that we cannot assume stay alive over a C call.  
--	TODO: Why can we assume that o6 isn't clobbered? -- BL 2009/02
--
callClobberedRegs :: [Reg]
callClobberedRegs
	= map RealReg 
	        (  oReg 7 :
	          [oReg i | i <- [0..5]] ++
	          [gReg i | i <- [1..7]] ++
	          [fReg i | i <- [0..31]] )



-- | Make a virtual reg with this size.
mkVReg :: Unique -> Size -> Reg
mkVReg u size
	| not (isFloatSize size) 
	= VirtualRegI u

	| otherwise
	= case size of
		FF32    -> VirtualRegF u
		FF64	-> VirtualRegD u
		_ 	-> panic "mkVReg"


regDotColor :: Reg -> SDoc
regDotColor reg
 = case regClass reg of
 	RcInteger	-> text "blue"
	RcFloat		-> text "red"
	RcDouble	-> text "green"





-- Hard coded freeReg / globalRegMaybe -----------------------------------------
-- This isn't being used at the moment because we're generating
--	these functions from the information in includes/MachRegs.hs via RegPlate.hs
	
-- | Check whether a machine register is free for allocation.
--	This needs to match the info in includes/MachRegs.h otherwise modules
--	compiled with the NCG won't be compatible with via-C ones.
--
{-
freeReg :: RegNo -> FastBool
freeReg regno
 = case regno of
	-- %g0(r0) is always 0.
 	0	-> fastBool False	

 	-- %g1(r1) - %g4(r4) are allocable -----------------

	-- %g5(r5) - %g7(r7) 
	--	are reserved for the OS
	5	-> fastBool False
	6	-> fastBool False
	7	-> fastBool False

	-- %o0(r8) - %o5(r13) are allocable ----------------

	-- %o6(r14) 
	--	is the C stack pointer
	14	-> fastBool False

	-- %o7(r15) 
	--	holds C return addresses (???)
	15	-> fastBool False

	-- %l0(r16) is allocable ---------------------------

	-- %l1(r17) - %l5(r21) 
	--	are STG regs R1 - R5
	17	-> fastBool False
	18	-> fastBool False
	19	-> fastBool False
	20	-> fastBool False
	21	-> fastBool False
	
	-- %l6(r22) - %l7(r23) are allocable --------------
	
	-- %i0(r24) - %i5(r29)
	--	are STG regs Sp, Base, SpLim, Hp, R6
	24	-> fastBool False
	25	-> fastBool False
	26	-> fastBool False
	27	-> fastBool False

	-- %i5(r28) is allocable --------------------------

	29	-> fastBool False
	
	-- %i6(r30) 
	--	is the C frame pointer
	30	-> fastBool False

	-- %i7(r31) 
	--	is used for C return addresses
	31	-> fastBool False
	
	-- %f0(r32) - %f1(r33)
	--	are C fp return registers
	32	-> fastBool False
	33	-> fastBool False

	-- %f2(r34) - %f5(r37)
	--	are STG regs D1 - D2
	34	-> fastBool False
	35	-> fastBool False
	36	-> fastBool False
	37	-> fastBool False

	-- %f22(r54) - %f25(r57)
	--	are STG regs F1 - F4
	54	-> fastBool False
	55	-> fastBool False
	56	-> fastBool False
	57	-> fastBool False

	-- regs not matched above are allocable.
	_	-> fastBool True

-}

-- | Returns Just the real register that a global register is stored in.
--	Returns Nothing if the global has no real register, and is stored
--	in the in-memory register table instead.
--
{-
globalRegMaybe  :: GlobalReg -> Maybe Reg
globalRegMaybe gg
 = case gg of
	-- Argument and return regs
	VanillaReg 1 _	-> Just (RealReg 17)	-- %l1
	VanillaReg 2 _	-> Just (RealReg 18)	-- %l2
	VanillaReg 3 _	-> Just (RealReg 19)	-- %l3
	VanillaReg 4 _	-> Just (RealReg 20)	-- %l4
	VanillaReg 5 _	-> Just (RealReg 21)	-- %l5
	VanillaReg 6 _	-> Just (RealReg 29)	-- %i5

	FloatReg 1	-> Just (RealReg 54)	-- %f22
	FloatReg 2	-> Just (RealReg 55)	-- %f23
	FloatReg 3	-> Just (RealReg 56)	-- %f24
	FloatReg 4	-> Just (RealReg 57)	-- %f25

	DoubleReg 1	-> Just (RealReg 34)	-- %f2
	DoubleReg 2	-> Just (RealReg 36)	-- %f4

	-- STG Regs
	Sp		-> Just (RealReg 24)	-- %i0
	SpLim		-> Just (RealReg 26)	-- %i2
	Hp		-> Just (RealReg 27)	-- %i3

	BaseReg		-> Just (RealReg 25)	-- %i1
		
	_		-> Nothing 	
-}
