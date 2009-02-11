-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1994-2004
-- 
-- -----------------------------------------------------------------------------

module SPARC.Regs (

	-- sizes
	Size(..),
	intSize, 
	floatSize, 
	isFloatSize, 
	wordSize,
	cmmTypeSize,
	sizeToWidth,
	mkVReg,

	-- immediate values
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

	-- machine specific info
	fpRel,
	fits13Bits, 
	largeOffsetError,
	gReg, iReg, lReg, oReg, fReg,
	fp, sp, g0, g1, g2, o0, o1, f0, f6, f8, f26, f27,
	nCG_FirstFloatReg,

	-- horror show
	freeReg,
	globalRegMaybe
)

where

#include "nativeGen/NCG.h"
#include "HsVersions.h"
#include "../includes/MachRegs.h"

import RegsBase

import BlockId
import Cmm
import CLabel           ( CLabel )
import Pretty
import Outputable	( Outputable(..), pprPanic, panic )
import qualified Outputable
import Unique
import Constants
import FastBool

-- sizes -----------------------------------------------------------------------

-- | A 'Size' also includes format information, such as whether
--	the word is signed or unsigned.
--
data Size
	= II8     -- byte (signed)
	| II16    -- halfword (signed, 2 bytes)
	| II32    -- word (4 bytes)
	| II64    -- word (8 bytes)
	| FF32    -- IEEE single-precision floating pt
	| FF64    -- IEEE single-precision floating pt
	deriving Eq


-- | Get the integer size of this width.
intSize :: Width -> Size
intSize width
 = case width of
 	W8 	-> II8
	W16	-> II16
	W32	-> II32
	W64 	-> II64
	other	-> pprPanic "SPARC.Regs.intSize" (ppr other)


-- | Get the float size of this width.
floatSize :: Width -> Size
floatSize width
 = case width of
 	W32	-> FF32
	W64	-> FF64
	other 	-> pprPanic "SPARC.Regs.intSize" (ppr other)


-- | Check if a size represents a floating point value.
isFloatSize :: Size -> Bool
isFloatSize size
 = case size of
 	FF32	-> True
	FF64	-> True
	_	-> False


-- | Size of a machine word. 
--	This is big enough to hold a pointer.
wordSize :: Size
wordSize = intSize wordWidth


-- | Convert a Cmm type to a Size.
cmmTypeSize :: CmmType -> Size
cmmTypeSize ty 
	| isFloatType ty	= floatSize (typeWidth ty)
	| otherwise		= intSize (typeWidth ty)


-- | Get the Width of a Size.
sizeToWidth :: Size -> Width
sizeToWidth size
 = case size of
 	II8		-> W8
	II16		-> W16
	II32		-> W32
	II64		-> W64
	FF32		-> W32
	FF64		-> W64


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


-- immediates ------------------------------------------------------------------

-- | An immediate value.
--	Not all of these are directly representable by the machine. 
--	Things like ImmLit are slurped out and put in a data segment instead.
--
data Imm
	= ImmInt	Int

	-- Sigh.
	| ImmInteger	Integer	    

	-- AbstractC Label (with baggage)
	| ImmCLbl	CLabel	    

	-- Simple string
	| ImmLit	Doc	    
	| ImmIndex	CLabel Int
	| ImmFloat	Rational
	| ImmDouble	Rational

	| ImmConstantSum  Imm Imm
	| ImmConstantDiff Imm Imm

	| LO 	Imm		   
	| HI 	Imm


-- | Create a ImmLit containing this string.
strImmLit :: String -> Imm
strImmLit s = ImmLit (text s)


-- | Convert a CmmLit to an Imm.
-- 	Narrow to the width: a CmmInt might be out of
-- 	range, but we assume that ImmInteger only contains
-- 	in-range values.  A signed value should be fine here.
--
litToImm :: CmmLit -> Imm
litToImm lit
 = case lit of
 	CmmInt i w		-> ImmInteger (narrowS w i)
	CmmFloat f W32		-> ImmFloat f
	CmmFloat f W64		-> ImmDouble f
	CmmLabel l		-> ImmCLbl l
	CmmLabelOff l off	-> ImmIndex l off

	CmmLabelDiffOff l1 l2 off
	 -> ImmConstantSum
		(ImmConstantDiff (ImmCLbl l1) (ImmCLbl l2))
		(ImmInt off)

	CmmBlock id	-> ImmCLbl (infoTblLbl id)
	_		-> panic "SPARC.Regs.litToImm: no match"



-- addressing modes ------------------------------------------------------------

-- | Represents a memory address in an instruction.
--	Being a RISC machine, the SPARC addressing modes are very regular.
--
data AddrMode
	= AddrRegReg	Reg Reg		-- addr = r1 + r2
	| AddrRegImm	Reg Imm		-- addr = r1 + imm


-- | Add an integer offset to the address in an AddrMode.
--
addrOffset :: AddrMode -> Int -> Maybe AddrMode
addrOffset addr off
  = case addr of
      AddrRegImm r (ImmInt n)
       | fits13Bits n2 -> Just (AddrRegImm r (ImmInt n2))
       | otherwise     -> Nothing
       where n2 = n + off

      AddrRegImm r (ImmInteger n)
       | fits13Bits n2 -> Just (AddrRegImm r (ImmInt (fromInteger n2)))
       | otherwise     -> Nothing
       where n2 = n + toInteger off

      AddrRegReg r (RealReg 0)
       | fits13Bits off -> Just (AddrRegImm r (ImmInt off))
       | otherwise     -> Nothing
       
      _ -> Nothing



-- registers -------------------------------------------------------------------

-- | Get an AddrMode relative to the address in sp.
--	This gives us a stack relative addressing mode for volatile
-- 	temporaries and for excess call arguments.  
--
spRel :: Int		-- ^ stack offset in words, positive or negative
      -> AddrMode

spRel n	= AddrRegImm sp (ImmInt (n * wORD_SIZE))


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


-- | The RegNos corresponding to all the registers in the machine.
--	For SPARC we use f0-f22 as doubles, so pretend that the high halves
--	of these, ie f23, f25 .. don't exist.
--
allMachRegNos :: [RegNo]
allMachRegNos	
	= ([0..31]
               ++ [32,34 .. nCG_FirstFloatReg-1]
               ++ [nCG_FirstFloatReg .. 63])	


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


-- | Get the standard name for the register with this number.
showReg :: RegNo -> String
showReg n
	| n >= 0  && n < 8   = "%g" ++ show n
	| n >= 8  && n < 16  = "%o" ++ show (n-8)
	| n >= 16 && n < 24  = "%l" ++ show (n-16)
	| n >= 24 && n < 32  = "%i" ++ show (n-24)
	| n >= 32 && n < 64  = "%f" ++ show (n-32)
	| otherwise          = panic "SPARC.Regs.showReg: unknown sparc register"


-- machine specific ------------------------------------------------------------

-- | Get an address relative to the frame pointer.
--	This doesn't work work for offsets greater than 13 bits; we just hope for the best
--
fpRel :: Int -> AddrMode
fpRel n
	= AddrRegImm fp (ImmInt (n * wORD_SIZE))


-- | Check whether an offset is representable with 13 bits.
fits13Bits :: Integral a => a -> Bool
fits13Bits x = x >= -4096 && x < 4096

{-# SPECIALIZE fits13Bits :: Int -> Bool, Integer -> Bool #-}


-- | Sadness.
largeOffsetError :: Integral a => a -> b
largeOffsetError i
  = panic ("ERROR: SPARC native-code generator cannot handle large offset ("
		++ show i ++ ");\nprobably because of large constant data structures;" ++ 
		"\nworkaround: use -fvia-C on this module.\n")



{-
	The SPARC has 64 registers of interest; 32 integer registers and 32
	floating point registers.  The mapping of STG registers to SPARC
	machine registers is defined in StgRegs.h.  We are, of course,
	prepared for any eventuality.

	The whole fp-register pairing thing on sparcs is a huge nuisance.  See
	fptools/ghc/includes/MachRegs.h for a description of what's going on
	here.
-}


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


nCG_FirstFloatReg :: RegNo
nCG_FirstFloatReg = unRealReg NCG_FirstFloatReg
#else
nCG_FirstFloatReg :: RegNo
nCG_FirstFloatReg = unRealReg f22
#endif


-- horror show -----------------------------------------------------------------
#if sparc_TARGET_ARCH
#define g0 0
#define g1 1
#define g2 2
#define g3 3
#define g4 4
#define g5 5
#define g6 6
#define g7 7
#define o0 8
#define o1 9
#define o2 10
#define o3 11
#define o4 12
#define o5 13
#define o6 14
#define o7 15
#define l0 16
#define l1 17
#define l2 18
#define l3 19
#define l4 20
#define l5 21
#define l6 22
#define l7 23
#define i0 24
#define i1 25
#define i2 26
#define i3 27
#define i4 28
#define i5 29
#define i6 30
#define i7 31

-- | Check whether a machine register is free for allocation.
--	This needs to match the info in includes/MachRegs.h otherwise modules
--	compiled with the NCG won't be compatible with via-C ones.
--
freeReg :: RegNo -> FastBool
freeReg regno
 = case regno of
	-- %g0(r0) is always 0.
 	0	-> fastBool False	

 	-- %g1(r1) - %g4(r4) are allocable -----------------

freeReg :: RegNo -> FastBool

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
	--	are STG regs Sp, Base, SpLim, Hp, HpLim, R6
	24	-> fastBool False
	25	-> fastBool False
	26	-> fastBool False
	27	-> fastBool False
	28	-> fastBool False
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
	


-- | Returns Just the real register that a global register is stored in.
--	Returns Nothing if the global has no real register, and is stored
--	in the in-memory register table instead.
--
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
	HpLim		-> Just (RealReg 28)	-- %i4

globalRegMaybe :: GlobalReg -> Maybe Reg



#else

freeReg	_	= 0#
globalRegMaybe	= panic "SPARC.Regs.globalRegMaybe: not defined"

#endif


