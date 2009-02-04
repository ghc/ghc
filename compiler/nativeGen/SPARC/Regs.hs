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


data Size
	= II8     -- byte (signed)
--	| II8u    -- byte (unsigned)
	| II16    -- halfword (signed, 2 bytes)
--	| II16u   -- halfword (unsigned, 2 bytes)
	| II32    -- word (4 bytes)
	| II64    -- word (8 bytes)
	| FF32    -- IEEE single-precision floating pt
	| FF64    -- IEEE single-precision floating pt
	deriving Eq


intSize, floatSize :: Width -> Size
intSize W8  	= II8
--intSize W16 = II16u
intSize W16 	= II16
intSize W32 	= II32
intSize W64 	= II64
intSize other	= pprPanic "MachInstrs.intSize" (ppr other)

floatSize W32	= FF32
floatSize W64	= FF64
floatSize other	= pprPanic "MachInstrs.intSize" (ppr other)


isFloatSize :: Size -> Bool
isFloatSize FF32	= True
isFloatSize FF64	= True
isFloatSize _		= False


wordSize :: Size
wordSize = intSize wordWidth


cmmTypeSize :: CmmType -> Size
cmmTypeSize ty 
	| isFloatType ty	= floatSize (typeWidth ty)
	| otherwise		= intSize (typeWidth ty)


sizeToWidth :: Size -> Width
sizeToWidth size
 = case size of
 	II8		-> W8
--	II8u		-> W8
	II16		-> W16
--	II16u		-> W16
	II32		-> W32
	II64		-> W64
	FF32		-> W32
	FF64		-> W64


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
data Imm
	= ImmInt	Int
	| ImmInteger	Integer	    -- Sigh.
	| ImmCLbl	CLabel	    -- AbstractC Label (with baggage)
	| ImmLit	Doc	    -- Simple string
	| ImmIndex    CLabel Int
	| ImmFloat	Rational
	| ImmDouble	Rational
	| ImmConstantSum Imm Imm
	| ImmConstantDiff Imm Imm
	| LO Imm		    {- Possible restrictions... -}
	| HI Imm


strImmLit :: String -> Imm
strImmLit s = ImmLit (text s)


-- narrow to the width: a CmmInt might be out of
-- range, but we assume that ImmInteger only contains
-- in-range values.  A signed value should be fine here.
litToImm :: CmmLit -> Imm
litToImm (CmmInt i w)        = ImmInteger (narrowS w i)
litToImm (CmmFloat f W32)    = ImmFloat f
litToImm (CmmFloat f W64)    = ImmDouble f
litToImm (CmmLabel l)        = ImmCLbl l
litToImm (CmmLabelOff l off) = ImmIndex l off

litToImm (CmmLabelDiffOff l1 l2 off)
                             = ImmConstantSum
                               (ImmConstantDiff (ImmCLbl l1) (ImmCLbl l2))
                               (ImmInt off)
litToImm (CmmBlock id)		= ImmCLbl (infoTblLbl id)
litToImm _
	= panic "SPARC.Regs.litToImm: no match"

-- addressing modes ------------------------------------------------------------
data AddrMode
	= AddrRegReg	Reg Reg
	| AddrRegImm	Reg Imm


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

-- @spRel@ gives us a stack relative addressing mode for volatile
-- temporaries and for excess call arguments.  @fpRel@, where
-- applicable, is the same but for the frame pointer.
spRel :: Int	-- desired stack offset in words, positive or negative
      -> AddrMode

spRel n	= AddrRegImm sp (ImmInt (n * wORD_SIZE))


argRegs :: RegNo -> [Reg]
argRegs 0 = []
argRegs 1 = map (RealReg . oReg) [0]
argRegs 2 = map (RealReg . oReg) [0,1]
argRegs 3 = map (RealReg . oReg) [0,1,2]
argRegs 4 = map (RealReg . oReg) [0,1,2,3]
argRegs 5 = map (RealReg . oReg) [0,1,2,3,4]
argRegs 6 = map (RealReg . oReg) [0,1,2,3,4,5]
argRegs _ = panic "MachRegs.argRegs(sparc): don't know about >6 arguments!"


allArgRegs :: [Reg]
allArgRegs = map RealReg [oReg i | i <- [0..5]]


-- These are the regs which we cannot assume stay alive over a C call.  
callClobberedRegs :: [Reg]
callClobberedRegs
	= map RealReg 
	        ( oReg 7 :
	          [oReg i | i <- [0..5]] ++
	          [gReg i | i <- [1..7]] ++
	          [fReg i | i <- [0..31]] )


allMachRegNos :: [RegNo]
allMachRegNos
	= ([0..31]
               ++ [32,34 .. nCG_FirstFloatReg-1]
               ++ [nCG_FirstFloatReg .. 63])	


-- | Get the class of a register.
{-# INLINE regClass      #-}
regClass :: Reg -> RegClass
regClass (VirtualRegI  _)	= RcInteger
regClass (VirtualRegHi _)	= RcInteger
regClass (VirtualRegF  _)	= RcFloat
regClass (VirtualRegD  _)	= RcDouble
regClass (RealReg i) 
	| i < 32		= RcInteger 
	| i < nCG_FirstFloatReg	= RcDouble
	| otherwise		= RcFloat


showReg :: RegNo -> String
showReg n
   | n >= 0  && n < 8   = "%g" ++ show n
   | n >= 8  && n < 16  = "%o" ++ show (n-8)
   | n >= 16 && n < 24  = "%l" ++ show (n-16)
   | n >= 24 && n < 32  = "%i" ++ show (n-24)
   | n >= 32 && n < 64  = "%f" ++ show (n-32)
   | otherwise          = "%unknown_sparc_real_reg_" ++ show n


-- machine specific ------------------------------------------------------------

-- Duznae work for offsets greater than 13 bits; we just hope for the best
fpRel :: Int -> AddrMode
fpRel n
  = AddrRegImm fp (ImmInt (n * wORD_SIZE))


{-# SPECIALIZE fits13Bits :: Int -> Bool, Integer -> Bool #-}
fits13Bits :: Integral a => a -> Bool
fits13Bits x = x >= -4096 && x < 4096


largeOffsetError :: Integral a => a -> b
largeOffsetError i
  = error ("ERROR: SPARC native-code generator cannot handle large offset ("
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


gReg,lReg,iReg,oReg,fReg :: Int -> RegNo
gReg x = x
oReg x = (8 + x)
lReg x = (16 + x)
iReg x = (24 + x)
fReg x = (32 + x)


g0, g1, g2, fp, sp, o0, o1, f0, f6, f8, f22, f26, f27 :: Reg
f6  = RealReg (fReg 6)
f8  = RealReg (fReg 8)
f22 = RealReg (fReg 22)
f26 = RealReg (fReg 26)
f27 = RealReg (fReg 27)


-- g0 is useful for codegen; is always zero, and writes to it vanish.
g0  = RealReg (gReg 0)
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

#define f0  32
#define f1  33
#define f2  34
#define f3  35
#define f4  36
#define f5  37
#define f6  38
#define f7  39
#define f8  40
#define f9  41
#define f10 42
#define f11 43
#define f12 44
#define f13 45
#define f14 46
#define f15 47
#define f16 48
#define f17 49
#define f18 50
#define f19 51
#define f20 52
#define f21 53
#define f22 54
#define f23 55
#define f24 56
#define f25 57
#define f26 58
#define f27 59
#define f28 60
#define f29 61
#define f30 62
#define f31 63
#endif


freeReg :: RegNo -> FastBool

freeReg g0 = fastBool False  --	%g0 is always 0.

freeReg g5 = fastBool False  --	%g5 is reserved (ABI).
freeReg g6 = fastBool False  --	%g6 is reserved (ABI).
freeReg g7 = fastBool False  --	%g7 is reserved (ABI).
freeReg i6 = fastBool False  --	%i6 is our frame pointer.
freeReg i7 = fastBool False  --	%i7 tends to have ret-addr-ish things
freeReg o6 = fastBool False  --	%o6 is our stack pointer.
freeReg o7 = fastBool False  --	%o7 holds ret addrs (???)
freeReg f0 = fastBool False  --  %f0/%f1 are the C fp return registers.
freeReg f1 = fastBool False

-- TODO: Not sure about these BL 2009/01/10
--	Used for NCG spill tmps? what is this?

{-
freeReg g1  = fastBool False  -- %g1 is used for NCG spill tmp
freeReg g2  = fastBool False 
freeReg f6  = fastBool False
freeReg f8  = fastBool False
freeReg f26 = fastBool False
freeReg f27 = fastBool False
-}

#ifdef REG_Base
freeReg REG_Base = fastBool False
#endif
#ifdef REG_R1
freeReg REG_R1   = fastBool False
#endif	
#ifdef REG_R2  
freeReg REG_R2   = fastBool False
#endif	
#ifdef REG_R3  
freeReg REG_R3   = fastBool False
#endif	
#ifdef REG_R4  
freeReg REG_R4   = fastBool False
#endif	
#ifdef REG_R5  
freeReg REG_R5   = fastBool False
#endif	
#ifdef REG_R6  
freeReg REG_R6   = fastBool False
#endif	
#ifdef REG_R7  
freeReg REG_R7   = fastBool False
#endif	
#ifdef REG_R8  
freeReg REG_R8   = fastBool False
#endif
#ifdef REG_F1
freeReg REG_F1 = fastBool False
#endif
#ifdef REG_F2
freeReg REG_F2 = fastBool False
#endif
#ifdef REG_F3
freeReg REG_F3 = fastBool False
#endif
#ifdef REG_F4
freeReg REG_F4 = fastBool False
#endif
#ifdef REG_D1
freeReg REG_D1 = fastBool False
#endif
#ifdef REG_D2
freeReg REG_D2 = fastBool False
#endif
#ifdef REG_Sp 
freeReg REG_Sp   = fastBool False
#endif 
#ifdef REG_Su
freeReg REG_Su   = fastBool False
#endif 
#ifdef REG_SpLim 
freeReg REG_SpLim = fastBool False
#endif 
#ifdef REG_Hp 
freeReg REG_Hp   = fastBool False
#endif
#ifdef REG_HpLim
freeReg REG_HpLim = fastBool False
#endif
freeReg _         = fastBool True



--  | Returns 'Nothing' if this global register is not stored
-- in a real machine register, otherwise returns @'Just' reg@, where
-- reg is the machine register it is stored in.

globalRegMaybe :: GlobalReg -> Maybe Reg

#ifdef REG_Base
globalRegMaybe BaseReg			= Just (RealReg REG_Base)
#endif
#ifdef REG_R1
globalRegMaybe (VanillaReg 1 _)		= Just (RealReg REG_R1)
#endif 
#ifdef REG_R2 
globalRegMaybe (VanillaReg 2 _)		= Just (RealReg REG_R2)
#endif 
#ifdef REG_R3 
globalRegMaybe (VanillaReg 3 _) 	= Just (RealReg REG_R3)
#endif 
#ifdef REG_R4 
globalRegMaybe (VanillaReg 4 _)		= Just (RealReg REG_R4)
#endif 
#ifdef REG_R5 
globalRegMaybe (VanillaReg 5 _)		= Just (RealReg REG_R5)
#endif 
#ifdef REG_R6 
globalRegMaybe (VanillaReg 6 _)		= Just (RealReg REG_R6)
#endif 
#ifdef REG_R7 
globalRegMaybe (VanillaReg 7 _)		= Just (RealReg REG_R7)
#endif 
#ifdef REG_R8 
globalRegMaybe (VanillaReg 8 _)		= Just (RealReg REG_R8)
#endif
#ifdef REG_R9 
globalRegMaybe (VanillaReg 9 _)		= Just (RealReg REG_R9)
#endif
#ifdef REG_R10 
globalRegMaybe (VanillaReg 10 _)	= Just (RealReg REG_R10)
#endif
#ifdef REG_F1
globalRegMaybe (FloatReg 1)		= Just (RealReg REG_F1)
#endif				 	
#ifdef REG_F2			 	
globalRegMaybe (FloatReg 2)		= Just (RealReg REG_F2)
#endif				 	
#ifdef REG_F3			 	
globalRegMaybe (FloatReg 3)		= Just (RealReg REG_F3)
#endif				 	
#ifdef REG_F4			 	
globalRegMaybe (FloatReg 4)		= Just (RealReg REG_F4)
#endif				 	
#ifdef REG_D1			 	
globalRegMaybe (DoubleReg 1)		= Just (RealReg REG_D1)
#endif				 	
#ifdef REG_D2			 	
globalRegMaybe (DoubleReg 2)		= Just (RealReg REG_D2)
#endif
#ifdef REG_Sp	    
globalRegMaybe Sp		   	= Just (RealReg REG_Sp)
#endif
#ifdef REG_Lng1			 	
globalRegMaybe (LongReg 1)		= Just (RealReg REG_Lng1)
#endif				 	
#ifdef REG_Lng2			 	
globalRegMaybe (LongReg 2)		= Just (RealReg REG_Lng2)
#endif
#ifdef REG_SpLim	    			
globalRegMaybe SpLim		   	= Just (RealReg REG_SpLim)
#endif	    				
#ifdef REG_Hp	   			
globalRegMaybe Hp		   	= Just (RealReg REG_Hp)
#endif	    				
#ifdef REG_HpLim      			
globalRegMaybe HpLim		   	= Just (RealReg REG_HpLim)
#endif	    				
#ifdef REG_CurrentTSO      			
globalRegMaybe CurrentTSO	   	= Just (RealReg REG_CurrentTSO)
#endif	    				
#ifdef REG_CurrentNursery      			
globalRegMaybe CurrentNursery	   	= Just (RealReg REG_CurrentNursery)
#endif	    				
globalRegMaybe _		   	= Nothing
