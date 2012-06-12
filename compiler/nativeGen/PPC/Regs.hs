-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1994-2004
-- 
-- -----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module PPC.Regs (
	-- squeeze functions
	virtualRegSqueeze,
	realRegSqueeze,

	mkVirtualReg,
	regDotColor,

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
	classOfRealReg,
	showReg,
	
	-- machine specific
	allFPArgRegs,
	fits16Bits,
	makeImmediate,
	fReg,
	sp, r3, r4, r27, r28, f1, f20, f21,

	-- horrow show
	freeReg,
	globalRegMaybe,
	allocatableRegs

)

where

#include "nativeGen/NCG.h"
#include "HsVersions.h"
#include "../includes/stg/MachRegs.h"

import Reg
import RegClass
import Size

import BlockId
import OldCmm
import CLabel           ( CLabel )
import Unique

import Outputable
import Constants
import FastBool
import FastTypes

import Data.Word	( Word8, Word16, Word32 )
import Data.Int 	( Int8, Int16, Int32 )


-- squeese functions for the graph allocator -----------------------------------

-- | regSqueeze_class reg
--	Calculuate the maximum number of register colors that could be
--	denied to a node of this class due to having this reg 
--	as a neighbour.
--
{-# INLINE virtualRegSqueeze #-}
virtualRegSqueeze :: RegClass -> VirtualReg -> FastInt
virtualRegSqueeze cls vr
 = case cls of
 	RcInteger
	 -> case vr of
	 	VirtualRegI{}		-> _ILIT(1)
		VirtualRegHi{}		-> _ILIT(1)
                _other                  -> _ILIT(0)

	RcDouble
	 -> case vr of
		VirtualRegD{}		-> _ILIT(1)
		VirtualRegF{}		-> _ILIT(0)
                _other                  -> _ILIT(0)

        _other -> _ILIT(0)

{-# INLINE realRegSqueeze #-}
realRegSqueeze :: RegClass -> RealReg -> FastInt
realRegSqueeze cls rr
 = case cls of
 	RcInteger
	 -> case rr of
	 	RealRegSingle regNo
			| regNo	< 32	-> _ILIT(1)	-- first fp reg is 32 
			| otherwise	-> _ILIT(0)
			
		RealRegPair{}		-> _ILIT(0)

	RcDouble
	 -> case rr of
	 	RealRegSingle regNo
			| regNo	< 32	-> _ILIT(0)
			| otherwise	-> _ILIT(1)
			
		RealRegPair{}		-> _ILIT(0)

        _other -> _ILIT(0)

mkVirtualReg :: Unique -> Size -> VirtualReg
mkVirtualReg u size
   | not (isFloatSize size) = VirtualRegI u
   | otherwise
   = case size of
        FF32    -> VirtualRegD u
        FF64    -> VirtualRegD u
        _       -> panic "mkVirtualReg"

regDotColor :: RealReg -> SDoc
regDotColor reg
 = case classOfRealReg reg of
        RcInteger       -> text "blue"
        RcFloat         -> text "red"
        RcDouble        -> text "green"
        RcDoubleSSE     -> text "yellow"


-- immediates ------------------------------------------------------------------
data Imm
	= ImmInt	Int
	| ImmInteger	Integer	    -- Sigh.
	| ImmCLbl	CLabel	    -- AbstractC Label (with baggage)
	| ImmLit	SDoc	    -- Simple string
	| ImmIndex    CLabel Int
	| ImmFloat	Rational
	| ImmDouble	Rational
	| ImmConstantSum Imm Imm
	| ImmConstantDiff Imm Imm
	| LO Imm
	| HI Imm
	| HA Imm	{- high halfword adjusted -}


strImmLit :: String -> Imm
strImmLit s = ImmLit (text s)


litToImm :: CmmLit -> Imm
litToImm (CmmInt i w)        = ImmInteger (narrowS w i)
                -- narrow to the width: a CmmInt might be out of
                -- range, but we assume that ImmInteger only contains
                -- in-range values.  A signed value should be fine here.
litToImm (CmmFloat f W32)    = ImmFloat f
litToImm (CmmFloat f W64)    = ImmDouble f
litToImm (CmmLabel l)        = ImmCLbl l
litToImm (CmmLabelOff l off) = ImmIndex l off
litToImm (CmmLabelDiffOff l1 l2 off)
                             = ImmConstantSum
                               (ImmConstantDiff (ImmCLbl l1) (ImmCLbl l2))
                               (ImmInt off)
litToImm (CmmBlock id)       = ImmCLbl (infoTblLbl id)
litToImm _                   = panic "PPC.Regs.litToImm: no match"


-- addressing modes ------------------------------------------------------------

data AddrMode
	= AddrRegReg	Reg Reg
	| AddrRegImm	Reg Imm


addrOffset :: AddrMode -> Int -> Maybe AddrMode
addrOffset addr off
  = case addr of
      AddrRegImm r (ImmInt n)
       | fits16Bits n2 -> Just (AddrRegImm r (ImmInt n2))
       | otherwise     -> Nothing
       where n2 = n + off

      AddrRegImm r (ImmInteger n)
       | fits16Bits n2 -> Just (AddrRegImm r (ImmInt (fromInteger n2)))
       | otherwise     -> Nothing
       where n2 = n + toInteger off
       
      _ -> Nothing


-- registers -------------------------------------------------------------------
-- @spRel@ gives us a stack relative addressing mode for volatile
-- temporaries and for excess call arguments.  @fpRel@, where
-- applicable, is the same but for the frame pointer.

spRel :: Int	-- desired stack offset in words, positive or negative
      -> AddrMode

spRel n	= AddrRegImm sp (ImmInt (n * wORD_SIZE))


-- argRegs is the set of regs which are read for an n-argument call to C.
-- For archs which pass all args on the stack (x86), is empty.
-- Sparc passes up to the first 6 args in regs.
argRegs :: RegNo -> [Reg]
argRegs 0 = []
argRegs 1 = map regSingle [3]
argRegs 2 = map regSingle [3,4]
argRegs 3 = map regSingle [3..5]
argRegs 4 = map regSingle [3..6]
argRegs 5 = map regSingle [3..7]
argRegs 6 = map regSingle [3..8]
argRegs 7 = map regSingle [3..9]
argRegs 8 = map regSingle [3..10]
argRegs _ = panic "MachRegs.argRegs(powerpc): don't know about >8 arguments!"


allArgRegs :: [Reg]
allArgRegs = map regSingle [3..10]


-- these are the regs which we cannot assume stay alive over a C call.  
callClobberedRegs :: [Reg]
#if   defined(darwin_TARGET_OS)
callClobberedRegs
  = map regSingle (0:[2..12] ++ map fReg [0..13])

#elif defined(linux_TARGET_OS)
callClobberedRegs
  = map regSingle (0:[2..13] ++ map fReg [0..13])

#else
callClobberedRegs
	= panic "PPC.Regs.callClobberedRegs: not defined for this architecture"
#endif


allMachRegNos 	:: [RegNo]
allMachRegNos	= [0..63]


{-# INLINE classOfRealReg      #-}
classOfRealReg :: RealReg -> RegClass
classOfRealReg (RealRegSingle i)
	| i < 32	= RcInteger 
	| otherwise	= RcDouble

classOfRealReg (RealRegPair{})
	= panic "regClass(ppr): no reg pairs on this architecture"

showReg :: RegNo -> String
showReg n
    | n >= 0 && n <= 31	  = "%r" ++ show n
    | n >= 32 && n <= 63  = "%f" ++ show (n - 32)
    | otherwise           = "%unknown_powerpc_real_reg_" ++ show n



-- machine specific ------------------------------------------------------------

allFPArgRegs :: [Reg]
#if    defined(darwin_TARGET_OS)
allFPArgRegs = map (regSingle . fReg) [1..13]

#elif  defined(linux_TARGET_OS)
allFPArgRegs = map (regSingle . fReg) [1..8]

#else
allFPArgRegs = panic "PPC.Regs.allFPArgRegs: not defined for this architecture"

#endif

fits16Bits :: Integral a => a -> Bool
fits16Bits x = x >= -32768 && x < 32768

makeImmediate :: Integral a => Width -> Bool -> a -> Maybe Imm
makeImmediate rep signed x = fmap ImmInt (toI16 rep signed)
    where
        narrow W32 False = fromIntegral (fromIntegral x :: Word32)
        narrow W16 False = fromIntegral (fromIntegral x :: Word16)
        narrow W8  False = fromIntegral (fromIntegral x :: Word8)
        narrow W32 True  = fromIntegral (fromIntegral x :: Int32)
        narrow W16 True  = fromIntegral (fromIntegral x :: Int16)
        narrow W8  True  = fromIntegral (fromIntegral x :: Int8)
	narrow _   _     = panic "PPC.Regs.narrow: no match"
        
        narrowed = narrow rep signed
        
        toI16 W32 True
            | narrowed >= -32768 && narrowed < 32768 = Just narrowed
            | otherwise = Nothing
        toI16 W32 False
            | narrowed >= 0 && narrowed < 65536 = Just narrowed
            | otherwise = Nothing
        toI16 _ _  = Just narrowed


{-
The PowerPC has 64 registers of interest; 32 integer registers and 32 floating
point registers.
-}

fReg :: Int -> RegNo
fReg x = (32 + x)

sp, r3, r4, r27, r28, f1, f20, f21 :: Reg
sp 	= regSingle 1
r3 	= regSingle 3
r4 	= regSingle 4
r27 	= regSingle 27
r28 	= regSingle 28
f1 	= regSingle $ fReg 1
f20 	= regSingle $ fReg 20
f21 	= regSingle $ fReg 21



-- horror show -----------------------------------------------------------------
freeReg :: RegNo -> FastBool
globalRegMaybe :: GlobalReg -> Maybe Reg


#if powerpc_TARGET_ARCH
#define r0 0
#define r1 1
#define r2 2
#define r3 3
#define r4 4
#define r5 5
#define r6 6
#define r7 7
#define r8 8
#define r9 9
#define r10 10
#define r11 11
#define r12 12
#define r13 13
#define r14 14
#define r15 15
#define r16 16
#define r17 17
#define r18 18
#define r19 19
#define r20 20
#define r21 21
#define r22 22
#define r23 23
#define r24 24
#define r25 25
#define r26 26
#define r27 27
#define r28 28
#define r29 29
#define r30 30
#define r31 31

#ifdef darwin_TARGET_OS
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
#else
#define fr0  32
#define fr1  33
#define fr2  34
#define fr3  35
#define fr4  36
#define fr5  37
#define fr6  38
#define fr7  39
#define fr8  40
#define fr9  41
#define fr10 42
#define fr11 43
#define fr12 44
#define fr13 45
#define fr14 46
#define fr15 47
#define fr16 48
#define fr17 49
#define fr18 50
#define fr19 51
#define fr20 52
#define fr21 53
#define fr22 54
#define fr23 55
#define fr24 56
#define fr25 57
#define fr26 58
#define fr27 59
#define fr28 60
#define fr29 61
#define fr30 62
#define fr31 63
#endif



freeReg 0 = fastBool False -- Hack: r0 can't be used in all insns, but it's actually free
freeReg 1 = fastBool False -- The Stack Pointer
#if !darwin_TARGET_OS
 -- most non-darwin powerpc OSes use r2 as a TOC pointer or something like that
freeReg 2 = fastBool False
#endif

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
#ifdef REG_R9
freeReg REG_R9   = fastBool False
#endif
#ifdef REG_R10
freeReg REG_R10  = fastBool False
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
freeReg _               = fastBool True


--  | Returns 'Nothing' if this global register is not stored
-- in a real machine register, otherwise returns @'Just' reg@, where
-- reg is the machine register it is stored in.


#ifdef REG_Base
globalRegMaybe BaseReg			= Just (regSingle REG_Base)
#endif
#ifdef REG_R1
globalRegMaybe (VanillaReg 1 _)		= Just (regSingle REG_R1)
#endif 
#ifdef REG_R2 
globalRegMaybe (VanillaReg 2 _)		= Just (regSingle REG_R2)
#endif 
#ifdef REG_R3 
globalRegMaybe (VanillaReg 3 _) 	= Just (regSingle REG_R3)
#endif 
#ifdef REG_R4 
globalRegMaybe (VanillaReg 4 _)		= Just (regSingle REG_R4)
#endif 
#ifdef REG_R5 
globalRegMaybe (VanillaReg 5 _)		= Just (regSingle REG_R5)
#endif 
#ifdef REG_R6 
globalRegMaybe (VanillaReg 6 _)		= Just (regSingle REG_R6)
#endif 
#ifdef REG_R7 
globalRegMaybe (VanillaReg 7 _)		= Just (regSingle REG_R7)
#endif 
#ifdef REG_R8 
globalRegMaybe (VanillaReg 8 _)		= Just (regSingle REG_R8)
#endif
#ifdef REG_R9 
globalRegMaybe (VanillaReg 9 _)		= Just (regSingle REG_R9)
#endif
#ifdef REG_R10 
globalRegMaybe (VanillaReg 10 _)	= Just (regSingle REG_R10)
#endif
#ifdef REG_F1
globalRegMaybe (FloatReg 1)		= Just (regSingle REG_F1)
#endif				 	
#ifdef REG_F2			 	
globalRegMaybe (FloatReg 2)		= Just (regSingle REG_F2)
#endif				 	
#ifdef REG_F3			 	
globalRegMaybe (FloatReg 3)		= Just (regSingle REG_F3)
#endif				 	
#ifdef REG_F4			 	
globalRegMaybe (FloatReg 4)		= Just (regSingle REG_F4)
#endif				 	
#ifdef REG_D1			 	
globalRegMaybe (DoubleReg 1)		= Just (regSingle REG_D1)
#endif				 	
#ifdef REG_D2			 	
globalRegMaybe (DoubleReg 2)		= Just (regSingle REG_D2)
#endif
#ifdef REG_Sp	    
globalRegMaybe Sp		   	= Just (regSingle REG_Sp)
#endif
#ifdef REG_Lng1			 	
globalRegMaybe (LongReg 1)		= Just (regSingle REG_Lng1)
#endif				 	
#ifdef REG_Lng2			 	
globalRegMaybe (LongReg 2)		= Just (regSingle REG_Lng2)
#endif
#ifdef REG_SpLim	    			
globalRegMaybe SpLim		   	= Just (regSingle REG_SpLim)
#endif	    				
#ifdef REG_Hp	   			
globalRegMaybe Hp		   	= Just (regSingle REG_Hp)
#endif	    				
#ifdef REG_HpLim      			
globalRegMaybe HpLim		   	= Just (regSingle REG_HpLim)
#endif	    				
#ifdef REG_CurrentTSO      			
globalRegMaybe CurrentTSO	   	= Just (regSingle REG_CurrentTSO)
#endif	    				
#ifdef REG_CurrentNursery      			
globalRegMaybe CurrentNursery	   	= Just (regSingle REG_CurrentNursery)
#endif	    				
globalRegMaybe _		   	= Nothing


#else  /* powerpc_TARGET_ARCH */

freeReg _		= 0#
globalRegMaybe _	= panic "PPC.Regs.globalRegMaybe: not defined"

#endif /* powerpc_TARGET_ARCH */


-- allocatableRegs is allMachRegNos with the fixed-use regs removed.
-- i.e., these are the regs for which we are prepared to allow the
-- register allocator to attempt to map VRegs to.
allocatableRegs :: [RealReg]
allocatableRegs
   = let isFree i = isFastTrue (freeReg i)
     in  map RealRegSingle $ filter isFree allMachRegNos
