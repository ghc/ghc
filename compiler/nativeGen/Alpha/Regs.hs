-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1994-2004
-- 
-- Alpha support is rotted and incomplete.
-- -----------------------------------------------------------------------------


module Alpha.Regs (
{-
	Size(..),
	AddrMode(..),
	fits8Bits,
	fReg,
	gp, pv, ra, sp, t9, t10, t11, t12, v0, f0, zeroh
-}
)

where

{-
#include "nativeGen/NCG.h"
#include "HsVersions.h"
#include "../includes/stg/MachRegs.h"

import RegsBase

import BlockId
import Cmm
import CgUtils          ( get_GlobalReg_addr )
import CLabel           ( CLabel, mkMainCapabilityLabel )
import Pretty
import Outputable	( Outputable(..), pprPanic, panic )
import qualified Outputable
import Unique
import UniqSet
import Constants
import FastTypes
import FastBool
import UniqFM


data Size
	= B	    -- byte
	| Bu
--	| W	    -- word (2 bytes): UNUSED
--	| Wu    -- : UNUSED
	| L	    -- longword (4 bytes)
	| Q	    -- quadword (8 bytes)
--	| FF    -- VAX F-style floating pt: UNUSED
--	| GF    -- VAX G-style floating pt: UNUSED
--	| DF    -- VAX D-style floating pt: UNUSED
--	| SF    -- IEEE single-precision floating pt: UNUSED
	| TF    -- IEEE double-precision floating pt
	deriving Eq


data AddrMode
	= AddrImm	Imm
	| AddrReg	Reg
	| AddrRegImm	Reg Imm


addrOffset :: AddrMode -> Int -> Maybe AddrMode
addrOffset addr off
  = case addr of
      _ -> panic "MachMisc.addrOffset not defined for Alpha"

fits8Bits :: Integer -> Bool
fits8Bits i = i >= -256 && i < 256


-- The Alpha has 64 registers of interest; 32 integer registers and 32 floating
-- point registers.  The mapping of STG registers to alpha machine registers
-- is defined in StgRegs.h.  We are, of course, prepared for any eventuality.

fReg :: Int -> RegNo
fReg x = (32 + x)

v0, f0, ra, pv, gp, sp, zeroh :: Reg
v0    = realReg 0
f0    = realReg (fReg 0)
ra    = FixedReg ILIT(26)
pv    = t12
gp    = FixedReg ILIT(29)
sp    = FixedReg ILIT(30)
zeroh = FixedReg ILIT(31) -- "zero" is used in 1.3 (MonadZero method)

t9, t10, t11, t12 :: Reg
t9  = realReg 23
t10 = realReg 24
t11 = realReg 25
t12 = realReg 27


#define f0 32
#define f1 33
#define f2 34
#define f3 35
#define f4 36
#define f5 37
#define f6 38
#define f7 39
#define f8 40
#define f9 41
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


-- allMachRegs is the complete set of machine regs.
allMachRegNos :: [RegNo]
allMachRegNos	= [0..63]


-- these are the regs which we cannot assume stay alive over a
-- C call.  
callClobberedRegs :: [Reg]
callClobberedRegs
 =	[0, 1, 2, 3, 4, 5, 6, 7, 8,
	 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
	 fReg 0, fReg 1, fReg 10, fReg 11, fReg 12, fReg 13, fReg 14, fReg 15,
	 fReg 16, fReg 17, fReg 18, fReg 19, fReg 20, fReg 21, fReg 22, fReg 23,
	 fReg 24, fReg 25, fReg 26, fReg 27, fReg 28, fReg 29, fReg 30]


-- argRegs is the set of regs which are read for an n-argument call to C.
-- For archs which pass all args on the stack (x86), is empty.
-- Sparc passes up to the first 6 args in regs.
-- Dunno about Alpha.
argRegs :: RegNo -> [Reg]

argRegs 0 = []
argRegs 1 = freeMappedRegs [16, fReg 16]
argRegs 2 = freeMappedRegs [16, 17, fReg 16, fReg 17]
argRegs 3 = freeMappedRegs [16, 17, 18, fReg 16, fReg 17, fReg 18]
argRegs 4 = freeMappedRegs [16, 17, 18, 19, fReg 16, fReg 17, fReg 18, fReg 19]
argRegs 5 = freeMappedRegs [16, 17, 18, 19, 20, fReg 16, fReg 17, fReg 18, fReg 19, fReg 20]
argRegs 6 = freeMappedRegs [16, 17, 18, 19, 20, 21, fReg 16, fReg 17, fReg 18, fReg 19, fReg 20, fReg 21]
argRegs _ = panic "MachRegs.argRegs(alpha): don't know about >6 arguments!"


-- all of the arg regs ??
allArgRegs :: [(Reg, Reg)]
allArgRegs = [(realReg i, realReg (fReg i)) | i <- [16..21]]


-- horror show -----------------------------------------------------------------

freeReg :: RegNo -> FastBool

freeReg 26 = fastBool False  -- return address (ra)
freeReg 28 = fastBool False  -- reserved for the assembler (at)
freeReg 29 = fastBool False  -- global pointer (gp)
freeReg 30 = fastBool False  -- stack pointer (sp)
freeReg 31 = fastBool False  -- always zero (zeroh)
freeReg 63 = fastBool False  -- always zero (f31)

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
freeReg n               = fastBool True


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

-}
