
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

-- | Nasty #ifdefery that generates the definitions for
--	freeReg and globalRegMaybe from the information in includes/MachRegs.h.
--	
--	If the current TARGET_ARCH isn't sparc then these functions will be wrong.
--
module SPARC.RegPlate (
	freeReg,
	globalRegMaybe
)

where

#include "HsVersions.h"

import Reg
import CmmExpr
import FastBool

-- Register numbers for SPARC hardware registers.
--	These names are the same as the ones in Regs.hs, but those have
--	type Reg and not RegNo.
--
#ifdef sparc_TARGET_ARCH

#define g0	0
#define g1	1
#define g2	2
#define g3	3
#define g4	4
#define g5	5
#define g6	6
#define g7	7

#define o0	8
#define o1	9	
#define o2	10
#define o3	11
#define o4	12
#define o5	13
#define o6	14
#define o7	15

#define l0	16
#define l1	17
#define l2	18
#define l3	19
#define l4	20
#define l5	21
#define l6	22
#define l7	23

#define i0	24
#define i1	25
#define i2	26
#define i3	27
#define i4	28
#define i5	29
#define i6	30
#define i7	31

#define f0	32
#define f1	33
#define f2	34
#define f3	35
#define f4	36
#define f5	37
#define f6	38
#define f7	39
#define f8	40
#define f9	41
#define f10	42
#define f11	43
#define f12	44
#define f13	45
#define f14	46
#define f15	47
#define f16	48
#define f17	49
#define f18	50
#define f19	51
#define f20	52
#define f21	53
#define f22	54
#define f23	55
#define f24	56
#define f25	57
#define f26	58
#define f27	59
#define f28	60
#define f29	61
#define f30	62
#define f31	63


#include "../includes/stg/HaskellMachRegs.h"

-- | Check whether a machine register is free for allocation.
freeReg :: RegNo -> FastBool


-- SPARC regs used by the OS / ABI
-- %g0(r0) is always zero
freeReg g0	= fastBool False

-- %g5(r5) - %g7(r7) 
--	are reserved for the OS
freeReg g5	= fastBool False
freeReg g6	= fastBool False
freeReg g7	= fastBool False

-- %o6(r14) 
--	is the C stack pointer
freeReg	o6	= fastBool False

-- %o7(r15) 
--	holds the C return address
freeReg	o7	= fastBool False

-- %i6(r30) 
--	is the C frame pointer
freeReg	i6	= fastBool False

-- %i7(r31) 
--	is used for C return addresses
freeReg	i7	= fastBool False

-- %f0(r32) - %f1(r32)
--	are C floating point return regs
freeReg f0	= fastBool False
freeReg f1	= fastBool False
	
{-
freeReg regNo
	-- don't release high half of double regs
	| regNo >= f0
	, regNo <  NCG_FirstFloatReg
	, regNo `mod` 2 /= 0
	= fastBool False
-}
--------------------------------------


#ifdef REG_Base
freeReg REG_Base = fastBool False
#endif
#ifdef REG_R1
freeReg REG_R1	= fastBool False
#endif	
#ifdef REG_R2  
freeReg REG_R2	= fastBool False
#endif	
#ifdef REG_R3  
freeReg REG_R3	= fastBool False
#endif	
#ifdef REG_R4  
freeReg REG_R4	= fastBool False
#endif	
#ifdef REG_R5  
freeReg REG_R5	= fastBool False
#endif	
#ifdef REG_R6  
freeReg REG_R6	= fastBool False
#endif	
#ifdef REG_R7  
freeReg REG_R7	= fastBool False
#endif	
#ifdef REG_R8  
freeReg REG_R8	= fastBool False
#endif
#ifdef REG_R9
freeReg REG_R9	= fastBool False
#endif
#ifdef REG_R10
freeReg REG_R10	= fastBool False
#endif
#ifdef REG_F1
freeReg REG_F1	= fastBool False
#endif
#ifdef REG_F2
freeReg REG_F2	= fastBool False
#endif
#ifdef REG_F3
freeReg REG_F3	= fastBool False
#endif
#ifdef REG_F4
freeReg REG_F4	= fastBool False
#endif
#ifdef REG_D1
freeReg REG_D1	= fastBool False
#endif
#ifdef REG_D1_2
freeReg REG_D1_2 = fastBool False
#endif
#ifdef REG_D2
freeReg REG_D2	= fastBool False
#endif
#ifdef REG_D2_2
freeReg REG_D2_2 = fastBool False
#endif
#ifdef REG_Sp 
freeReg REG_Sp	= fastBool False
#endif 
#ifdef REG_Su
freeReg REG_Su	= fastBool False
#endif 
#ifdef REG_SpLim 
freeReg REG_SpLim = fastBool False
#endif 
#ifdef REG_Hp 
freeReg REG_Hp	= fastBool False
#endif
#ifdef REG_HpLim
freeReg REG_HpLim = fastBool False
#endif
freeReg _	= fastBool True



--  | Returns 'Nothing' if this global register is not stored
-- in a real machine register, otherwise returns @'Just' reg@, where
-- reg is the machine register it is stored in.


globalRegMaybe :: GlobalReg -> Maybe RealReg

#ifdef REG_Base
globalRegMaybe BaseReg			= Just (RealRegSingle REG_Base)
#endif
#ifdef REG_R1
globalRegMaybe (VanillaReg 1 _)		= Just (RealRegSingle REG_R1)
#endif 
#ifdef REG_R2 
globalRegMaybe (VanillaReg 2 _)		= Just (RealRegSingle REG_R2)
#endif 
#ifdef REG_R3 
globalRegMaybe (VanillaReg 3 _) 	= Just (RealRegSingle REG_R3)
#endif 
#ifdef REG_R4 
globalRegMaybe (VanillaReg 4 _)		= Just (RealRegSingle REG_R4)
#endif 
#ifdef REG_R5 
globalRegMaybe (VanillaReg 5 _)		= Just (RealRegSingle REG_R5)
#endif 
#ifdef REG_R6 
globalRegMaybe (VanillaReg 6 _)		= Just (RealRegSingle REG_R6)
#endif 
#ifdef REG_R7 
globalRegMaybe (VanillaReg 7 _)		= Just (RealRegSingle REG_R7)
#endif 
#ifdef REG_R8 
globalRegMaybe (VanillaReg 8 _)		= Just (RealRegSingle REG_R8)
#endif
#ifdef REG_R9 
globalRegMaybe (VanillaReg 9 _)		= Just (RealRegSingle REG_R9)
#endif
#ifdef REG_R10 
globalRegMaybe (VanillaReg 10 _)	= Just (RealRegSingle REG_R10)
#endif
#ifdef REG_F1
globalRegMaybe (FloatReg 1)		= Just (RealRegSingle REG_F1)
#endif				 	
#ifdef REG_F2			 	
globalRegMaybe (FloatReg 2)		= Just (RealRegSingle REG_F2)
#endif				 	
#ifdef REG_F3			 	
globalRegMaybe (FloatReg 3)		= Just (RealRegSingle REG_F3)
#endif				 	
#ifdef REG_F4			 	
globalRegMaybe (FloatReg 4)		= Just (RealRegSingle REG_F4)
#endif				 	
#ifdef REG_D1			 	
globalRegMaybe (DoubleReg 1)		= Just (RealRegPair REG_D1 (REG_D1 + 1))
#endif				 	
#ifdef REG_D2			 	
globalRegMaybe (DoubleReg 2)		= Just (RealRegPair REG_D2 (REG_D2 + 1))
#endif
#ifdef REG_Sp	    
globalRegMaybe Sp		   	= Just (RealRegSingle REG_Sp)
#endif
#ifdef REG_Lng1			 	
globalRegMaybe (LongReg 1)		= Just (RealRegSingle REG_Lng1)
#endif				 	
#ifdef REG_Lng2			 	
globalRegMaybe (LongReg 2)		= Just (RealRegSingle REG_Lng2)
#endif
#ifdef REG_SpLim	    			
globalRegMaybe SpLim		   	= Just (RealRegSingle REG_SpLim)
#endif	    				
#ifdef REG_Hp	   			
globalRegMaybe Hp		   	= Just (RealRegSingle REG_Hp)
#endif	    				
#ifdef REG_HpLim      			
globalRegMaybe HpLim		   	= Just (RealRegSingle REG_HpLim)
#endif	    				
#ifdef REG_CurrentTSO      			
globalRegMaybe CurrentTSO	   	= Just (RealRegSingle REG_CurrentTSO)
#endif	    				
#ifdef REG_CurrentNursery      			
globalRegMaybe CurrentNursery	   	= Just (RealRegSingle REG_CurrentNursery)
#endif	    				
globalRegMaybe _		   	= Nothing

#else
freeReg :: RegNo -> FastBool
freeReg		= error "SPARC.RegPlate.freeReg: not defined"

globalRegMaybe :: GlobalReg -> Maybe RealReg
globalRegMaybe	= error "SPARC.RegPlate.globalRegMaybe: not defined"

#endif
