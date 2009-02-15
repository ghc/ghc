module X86.Regs (
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

	-- machine specific
	EABase(..), EAIndex(..), addrModeRegs,

	eax, ebx, ecx, edx, esi, edi, ebp, esp,
	fake0, fake1, fake2, fake3, fake4, fake5,

	rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp,
	r8,  r9,  r10, r11, r12, r13, r14, r15,
  	xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7,
  	xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15,
	xmm,

	ripRel,
	allFPArgRegs,

	-- horror show
	freeReg,
	globalRegMaybe,
	
	get_GlobalReg_reg_or_addr,
	allocatableRegs
)

where

#include "nativeGen/NCG.h"
#include "HsVersions.h"

#if i386_TARGET_ARCH
# define STOLEN_X86_REGS 4
-- HACK: go for the max
#endif

#include "../includes/MachRegs.h"

import Reg
import RegClass

import CgUtils          ( get_GlobalReg_addr )
import BlockId
import Cmm
import CLabel           ( CLabel )
import Pretty
import Outputable	( panic )
import qualified Outputable
import FastBool

#if  defined(i386_TARGET_ARCH) || defined(x86_64_TARGET_ARCH)
import Constants
#endif


-- -----------------------------------------------------------------------------
-- Immediates

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
litToImm _                   = panic "X86.Regs.litToImm: no match"

-- addressing modes ------------------------------------------------------------

data AddrMode
	= AddrBaseIndex	EABase EAIndex Displacement
	| ImmAddr Imm Int

data EABase       = EABaseNone  | EABaseReg Reg | EABaseRip
data EAIndex      = EAIndexNone | EAIndex Reg Int
type Displacement = Imm


addrOffset :: AddrMode -> Int -> Maybe AddrMode
addrOffset addr off
  = case addr of
      ImmAddr i off0	  -> Just (ImmAddr i (off0 + off))

      AddrBaseIndex r i (ImmInt n) -> Just (AddrBaseIndex r i (ImmInt (n + off)))
      AddrBaseIndex r i (ImmInteger n)
	-> Just (AddrBaseIndex r i (ImmInt (fromInteger (n + toInteger off))))

      AddrBaseIndex r i (ImmCLbl lbl)
	-> Just (AddrBaseIndex r i (ImmIndex lbl off))

      AddrBaseIndex r i (ImmIndex lbl ix)
	-> Just (AddrBaseIndex r i (ImmIndex lbl (ix+off)))

      _ -> Nothing  -- in theory, shouldn't happen


addrModeRegs :: AddrMode -> [Reg]
addrModeRegs (AddrBaseIndex b i _) =  b_regs ++ i_regs
  where
   b_regs = case b of { EABaseReg r -> [r]; _ -> [] }
   i_regs = case i of { EAIndex r _ -> [r]; _ -> [] }
addrModeRegs _ = []


-- registers -------------------------------------------------------------------

-- @spRel@ gives us a stack relative addressing mode for volatile
-- temporaries and for excess call arguments.  @fpRel@, where
-- applicable, is the same but for the frame pointer.


spRel :: Int		-- ^ desired stack offset in words, positive or negative
      -> AddrMode

#if   i386_TARGET_ARCH
spRel n	= AddrBaseIndex (EABaseReg esp) EAIndexNone (ImmInt (n * wORD_SIZE))

#elif x86_64_TARGET_ARCH
spRel n	= AddrBaseIndex (EABaseReg rsp) EAIndexNone (ImmInt (n * wORD_SIZE))

#else
spRel _	= panic "X86.Regs.spRel: not defined for this architecture"

#endif


-- argRegs is the set of regs which are read for an n-argument call to C.
-- For archs which pass all args on the stack (x86), is empty.
-- Sparc passes up to the first 6 args in regs.
-- Dunno about Alpha.
argRegs :: RegNo -> [Reg]
argRegs _	= panic "MachRegs.argRegs(x86): should not be used!"





-- | The complete set of machine registers.
allMachRegNos :: [RegNo]

#if   i386_TARGET_ARCH
allMachRegNos	= [0..13]

#elif x86_64_TARGET_ARCH
allMachRegNos  = [0..31]

#else
allMachRegNos	= panic "X86.Regs.callClobberedRegs: not defined for this architecture"

#endif


-- | Take the class of a register.
{-# INLINE regClass      #-}
regClass :: Reg -> RegClass

#if   i386_TARGET_ARCH
-- On x86, we might want to have an 8-bit RegClass, which would
-- contain just regs 1-4 (the others don't have 8-bit versions).
-- However, we can get away without this at the moment because the
-- only allocatable integer regs are also 8-bit compatible (1, 3, 4).
regClass (RealReg i)     = if i < 8 then RcInteger else RcDouble
regClass (VirtualRegI  _) = RcInteger
regClass (VirtualRegHi _) = RcInteger
regClass (VirtualRegD  _) = RcDouble
regClass (VirtualRegF  u) = pprPanic ("regClass(x86):VirtualRegF") (ppr u)

#elif x86_64_TARGET_ARCH
-- On x86, we might want to have an 8-bit RegClass, which would
-- contain just regs 1-4 (the others don't have 8-bit versions).
-- However, we can get away without this at the moment because the
-- only allocatable integer regs are also 8-bit compatible (1, 3, 4).
regClass (RealReg i)     = if i < 16 then RcInteger else RcDouble
regClass (VirtualRegI  _) = RcInteger
regClass (VirtualRegHi _) = RcInteger
regClass (VirtualRegD  _) = RcDouble
regClass (VirtualRegF  u) = pprPanic "regClass(x86_64):VirtualRegF" (ppr u)

#else
regClass _	= panic "X86.Regs.regClass: not defined for this architecture"

#endif


-- | Get the name of the register with this number.
showReg :: RegNo -> String

#if   i386_TARGET_ARCH
showReg n
   = if   n >= 0 && n < 14
     then regNames !! n
     else "%unknown_x86_real_reg_" ++ show n

regNames :: [String]
regNames 
   = ["%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi", "%ebp", "%esp", 
      "%fake0", "%fake1", "%fake2", "%fake3", "%fake4", "%fake5", "%fake6"]

#elif x86_64_TARGET_ARCH
showReg n
	| n >= 16	= "%xmm" ++ show (n-16)
	| n >= 8	= "%r" ++ show n
	| otherwise	= regNames !! n

regNames :: [String]
regNames 
 = ["%rax", "%rbx", "%rcx", "%rdx", "%rsi", "%rdi", "%rbp", "%rsp" ]

#else
showReg _	= panic "X86.Regs.showReg: not defined for this architecture"

#endif




-- machine specific ------------------------------------------------------------


{-
Intel x86 architecture:
- All registers except 7 (esp) are available for use.
- Only ebx, esi, edi and esp are available across a C call (they are callee-saves).
- Registers 0-7 have 16-bit counterparts (ax, bx etc.)
- Registers 0-3 have 8 bit counterparts (ah, bh etc.)
- Registers 8-13 are fakes; we pretend x86 has 6 conventionally-addressable
  fp registers, and 3-operand insns for them, and we translate this into
  real stack-based x86 fp code after register allocation.

The fp registers are all Double registers; we don't have any RcFloat class
regs.  @regClass@ barfs if you give it a VirtualRegF, and mkVReg above should
never generate them.
-}

fake0, fake1, fake2, fake3, fake4, fake5, 
       eax, ebx, ecx, edx, esp, ebp, esi, edi :: Reg

eax   = RealReg 0
ebx   = RealReg 1
ecx   = RealReg 2
edx   = RealReg 3
esi   = RealReg 4
edi   = RealReg 5
ebp   = RealReg 6
esp   = RealReg 7
fake0 = RealReg 8
fake1 = RealReg 9
fake2 = RealReg 10
fake3 = RealReg 11
fake4 = RealReg 12
fake5 = RealReg 13



{-
AMD x86_64 architecture:
- Registers 0-16 have 32-bit counterparts (eax, ebx etc.)
- Registers 0-7 have 16-bit counterparts (ax, bx etc.)
- Registers 0-3 have 8 bit counterparts (ah, bh etc.)

-}

rax, rbx, rcx, rdx, rsp, rbp, rsi, rdi, 
  r8, r9, r10, r11, r12, r13, r14, r15,
  xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7,
  xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15 :: Reg

rax   = RealReg 0
rbx   = RealReg 1
rcx   = RealReg 2
rdx   = RealReg 3
rsi   = RealReg 4
rdi   = RealReg 5
rbp   = RealReg 6
rsp   = RealReg 7
r8    = RealReg 8
r9    = RealReg 9
r10   = RealReg 10
r11   = RealReg 11
r12   = RealReg 12
r13   = RealReg 13
r14   = RealReg 14
r15   = RealReg 15
xmm0  = RealReg 16
xmm1  = RealReg 17
xmm2  = RealReg 18
xmm3  = RealReg 19
xmm4  = RealReg 20
xmm5  = RealReg 21
xmm6  = RealReg 22
xmm7  = RealReg 23
xmm8  = RealReg 24
xmm9  = RealReg 25
xmm10 = RealReg 26
xmm11 = RealReg 27
xmm12 = RealReg 28
xmm13 = RealReg 29
xmm14 = RealReg 30
xmm15 = RealReg 31

allFPArgRegs :: [Reg]
allFPArgRegs	= map RealReg [16 .. 23]

ripRel :: Displacement -> AddrMode
ripRel imm	= AddrBaseIndex EABaseRip EAIndexNone imm


 -- so we can re-use some x86 code:
{-
eax = rax
ebx = rbx
ecx = rcx
edx = rdx
esi = rsi
edi = rdi
ebp = rbp
esp = rsp
-}

xmm :: RegNo -> Reg
xmm n = RealReg (16+n)




-- horror show -----------------------------------------------------------------
freeReg 		:: RegNo -> FastBool
globalRegMaybe 		:: GlobalReg -> Maybe Reg
allArgRegs 		:: [Reg]
callClobberedRegs 	:: [Reg]

#if defined(i386_TARGET_ARCH) || defined(x86_64_TARGET_ARCH)

#if i386_TARGET_ARCH
#define eax 0
#define ebx 1
#define ecx 2
#define edx 3
#define esi 4
#define edi 5
#define ebp 6
#define esp 7
#define fake0 8
#define fake1 9
#define fake2 10
#define fake3 11
#define fake4 12
#define fake5 13
#endif

#if x86_64_TARGET_ARCH
#define rax   0
#define rbx   1
#define rcx   2
#define rdx   3
#define rsi   4
#define rdi   5
#define rbp   6
#define rsp   7
#define r8    8
#define r9    9
#define r10   10
#define r11   11
#define r12   12
#define r13   13
#define r14   14
#define r15   15
#define xmm0  16
#define xmm1  17
#define xmm2  18
#define xmm3  19
#define xmm4  20
#define xmm5  21
#define xmm6  22
#define xmm7  23
#define xmm8  24
#define xmm9  25
#define xmm10 26
#define xmm11 27
#define xmm12 28
#define xmm13 29
#define xmm14 30
#define xmm15 31
#endif



#if i386_TARGET_ARCH
freeReg esp = fastBool False  --	%esp is the C stack pointer
#endif

#if x86_64_TARGET_ARCH
freeReg rsp = fastBool False  --	%rsp is the C stack pointer
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

-- 

#if   i386_TARGET_ARCH
allArgRegs = panic "X86.Regs.allArgRegs: should not be used!"

#elif x86_64_TARGET_ARCH
allArgRegs = map RealReg [rdi,rsi,rdx,rcx,r8,r9]

#else
allArgRegs  = panic "X86.Regs.allArgRegs: not defined for this architecture"
#endif


-- | these are the regs which we cannot assume stay alive over a C call.  

#if   i386_TARGET_ARCH
-- caller-saves registers
callClobberedRegs
  = map RealReg [eax,ecx,edx,fake0,fake1,fake2,fake3,fake4,fake5]

#elif x86_64_TARGET_ARCH
-- all xmm regs are caller-saves
-- caller-saves registers
callClobberedRegs    
  = map RealReg ([rax,rcx,rdx,rsi,rdi,r8,r9,r10,r11] ++ [16..31])

#else
callClobberedRegs
  = panic "X86.Regs.callClobberedRegs: not defined for this architecture"
#endif

#else /* i386_TARGET_ARCH || x86_64_TARGET_ARCH */



freeReg	_		= 0#
globalRegMaybe _	= panic "X86.Regs.globalRegMaybe: not defined"

allArgRegs		= panic "X86.Regs.globalRegMaybe: not defined"
callClobberedRegs	= panic "X86.Regs.globalRegMaybe: not defined"


#endif

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


-- allocatableRegs is allMachRegNos with the fixed-use regs removed.
-- i.e., these are the regs for which we are prepared to allow the
-- register allocator to attempt to map VRegs to.
allocatableRegs :: [RegNo]
allocatableRegs
   = let isFree i = isFastTrue (freeReg i)
     in  filter isFree allMachRegNos


