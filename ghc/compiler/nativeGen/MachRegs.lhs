-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1994-2004
-- 
-- Machine-specific info about registers.
-- 
-- Also includes stuff about immediate operands, which are
-- often/usually quite entangled with registers.
-- 
-- (Immediates could be untangled from registers at some cost in tangled
-- modules --- the pleasure has been foregone.)
-- 
-- -----------------------------------------------------------------------------

\begin{code}
#include "nativeGen/NCG.h"

module MachRegs (

	-- * Immediate values
	Imm(..), strImmLit, litToImm,

	-- * Addressing modes
	AddrMode(..),
	addrOffset,

	-- * The 'Reg' type
	RegNo,
	Reg(..), isRealReg, isVirtualReg,
        RegClass(..), regClass,
	getHiVRegFromLo, 
	mkVReg,

	-- * Global registers
        get_GlobalReg_reg_or_addr,
	callerSaves,

	-- * Machine-dependent register-related stuff
        allocatableRegs, argRegs, allArgRegs, callClobberedRegs,
	freeReg,
	spRel,

#if alpha_TARGET_ARCH
	allArgRegs,
	fits8Bits,
	fReg,
	gp, pv, ra, sp, t9, t10, t11, t12, v0, f0, zeroh,
#endif
#if i386_TARGET_ARCH
	eax, ebx, ecx, edx, esi, edi, ebp, esp,
	fake0, fake1, fake2, fake3, fake4, fake5,
	addrModeRegs,
#endif
#if x86_64_TARGET_ARCH
	rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp,
	eax, ebx, ecx, edx, esi, edi, ebp, esp,
	r8, r9, r10, r11, r12, r13, r14, r15,
  	xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7,
  	xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15,
	xmm, eax, edx,
	addrModeRegs, allFPArgRegs,
#endif
#if sparc_TARGET_ARCH
	fits13Bits,
	fpRel, gReg, iReg, lReg, oReg, largeOffsetError,
	fp, sp, g0, g1, g2, o0, o1, f0, f6, f8, f26, f27,
#endif
#if powerpc_TARGET_ARCH
	allFPArgRegs,
	makeImmediate,
	sp,
	r3, r4, r27, r28,
	f1, f20, f21,
#endif
    ) where

#include "HsVersions.h"

#if i386_TARGET_ARCH
# define STOLEN_X86_REGS 4
-- HACK: go for the max
#endif

#include "../includes/MachRegs.h"

import Cmm
import MachOp		( MachRep(..) )

import CLabel           ( CLabel, mkMainCapabilityLabel )
import Unique		( Unique )
import Pretty
import Outputable	( Outputable(..), pprPanic, panic )
import qualified Outputable
import Unique
import Constants
import FastTypes

#if powerpc_TARGET_ARCH
#if __GLASGOW_HASKELL__ >= 504
import Data.Word	( Word8, Word16, Word32 )
import Data.Int 	( Int8, Int16, Int32 )
#else
import Word     	( Word8, Word16, Word32 )
import Int 	        ( Int8, Int16, Int32 )
#endif
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
#if sparc_TARGET_ARCH
  | LO Imm		    {- Possible restrictions... -}
  | HI Imm
#endif
#if powerpc_TARGET_ARCH
  | LO Imm
  | HI Imm
  | HA Imm	{- high halfword adjusted -}
#endif
strImmLit s = ImmLit (text s)

litToImm :: CmmLit -> Imm
litToImm (CmmInt i _)        = ImmInteger i
litToImm (CmmFloat f F32)    = ImmFloat f
litToImm (CmmFloat f F64)    = ImmDouble f
litToImm (CmmLabel l)        = ImmCLbl l
litToImm (CmmLabelOff l off) = ImmIndex l off
litToImm (CmmLabelDiffOff l1 l2 off)
                             = ImmConstantSum
                               (ImmConstantDiff (ImmCLbl l1) (ImmCLbl l2))
                               (ImmInt off)

-- -----------------------------------------------------------------------------
-- Addressing modes

data AddrMode
#if alpha_TARGET_ARCH
  = AddrImm	Imm
  | AddrReg	Reg
  | AddrRegImm	Reg Imm
#endif

#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
  = AddrBaseIndex	Base Index Displacement
  | ImmAddr		Imm Int

type Base         = Maybe Reg
type Index        = Maybe (Reg, Int)	-- Int is 2, 4 or 8
type Displacement = Imm
#endif

#if sparc_TARGET_ARCH
  = AddrRegReg	Reg Reg
  | AddrRegImm	Reg Imm
#endif

#if powerpc_TARGET_ARCH
  = AddrRegReg	Reg Reg
  | AddrRegImm	Reg Imm
#endif

#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
addrModeRegs :: AddrMode -> [Reg]
addrModeRegs (AddrBaseIndex b i _) =  b_regs ++ i_regs
  where
   b_regs = case b of { Just r -> [r]; _ -> [] }
   i_regs = case i of { Just (r,_) -> [r]; _ -> [] }
addrModeRegs _ = []
#endif


addrOffset :: AddrMode -> Int -> Maybe AddrMode

addrOffset addr off
  = case addr of
#if alpha_TARGET_ARCH
      _ -> panic "MachMisc.addrOffset not defined for Alpha"
#endif
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
      ImmAddr i off0	  -> Just (ImmAddr i (off0 + off))

      AddrBaseIndex r i (ImmInt n) -> Just (AddrBaseIndex r i (ImmInt (n + off)))
      AddrBaseIndex r i (ImmInteger n)
	-> Just (AddrBaseIndex r i (ImmInt (fromInteger (n + toInteger off))))

      AddrBaseIndex r i (ImmCLbl lbl)
	-> Just (AddrBaseIndex r i (ImmIndex lbl off))

      AddrBaseIndex r i (ImmIndex lbl ix)
	-> Just (AddrBaseIndex r i (ImmIndex lbl (ix+off)))

      _ -> Nothing  -- in theory, shouldn't happen
#endif
#if sparc_TARGET_ARCH
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
#endif /* sparc */
#if powerpc_TARGET_ARCH
      AddrRegImm r (ImmInt n)
       | fits16Bits n2 -> Just (AddrRegImm r (ImmInt n2))
       | otherwise     -> Nothing
       where n2 = n + off

      AddrRegImm r (ImmInteger n)
       | fits16Bits n2 -> Just (AddrRegImm r (ImmInt (fromInteger n2)))
       | otherwise     -> Nothing
       where n2 = n + toInteger off
       
      _ -> Nothing
#endif /* powerpc */

-----------------
#if alpha_TARGET_ARCH

fits8Bits :: Integer -> Bool
fits8Bits i = i >= -256 && i < 256

#endif

#if sparc_TARGET_ARCH

{-# SPECIALIZE fits13Bits :: Int -> Bool, Integer -> Bool #-}
fits13Bits :: Integral a => a -> Bool
fits13Bits x = x >= -4096 && x < 4096

-----------------
largeOffsetError i
  = error ("ERROR: SPARC native-code generator cannot handle large offset ("
           ++show i++");\nprobably because of large constant data structures;" ++ 
           "\nworkaround: use -fvia-C on this module.\n")

#endif /* sparc */

#if powerpc_TARGET_ARCH
fits16Bits :: Integral a => a -> Bool
fits16Bits x = x >= -32768 && x < 32768

makeImmediate :: Integral a => MachRep -> Bool -> a -> Maybe Imm

makeImmediate rep signed x = fmap ImmInt (toI16 rep signed)
    where
        narrow I32 False = fromIntegral (fromIntegral x :: Word32)
        narrow I16 False = fromIntegral (fromIntegral x :: Word16)
        narrow I8  False = fromIntegral (fromIntegral x :: Word8)
        narrow I32 True  = fromIntegral (fromIntegral x :: Int32)
        narrow I16 True  = fromIntegral (fromIntegral x :: Int16)
        narrow I8  True  = fromIntegral (fromIntegral x :: Int8)
        
        narrowed = narrow rep signed
        
        toI16 I32 True
            | narrowed >= -32768 && narrowed < 32768 = Just narrowed
            | otherwise = Nothing
        toI16 I32 False
            | narrowed >= 0 && narrowed < 65536 = Just narrowed
            | otherwise = Nothing
        toI16 _ _  = Just narrowed
#endif


-- @spRel@ gives us a stack relative addressing mode for volatile
-- temporaries and for excess call arguments.  @fpRel@, where
-- applicable, is the same but for the frame pointer.

spRel :: Int	-- desired stack offset in words, positive or negative
      -> AddrMode

spRel n
#if defined(i386_TARGET_ARCH)
  = AddrBaseIndex (Just esp) Nothing (ImmInt (n * wORD_SIZE))
#elif defined(x86_64_TARGET_ARCH)
  = AddrBaseIndex (Just rsp) Nothing (ImmInt (n * wORD_SIZE))
#else
  = AddrRegImm sp (ImmInt (n * wORD_SIZE))
#endif

#if sparc_TARGET_ARCH
fpRel :: Int -> AddrMode
    -- Duznae work for offsets greater than 13 bits; we just hope for
    -- the best
fpRel n
  = AddrRegImm fp (ImmInt (n * wORD_SIZE))
#endif


-- -----------------------------------------------------------------------------
-- Global registers

-- We map STG registers onto appropriate CmmExprs.  Either they map
-- to real machine registers or stored as offsets from BaseReg.  Given
-- a GlobalReg, get_GlobalReg_reg_or_addr produces either the real
-- register it is in, on this platform, or a StixExpr denoting the
-- address in the register table holding it.  get_MagicId_addr always
-- produces the register table address for it.

get_GlobalReg_reg_or_addr       :: GlobalReg -> Either Reg CmmExpr
get_GlobalReg_addr              :: GlobalReg -> CmmExpr
get_Regtable_addr_from_offset   :: MachRep -> Int -> CmmExpr

get_GlobalReg_reg_or_addr mid
   = case globalRegMaybe mid of
        Just rr -> Left rr
        Nothing -> Right (get_GlobalReg_addr mid)

get_GlobalReg_addr BaseReg = regTableOffset 0
get_GlobalReg_addr mid     = get_Regtable_addr_from_offset 
				(globalRegRep mid) (baseRegOffset mid)

-- Calculate a literal representing an offset into the register table.
-- Used when we don't have an actual BaseReg to offset from.
regTableOffset n = 
  CmmLit (CmmLabelOff mkMainCapabilityLabel (oFFSET_Capability_r + n))

get_Regtable_addr_from_offset rep offset
   = case globalRegMaybe BaseReg of
                 Nothing -> regTableOffset offset
                 Just _  -> CmmRegOff (CmmGlobal BaseReg) offset

-- ---------------------------------------------------------------------------
-- Registers

-- RealRegs are machine regs which are available for allocation, in
-- the usual way.  We know what class they are, because that's part of
-- the processor's architecture.

-- VirtualRegs are virtual registers.  The register allocator will
-- eventually have to map them into RealRegs, or into spill slots.
-- VirtualRegs are allocated on the fly, usually to represent a single
-- value in the abstract assembly code (i.e. dynamic registers are
-- usually single assignment).  With the new register allocator, the
-- single assignment restriction isn't necessary to get correct code,
-- although a better register allocation will result if single
-- assignment is used -- because the allocator maps a VirtualReg into
-- a single RealReg, even if the VirtualReg has multiple live ranges.

-- Virtual regs can be of either class, so that info is attached.

-- Determine the upper-half vreg for a 64-bit quantity on a 32-bit platform
-- when supplied with the vreg for the lower-half of the quantity.
-- (NB. Not reversible).
getHiVRegFromLo (VirtualRegI u) 
   = VirtualRegHi (newTagUnique u 'H') -- makes a pseudo-unique with tag 'H'
getHiVRegFromLo other 
   = pprPanic "getHiVRegFromLo" (ppr other)

data RegClass 
   = RcInteger 
   | RcFloat
   | RcDouble
     deriving Eq

type RegNo = Int

data Reg
   = RealReg      {-# UNPACK #-} !RegNo
   | VirtualRegI  {-# UNPACK #-} !Unique
   | VirtualRegHi {-# UNPACK #-} !Unique  -- High part of 2-word register
   | VirtualRegF  {-# UNPACK #-} !Unique
   | VirtualRegD  {-# UNPACK #-} !Unique
   deriving (Eq,Ord)

-- We like to have Uniques for Reg so that we can make UniqFM and UniqSets 
-- in the register allocator.
instance Uniquable Reg where
   getUnique (RealReg i)      = mkUnique 'C' i
   getUnique (VirtualRegI u)  = u
   getUnique (VirtualRegHi u) = u
   getUnique (VirtualRegF u)  = u
   getUnique (VirtualRegD u)  = u

mkVReg :: Unique -> MachRep -> Reg
mkVReg u rep
   = case rep of
#if sparc_TARGET_ARCH
        F32   -> VirtualRegF u
#else
        F32   -> VirtualRegD u
#endif
        F64   -> VirtualRegD u
        other -> VirtualRegI u

isVirtualReg :: Reg -> Bool
isVirtualReg (RealReg _)      = False
isVirtualReg (VirtualRegI _)  = True
isVirtualReg (VirtualRegHi _) = True
isVirtualReg (VirtualRegF _)  = True
isVirtualReg (VirtualRegD _)  = True

isRealReg :: Reg -> Bool
isRealReg = not . isVirtualReg

instance Show Reg where
    show (RealReg i)      = showReg i
    show (VirtualRegI u)  = "%vI_" ++ show u
    show (VirtualRegHi u) = "%vHi_" ++ show u
    show (VirtualRegF u)  = "%vF_" ++ show u
    show (VirtualRegD u)  = "%vD_" ++ show u

instance Outputable Reg where
    ppr r = Outputable.text (show r)


-- -----------------------------------------------------------------------------
-- Machine-specific register stuff

-- The Alpha has 64 registers of interest; 32 integer registers and 32 floating
-- point registers.  The mapping of STG registers to alpha machine registers
-- is defined in StgRegs.h.  We are, of course, prepared for any eventuality.

#if alpha_TARGET_ARCH
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
#endif

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

#if i386_TARGET_ARCH

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

-- On x86, we might want to have an 8-bit RegClass, which would
-- contain just regs 1-4 (the others don't have 8-bit versions).
-- However, we can get away without this at the moment because the
-- only allocatable integer regs are also 8-bit compatible (1, 3, 4).
regClass (RealReg i)     = if i < 8 then RcInteger else RcDouble
regClass (VirtualRegI  u) = RcInteger
regClass (VirtualRegHi u) = RcInteger
regClass (VirtualRegD  u) = RcDouble
regClass (VirtualRegF  u) = pprPanic "regClass(x86):VirtualRegF" 
                                    (ppr (VirtualRegF u))

regNames 
   = ["%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi", "%ebp", "%esp", 
      "%fake0", "%fake1", "%fake2", "%fake3", "%fake4", "%fake5", "%fake6"]

showReg :: RegNo -> String
showReg n
   = if   n >= 0 && n < 14
     then regNames !! n
     else "%unknown_x86_real_reg_" ++ show n

#endif

{-
AMD x86_64 architecture:
- Registers 0-16 have 32-bit counterparts (eax, ebx etc.)
- Registers 0-7 have 16-bit counterparts (ax, bx etc.)
- Registers 0-3 have 8 bit counterparts (ah, bh etc.)

-}

#if x86_64_TARGET_ARCH

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

 -- so we can re-use some x86 code:
eax = rax
ebx = rbx
ecx = rcx
edx = rdx
esi = rsi
edi = rdi
ebp = rbp
esp = rsp

xmm n = RealReg (16+n)

-- On x86, we might want to have an 8-bit RegClass, which would
-- contain just regs 1-4 (the others don't have 8-bit versions).
-- However, we can get away without this at the moment because the
-- only allocatable integer regs are also 8-bit compatible (1, 3, 4).
regClass (RealReg i)     = if i < 16 then RcInteger else RcDouble
regClass (VirtualRegI  u) = RcInteger
regClass (VirtualRegHi u) = RcInteger
regClass (VirtualRegD  u) = RcDouble
regClass (VirtualRegF  u) = pprPanic "regClass(x86_64):VirtualRegF" 
                                    (ppr (VirtualRegF u))

regNames 
 = ["%rax", "%rbx", "%rcx", "%rdx", "%rsi", "%rdi", "%rbp", "%rsp" ]

showReg :: RegNo -> String
showReg n
  | n >= 16 = "%xmm" ++ show n  
  | n >= 8  = "%r" ++ show n
  | otherwise = regNames !! n

#endif

{-
The SPARC has 64 registers of interest; 32 integer registers and 32
floating point registers.  The mapping of STG registers to SPARC
machine registers is defined in StgRegs.h.  We are, of course,
prepared for any eventuality.

The whole fp-register pairing thing on sparcs is a huge nuisance.  See
fptools/ghc/includes/MachRegs.h for a description of what's going on
here.
-}

#if sparc_TARGET_ARCH

gReg,lReg,iReg,oReg,fReg :: Int -> RegNo
gReg x = x
oReg x = (8 + x)
lReg x = (16 + x)
iReg x = (24 + x)
fReg x = (32 + x)

nCG_FirstFloatReg :: RegNo
nCG_FirstFloatReg = unRealReg NCG_FirstFloatReg

regClass (VirtualRegI u) = RcInteger
regClass (VirtualRegF u) = RcFloat
regClass (VirtualRegD u) = RcDouble
regClass (RealReg i) | i < 32                = RcInteger 
                     | i < nCG_FirstFloatReg = RcDouble
                     | otherwise             = RcFloat

showReg :: RegNo -> String
showReg n
   | n >= 0  && n < 8   = "%g" ++ show n
   | n >= 8  && n < 16  = "%o" ++ show (n-8)
   | n >= 16 && n < 24  = "%l" ++ show (n-16)
   | n >= 24 && n < 32  = "%i" ++ show (n-24)
   | n >= 32 && n < 64  = "%f" ++ show (n-32)
   | otherwise          = "%unknown_sparc_real_reg_" ++ show n

g0, g1, g2, fp, sp, o0, o1, f0, f1, f6, f8, f22, f26, f27 :: Reg

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
f1  = RealReg (fReg 1)

#endif

{-
The PowerPC has 64 registers of interest; 32 integer registers and 32 floating
point registers.
-}

#if powerpc_TARGET_ARCH
fReg :: Int -> RegNo
fReg x = (32 + x)

regClass (VirtualRegI  u) = RcInteger
regClass (VirtualRegHi u) = RcInteger
regClass (VirtualRegF  u) = pprPanic "regClass(ppc):VirtualRegF" 
                                    (ppr (VirtualRegF u))
regClass (VirtualRegD u) = RcDouble
regClass (RealReg i) | i < 32                = RcInteger 
		     | otherwise	     = RcDouble

showReg :: RegNo -> String
showReg n
    | n >= 0 && n <= 31	  = "%r" ++ show n
    | n >= 32 && n <= 63  = "%f" ++ show (n - 32)
    | otherwise           = "%unknown_powerpc_real_reg_" ++ show n

sp = RealReg 1
r3 = RealReg 3
r4 = RealReg 4
r27 = RealReg 27
r28 = RealReg 28
f1 = RealReg $ fReg 1
f20 = RealReg $ fReg 20
f21 = RealReg $ fReg 21
#endif

{-
Redefine the literals used for machine-registers with non-numeric
names in the header files.  Gag me with a spoon, eh?
-}

#if alpha_TARGET_ARCH
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
#endif
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
#endif


-- allMachRegs is the complete set of machine regs.
allMachRegNos :: [RegNo]
allMachRegNos
   = IF_ARCH_alpha( [0..63],
     IF_ARCH_i386(  [0..13],
     IF_ARCH_x86_64( [0..31],
     IF_ARCH_sparc( ([0..31]
                     ++ [f0,f2 .. nCG_FirstFloatReg-1]
                     ++ [nCG_FirstFloatReg .. f31]),
     IF_ARCH_powerpc([0..63],
                   )))))

-- allocatableRegs is allMachRegNos with the fixed-use regs removed.
-- i.e., these are the regs for which we are prepared to allow the
-- register allocator to attempt to map VRegs to.
allocatableRegs :: [RegNo]
allocatableRegs
   = let isFree i = isFastTrue (freeReg i)
     in  filter isFree allMachRegNos

-- these are the regs which we cannot assume stay alive over a
-- C call.  
callClobberedRegs :: [Reg]
callClobberedRegs
  =
#if alpha_TARGET_ARCH
    [0, 1, 2, 3, 4, 5, 6, 7, 8,
     16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
     fReg 0, fReg 1, fReg 10, fReg 11, fReg 12, fReg 13, fReg 14, fReg 15,
     fReg 16, fReg 17, fReg 18, fReg 19, fReg 20, fReg 21, fReg 22, fReg 23,
     fReg 24, fReg 25, fReg 26, fReg 27, fReg 28, fReg 29, fReg 30]
#endif /* alpha_TARGET_ARCH */
#if i386_TARGET_ARCH
    -- caller-saves registers
    map RealReg [eax,ecx,edx,fake0,fake1,fake2,fake3,fake4,fake5]
#endif /* i386_TARGET_ARCH */
#if x86_64_TARGET_ARCH
    -- caller-saves registers
    map RealReg ([rax,rcx,rdx,rsi,rdi,r8,r9,r10,r11] ++ [16..31])
       -- all xmm regs are caller-saves
#endif /* x86_64_TARGET_ARCH */
#if sparc_TARGET_ARCH
    map RealReg 
        ( oReg 7 :
          [oReg i | i <- [0..5]] ++
          [gReg i | i <- [1..7]] ++
          [fReg i | i <- [0..31]] )
#endif /* sparc_TARGET_ARCH */
#if powerpc_TARGET_ARCH
#if darwin_TARGET_OS
    map RealReg (0:[2..12] ++ map fReg [0..13])
#elif linux_TARGET_OS
    map RealReg (0:[2..13] ++ map fReg [0..13])
#endif
#endif /* powerpc_TARGET_ARCH */


-- argRegs is the set of regs which are read for an n-argument call to C.
-- For archs which pass all args on the stack (x86), is empty.
-- Sparc passes up to the first 6 args in regs.
-- Dunno about Alpha.
argRegs :: RegNo -> [Reg]

#if i386_TARGET_ARCH
argRegs _ = panic "MachRegs.argRegs(x86): should not be used!"
#endif

#if x86_64_TARGET_ARCH
argRegs _ = panic "MachRegs.argRegs(x86_64): should not be used!"
#endif

#if alpha_TARGET_ARCH
argRegs 0 = []
argRegs 1 = freeMappedRegs [16, fReg 16]
argRegs 2 = freeMappedRegs [16, 17, fReg 16, fReg 17]
argRegs 3 = freeMappedRegs [16, 17, 18, fReg 16, fReg 17, fReg 18]
argRegs 4 = freeMappedRegs [16, 17, 18, 19, fReg 16, fReg 17, fReg 18, fReg 19]
argRegs 5 = freeMappedRegs [16, 17, 18, 19, 20, fReg 16, fReg 17, fReg 18, fReg 19, fReg 20]
argRegs 6 = freeMappedRegs [16, 17, 18, 19, 20, 21, fReg 16, fReg 17, fReg 18, fReg 19, fReg 20, fReg 21]
argRegs _ = panic "MachRegs.argRegs(alpha): don't know about >6 arguments!"
#endif /* alpha_TARGET_ARCH */

#if sparc_TARGET_ARCH
argRegs 0 = []
argRegs 1 = map (RealReg . oReg) [0]
argRegs 2 = map (RealReg . oReg) [0,1]
argRegs 3 = map (RealReg . oReg) [0,1,2]
argRegs 4 = map (RealReg . oReg) [0,1,2,3]
argRegs 5 = map (RealReg . oReg) [0,1,2,3,4]
argRegs 6 = map (RealReg . oReg) [0,1,2,3,4,5]
argRegs _ = panic "MachRegs.argRegs(sparc): don't know about >6 arguments!"
#endif /* sparc_TARGET_ARCH */

#if powerpc_TARGET_ARCH
argRegs 0 = []
argRegs 1 = map RealReg [3]
argRegs 2 = map RealReg [3,4]
argRegs 3 = map RealReg [3..5]
argRegs 4 = map RealReg [3..6]
argRegs 5 = map RealReg [3..7]
argRegs 6 = map RealReg [3..8]
argRegs 7 = map RealReg [3..9]
argRegs 8 = map RealReg [3..10]
argRegs _ = panic "MachRegs.argRegs(powerpc): don't know about >8 arguments!"
#endif /* powerpc_TARGET_ARCH */


-- all of the arg regs ??
#if alpha_TARGET_ARCH
allArgRegs :: [(Reg, Reg)]
allArgRegs = [(realReg i, realReg (fReg i)) | i <- [16..21]]
#endif /* alpha_TARGET_ARCH */

#if sparc_TARGET_ARCH
allArgRegs :: [Reg]
allArgRegs = map RealReg [oReg i | i <- [0..5]]
#endif /* sparc_TARGET_ARCH */

#if i386_TARGET_ARCH
allArgRegs :: [Reg]
allArgRegs = panic "MachRegs.allArgRegs(x86): should not be used!"
#endif

#if x86_64_TARGET_ARCH
allArgRegs :: [Reg]
allArgRegs = map RealReg [rdi,rsi,rdx,rcx,r8,r9]
allFPArgRegs :: [Reg]
allFPArgRegs = map RealReg [xmm0 .. xmm7]
#endif

#if powerpc_TARGET_ARCH
allArgRegs :: [Reg]
allArgRegs = map RealReg [3..10]
allFPArgRegs :: [Reg]
#if darwin_TARGET_OS
allFPArgRegs = map (RealReg . fReg) [1..13]
#elif linux_TARGET_OS
allFPArgRegs = map (RealReg . fReg) [1..8]
#endif
#endif /* powerpc_TARGET_ARCH */
\end{code}

\begin{code}
freeReg :: RegNo -> FastBool

#if alpha_TARGET_ARCH
freeReg 26 = fastBool False  -- return address (ra)
freeReg 28 = fastBool False  -- reserved for the assembler (at)
freeReg 29 = fastBool False  -- global pointer (gp)
freeReg 30 = fastBool False  -- stack pointer (sp)
freeReg 31 = fastBool False  -- always zero (zeroh)
freeReg 63 = fastBool False  -- always zero (f31)
#endif

#if i386_TARGET_ARCH
freeReg esp = fastBool False  --	%esp is the C stack pointer
#endif

#if x86_64_TARGET_ARCH
freeReg rsp = fastBool False  --	%rsp is the C stack pointer
#endif

#if sparc_TARGET_ARCH
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
#endif

#if powerpc_TARGET_ARCH
freeReg 0 = fastBool False -- Hack: r0 can't be used in all insns, but it's actually free
freeReg 1 = fastBool False -- The Stack Pointer
#if !darwin_TARGET_OS
 -- most non-darwin powerpc OSes use r2 as a TOC pointer or something like that
freeReg 2 = fastBool False
#endif
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
freeReg n               = fastBool True


-- -----------------------------------------------------------------------------
-- Information about global registers

baseRegOffset :: GlobalReg -> Int

baseRegOffset (VanillaReg 1)      = oFFSET_StgRegTable_rR1
baseRegOffset (VanillaReg 2)      = oFFSET_StgRegTable_rR2
baseRegOffset (VanillaReg 3)      = oFFSET_StgRegTable_rR3
baseRegOffset (VanillaReg 4)      = oFFSET_StgRegTable_rR4
baseRegOffset (VanillaReg 5)      = oFFSET_StgRegTable_rR5
baseRegOffset (VanillaReg 6)      = oFFSET_StgRegTable_rR6
baseRegOffset (VanillaReg 7)      = oFFSET_StgRegTable_rR7
baseRegOffset (VanillaReg 8)      = oFFSET_StgRegTable_rR8
baseRegOffset (VanillaReg 9)      = oFFSET_StgRegTable_rR9
baseRegOffset (VanillaReg 10)     = oFFSET_StgRegTable_rR10
baseRegOffset (FloatReg  1)       = oFFSET_StgRegTable_rF1
baseRegOffset (FloatReg  2)       = oFFSET_StgRegTable_rF2
baseRegOffset (FloatReg  3)       = oFFSET_StgRegTable_rF3
baseRegOffset (FloatReg  4)       = oFFSET_StgRegTable_rF4
baseRegOffset (DoubleReg 1)       = oFFSET_StgRegTable_rD1
baseRegOffset (DoubleReg 2)       = oFFSET_StgRegTable_rD2
baseRegOffset Sp		  = oFFSET_StgRegTable_rSp
baseRegOffset SpLim		  = oFFSET_StgRegTable_rSpLim
baseRegOffset (LongReg 1)         = oFFSET_StgRegTable_rL1
baseRegOffset Hp		  = oFFSET_StgRegTable_rHp
baseRegOffset HpLim		  = oFFSET_StgRegTable_rHpLim
baseRegOffset CurrentTSO	  = oFFSET_StgRegTable_rCurrentTSO
baseRegOffset CurrentNursery	  = oFFSET_StgRegTable_rCurrentNursery
baseRegOffset HpAlloc		  = oFFSET_StgRegTable_rHpAlloc
baseRegOffset GCEnter1		  = oFFSET_stgGCEnter1
baseRegOffset GCFun		  = oFFSET_stgGCFun
#ifdef DEBUG
baseRegOffset BaseReg		  = panic "baseRegOffset:BaseReg"
baseRegOffset _			  = panic "baseRegOffset:other"
#endif


-- | Returns 'True' if this global register is stored in a caller-saves
-- machine register.

callerSaves :: GlobalReg -> Bool

#ifdef CALLER_SAVES_Base
callerSaves BaseReg			= True
#endif
#ifdef CALLER_SAVES_R1
callerSaves (VanillaReg _ ILIT(1))	= True
#endif
#ifdef CALLER_SAVES_R2
callerSaves (VanillaReg _ ILIT(2))    	= True
#endif
#ifdef CALLER_SAVES_R3
callerSaves (VanillaReg _ ILIT(3))    	= True
#endif
#ifdef CALLER_SAVES_R4
callerSaves (VanillaReg _ ILIT(4))	= True
#endif
#ifdef CALLER_SAVES_R5
callerSaves (VanillaReg _ ILIT(5))	= True
#endif
#ifdef CALLER_SAVES_R6
callerSaves (VanillaReg _ ILIT(6))	= True
#endif
#ifdef CALLER_SAVES_R7
callerSaves (VanillaReg _ ILIT(7))	= True
#endif
#ifdef CALLER_SAVES_R8
callerSaves (VanillaReg _ ILIT(8))	= True
#endif
#ifdef CALLER_SAVES_F1
callerSaves (FloatReg 1#)		= True
#endif
#ifdef CALLER_SAVES_F2
callerSaves (FloatReg 2#)		= True
#endif
#ifdef CALLER_SAVES_F3
callerSaves (FloatReg 3#)		= True
#endif
#ifdef CALLER_SAVES_F4
callerSaves (FloatReg 4#)		= True
#endif
#ifdef CALLER_SAVES_D1
callerSaves (DoubleReg 1#)		= True
#endif
#ifdef CALLER_SAVES_D2
callerSaves (DoubleReg 2#)		= True
#endif
#ifdef CALLER_SAVES_L1
callerSaves (LongReg _ ILIT(1))		= True
#endif
#ifdef CALLER_SAVES_Sp
callerSaves Sp				= True
#endif
#ifdef CALLER_SAVES_SpLim
callerSaves SpLim			= True
#endif
#ifdef CALLER_SAVES_Hp
callerSaves Hp				= True
#endif
#ifdef CALLER_SAVES_HpLim
callerSaves HpLim			= True
#endif
#ifdef CALLER_SAVES_CurrentTSO
callerSaves CurrentTSO			= True
#endif
#ifdef CALLER_SAVES_CurrentNursery
callerSaves CurrentNursery		= True
#endif
callerSaves _				= False


--  | Returns 'Nothing' if this global register is not stored
-- in a real machine register, otherwise returns @'Just' reg@, where
-- reg is the machine register it is stored in.

globalRegMaybe :: GlobalReg -> Maybe Reg

#ifdef REG_Base
globalRegMaybe BaseReg			= Just (RealReg REG_Base)
#endif
#ifdef REG_R1
globalRegMaybe (VanillaReg 1) 		= Just (RealReg REG_R1)
#endif 
#ifdef REG_R2 
globalRegMaybe (VanillaReg 2) 		= Just (RealReg REG_R2)
#endif 
#ifdef REG_R3 
globalRegMaybe (VanillaReg 3) 		= Just (RealReg REG_R3)
#endif 
#ifdef REG_R4 
globalRegMaybe (VanillaReg 4) 		= Just (RealReg REG_R4)
#endif 
#ifdef REG_R5 
globalRegMaybe (VanillaReg 5) 		= Just (RealReg REG_R5)
#endif 
#ifdef REG_R6 
globalRegMaybe (VanillaReg 6) 		= Just (RealReg REG_R6)
#endif 
#ifdef REG_R7 
globalRegMaybe (VanillaReg 7) 		= Just (RealReg REG_R7)
#endif 
#ifdef REG_R8 
globalRegMaybe (VanillaReg 8) 		= Just (RealReg REG_R8)
#endif
#ifdef REG_R9 
globalRegMaybe (VanillaReg 9) 		= Just (RealReg REG_R9)
#endif
#ifdef REG_R10 
globalRegMaybe (VanillaReg 10)		= Just (RealReg REG_R10)
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


\end{code}
