%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[MachRegs]{Machine-specific info about registers}

Also includes stuff about immediate operands, which are
often/usually quite entangled with registers.

(Immediates could be untangled from registers at some cost in tangled
modules --- the pleasure has been foregone.)

\begin{code}
#include "nativeGen/NCG.h"

module MachRegs (

        RegClass(..), regClass,
	VRegUnique(..), pprVRegUnique, getHiVRegFromLo, 
	Reg(..), isRealReg, isVirtualReg, getVRegUnique,
        allocatableRegs, argRegs, allArgRegs, callClobberedRegs,

	Imm(..),
	MachRegsAddr(..),

	addrOffset,
	baseRegOffset,
	callerSaves,
	freeReg,
	getNewRegNCG,
	mkVReg,
        get_MagicId_reg_or_addr,
        get_MagicId_addr,
        get_Regtable_addr_from_offset,
	spRel,
	strImmLit

#if alpha_TARGET_ARCH
	, allArgRegs
	, fits8Bits
	, fReg
	, gp, pv, ra, sp, t9, t10, t11, t12, v0, f0, zeroh
#endif
#if i386_TARGET_ARCH
	, eax, ebx, ecx, edx, esi, esp
	, fake0, fake1, fake2, fake3, fake4, fake5
#endif
#if sparc_TARGET_ARCH
	, fits13Bits
	, fpRel, gReg, iReg, lReg, oReg, largeOffsetError
	, fp, sp, g0, g1, g2, o0, o1, f0, f6, f8, f26, f27
	
#endif
#if powerpc_TARGET_ARCH
	, allFPArgRegs
	, fits16Bits
	, sp
	, r3, r4, r27, r28
	, f1, f20, f21
#endif
    ) where

#include "HsVersions.h"

import AbsCSyn		( MagicId(..) )
import CLabel           ( CLabel, mkMainCapabilityLabel )
import PrimRep		( PrimRep(..), isFloatingRep )
import Stix		( StixExpr(..), StixReg(..),
                          getUniqueNat, returnNat, thenNat, NatM )
import Unique		( Unique )
import Pretty
import Outputable	( Outputable(..), pprPanic, panic )
import qualified Outputable
import FastTypes
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

\begin{code}
data Imm
  = ImmInt	Int
  | ImmInteger	Integer	    -- Sigh.
  | ImmCLbl	CLabel	    -- AbstractC Label (with baggage)
  | ImmLab	Bool Doc    -- Simple string label (underscore-able)
                             -- Bool==True ==> in a different DLL
  | ImmLit	Doc    -- Simple string
  | ImmIndex    CLabel Int
  | ImmFloat	Rational
  | ImmDouble	Rational
  IF_ARCH_sparc(
  | LO Imm		    {- Possible restrictions... -}
  | HI Imm
  ,IF_ARCH_powerpc(
  | LO Imm
  | HI Imm
  | HA Imm	{- high halfword adjusted -}
  ,))
strImmLit s = ImmLit (text s)
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

\begin{code}
data MachRegsAddr
#if alpha_TARGET_ARCH
  = AddrImm	Imm
  | AddrReg	Reg
  | AddrRegImm	Reg Imm
#endif

#if i386_TARGET_ARCH
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

addrOffset :: MachRegsAddr -> Int -> Maybe MachRegsAddr

addrOffset addr off
  = case addr of
#if alpha_TARGET_ARCH
      _ -> panic "MachMisc.addrOffset not defined for Alpha"
#endif
#if i386_TARGET_ARCH
      ImmAddr i off0	  -> Just (ImmAddr i (off0 + off))
      AddrBaseIndex r i (ImmInt n) -> Just (AddrBaseIndex r i (ImmInt (n + off)))
      AddrBaseIndex r i (ImmInteger n)
	-> Just (AddrBaseIndex r i (ImmInt (fromInteger (n + toInteger off))))
      _ -> Nothing
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

      AddrRegReg r (RealReg 0)
       | fits16Bits off -> Just (AddrRegImm r (ImmInt off))
       | otherwise     -> Nothing
       
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
#endif
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@stgReg@: we map STG registers onto appropriate Stix Trees.  Either
they map to real machine registers or stored as offsets from BaseReg.
Given a MagicId, get_MagicId_reg_or_addr produces either the real
register it is in, on this platform, or a StixExpr denoting the
address in the register table holding it.  get_MagicId_addr always
produces the register table address for it.

\begin{code}
get_MagicId_reg_or_addr       :: MagicId -> Either Reg StixExpr
get_MagicId_addr              :: MagicId -> StixExpr
get_Regtable_addr_from_offset :: Int -> StixExpr

get_MagicId_reg_or_addr mid
   = case magicIdRegMaybe mid of
        Just rr -> Left rr
        Nothing -> Right (get_MagicId_addr mid)

get_MagicId_addr BaseReg
   = -- This arch doesn't have BaseReg in a register, so we have to 
     -- use &MainRegTable.r instead.
     StIndex PtrRep (StCLbl mkMainCapabilityLabel)
                    (StInt (toInteger OFFW_Capability_r))
get_MagicId_addr mid
   = get_Regtable_addr_from_offset (baseRegOffset mid)

get_Regtable_addr_from_offset offset_in_words
   = let ptr_to_RegTable
            = case magicIdRegMaybe BaseReg of
                 Nothing 
                    -> -- This arch doesn't have BaseReg in a register, so we have to 
                       -- use &MainRegTable.r instead.
                       StIndex PtrRep (StCLbl mkMainCapabilityLabel)
                                      (StInt (toInteger OFFW_Capability_r))
                 Just _
                    -> -- It's in a reg, so leave it as it is
                       StReg (StixMagicId BaseReg)
     in
         StIndex PtrRep ptr_to_RegTable (StInt (toInteger offset_in_words))
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@spRel@ gives us a stack relative addressing mode for volatile
temporaries and for excess call arguments.  @fpRel@, where
applicable, is the same but for the frame pointer.

\begin{code}
spRel :: Int	-- desired stack offset in words, positive or negative
      -> MachRegsAddr

spRel n
#if i386_TARGET_ARCH
  = AddrBaseIndex (Just esp) Nothing (ImmInt (n * BYTES_PER_WORD))
#else
  = AddrRegImm sp (ImmInt (n * BYTES_PER_WORD))
#endif

#if sparc_TARGET_ARCH
fpRel :: Int -> MachRegsAddr
    -- Duznae work for offsets greater than 13 bits; we just hope for
    -- the best
fpRel n
  = AddrRegImm fp (ImmInt (n * BYTES_PER_WORD))
#endif
\end{code}

%************************************************************************
%*									*
\subsection[Reg]{Real registers}
%*									*
%************************************************************************

RealRegs are machine regs which are available for allocation, in the
usual way.  We know what class they are, because that's part of the
processor's architecture.

VirtualRegs are virtual registers.  The register allocator will
eventually have to map them into RealRegs, or into spill slots.
VirtualRegs are allocated on the fly, usually to represent a single
value in the abstract assembly code (i.e. dynamic registers are
usually single assignment).  With the new register allocator, the
single assignment restriction isn't necessary to get correct code,
although a better register allocation will result if single assignment
is used -- because the allocator maps a VirtualReg into a single
RealReg, even if the VirtualReg has multiple live ranges.

Virtual regs can be of either class, so that info is attached.

\begin{code}

data VRegUnique
   = VRegUniqueLo Unique		-- lower part of a split quantity
   | VRegUniqueHi Unique		-- upper part thereof
     deriving (Eq, Ord)

instance Show VRegUnique where
   show (VRegUniqueLo u) = show u
   show (VRegUniqueHi u) = "_hi_" ++ show u

pprVRegUnique :: VRegUnique -> Outputable.SDoc
pprVRegUnique 
   = Outputable.text . show

-- Determine the upper-half vreg for a 64-bit quantity on a 32-bit platform
-- when supplied with the vreg for the lower-half of the quantity.
getHiVRegFromLo (VirtualRegI (VRegUniqueLo u)) 
   = VirtualRegI (VRegUniqueHi u)
getHiVRegFromLo other 
   = pprPanic "getHiVRegFromLo" (ppr other)

data RegClass 
   = RcInteger 
   | RcFloat
   | RcDouble
     deriving Eq

data Reg
   = RealReg     Int
   | VirtualRegI VRegUnique
   | VirtualRegF VRegUnique
   | VirtualRegD VRegUnique

unRealReg (RealReg i) = i
unRealReg vreg        = pprPanic "unRealReg on VirtualReg" (ppr vreg)

getVRegUnique :: Reg -> VRegUnique
getVRegUnique (VirtualRegI vu) = vu
getVRegUnique (VirtualRegF vu) = vu
getVRegUnique (VirtualRegD vu) = vu
getVRegUnique rreg             = pprPanic "getVRegUnique on RealReg" (ppr rreg)

mkVReg :: Unique -> PrimRep -> Reg
mkVReg u pk
#if sparc_TARGET_ARCH
   = case pk of
        FloatRep  -> VirtualRegF (VRegUniqueLo u)
        DoubleRep -> VirtualRegD (VRegUniqueLo u)
        other     -> VirtualRegI (VRegUniqueLo u)
#else
   = if isFloatingRep pk then VirtualRegD (VRegUniqueLo u) 
                         else VirtualRegI (VRegUniqueLo u)
#endif

isVirtualReg (RealReg _)     = False
isVirtualReg (VirtualRegI _) = True
isVirtualReg (VirtualRegF _) = True
isVirtualReg (VirtualRegD _) = True
isRealReg = not . isVirtualReg

getNewRegNCG :: PrimRep -> NatM Reg
getNewRegNCG pk
   = getUniqueNat `thenNat` \ u -> returnNat (mkVReg u pk)

instance Eq Reg where
   (==) (RealReg i1)     (RealReg i2)     = i1 == i2
   (==) (VirtualRegI u1) (VirtualRegI u2) = u1 == u2
   (==) (VirtualRegF u1) (VirtualRegF u2) = u1 == u2
   (==) (VirtualRegD u1) (VirtualRegD u2) = u1 == u2
   (==) reg1             reg2             = False

instance Ord Reg where
   compare (RealReg i1)     (RealReg i2)     = compare i1 i2
   compare (RealReg _)      (VirtualRegI _)  = LT
   compare (RealReg _)      (VirtualRegF _)  = LT
   compare (RealReg _)      (VirtualRegD _)  = LT

   compare (VirtualRegI _)  (RealReg _)      = GT
   compare (VirtualRegI u1) (VirtualRegI u2) = compare u1 u2
   compare (VirtualRegI _)  (VirtualRegF _)  = LT
   compare (VirtualRegI _)  (VirtualRegD _)  = LT

   compare (VirtualRegF _)  (RealReg _)      = GT
   compare (VirtualRegF _)  (VirtualRegI _)  = GT
   compare (VirtualRegF u1) (VirtualRegF u2) = compare u1 u2
   compare (VirtualRegF _)  (VirtualRegD _)  = LT

   compare (VirtualRegD _)  (RealReg _)      = GT
   compare (VirtualRegD _)  (VirtualRegI _)  = GT
   compare (VirtualRegD _)  (VirtualRegF _)  = GT
   compare (VirtualRegD u1) (VirtualRegD u2) = compare u1 u2


instance Show Reg where
    show (RealReg i)     = showReg i
    show (VirtualRegI u) = "%vI_" ++ show u
    show (VirtualRegF u) = "%vF_" ++ show u
    show (VirtualRegD u) = "%vD_" ++ show u

instance Outputable Reg where
    ppr r = Outputable.text (show r)
\end{code}

** Machine-specific Reg stuff: **

The Alpha has 64 registers of interest; 32 integer registers and 32 floating
point registers.  The mapping of STG registers to alpha machine registers
is defined in StgRegs.h.  We are, of course, prepared for any eventuality.
\begin{code}
#if alpha_TARGET_ARCH
fReg :: Int -> Int
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
\end{code}

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

\begin{code}
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

regClass (RealReg i)     = if i < 8 then RcInteger else RcDouble
regClass (VirtualRegI u) = RcInteger
regClass (VirtualRegD u) = RcDouble
regClass (VirtualRegF u) = pprPanic "regClass(x86):VirtualRegF" 
                                    (ppr (VirtualRegF u))

regNames 
   = ["%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi", "%ebp", "%esp", 
      "%fake0", "%fake1", "%fake2", "%fake3", "%fake4", "%fake5", "%fake6"]

showReg :: Int -> String
showReg n
   = if   n >= 0 && n < 14
     then regNames !! n
     else "%unknown_x86_real_reg_" ++ show n

#endif
\end{code}

The SPARC has 64 registers of interest; 32 integer registers and 32
floating point registers.  The mapping of STG registers to SPARC
machine registers is defined in StgRegs.h.  We are, of course,
prepared for any eventuality.

The whole fp-register pairing thing on sparcs is a huge nuisance.  See
fptools/ghc/includes/MachRegs.h for a description of what's going on
here.

\begin{code}
#if sparc_TARGET_ARCH

gReg,lReg,iReg,oReg,fReg :: Int -> Int
gReg x = x
oReg x = (8 + x)
lReg x = (16 + x)
iReg x = (24 + x)
fReg x = (32 + x)

nCG_FirstFloatReg :: Int
nCG_FirstFloatReg = unRealReg NCG_FirstFloatReg

regClass (VirtualRegI u) = RcInteger
regClass (VirtualRegF u) = RcFloat
regClass (VirtualRegD u) = RcDouble
regClass (RealReg i) | i < 32                = RcInteger 
                     | i < nCG_FirstFloatReg = RcDouble
                     | otherwise             = RcFloat

showReg :: Int -> String
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
\end{code}

The PowerPC has 64 registers of interest; 32 integer registers and 32 floating
point registers.
\begin{code}
#if powerpc_TARGET_ARCH
fReg :: Int -> Int
fReg x = (32 + x)

regClass (VirtualRegI u) = RcInteger
regClass (VirtualRegF u) = RcFloat
regClass (VirtualRegD u) = RcDouble
regClass (RealReg i) | i < 32                = RcInteger 
		     | otherwise	     = RcDouble
                  --   | i < nCG_FirstFloatReg = RcDouble
                  --   | otherwise             = RcFloat

showReg :: Int -> String
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
\end{code}

Redefine the literals used for machine-registers with non-numeric
names in the header files.  Gag me with a spoon, eh?
\begin{code}
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
\end{code}

\begin{code}
baseRegOffset :: MagicId -> Int

baseRegOffset (VanillaReg _ 1#)      = OFFSET_R1
baseRegOffset (VanillaReg _ 2#)      = OFFSET_R2
baseRegOffset (VanillaReg _ 3#)      = OFFSET_R3
baseRegOffset (VanillaReg _ 4#)      = OFFSET_R4
baseRegOffset (VanillaReg _ 5#)      = OFFSET_R5
baseRegOffset (VanillaReg _ 6#)      = OFFSET_R6
baseRegOffset (VanillaReg _ 7#)      = OFFSET_R7
baseRegOffset (VanillaReg _ 8#)      = OFFSET_R8
baseRegOffset (VanillaReg _ 9#)      = OFFSET_R9
baseRegOffset (VanillaReg _ 10#)     = OFFSET_R10
baseRegOffset (FloatReg  1#)         = OFFSET_F1
baseRegOffset (FloatReg  2#)         = OFFSET_F2
baseRegOffset (FloatReg  3#)         = OFFSET_F3
baseRegOffset (FloatReg  4#)         = OFFSET_F4
baseRegOffset (DoubleReg 1#)         = OFFSET_D1
baseRegOffset (DoubleReg 2#)         = OFFSET_D2
baseRegOffset Sp		     = OFFSET_Sp
baseRegOffset SpLim		     = OFFSET_SpLim
#ifdef OFFSET_L1
baseRegOffset (LongReg _ 1#)         = OFFSET_L1
#endif
baseRegOffset Hp		     = OFFSET_Hp
baseRegOffset HpLim		     = OFFSET_HpLim
baseRegOffset CurrentTSO	     = OFFSET_CurrentTSO
baseRegOffset CurrentNursery	     = OFFSET_CurrentNursery
baseRegOffset HpAlloc		     = OFFSET_HpAlloc
#ifdef NCG_DEBUG
baseRegOffset BaseReg		     = panic "baseRegOffset:BaseReg"
baseRegOffset CurCostCentre	     = panic "baseRegOffset:CurCostCentre"
baseRegOffset VoidReg		     = panic "baseRegOffset:VoidReg"
#endif
\end{code}

\begin{code}
callerSaves :: MagicId -> Bool

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
\end{code}

\begin{code}
magicIdRegMaybe :: MagicId -> Maybe Reg

#ifdef REG_Base
magicIdRegMaybe BaseReg			= Just (RealReg REG_Base)
#endif
#ifdef REG_R1
magicIdRegMaybe (VanillaReg _ 1#) 	= Just (RealReg REG_R1)
#endif 
#ifdef REG_R2 
magicIdRegMaybe (VanillaReg _ 2#) 	= Just (RealReg REG_R2)
#endif 
#ifdef REG_R3 
magicIdRegMaybe (VanillaReg _ 3#) 	= Just (RealReg REG_R3)
#endif 
#ifdef REG_R4 
magicIdRegMaybe (VanillaReg _ 4#) 	= Just (RealReg REG_R4)
#endif 
#ifdef REG_R5 
magicIdRegMaybe (VanillaReg _ 5#) 	= Just (RealReg REG_R5)
#endif 
#ifdef REG_R6 
magicIdRegMaybe (VanillaReg _ 6#) 	= Just (RealReg REG_R6)
#endif 
#ifdef REG_R7 
magicIdRegMaybe (VanillaReg _ 7#) 	= Just (RealReg REG_R7)
#endif 
#ifdef REG_R8 
magicIdRegMaybe (VanillaReg _ 8#) 	= Just (RealReg REG_R8)
#endif
#ifdef REG_R9 
magicIdRegMaybe (VanillaReg _ 9#) 	= Just (RealReg REG_R9)
#endif
#ifdef REG_R10 
magicIdRegMaybe (VanillaReg _ 10#)	= Just (RealReg REG_R10)
#endif
#ifdef REG_F1
magicIdRegMaybe (FloatReg 1#)	= Just (RealReg REG_F1)
#endif				 	
#ifdef REG_F2			 	
magicIdRegMaybe (FloatReg 2#)	= Just (RealReg REG_F2)
#endif				 	
#ifdef REG_F3			 	
magicIdRegMaybe (FloatReg 3#)	= Just (RealReg REG_F3)
#endif				 	
#ifdef REG_F4			 	
magicIdRegMaybe (FloatReg 4#)	= Just (RealReg REG_F4)
#endif				 	
#ifdef REG_D1			 	
magicIdRegMaybe (DoubleReg 1#)	= Just (RealReg REG_D1)
#endif				 	
#ifdef REG_D2			 	
magicIdRegMaybe (DoubleReg 2#)	= Just (RealReg REG_D2)
#endif
#ifdef REG_Sp	    
magicIdRegMaybe Sp		   	= Just (RealReg REG_Sp)
#endif
#ifdef REG_Lng1			 	
magicIdRegMaybe (LongReg _ ILIT(1))	= Just (RealReg REG_Lng1)
#endif				 	
#ifdef REG_Lng2			 	
magicIdRegMaybe (LongReg _ ILIT(2))	= Just (RealReg REG_Lng2)
#endif
#ifdef REG_SpLim	    			
magicIdRegMaybe SpLim		   	= Just (RealReg REG_SpLim)
#endif	    				
#ifdef REG_Hp	   			
magicIdRegMaybe Hp		   	= Just (RealReg REG_Hp)
#endif	    				
#ifdef REG_HpLim      			
magicIdRegMaybe HpLim		   	= Just (RealReg REG_HpLim)
#endif	    				
#ifdef REG_CurrentTSO      			
magicIdRegMaybe CurrentTSO	   	= Just (RealReg REG_CurrentTSO)
#endif	    				
#ifdef REG_CurrentNursery      			
magicIdRegMaybe CurrentNursery	   	= Just (RealReg REG_CurrentNursery)
#endif	    				
magicIdRegMaybe _		   	= Nothing
\end{code}

\begin{code}
-------------------------------
-- allMachRegs is the complete set of machine regs.
allMachRegNos :: [Int]
allMachRegNos
   = IF_ARCH_alpha( [0..63],
     IF_ARCH_i386(  [0..13],
     IF_ARCH_sparc( ([0..31]
                     ++ [f0,f2 .. nCG_FirstFloatReg-1]
                     ++ [nCG_FirstFloatReg .. f31]),
     IF_ARCH_powerpc([0..63],
                   ))))
-- allocatableRegs is allMachRegNos with the fixed-use regs removed.
-- i.e., these are the regs for which we are prepared to allow the
-- register allocator to attempt to map VRegs to.
allocatableRegs :: [Reg]
allocatableRegs
   = let isFree i = isFastTrue (freeReg i)
     in  map RealReg (filter isFree allMachRegNos)

-------------------------------
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
#if sparc_TARGET_ARCH
    map RealReg 
        ( oReg 7 :
          [oReg i | i <- [0..5]] ++
          [gReg i | i <- [1..7]] ++
          [fReg i | i <- [0..31]] )
#endif /* sparc_TARGET_ARCH */
#if powerpc_TARGET_ARCH
    map RealReg ([0..12] ++ map fReg [0..13])
#endif /* powerpc_TARGET_ARCH */

-------------------------------
-- argRegs is the set of regs which are read for an n-argument call to C.
-- For archs which pass all args on the stack (x86), is empty.
-- Sparc passes up to the first 6 args in regs.
-- Dunno about Alpha.
argRegs :: Int -> [Reg]

#if i386_TARGET_ARCH
argRegs _ = panic "MachRegs.argRegs(x86): should not be used!"
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

-------------------------------
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

#if powerpc_TARGET_ARCH
allArgRegs :: [Reg]
allArgRegs = map RealReg [3..10]
allFPArgRegs :: [Reg]
allFPArgRegs = map (RealReg . fReg) [1..13]
#endif /* powerpc_TARGET_ARCH */
\end{code}

\begin{code}
freeReg :: Int -> FastBool

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
\end{code}
