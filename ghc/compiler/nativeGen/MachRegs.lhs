%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[MachRegs]{Machine-specific info about registers}

Also includes stuff about immediate operands, which are
often/usually quite entangled with registers.

(Immediates could be untangled from registers at some cost in tangled
modules --- the pleasure has been foregone.)

\begin{code}
#include "HsVersions.h"
#include "nativeGen/NCG.h"

module MachRegs (

	Reg(..),
	Imm(..),
	Addr(..),
	RegLoc(..),
	RegNo(..),

	addrOffset,
	argRegs,
	baseRegOffset,
	callClobberedRegs,
	callerSaves,
	dblImmLit,
	extractMappedRegNos,
	freeMappedRegs,
	freeReg, freeRegs,
	getNewRegNCG,
	magicIdRegMaybe,
	mkReg,
	realReg,
	reservedRegs,
	saveLoc,
	spRel,
	stgReg,
	strImmLit

#if alpha_TARGET_ARCH
	, allArgRegs
	, fits8Bits
	, fReg
	, gp, pv, ra, sp, t9, t10, t11, t12, v0, f0, zero
#endif
#if i386_TARGET_ARCH
	, eax, ebx, ecx, edx, esi, esp
	, st0, st1, st2, st3, st4, st5, st6, st7
#endif
#if sparc_TARGET_ARCH
	, allArgRegs
	, fits13Bits
	, fPair, fpRel, gReg, iReg, lReg, oReg, largeOffsetError
	, fp, g0, o0, f0
	
#endif
    ) where

IMP_Ubiq(){-uitous-}

import AbsCSyn		( MagicId(..) )
import AbsCUtils	( magicIdPrimRep )
import Pretty		( ppStr, ppRational, ppShow )
import PrimOp		( PrimOp(..) )
import PrimRep		( PrimRep(..) )
import Stix		( sStLitLbl, StixTree(..), StixReg(..),
			  CodeSegment
			)
import Unique		( mkPseudoUnique1, mkPseudoUnique2, mkPseudoUnique3,
			  Unique{-instance Ord3-}
			)
import UniqSupply	( getUnique, returnUs, thenUs, UniqSM(..) )
import Unpretty		( uppStr, Unpretty(..) )
import Util		( panic )
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

\begin{code}
data Imm
  = ImmInt	Int
  | ImmInteger	Integer	    -- Sigh.
  | ImmCLbl	CLabel	    -- AbstractC Label (with baggage)
  | ImmLab	Unpretty    -- Simple string label (underscore-able)
  | ImmLit	Unpretty    -- Simple string
  IF_ARCH_sparc(
  | LO Imm		    -- Possible restrictions...
  | HI Imm
  ,)

strImmLit s = ImmLit (uppStr s)
dblImmLit r
  = strImmLit (
	 IF_ARCH_alpha({-prepend nothing-}
	,IF_ARCH_i386( '0' : 'd' :
	,IF_ARCH_sparc('0' : 'r' :,)))
	ppShow 80 (ppRational r))
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

\begin{code}
data Addr
#if alpha_TARGET_ARCH
  = AddrImm	Imm
  | AddrReg	Reg
  | AddrRegImm	Reg Imm
#endif

#if i386_TARGET_ARCH
  = Addr	Base Index Displacement
  | ImmAddr	Imm Int

type Base         = Maybe Reg
type Index        = Maybe (Reg, Int)	-- Int is 2, 4 or 8
type Displacement = Imm
#endif

#if sparc_TARGET_ARCH
  = AddrRegReg	Reg Reg
  | AddrRegImm	Reg Imm
#endif

addrOffset :: Addr -> Int -> Maybe Addr

addrOffset addr off
  = case addr of
#if alpha_TARGET_ARCH
      _ -> panic "MachMisc.addrOffset not defined for Alpha"
#endif
#if i386_TARGET_ARCH
      ImmAddr i off0	  -> Just (ImmAddr i (off0 + off))
      Addr r i (ImmInt n) -> Just (Addr r i (ImmInt (n + off)))
      Addr r i (ImmInteger n)
	-> Just (Addr r i (ImmInt (fromInteger (n + toInteger off))))
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

      AddrRegReg r (FixedReg ILIT(0))
       | fits13Bits off -> Just (AddrRegImm r (ImmInt off))
       | otherwise     -> Nothing
       
      _ -> Nothing

#endif {-sparc-}

-----------------
#if alpha_TARGET_ARCH

fits8Bits :: Integer -> Bool
fits8Bits i = i >= -256 && i < 256

#endif

#if sparc_TARGET_ARCH
{-# SPECIALIZE
    fits13Bits :: Int -> Bool
  #-}
{-# SPECIALIZE
    fits13Bits :: Integer -> Bool
  #-}

fits13Bits :: Integral a => a -> Bool
fits13Bits x = x >= -4096 && x < 4096

-----------------
largeOffsetError i
  = error ("ERROR: SPARC native-code generator cannot handle large offset ("++show i++");\nprobably because of large constant data structures;\nworkaround: use -fvia-C on this module.\n")

#endif {-sparc-}
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@stgReg@: we map STG registers onto appropriate Stix Trees.  First, we
handle the two constants, @STK_STUB_closure@ and @vtbl_StdUpdFrame@.
The rest are either in real machine registers or stored as offsets
from BaseReg.

\begin{code}
data RegLoc = Save StixTree | Always StixTree
\end{code}

Trees for register save locations:
\begin{code}
saveLoc :: MagicId -> StixTree

saveLoc reg = case (stgReg reg) of {Always loc -> loc; Save loc -> loc}
\end{code}

\begin{code}
stgReg :: MagicId -> RegLoc

stgReg x
  = case (magicIdRegMaybe x) of
	Just _  -> Save   nonReg
	Nothing -> Always nonReg
  where
    offset = baseRegOffset x

    baseLoc = case (magicIdRegMaybe BaseReg) of
      Just _  -> StReg (StixMagicId BaseReg)
      Nothing -> sStLitLbl SLIT("MainRegTable")

    nonReg = case x of
      StkStubReg	-> sStLitLbl SLIT("STK_STUB_closure")
      StdUpdRetVecReg	-> sStLitLbl SLIT("vtbl_StdUpdFrame")
      BaseReg		-> sStLitLbl SLIT("MainRegTable")
	-- these Hp&HpLim cases perhaps should
	-- not be here for i386 (???) WDP 96/03
      Hp		-> StInd PtrRep (sStLitLbl SLIT("StorageMgrInfo"))
      HpLim		-> StInd PtrRep (sStLitLbl
				(_PK_ ("StorageMgrInfo+" ++ BYTES_PER_WORD_STR)))
      TagReg		-> StInd IntRep (StPrim IntSubOp [infoptr,
				StInt (1*BYTES_PER_WORD)])
			where
			    r2      = VanillaReg PtrRep ILIT(2)
			    infoptr = case (stgReg r2) of
					  Always t -> t
					  Save   _ -> StReg (StixMagicId r2)
      _ -> StInd (magicIdPrimRep x)
		 (StPrim IntAddOp [baseLoc,
			StInt (toInteger (offset*BYTES_PER_WORD))])
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@spRel@ gives us a stack relative addressing mode for volatile
temporaries and for excess call arguments.  @fpRel@, where
applicable, is the same but for the frame pointer.

\begin{code}
spRel :: Int	-- desired stack offset in words, positive or negative
      -> Addr

spRel n
#if i386_TARGET_ARCH
  = Addr (Just esp) Nothing (ImmInt (n * BYTES_PER_WORD))
#else
  = AddrRegImm sp (ImmInt (n * BYTES_PER_WORD))
#endif

#if sparc_TARGET_ARCH
fpRel :: Int -> Addr
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

Static Registers correspond to actual machine registers.  These should
be avoided until the last possible moment.

Dynamic registers are allocated on the fly, usually to represent a single
value in the abstract assembly code (i.e. dynamic registers are usually
single assignment).  Ultimately, they are mapped to available machine
registers before spitting out the code.

\begin{code}
data Reg
  = FixedReg  FAST_INT		-- A pre-allocated machine register

  | MappedReg FAST_INT		-- A dynamically allocated machine register

  | MemoryReg Int PrimRep	-- A machine "register" actually held in
				-- a memory allocated table of
				-- registers which didn't fit in real
				-- registers.

  | UnmappedReg Unique PrimRep	-- One of an infinite supply of registers,
				-- always mapped to one of the earlier
				-- two (?)  before we're done.

mkReg :: Unique -> PrimRep -> Reg
mkReg = UnmappedReg

getNewRegNCG :: PrimRep -> UniqSM Reg
getNewRegNCG pk
  = getUnique	`thenUs` \ u ->
    returnUs (UnmappedReg u pk)

instance Text Reg where
    showsPrec _ (FixedReg i)	= showString "%"  . shows IBOX(i)
    showsPrec _ (MappedReg i)	= showString "%"  . shows IBOX(i)
    showsPrec _ (MemoryReg i _) = showString "%M"  . shows i
    showsPrec _ (UnmappedReg i _) = showString "%U" . shows i

#ifdef DEBUG
instance Outputable Reg where
    ppr sty r = ppStr (show r)
#endif

cmpReg (FixedReg i)      (FixedReg i')      = cmp_ihash i i'
cmpReg (MappedReg i)     (MappedReg i')     = cmp_ihash i i'
cmpReg (MemoryReg i _)   (MemoryReg i' _)   = cmp_i i i'
cmpReg (UnmappedReg u _) (UnmappedReg u' _) = cmp u u'
cmpReg r1 r2
  = let tag1 = tagReg r1
	tag2 = tagReg r2
    in
	if tag1 _LT_ tag2 then LT_ else GT_
    where
	tagReg (FixedReg _)	 = (ILIT(1) :: FAST_INT)
	tagReg (MappedReg _)	 = ILIT(2)
	tagReg (MemoryReg _ _)	 = ILIT(3)
	tagReg (UnmappedReg _ _) = ILIT(4)

cmp_i :: Int -> Int -> TAG_
cmp_i a1 a2 = if a1 == a2 then EQ_ else if a1 < a2 then LT_ else GT_

cmp_ihash :: FAST_INT -> FAST_INT -> TAG_
cmp_ihash a1 a2 = if a1 _EQ_ a2 then EQ_ else if a1 _LT_ a2 then LT_ else GT_

instance Ord3 Reg where
    cmp = cmpReg

instance Eq Reg where
    a == b = case (a `cmp` b) of { EQ_ -> True;  _ -> False }
    a /= b = case (a `cmp` b) of { EQ_ -> False; _ -> True  }

instance Ord Reg where
    a <= b = case (a `cmp` b) of { LT_ -> True;	EQ_ -> True;  GT__ -> False }
    a <	 b = case (a `cmp` b) of { LT_ -> True;	EQ_ -> False; GT__ -> False }
    a >= b = case (a `cmp` b) of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    a >	 b = case (a `cmp` b) of { LT_ -> False; EQ_ -> False; GT__ -> True  }
    _tagCmp a b = case (a `cmp` b) of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }

instance Uniquable Reg where
    uniqueOf (UnmappedReg u _) = u
    uniqueOf (FixedReg i)      = mkPseudoUnique1 IBOX(i)
    uniqueOf (MappedReg i)     = mkPseudoUnique2 IBOX(i)
    uniqueOf (MemoryReg i _)   = mkPseudoUnique3 i
\end{code}

\begin{code}
type RegNo = Int

realReg :: RegNo -> Reg
realReg n@IBOX(i)
  = if _IS_TRUE_(freeReg i) then MappedReg i else FixedReg i

extractMappedRegNos :: [Reg] -> [RegNo]

extractMappedRegNos regs
  = foldr ex [] regs
  where
    ex (MappedReg i) acc = IBOX(i) : acc  -- we'll take it
    ex _	     acc = acc		  -- leave it out
\end{code}

** Machine-specific Reg stuff: **

The Alpha has 64 registers of interest; 32 integer registers and 32 floating
point registers.  The mapping of STG registers to alpha machine registers
is defined in StgRegs.h.  We are, of course, prepared for any eventuality.
\begin{code}
#if alpha_TARGET_ARCH
fReg :: Int -> Int
fReg x = (32 + x)

v0, f0, ra, pv, gp, sp, zero :: Reg
v0   = realReg 0
f0   = realReg (fReg 0)
ra   = FixedReg ILIT(26)
pv   = t12
gp   = FixedReg ILIT(29)
sp   = FixedReg ILIT(30)
zero = FixedReg ILIT(31)

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
- Registers 8-15 hold extended floating point values.
\begin{code}
#if i386_TARGET_ARCH

gReg,fReg :: Int -> Int
gReg x = x
fReg x = (8 + x)

st0, st1, st2, st3, st4, st5, st6, st7, eax, ebx, ecx, edx, esp :: Reg
eax = case (gReg 0) of { IBOX(g0) -> FixedReg g0 }
ebx = case (gReg 1) of { IBOX(g1) -> FixedReg g1 }
ecx = case (gReg 2) of { IBOX(g2) -> FixedReg g2 }
edx = case (gReg 3) of { IBOX(g3) -> FixedReg g3 }
esi = case (gReg 4) of { IBOX(g4) -> FixedReg g4 }
edi = case (gReg 5) of { IBOX(g5) -> FixedReg g5 }
ebp = case (gReg 6) of { IBOX(g6) -> FixedReg g6 }
esp = case (gReg 7) of { IBOX(g7) -> FixedReg g7 }
st0 = realReg  (fReg 0)
st1 = realReg  (fReg 1)
st2 = realReg  (fReg 2)
st3 = realReg  (fReg 3)
st4 = realReg  (fReg 4)
st5 = realReg  (fReg 5)
st6 = realReg  (fReg 6)
st7 = realReg  (fReg 7)

#endif
\end{code}

The SPARC has 64 registers of interest; 32 integer registers and 32
floating point registers.  The mapping of STG registers to SPARC
machine registers is defined in StgRegs.h.  We are, of course,
prepared for any eventuality.

\begin{code}
#if sparc_TARGET_ARCH

gReg,lReg,iReg,oReg,fReg :: Int -> Int
gReg x = x
oReg x = (8 + x)
lReg x = (16 + x)
iReg x = (24 + x)
fReg x = (32 + x)

fPair :: Reg -> Reg
fPair (FixedReg i) = FixedReg (i _ADD_ ILIT(1))
fPair (MappedReg i) = MappedReg (i _ADD_ ILIT(1))

g0, fp, sp, o0, f0 :: Reg
g0 = case (gReg 0) of { IBOX(g0) -> FixedReg g0 }
fp = case (iReg 6) of { IBOX(i6) -> FixedReg i6 }
sp = case (oReg 6) of { IBOX(o6) -> FixedReg o6 }
o0 = realReg  (oReg 0)
f0 = realReg  (fReg 0)

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
#define st0 8
#define st1 9
#define st2 10
#define st3 11
#define st4 12
#define st5 13
#define st6 14
#define st7 15
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
\end{code}

\begin{code}
baseRegOffset :: MagicId -> Int

baseRegOffset StkOReg		     = OFFSET_StkO
baseRegOffset (VanillaReg _ ILIT(1)) = OFFSET_R1
baseRegOffset (VanillaReg _ ILIT(2)) = OFFSET_R2
baseRegOffset (VanillaReg _ ILIT(3)) = OFFSET_R3
baseRegOffset (VanillaReg _ ILIT(4)) = OFFSET_R4
baseRegOffset (VanillaReg _ ILIT(5)) = OFFSET_R5
baseRegOffset (VanillaReg _ ILIT(6)) = OFFSET_R6
baseRegOffset (VanillaReg _ ILIT(7)) = OFFSET_R7
baseRegOffset (VanillaReg _ ILIT(8)) = OFFSET_R8
baseRegOffset (FloatReg  ILIT(1))    = OFFSET_Flt1
baseRegOffset (FloatReg  ILIT(2))    = OFFSET_Flt2
baseRegOffset (FloatReg  ILIT(3))    = OFFSET_Flt3
baseRegOffset (FloatReg  ILIT(4))    = OFFSET_Flt4
baseRegOffset (DoubleReg ILIT(1))    = OFFSET_Dbl1
baseRegOffset (DoubleReg ILIT(2))    = OFFSET_Dbl2
baseRegOffset TagReg		     = OFFSET_Tag
baseRegOffset RetReg		     = OFFSET_Ret
baseRegOffset SpA		     = OFFSET_SpA
baseRegOffset SuA		     = OFFSET_SuA
baseRegOffset SpB		     = OFFSET_SpB
baseRegOffset SuB		     = OFFSET_SuB
baseRegOffset Hp		     = OFFSET_Hp
baseRegOffset HpLim		     = OFFSET_HpLim
baseRegOffset LivenessReg	     = OFFSET_Liveness
#ifdef DEBUG
baseRegOffset BaseReg		     = panic "baseRegOffset:BaseReg"
baseRegOffset StdUpdRetVecReg	     = panic "baseRegOffset:StgUpdRetVecReg"
baseRegOffset StkStubReg	     = panic "baseRegOffset:StkStubReg"
baseRegOffset CurCostCentre	     = panic "baseRegOffset:CurCostCentre"
baseRegOffset VoidReg		     = panic "baseRegOffset:VoidReg"
#endif
\end{code}

\begin{code}
callerSaves :: MagicId -> Bool

#ifdef CALLER_SAVES_Base
callerSaves BaseReg			= True
#endif
#ifdef CALLER_SAVES_StkO
callerSaves StkOReg			= True
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
#ifdef CALLER_SAVES_FltReg1
callerSaves (FloatReg ILIT(1))		= True
#endif
#ifdef CALLER_SAVES_FltReg2
callerSaves (FloatReg ILIT(2))		= True
#endif
#ifdef CALLER_SAVES_FltReg3
callerSaves (FloatReg ILIT(3))		= True
#endif
#ifdef CALLER_SAVES_FltReg4
callerSaves (FloatReg ILIT(4))		= True
#endif
#ifdef CALLER_SAVES_DblReg1
callerSaves (DoubleReg ILIT(1))		= True
#endif
#ifdef CALLER_SAVES_DblReg2
callerSaves (DoubleReg ILIT(2))		= True
#endif
#ifdef CALLER_SAVES_Tag
callerSaves TagReg			= True
#endif
#ifdef CALLER_SAVES_Ret
callerSaves RetReg			= True
#endif
#ifdef CALLER_SAVES_SpA
callerSaves SpA				= True
#endif
#ifdef CALLER_SAVES_SuA
callerSaves SuA				= True
#endif
#ifdef CALLER_SAVES_SpB
callerSaves SpB				= True
#endif
#ifdef CALLER_SAVES_SuB
callerSaves SuB				= True
#endif
#ifdef CALLER_SAVES_Hp
callerSaves Hp				= True
#endif
#ifdef CALLER_SAVES_HpLim
callerSaves HpLim			= True
#endif
#ifdef CALLER_SAVES_Liveness
callerSaves LivenessReg			= True
#endif
#ifdef CALLER_SAVES_StdUpdRetVec
callerSaves StdUpdRetVecReg		= True
#endif
#ifdef CALLER_SAVES_StkStub
callerSaves StkStubReg			= True
#endif
callerSaves _				= False
\end{code}

\begin{code}
magicIdRegMaybe :: MagicId -> Maybe Reg

#ifdef REG_Base
magicIdRegMaybe BaseReg			= Just (FixedReg ILIT(REG_Base))
#endif
#ifdef REG_StkO
magicIdRegMaybe StkOReg			= Just (FixedReg ILIT(REG_StkOReg))
#endif
#ifdef REG_R1
magicIdRegMaybe (VanillaReg _ ILIT(1)) 	= Just (FixedReg ILIT(REG_R1))
#endif 
#ifdef REG_R2 
magicIdRegMaybe (VanillaReg _ ILIT(2)) 	= Just (FixedReg ILIT(REG_R2))
#endif 
#ifdef REG_R3 
magicIdRegMaybe (VanillaReg _ ILIT(3)) 	= Just (FixedReg ILIT(REG_R3))
#endif 
#ifdef REG_R4 
magicIdRegMaybe (VanillaReg _ ILIT(4)) 	= Just (FixedReg ILIT(REG_R4))
#endif 
#ifdef REG_R5 
magicIdRegMaybe (VanillaReg _ ILIT(5)) 	= Just (FixedReg ILIT(REG_R5))
#endif 
#ifdef REG_R6 
magicIdRegMaybe (VanillaReg _ ILIT(6)) 	= Just (FixedReg ILIT(REG_R6))
#endif 
#ifdef REG_R7 
magicIdRegMaybe (VanillaReg _ ILIT(7)) 	= Just (FixedReg ILIT(REG_R7))
#endif 
#ifdef REG_R8 
magicIdRegMaybe (VanillaReg _ ILIT(8)) 	= Just (FixedReg ILIT(REG_R8))
#endif
#ifdef REG_Flt1
magicIdRegMaybe (FloatReg ILIT(1))	= Just (FixedReg ILIT(REG_Flt1))
#endif				 	
#ifdef REG_Flt2			 	
magicIdRegMaybe (FloatReg ILIT(2))	= Just (FixedReg ILIT(REG_Flt2))
#endif				 	
#ifdef REG_Flt3			 	
magicIdRegMaybe (FloatReg ILIT(3))	= Just (FixedReg ILIT(REG_Flt3))
#endif				 	
#ifdef REG_Flt4			 	
magicIdRegMaybe (FloatReg ILIT(4))	= Just (FixedReg ILIT(REG_Flt4))
#endif				 	
#ifdef REG_Dbl1			 	
magicIdRegMaybe (DoubleReg ILIT(1))	= Just (FixedReg ILIT(REG_Dbl1))
#endif				 	
#ifdef REG_Dbl2			 	
magicIdRegMaybe (DoubleReg ILIT(2))	= Just (FixedReg ILIT(REG_Dbl2))
#endif
#ifdef REG_Tag
magicIdRegMaybe TagReg			= Just (FixedReg ILIT(REG_TagReg))
#endif	    
#ifdef REG_Ret	    
magicIdRegMaybe RetReg			= Just (FixedReg ILIT(REG_Ret))
#endif	    
#ifdef REG_SpA	    
magicIdRegMaybe SpA		   	= Just (FixedReg ILIT(REG_SpA))
#endif	    				
#ifdef REG_SuA	    			
magicIdRegMaybe SuA		   	= Just (FixedReg ILIT(REG_SuA))
#endif	    				
#ifdef REG_SpB	    			
magicIdRegMaybe SpB		   	= Just (FixedReg ILIT(REG_SpB))
#endif	    				
#ifdef REG_SuB	    			
magicIdRegMaybe SuB		   	= Just (FixedReg ILIT(REG_SuB))
#endif	    				
#ifdef REG_Hp	   			
magicIdRegMaybe Hp		   	= Just (FixedReg ILIT(REG_Hp))
#endif	    				
#ifdef REG_HpLim      			
magicIdRegMaybe HpLim		   	= Just (FixedReg ILIT(REG_HpLim))
#endif	    				
#ifdef REG_Liveness	 		
magicIdRegMaybe LivenessReg	   	= Just (FixedReg ILIT(REG_Liveness))
#endif	    				
#ifdef REG_StdUpdRetVec	     		
magicIdRegMaybe StdUpdRetVecReg  	= Just (FixedReg ILIT(REG_StdUpdRetVec))
#endif	    				
#ifdef REG_StkStub			
magicIdRegMaybe StkStubReg	   	= Just (FixedReg ILIT(REG_StkStub))
#endif	    				
magicIdRegMaybe _		   	= Nothing
\end{code}

%************************************************************************
%*									*
\subsection{Free, reserved, call-clobbered, and argument registers}
%*									*
%************************************************************************

@freeRegs@ is the list of registers we can use in register allocation.
@freeReg@ (below) says if a particular register is free.

With a per-instruction clobber list, we might be able to get some of
these back, but it's probably not worth the hassle.

@callClobberedRegs@ ... the obvious.

@argRegs@: assuming a call with N arguments, what registers will be
used to hold arguments?  (NB: it doesn't know whether the arguments
are integer or floating-point...)

\begin{code}
reservedRegs :: [RegNo]
reservedRegs
#if alpha_TARGET_ARCH
  = [NCG_Reserved_I1, NCG_Reserved_I2,
     NCG_Reserved_F1, NCG_Reserved_F2]
#endif
#if i386_TARGET_ARCH
  = [{-certainly cannot afford any!-}]
#endif
#if sparc_TARGET_ARCH
  = [NCG_Reserved_I1, NCG_Reserved_I2,
     NCG_Reserved_F1, NCG_Reserved_F2,
     NCG_Reserved_D1, NCG_Reserved_D2]
#endif

-------------------------------
freeRegs :: [Reg]
freeRegs
  = freeMappedRegs IF_ARCH_alpha( [0..63],
		   IF_ARCH_i386(  [0..15],
		   IF_ARCH_sparc( [0..63],)))

-------------------------------
callClobberedRegs :: [Reg]
callClobberedRegs
  = freeMappedRegs
#if alpha_TARGET_ARCH
    [0, 1, 2, 3, 4, 5, 6, 7, 8,
     16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
     fReg 0, fReg 1, fReg 10, fReg 11, fReg 12, fReg 13, fReg 14, fReg 15,
     fReg 16, fReg 17, fReg 18, fReg 19, fReg 20, fReg 21, fReg 22, fReg 23,
     fReg 24, fReg 25, fReg 26, fReg 27, fReg 28, fReg 29, fReg 30]
#endif {- alpha_TARGET_ARCH -}
#if i386_TARGET_ARCH
    [{-none-}]
#endif {- i386_TARGET_ARCH -}
#if sparc_TARGET_ARCH
    ( oReg 7 :
      [oReg i | i <- [0..5]] ++
      [gReg i | i <- [1..7]] ++
      [fReg i | i <- [0..31]] )
#endif {- sparc_TARGET_ARCH -}

-------------------------------
argRegs :: Int -> [Reg]

argRegs 0 = []
#if alpha_TARGET_ARCH
argRegs 1 = freeMappedRegs [16, fReg 16]
argRegs 2 = freeMappedRegs [16, 17, fReg 16, fReg 17]
argRegs 3 = freeMappedRegs [16, 17, 18, fReg 16, fReg 17, fReg 18]
argRegs 4 = freeMappedRegs [16, 17, 18, 19, fReg 16, fReg 17, fReg 18, fReg 19]
argRegs 5 = freeMappedRegs [16, 17, 18, 19, 20, fReg 16, fReg 17, fReg 18, fReg 19, fReg 20]
argRegs 6 = freeMappedRegs [16, 17, 18, 19, 20, 21, fReg 16, fReg 17, fReg 18, fReg 19, fReg 20, fReg 21]
#endif {- alpha_TARGET_ARCH -}
#if i386_TARGET_ARCH
argRegs _ = panic "MachRegs.argRegs: doesn't work on I386"
#endif {- i386_TARGET_ARCH -}
#if sparc_TARGET_ARCH
argRegs 1 = freeMappedRegs (map oReg [0])
argRegs 2 = freeMappedRegs (map oReg [0,1])
argRegs 3 = freeMappedRegs (map oReg [0,1,2])
argRegs 4 = freeMappedRegs (map oReg [0,1,2,3])
argRegs 5 = freeMappedRegs (map oReg [0,1,2,3,4])
argRegs 6 = freeMappedRegs (map oReg [0,1,2,3,4,5])
#endif {- sparc_TARGET_ARCH -}
argRegs _ = panic "MachRegs.argRegs: don't know about >6 arguments!"

-------------------------------

#if alpha_TARGET_ARCH
allArgRegs :: [(Reg, Reg)]

allArgRegs = [(realReg i, realReg (fReg i)) | i <- [16..21]]
#endif {- alpha_TARGET_ARCH -}

#if sparc_TARGET_ARCH
allArgRegs :: [Reg]

allArgRegs = map realReg [oReg i | i <- [0..5]]
#endif {- sparc_TARGET_ARCH -}

-------------------------------
freeMappedRegs :: [Int] -> [Reg]

freeMappedRegs nums
  = foldr free [] nums
  where
    free IBOX(i) acc
      = if _IS_TRUE_(freeReg i) then (MappedReg i) : acc else acc
\end{code}

\begin{code}
freeReg :: FAST_INT -> FAST_BOOL

#if alpha_TARGET_ARCH
freeReg ILIT(26) = _FALSE_  -- return address (ra)
freeReg ILIT(28) = _FALSE_  -- reserved for the assembler (at)
freeReg ILIT(29) = _FALSE_  -- global pointer (gp)
freeReg ILIT(30) = _FALSE_  -- stack pointer (sp)
freeReg ILIT(31) = _FALSE_  -- always zero (zero)
freeReg ILIT(63) = _FALSE_  -- always zero (f31)
#endif

#if i386_TARGET_ARCH
freeReg ILIT(esp) = _FALSE_  --	%esp is the C stack pointer
#endif

#if sparc_TARGET_ARCH
freeReg ILIT(g0) = _FALSE_  --	%g0 is always 0.
freeReg ILIT(g5) = _FALSE_  --	%g5 is reserved (ABI).
freeReg ILIT(g6) = _FALSE_  --	%g6 is reserved (ABI).
freeReg ILIT(g7) = _FALSE_  --	%g7 is reserved (ABI).
freeReg ILIT(i6) = _FALSE_  --	%i6 is our frame pointer.
freeReg ILIT(o6) = _FALSE_  --	%o6 is our stack pointer.
#endif

#ifdef REG_Base
freeReg ILIT(REG_Base) = _FALSE_
#endif
#ifdef REG_StkO
freeReg ILIT(REG_StkO) = _FALSE_
#endif
#ifdef REG_R1
freeReg ILIT(REG_R1)   = _FALSE_
#endif	
#ifdef REG_R2  
freeReg ILIT(REG_R2)   = _FALSE_
#endif	
#ifdef REG_R3  
freeReg ILIT(REG_R3)   = _FALSE_
#endif	
#ifdef REG_R4  
freeReg ILIT(REG_R4)   = _FALSE_
#endif	
#ifdef REG_R5  
freeReg ILIT(REG_R5)   = _FALSE_
#endif	
#ifdef REG_R6  
freeReg ILIT(REG_R6)   = _FALSE_
#endif	
#ifdef REG_R7  
freeReg ILIT(REG_R7)   = _FALSE_
#endif	
#ifdef REG_R8  
freeReg ILIT(REG_R8)   = _FALSE_
#endif
#ifdef REG_Flt1
freeReg ILIT(REG_Flt1) = _FALSE_
#endif
#ifdef REG_Flt2
freeReg ILIT(REG_Flt2) = _FALSE_
#endif
#ifdef REG_Flt3
freeReg ILIT(REG_Flt3) = _FALSE_
#endif
#ifdef REG_Flt4
freeReg ILIT(REG_Flt4) = _FALSE_
#endif
#ifdef REG_Dbl1
freeReg ILIT(REG_Dbl1) = _FALSE_
#endif
#ifdef REG_Dbl2
freeReg ILIT(REG_Dbl2) = _FALSE_
#endif
#ifdef REG_Tag
freeReg ILIT(REG_Tag)  = _FALSE_
#endif 
#ifdef REG_Ret 
freeReg ILIT(REG_Ret)  = _FALSE_
#endif 
#ifdef REG_SpA 
freeReg ILIT(REG_SpA)  = _FALSE_
#endif 
#ifdef REG_SuA 
freeReg ILIT(REG_SuA)  = _FALSE_
#endif 
#ifdef REG_SpB 
freeReg ILIT(REG_SpB)  = _FALSE_
#endif 
#ifdef REG_SuB 
freeReg ILIT(REG_SuB)  = _FALSE_
#endif 
#ifdef REG_Hp 
freeReg ILIT(REG_Hp)   = _FALSE_
#endif
#ifdef REG_HpLim
freeReg ILIT(REG_HpLim) = _FALSE_
#endif
#ifdef REG_Liveness
freeReg ILIT(REG_Liveness) = _FALSE_
#endif
#ifdef REG_StdUpdRetVec
freeReg ILIT(REG_StdUpdRetVec) = _FALSE_
#endif
#ifdef REG_StkStub
freeReg ILIT(REG_StkStub) = _FALSE_
#endif
freeReg _ = _TRUE_
freeReg n
  -- we hang onto two double regs for dedicated
  -- use; this is not necessary on Alphas and
  -- may not be on other non-SPARCs.
#ifdef REG_Dbl1
  | n _EQ_ (ILIT(REG_Dbl1) _ADD_ ILIT(1)) = _FALSE_
#endif
#ifdef REG_Dbl2
  | n _EQ_ (ILIT(REG_Dbl2) _ADD_ ILIT(1)) = _FALSE_
#endif
  | otherwise = _TRUE_
\end{code}
