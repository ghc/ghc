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

	Reg(..),
	Imm(..),
	MachRegsAddr(..),
	RegLoc(..),
	RegNo,

	addrOffset,
	argRegs,
	baseRegOffset,
	callClobberedRegs,
	callerSaves,
	extractMappedRegNos,
        mappedRegNo,
	freeMappedRegs,
	freeReg, freeRegs,
	getNewRegNCG,
	magicIdRegMaybe,
	mkReg,
	realReg,
	saveLoc,
	spRel,
	stgReg,
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
	, allArgRegs
	, fits13Bits
	, fPair, fpRel, gReg, iReg, lReg, oReg, largeOffsetError
	, fp, g0, o0, f0
	
#endif
    ) where

#include "HsVersions.h"

import AbsCSyn		( MagicId(..) )
import AbsCUtils	( magicIdPrimRep )
import CLabel           ( CLabel )
import PrimOp		( PrimOp(..) )
import PrimRep		( PrimRep(..) )
import Stix		( sStLitLbl, StixTree(..), StixReg(..) )
import Unique		( mkPseudoUnique1, mkPseudoUnique2, mkPseudoUnique3,
			  Uniquable(..), Unique
			)
import UniqSupply	( getUniqueUs, returnUs, thenUs, UniqSM )
import Outputable
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

\begin{code}
data Imm
  = ImmInt	Int
  | ImmInteger	Integer	    -- Sigh.
  | ImmCLbl	CLabel	    -- AbstractC Label (with baggage)
  | ImmLab	SDoc    -- Simple string label (underscore-able)
  | ImmLit	SDoc    -- Simple string
  | ImmIndex    CLabel Int
  | ImmDouble	Rational
  IF_ARCH_sparc(
  | LO Imm		    -- Possible restrictions...
  | HI Imm
  ,)
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
      BaseReg		-> sStLitLbl SLIT("MainRegTable")

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
  = getUniqueUs	`thenUs` \ u ->
    returnUs (UnmappedReg u pk)

instance Text Reg where
    showsPrec _ (FixedReg i)	= showString "%"  . shows IBOX(i)
    showsPrec _ (MappedReg i)	= showString "%"  . shows IBOX(i)
    showsPrec _ (MemoryReg i _) = showString "%M"  . shows i
    showsPrec _ (UnmappedReg i _) = showString "%U" . shows i

#ifdef DEBUG
instance Outputable Reg where
    ppr r = text (show r)
#endif

cmpReg (FixedReg i)      (FixedReg i')      = cmp_ihash i i'
cmpReg (MappedReg i)     (MappedReg i')     = cmp_ihash i i'
cmpReg (MemoryReg i _)   (MemoryReg i' _)   = i `compare` i'
cmpReg (UnmappedReg u _) (UnmappedReg u' _) = compare u u'
cmpReg r1 r2
  = let tag1 = tagReg r1
	tag2 = tagReg r2
    in
	if tag1 _LT_ tag2 then LT else GT
    where
	tagReg (FixedReg _)	 = (ILIT(1) :: FAST_INT)
	tagReg (MappedReg _)	 = ILIT(2)
	tagReg (MemoryReg _ _)	 = ILIT(3)
	tagReg (UnmappedReg _ _) = ILIT(4)

cmp_ihash :: FAST_INT -> FAST_INT -> Ordering
cmp_ihash a1 a2 = if a1 _EQ_ a2 then EQ else if a1 _LT_ a2 then LT else GT

instance Eq Reg where
    a == b = case (a `compare` b) of { EQ -> True;  _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False; _ -> True  }

instance Ord Reg where
    a <= b = case (a `compare` b) of { LT -> True;	EQ -> True;  GT -> False }
    a <	 b = case (a `compare` b) of { LT -> True;	EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >	 b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = cmpReg a b

instance Uniquable Reg where
    getUnique (UnmappedReg u _) = u
    getUnique (FixedReg i)      = mkPseudoUnique1 IBOX(i)
    getUnique (MappedReg i)     = mkPseudoUnique2 IBOX(i)
    getUnique (MemoryReg i _)   = mkPseudoUnique3 i
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

mappedRegNo :: Reg -> RegNo
mappedRegNo (MappedReg i) = IBOX(i)
mappedRegNo _             = pprPanic "mappedRegNo" empty
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

\begin{code}
#if i386_TARGET_ARCH

gReg,fReg :: Int -> Int
gReg x = x
fReg x = (8 + x)

fake0, fake1, fake2, fake3, fake4, fake5, eax, ebx, ecx, edx, esp :: Reg
eax = realReg (gReg 0)
ebx = realReg (gReg 1)
ecx = realReg (gReg 2)
edx = realReg (gReg 3)
esi = realReg (gReg 4)
edi = realReg (gReg 5)
ebp = realReg (gReg 6)
esp = realReg (gReg 7)
fake0 = realReg (fReg 0)
fake1 = realReg (fReg 1)
fake2 = realReg (fReg 2)
fake3 = realReg (fReg 3)
fake4 = realReg (fReg 4)
fake5 = realReg (fReg 5)
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

baseRegOffset (VanillaReg _ ILIT(1)) = OFFSET_R1
baseRegOffset (VanillaReg _ ILIT(2)) = OFFSET_R2
baseRegOffset (VanillaReg _ ILIT(3)) = OFFSET_R3
baseRegOffset (VanillaReg _ ILIT(4)) = OFFSET_R4
baseRegOffset (VanillaReg _ ILIT(5)) = OFFSET_R5
baseRegOffset (VanillaReg _ ILIT(6)) = OFFSET_R6
baseRegOffset (VanillaReg _ ILIT(7)) = OFFSET_R7
baseRegOffset (VanillaReg _ ILIT(8)) = OFFSET_R8
baseRegOffset (VanillaReg _ ILIT(9)) = OFFSET_R9
baseRegOffset (VanillaReg _ ILIT(10)) = OFFSET_R10
baseRegOffset (FloatReg  ILIT(1))    = OFFSET_F1
baseRegOffset (FloatReg  ILIT(2))    = OFFSET_F2
baseRegOffset (FloatReg  ILIT(3))    = OFFSET_F3
baseRegOffset (FloatReg  ILIT(4))    = OFFSET_F4
baseRegOffset (DoubleReg ILIT(1))    = OFFSET_D1
baseRegOffset (DoubleReg ILIT(2))    = OFFSET_D2
baseRegOffset Sp		     = OFFSET_Sp
baseRegOffset Su		     = OFFSET_Su
baseRegOffset SpLim		     = OFFSET_SpLim
#ifdef OFFSET_Lng1
baseRegOffset (LongReg _ ILIT(1))    = OFFSET_Lng1
#endif
#ifdef OFFSET_Lng2
baseRegOffset (LongReg _ ILIT(2))    = OFFSET_Lng2
#endif
baseRegOffset Hp		     = OFFSET_Hp
baseRegOffset HpLim		     = OFFSET_HpLim
#ifdef DEBUG
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
callerSaves (FloatReg ILIT(1))		= True
#endif
#ifdef CALLER_SAVES_F2
callerSaves (FloatReg ILIT(2))		= True
#endif
#ifdef CALLER_SAVES_F3
callerSaves (FloatReg ILIT(3))		= True
#endif
#ifdef CALLER_SAVES_F4
callerSaves (FloatReg ILIT(4))		= True
#endif
#ifdef CALLER_SAVES_D1
callerSaves (DoubleReg ILIT(1))		= True
#endif
#ifdef CALLER_SAVES_D2
callerSaves (DoubleReg ILIT(2))		= True
#endif
#ifdef CALLER_SAVES_L1
callerSaves (LongReg _ ILIT(1))		= True
#endif
#ifdef CALLER_SAVES_Sp
callerSaves Sp				= True
#endif
#ifdef CALLER_SAVES_Su
callerSaves Su				= True
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
callerSaves _				= False
\end{code}

\begin{code}
magicIdRegMaybe :: MagicId -> Maybe Reg

#ifdef REG_Base
magicIdRegMaybe BaseReg			= Just (FixedReg ILIT(REG_Base))
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
#ifdef REG_R9 
magicIdRegMaybe (VanillaReg _ ILIT(9)) 	= Just (FixedReg ILIT(REG_R9))
#endif
#ifdef REG_R10 
magicIdRegMaybe (VanillaReg _ ILIT(10))	= Just (FixedReg ILIT(REG_R10))
#endif
#ifdef REG_F1
magicIdRegMaybe (FloatReg ILIT(1))	= Just (FixedReg ILIT(REG_F1))
#endif				 	
#ifdef REG_F2			 	
magicIdRegMaybe (FloatReg ILIT(2))	= Just (FixedReg ILIT(REG_F2))
#endif				 	
#ifdef REG_F3			 	
magicIdRegMaybe (FloatReg ILIT(3))	= Just (FixedReg ILIT(REG_F3))
#endif				 	
#ifdef REG_F4			 	
magicIdRegMaybe (FloatReg ILIT(4))	= Just (FixedReg ILIT(REG_F4))
#endif				 	
#ifdef REG_D1			 	
magicIdRegMaybe (DoubleReg ILIT(1))	= Just (FixedReg ILIT(REG_D1))
#endif				 	
#ifdef REG_D2			 	
magicIdRegMaybe (DoubleReg ILIT(2))	= Just (FixedReg ILIT(REG_D2))
#endif
#ifdef REG_Sp	    
magicIdRegMaybe Sp		   	= Just (FixedReg ILIT(REG_Sp))
#endif
#ifdef REG_Lng1			 	
magicIdRegMaybe (LongReg _ ILIT(1))	= Just (FixedReg ILIT(REG_Lng1))
#endif				 	
#ifdef REG_Lng2			 	
magicIdRegMaybe (LongReg _ ILIT(2))	= Just (FixedReg ILIT(REG_Lng2))
#endif
#ifdef REG_Su	    			
magicIdRegMaybe Su		   	= Just (FixedReg ILIT(REG_Su))
#endif	    				
#ifdef REG_SpLim	    			
magicIdRegMaybe SpLim		   	= Just (FixedReg ILIT(REG_SpLim))
#endif	    				
#ifdef REG_Hp	   			
magicIdRegMaybe Hp		   	= Just (FixedReg ILIT(REG_Hp))
#endif	    				
#ifdef REG_HpLim      			
magicIdRegMaybe HpLim		   	= Just (FixedReg ILIT(REG_HpLim))
#endif	    				
magicIdRegMaybe _		   	= Nothing
\end{code}

\begin{code}
-------------------------------
freeRegs :: [Reg]
freeRegs
  = freeMappedRegs IF_ARCH_alpha( [0..63],
		   IF_ARCH_i386(  [0..13],
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
#if i386_TARGET_ARCH
argRegs _ = panic "MachRegs.argRegs: doesn't work on I386"
#else
#if alpha_TARGET_ARCH
argRegs 1 = freeMappedRegs [16, fReg 16]
argRegs 2 = freeMappedRegs [16, 17, fReg 16, fReg 17]
argRegs 3 = freeMappedRegs [16, 17, 18, fReg 16, fReg 17, fReg 18]
argRegs 4 = freeMappedRegs [16, 17, 18, 19, fReg 16, fReg 17, fReg 18, fReg 19]
argRegs 5 = freeMappedRegs [16, 17, 18, 19, 20, fReg 16, fReg 17, fReg 18, fReg 19, fReg 20]
argRegs 6 = freeMappedRegs [16, 17, 18, 19, 20, 21, fReg 16, fReg 17, fReg 18, fReg 19, fReg 20, fReg 21]
#endif {- alpha_TARGET_ARCH -}
#if sparc_TARGET_ARCH
argRegs 1 = freeMappedRegs (map oReg [0])
argRegs 2 = freeMappedRegs (map oReg [0,1])
argRegs 3 = freeMappedRegs (map oReg [0,1,2])
argRegs 4 = freeMappedRegs (map oReg [0,1,2,3])
argRegs 5 = freeMappedRegs (map oReg [0,1,2,3,4])
argRegs 6 = freeMappedRegs (map oReg [0,1,2,3,4,5])
#endif {- sparc_TARGET_ARCH -}
argRegs _ = panic "MachRegs.argRegs: don't know about >6 arguments!"
#endif {- i386_TARGET_ARCH -}

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
freeReg ILIT(31) = _FALSE_  -- always zero (zeroh)
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
#ifdef REG_F1
freeReg ILIT(REG_F1) = _FALSE_
#endif
#ifdef REG_F2
freeReg ILIT(REG_F2) = _FALSE_
#endif
#ifdef REG_F3
freeReg ILIT(REG_F3) = _FALSE_
#endif
#ifdef REG_F4
freeReg ILIT(REG_F4) = _FALSE_
#endif
#ifdef REG_D1
freeReg ILIT(REG_D1) = _FALSE_
#endif
#ifdef REG_D2
freeReg ILIT(REG_D2) = _FALSE_
#endif
#ifdef REG_Sp 
freeReg ILIT(REG_Sp)   = _FALSE_
#endif 
#ifdef REG_Su
freeReg ILIT(REG_Su)   = _FALSE_
#endif 
#ifdef REG_SpLim 
freeReg ILIT(REG_SpLim) = _FALSE_
#endif 
#ifdef REG_Hp 
freeReg ILIT(REG_Hp)   = _FALSE_
#endif
#ifdef REG_HpLim
freeReg ILIT(REG_HpLim) = _FALSE_
#endif
freeReg n
  -- we hang onto two double regs for dedicated
  -- use; this is not necessary on Alphas and
  -- may not be on other non-SPARCs.
#ifdef REG_D1
  | n _EQ_ (ILIT(REG_D1) _ADD_ ILIT(1)) = _FALSE_
#endif
#ifdef REG_D2
  | n _EQ_ (ILIT(REG_D2) _ADD_ ILIT(1)) = _FALSE_
#endif
  | otherwise = _TRUE_
\end{code}
