%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[RegAllocInfo]{Machine-specific info used for register allocation}

The (machine-independent) allocator itself is in @AsmRegAlloc@.

\begin{code}
#include "nativeGen/NCG.h"

module RegAllocInfo (
	MRegsState(..),
	mkMRegsState,
	freeMReg,
	freeMRegs,
	possibleMRegs,
	useMReg,
	useMRegs,

	RegUsage(..),
	noUsage,
	endUsage,
	regUsage,

	FutureLive(..),
	RegAssignment,
	RegConflicts,
	RegFuture(..),
	RegHistory(..),
	RegInfo(..),
	RegLiveness(..),

	fstFL,
	loadReg,
	patchRegs,
	regLiveness,
	spillReg,
	IF_ARCH_i386(findReservedRegs COMMA,)

	RegSet,
	elementOfRegSet,
	emptyRegSet,
	isEmptyRegSet,
	minusRegSet,
	mkRegSet,
	regSetToList,
	unionRegSets,

	argRegSet,
	callClobberedRegSet,
	freeRegSet
    ) where

#include "HsVersions.h"

import List		( partition )
import MachMisc
import MachRegs
import MachCode		( InstrList )

import BitSet		( unitBS, mkBS, minusBS, unionBS, listBS, BitSet )
import CLabel		( pprCLabel_asm, CLabel{-instance Ord-} )
import FiniteMap	( addToFM, lookupFM, FiniteMap )
import OrdList		( mkUnitList )
import PrimRep		( PrimRep(..) )
import UniqSet		-- quite a bit of it
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{Register allocation information}
%*									*
%************************************************************************

\begin{code}
type RegSet = UniqSet Reg

mkRegSet :: [Reg] -> RegSet
emptyRegSet :: RegSet
unionRegSets, minusRegSet :: RegSet -> RegSet -> RegSet
elementOfRegSet :: Reg -> RegSet -> Bool
isEmptyRegSet :: RegSet -> Bool
regSetToList :: RegSet -> [Reg]

mkRegSet	= mkUniqSet
emptyRegSet	= emptyUniqSet
unionRegSets	= unionUniqSets
minusRegSet	= minusUniqSet
elementOfRegSet	= elementOfUniqSet
isEmptyRegSet	= isEmptyUniqSet
regSetToList	= uniqSetToList

freeRegSet, callClobberedRegSet :: RegSet
argRegSet :: Int -> RegSet

freeRegSet	    = mkRegSet freeRegs
callClobberedRegSet = mkRegSet callClobberedRegs
argRegSet n	    = mkRegSet (argRegs n)

type RegAssignment = FiniteMap Reg Reg
type RegConflicts  = FiniteMap Int RegSet

data FutureLive = FL RegSet (FiniteMap CLabel RegSet)

fstFL (FL a b)  = a

data RegHistory a
  = RH	a
	Int
	RegAssignment

data RegFuture
  = RF	RegSet		-- in use
	FutureLive	-- future
	RegConflicts

data RegInfo a
  = RI	RegSet		-- in use
	RegSet		-- sources
	RegSet		-- destinations
	[Reg]		-- last used
	RegConflicts
\end{code}

%************************************************************************
%*									*
\subsection{Register allocation information}
%*									*
%************************************************************************

COMMENT ON THE EXTRA BitSet FOR SPARC MRegsState: Getting the conflicts
right is a bit tedious for doubles.  We'd have to add a conflict
function to the MachineRegisters class, and we'd have to put a PrimRep
in the MappedReg datatype, or use some kludge (e.g. register 64 + n is
really the same as 32 + n, except that it's used for a double, so it
also conflicts with 33 + n) to deal with it.  It's just not worth the
bother, so we just partition the free floating point registers into
two sets: one for single precision and one for double precision.  We
never seem to run out of floating point registers anyway.

\begin{code}
data MRegsState
  = MRs	BitSet	-- integer registers
	BitSet	-- floating-point registers
	IF_ARCH_sparc(BitSet,) -- double registers handled separately
\end{code}

\begin{code}
#if alpha_TARGET_ARCH
# define INT_FLPT_CUTOFF 32
#endif
#if i386_TARGET_ARCH
# define INT_FLPT_CUTOFF 8
#endif
#if sparc_TARGET_ARCH
# define INT_FLPT_CUTOFF 32
# define SNGL_DBL_CUTOFF 48
#endif

mkMRegsState	:: [RegNo] -> MRegsState
possibleMRegs   :: PrimRep -> MRegsState -> [RegNo]
useMReg		:: MRegsState -> FAST_REG_NO -> MRegsState
useMRegs	:: MRegsState -> [RegNo]     -> MRegsState
freeMReg	:: MRegsState -> FAST_REG_NO -> MRegsState
freeMRegs	:: MRegsState -> [RegNo]     -> MRegsState

mkMRegsState xs
  = MRs (mkBS is) (mkBS fs2) IF_ARCH_sparc((mkBS ds2),)
  where
    (is, fs) = partition (< INT_FLPT_CUTOFF) xs
#if sparc_TARGET_ARCH
    (ss, ds) = partition (< SNGL_DBL_CUTOFF) fs
    fs2	 = map (subtract INT_FLPT_CUTOFF) ss
    ds2	 = map (subtract INT_FLPT_CUTOFF) (filter even ds)
#else
    fs2      = map (subtract INT_FLPT_CUTOFF) fs
#endif

------------------------------------------------
#if sparc_TARGET_ARCH
possibleMRegs FloatRep  (MRs _ ss _) = [ x + INT_FLPT_CUTOFF | x <- listBS ss]
possibleMRegs DoubleRep (MRs _ _ ds) = [ x + INT_FLPT_CUTOFF | x <- listBS ds]
possibleMRegs _         (MRs is _ _) = listBS is
#else
possibleMRegs FloatRep  (MRs _ fs) = [ x + INT_FLPT_CUTOFF | x <- listBS fs]
possibleMRegs DoubleRep (MRs _ fs) = [ x + INT_FLPT_CUTOFF | x <- listBS fs]
possibleMRegs _	    (MRs is _) = listBS is
#endif

------------------------------------------------
#if sparc_TARGET_ARCH
useMReg (MRs is ss ds) n
  = if (n _LT_ ILIT(INT_FLPT_CUTOFF)) then
	MRs (is `minusBS` unitBS IBOX(n)) ss ds
    else if (n _LT_ ILIT(SNGL_DBL_CUTOFF)) then
	MRs is (ss `minusBS` unitBS (IBOX(n _SUB_ ILIT(INT_FLPT_CUTOFF)))) ds
    else
	MRs is ss (ds `minusBS` unitBS (IBOX(n _SUB_ ILIT(INT_FLPT_CUTOFF))))
#else
useMReg (MRs is fs) n
  = if (n _LT_ ILIT(INT_FLPT_CUTOFF))
    then MRs (is `minusBS` unitBS IBOX(n)) fs
    else MRs is (fs `minusBS` unitBS (IBOX(n _SUB_ ILIT(INT_FLPT_CUTOFF))))
#endif

------------------------------------------------
#if sparc_TARGET_ARCH
useMRegs (MRs is ss ds) xs
  = MRs (is `minusBS` is2) (ss `minusBS` ss2) (ds `minusBS` ds2)
  where
    MRs is2 ss2 ds2 = mkMRegsState xs
#else
useMRegs (MRs is fs) xs
  = MRs (is `minusBS` is2) (fs `minusBS` fs2)
  where
    MRs is2 fs2 = mkMRegsState xs
#endif

------------------------------------------------
#if sparc_TARGET_ARCH
freeMReg (MRs is ss ds) n
  = if (n _LT_ ILIT(INT_FLPT_CUTOFF)) then
	MRs (is `unionBS` unitBS IBOX(n)) ss ds
    else if (n _LT_ ILIT(SNGL_DBL_CUTOFF)) then
	MRs is (ss `unionBS` unitBS (IBOX(n _SUB_ ILIT(INT_FLPT_CUTOFF)))) ds
    else
	MRs is ss (ds `unionBS` unitBS (IBOX(n _SUB_ ILIT(INT_FLPT_CUTOFF))))
#else
freeMReg (MRs is fs) n
  = if (n _LT_ ILIT(INT_FLPT_CUTOFF))
    then MRs (is `unionBS` unitBS IBOX(n)) fs
    else MRs is (fs `unionBS` unitBS (IBOX(n _SUB_ ILIT(INT_FLPT_CUTOFF))))
#endif

------------------------------------------------
#if sparc_TARGET_ARCH
freeMRegs (MRs is ss ds) xs
  = MRs (is `unionBS` is2) (ss `unionBS` ss2) (ds `unionBS` ds2)
  where
    MRs is2 ss2 ds2 = mkMRegsState xs
#else
freeMRegs (MRs is fs) xs
  = MRs (is `unionBS` is2) (fs `unionBS` fs2)
  where
    MRs is2 fs2 = mkMRegsState xs
#endif
\end{code}

%************************************************************************
%*									*
\subsection{@RegUsage@ type; @noUsage@, @endUsage@, @regUsage@ functions}
%*									*
%************************************************************************

@regUsage@ returns the sets of src and destination registers used by a
particular instruction.  Machine registers that are pre-allocated to
stgRegs are filtered out, because they are uninteresting from a
register allocation standpoint.  (We wouldn't want them to end up on
the free list!)

An important point: The @regUsage@ function for a particular
assembly language must not refer to fixed registers, such as Hp, SpA,
etc.  The source and destination MRegsStates should only refer to
dynamically allocated registers or static registers from the free
list.  As far as we are concerned, the fixed registers simply don't
exist (for allocation purposes, anyway).

\begin{code}
data RegUsage = RU RegSet RegSet

noUsage, endUsage :: RegUsage
noUsage  = RU emptyRegSet emptyRegSet
endUsage = RU emptyRegSet freeRegSet

regUsage :: Instr -> RegUsage

#if alpha_TARGET_ARCH

regUsage instr = case instr of
    LD B reg addr	-> usage (regAddr addr, [reg, t9])
    LD BU reg addr	-> usage (regAddr addr, [reg, t9])
--  LD W reg addr	-> usage (regAddr addr, [reg, t9]) : UNUSED
--  LD WU reg addr	-> usage (regAddr addr, [reg, t9]) : UNUSED
    LD sz reg addr	-> usage (regAddr addr, [reg])
    LDA reg addr	-> usage (regAddr addr, [reg])
    LDAH reg addr	-> usage (regAddr addr, [reg])
    LDGP reg addr	-> usage (regAddr addr, [reg])
    LDI sz reg imm	-> usage ([], [reg])
    ST B reg addr	-> usage (reg : regAddr addr, [t9, t10])
--  ST W reg addr	-> usage (reg : regAddr addr, [t9, t10]) : UNUSED
    ST sz reg addr	-> usage (reg : regAddr addr, [])
    CLR reg		-> usage ([], [reg])
    ABS sz ri reg	-> usage (regRI ri, [reg])
    NEG sz ov ri reg	-> usage (regRI ri, [reg])
    ADD sz ov r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SADD sz sc r1 ar r2 -> usage (r1 : regRI ar, [r2])
    SUB sz ov r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SSUB sz sc r1 ar r2 -> usage (r1 : regRI ar, [r2])
    MUL sz ov r1 ar r2	-> usage (r1 : regRI ar, [r2])
    DIV sz un r1 ar r2	-> usage (r1 : regRI ar, [r2, t9, t10, t11, t12])
    REM sz un r1 ar r2	-> usage (r1 : regRI ar, [r2, t9, t10, t11, t12])
    NOT ri reg		-> usage (regRI ri, [reg])
    AND r1 ar r2	-> usage (r1 : regRI ar, [r2])
    ANDNOT r1 ar r2	-> usage (r1 : regRI ar, [r2])
    OR r1 ar r2		-> usage (r1 : regRI ar, [r2])
    ORNOT r1 ar r2	-> usage (r1 : regRI ar, [r2])
    XOR r1 ar r2	-> usage (r1 : regRI ar, [r2])
    XORNOT r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SLL r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SRL r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SRA r1 ar r2	-> usage (r1 : regRI ar, [r2])
    ZAP r1 ar r2	-> usage (r1 : regRI ar, [r2])
    ZAPNOT r1 ar r2	-> usage (r1 : regRI ar, [r2])
    CMP co r1 ar r2	-> usage (r1 : regRI ar, [r2])
    FCLR reg		-> usage ([], [reg])
    FABS r1 r2		-> usage ([r1], [r2])
    FNEG sz r1 r2	-> usage ([r1], [r2])
    FADD sz r1 r2 r3	-> usage ([r1, r2], [r3])
    FDIV sz r1 r2 r3	-> usage ([r1, r2], [r3])
    FMUL sz r1 r2 r3	-> usage ([r1, r2], [r3])
    FSUB sz r1 r2 r3	-> usage ([r1, r2], [r3])
    CVTxy sz1 sz2 r1 r2 -> usage ([r1], [r2])
    FCMP sz co r1 r2 r3 -> usage ([r1, r2], [r3])
    FMOV r1 r2		-> usage ([r1], [r2])


    -- We assume that all local jumps will be BI/BF/BR.	 JMP must be out-of-line.
    BI cond reg lbl	-> usage ([reg], [])
    BF cond reg lbl	-> usage ([reg], [])
    JMP reg addr hint	-> RU (mkRegSet (filter interesting (regAddr addr))) freeRegSet

    BSR _ n		-> RU (argRegSet n) callClobberedRegSet
    JSR reg addr n	-> RU (argRegSet n) callClobberedRegSet

    _			-> noUsage

  where
    usage (src, dst) = RU (mkRegSet (filter interesting src))
			  (mkRegSet (filter interesting dst))

    interesting (FixedReg _) = False
    interesting _ = True

    regAddr (AddrReg r1)      = [r1]
    regAddr (AddrRegImm r1 _) = [r1]
    regAddr (AddrImm _)	      = []

    regRI (RIReg r) = [r]
    regRI  _	= []

#endif {- alpha_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

regUsage instr = case instr of
    MOV  sz src dst	-> usage2  src dst
    MOVZxL sz src dst	-> usage2  src dst
    MOVSxL sz src dst	-> usage2  src dst
    LEA  sz src dst	-> usage2  src dst
    ADD  sz src dst	-> usage2s src dst
    SUB  sz src dst	-> usage2s src dst
    IMUL sz src dst	-> usage2s src dst
    IDIV sz src		-> usage (eax:edx:opToReg src) [eax,edx]
    AND  sz src dst	-> usage2s src dst
    OR   sz src dst	-> usage2s src dst
    XOR  sz src dst	-> usage2s src dst
    NOT  sz op		-> usage1 op
    NEGI sz op		-> usage1 op
    SHL  sz len dst	-> usage2s len dst -- len is either an Imm or ecx.
    SAR  sz len dst	-> usage2s len dst -- len is either an Imm or ecx.
    SHR  sz len dst	-> usage2s len dst -- len is either an Imm or ecx.
    BT   sz imm src	-> usage (opToReg src) []

    PUSH sz op		-> usage (opToReg op) []
    POP  sz op		-> usage [] (opToReg op)
    TEST sz src dst	-> usage (opToReg src ++ opToReg dst) []
    CMP  sz src dst	-> usage (opToReg src ++ opToReg dst) []
    SETCC cond op	-> usage [] (opToReg op)
    JXX cond lbl	-> usage [] []
    JMP op		-> usage (opToReg op) freeRegs
    CALL imm		-> usage [] callClobberedRegs
    CLTD		-> usage [eax] [edx]
    NOP			-> usage [] []

    GMOV src dst	-> usage [src] [dst]
    GLD sz src dst	-> usage (addrToRegs src) [dst]
    GST sz src dst	-> usage [src] (addrToRegs dst)

    GFTOD src dst	-> usage [src] [dst]
    GFTOI src dst	-> usage [src] [dst]

    GDTOF src dst	-> usage [src] [dst]
    GDTOI src dst	-> usage [src] [dst]

    GITOF src dst	-> usage [src] [dst]
    GITOD src dst	-> usage [src] [dst]

    GADD sz s1 s2 dst	-> usage [s1,s2] [dst]
    GSUB sz s1 s2 dst	-> usage [s1,s2] [dst]
    GMUL sz s1 s2 dst	-> usage [s1,s2] [dst]
    GDIV sz s1 s2 dst	-> usage [s1,s2] [dst]

    GCMP sz src1 src2	-> usage [src1,src2] []
    GABS sz src dst	-> usage [src] [dst]
    GNEG sz src dst	-> usage [src] [dst]
    GSQRT sz src dst	-> usage [src] [dst]
    GSIN sz src dst	-> usage [src] [dst]
    GCOS sz src dst	-> usage [src] [dst]
    GTAN sz src dst	-> usage [src] [dst]

    COMMENT _		-> noUsage
    SEGMENT _ 		-> noUsage
    LABEL _		-> noUsage
    ASCII _ _		-> noUsage
    DATA _ _		-> noUsage
    _			-> pprPanic "regUsage(x86) " empty

 where
    -- 2 operand form in which the second operand is purely a destination
    usage2 :: Operand -> Operand -> RegUsage
    usage2 op (OpReg reg) = usage (opToReg op) [reg]
    usage2 op (OpAddr ea) = usage (opToReg op ++ addrToRegs ea) []
    usage2 op (OpImm imm) = usage (opToReg op) []

    -- 2 operand form in which the second operand is also an input
    usage2s :: Operand -> Operand -> RegUsage
    usage2s op (OpReg reg) = usage (opToReg op ++ [reg]) [reg]
    usage2s op (OpAddr ea) = usage (opToReg op ++ addrToRegs ea) []
    usage2s op (OpImm imm) = usage (opToReg op) []

    -- 1 operand form in which the operand is both used and written
    usage1 :: Operand -> RegUsage
    usage1 (OpReg reg)    = usage [reg] [reg]
    usage1 (OpAddr ea)    = usage (addrToRegs ea) []

    allFPRegs = [fake0,fake1,fake2,fake3,fake4,fake5]

    --callClobberedRegs = [ eax, ecx, edx ] -- according to gcc, anyway.
    callClobberedRegs = [eax,fake0,fake1,fake2,fake3,fake4,fake5]

-- General purpose register collecting functions.

    opToReg (OpReg reg)   = [reg]
    opToReg (OpImm imm)   = []
    opToReg (OpAddr  ea)  = addrToRegs ea

    addrToRegs (AddrBaseIndex base index _) = baseToReg base ++ indexToReg index
      where  baseToReg Nothing       = []
	     baseToReg (Just r)      = [r]
	     indexToReg Nothing      = []
	     indexToReg (Just (r,_)) = [r]
    addrToRegs (ImmAddr _ _) = []

    usage src dst = RU (mkRegSet (filter interesting src))
    	    	       (mkRegSet (filter interesting dst))

    interesting (FixedReg _) = False
    interesting _ = True


-- Allow the spiller to decide whether or not it can use 
-- %eax and %edx as spill temporaries.
hasFixedEAXorEDX instr = case instr of
    IDIV _ _ -> True
    CLTD     -> True
    other    -> False

#endif {- i386_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

regUsage instr = case instr of
    LD sz addr reg  	-> usage (regAddr addr, [reg])
    ST sz reg addr  	-> usage (reg : regAddr addr, [])
    ADD x cc r1 ar r2 	-> usage (r1 : regRI ar, [r2])
    SUB x cc r1 ar r2 	-> usage (r1 : regRI ar, [r2])
    AND b r1 ar r2  	-> usage (r1 : regRI ar, [r2])
    ANDN b r1 ar r2 	-> usage (r1 : regRI ar, [r2])
    OR b r1 ar r2   	-> usage (r1 : regRI ar, [r2])
    ORN b r1 ar r2  	-> usage (r1 : regRI ar, [r2])
    XOR b r1 ar r2  	-> usage (r1 : regRI ar, [r2])
    XNOR b r1 ar r2 	-> usage (r1 : regRI ar, [r2])
    SLL r1 ar r2    	-> usage (r1 : regRI ar, [r2])
    SRL r1 ar r2    	-> usage (r1 : regRI ar, [r2])
    SRA r1 ar r2    	-> usage (r1 : regRI ar, [r2])
    SETHI imm reg   	-> usage ([], [reg])
    FABS s r1 r2    	-> usage ([r1], [r2])
    FADD s r1 r2 r3 	-> usage ([r1, r2], [r3])
    FCMP e s r1 r2  	-> usage ([r1, r2], [])
    FDIV s r1 r2 r3 	-> usage ([r1, r2], [r3])
    FMOV s r1 r2    	-> usage ([r1], [r2])
    FMUL s r1 r2 r3 	-> usage ([r1, r2], [r3])
    FNEG s r1 r2    	-> usage ([r1], [r2])
    FSQRT s r1 r2   	-> usage ([r1], [r2])
    FSUB s r1 r2 r3 	-> usage ([r1, r2], [r3])
    FxTOy s1 s2 r1 r2 	-> usage ([r1], [r2])

    -- We assume that all local jumps will be BI/BF.  JMP must be out-of-line.
    JMP addr 	    	-> RU (mkRegSet (filter interesting (regAddr addr))) freeRegSet

    CALL _ n True   	-> endUsage
    CALL _ n False  	-> RU (argRegSet n) callClobberedRegSet

    _ 	    	    	-> noUsage
  where
    usage (src, dst) = RU (mkRegSet (filter interesting src))
    	    	    	  (mkRegSet (filter interesting dst))

    interesting (FixedReg _) = False
    interesting _ = True

    regAddr (AddrRegReg r1 r2) = [r1, r2]
    regAddr (AddrRegImm r1 _)  = [r1]

    regRI (RIReg r) = [r]
    regRI  _	= []

#endif {- sparc_TARGET_ARCH -}
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

findReservedRegs tells us which regs can be used as spill temporaries.
The list of instructions for which we are attempting allocation is
supplied.  This is so that we can (at least for x86) examine it to
discover which registers are being used in a fixed way -- for example,
%eax and %edx are used by integer division, so they can't be used as
spill temporaries.  However, most instruction lists don't do integer
division, so we don't want to rule them out altogether.

findReservedRegs returns not a list of spill temporaries, but a list
of list of them.  This is so that the allocator can attempt allocating
with at first no spill temps, then if that fails, increasing numbers.
For x86 it is important that we minimise the number of regs reserved
as spill temporaries, since there are so few.  For Alpha and Sparc
this isn't a concern; we just ignore the supplied code list and return
a singleton list which we know will satisfy all spill demands.

\begin{code}
findReservedRegs :: [Instr] -> [[RegNo]]
findReservedRegs instrs
#if alpha_TARGET_ARCH
  = [[NCG_Reserved_I1, NCG_Reserved_I2,
      NCG_Reserved_F1, NCG_Reserved_F2]]
#endif
#if sparc_TARGET_ARCH
  = [[NCG_Reserved_I1, NCG_Reserved_I2,
      NCG_Reserved_F1, NCG_Reserved_F2,
      NCG_Reserved_D1, NCG_Reserved_D2]]
#endif
#if i386_TARGET_ARCH
    -- Sigh.  This is where it gets complicated.
  = -- first of all, try without any at all.
    map (map mappedRegNo) (
    [ [],
    -- if that doesn't work, try one integer reg (which might fail)
    -- and two float regs (which will always fix any float insns)
      [ecx, fake4,fake5]
    ]
    -- dire straits (but still correct): see if we can bag %eax and %edx
    ++ if   any hasFixedEAXorEDX instrs
       then []  -- bummer
       else [ [ecx,edx,fake4,fake5],
              [ecx,edx,eax,fake4,fake5] ]
    )
#endif
\end{code}

%************************************************************************
%*									*
\subsection{@RegLiveness@ type; @regLiveness@ function}
%*									*
%************************************************************************

@regLiveness@ takes future liveness information and modifies it
according to the semantics of branches and labels.  (An out-of-line
branch clobbers the liveness passed back by the following instruction;
a forward local branch passes back the liveness from the target label;
a conditional branch merges the liveness from the target and the
liveness from its successor; a label stashes away the current liveness
in the future liveness environment).

\begin{code}
data RegLiveness = RL RegSet FutureLive

regLiveness :: Instr -> RegLiveness -> RegLiveness

regLiveness instr info@(RL live future@(FL all env))
  = let
	lookup lbl
	  = case (lookupFM env lbl) of
	    Just rs -> rs
	    Nothing -> pprTrace "Missing" (pprCLabel_asm lbl <+> text "in future?") 
		       emptyRegSet
    in
    case instr of -- the rest is machine-specific...

#if alpha_TARGET_ARCH

    -- We assume that all local jumps will be BI/BF.  JMP must be out-of-line.

    BR (ImmCLbl lbl)	 -> RL (lookup lbl) future
    BI _ _ (ImmCLbl lbl) -> RL (lookup lbl `unionRegSets` live) future
    BF _ _ (ImmCLbl lbl) -> RL (lookup lbl `unionRegSets` live) future
    JMP _ _ _		 -> RL emptyRegSet future
    BSR _ _		 -> RL live future
    JSR _ _ _		 -> RL live future
    LABEL lbl		 -> RL live (FL (all `unionRegSets` live) (addToFM env lbl live))
    _			 -> info

#endif {- alpha_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

    JXX _ lbl	-> RL (lookup lbl `unionRegSets` live) future
    JMP _	-> RL emptyRegSet future
    CALL _      -> RL live future
    LABEL lbl   -> RL live (FL (all `unionRegSets` live) (addToFM env lbl live))
    _		    -> info

#endif {- i386_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

    -- We assume that all local jumps will be BI/BF.  JMP must be out-of-line.

    BI ALWAYS _ (ImmCLbl lbl)	-> RL (lookup lbl) future
    BI _ _ (ImmCLbl lbl)	-> RL (lookup lbl `unionRegSets` live) future
    BF ALWAYS _ (ImmCLbl lbl)	-> RL (lookup lbl) future
    BF _ _ (ImmCLbl lbl)	-> RL (lookup lbl `unionRegSets` live) future
    JMP _			-> RL emptyRegSet future
    CALL _ i True   -> RL emptyRegSet future
    CALL _ i False  -> RL live future
    LABEL lbl	    -> RL live (FL (all `unionRegSets` live) (addToFM env lbl live))
    _		    -> info

#endif {- sparc_TARGET_ARCH -}
\end{code}

%************************************************************************
%*									*
\subsection{@patchRegs@ function}
%*									*
%************************************************************************

@patchRegs@ takes an instruction (possibly with
MemoryReg/UnmappedReg registers) and changes all register references
according to the supplied environment.

\begin{code}
patchRegs :: Instr -> (Reg -> Reg) -> Instr

#if alpha_TARGET_ARCH

patchRegs instr env = case instr of
    LD sz reg addr -> LD sz (env reg) (fixAddr addr)
    LDA reg addr -> LDA (env reg) (fixAddr addr)
    LDAH reg addr -> LDAH (env reg) (fixAddr addr)
    LDGP reg addr -> LDGP (env reg) (fixAddr addr)
    LDI sz reg imm -> LDI sz (env reg) imm
    ST sz reg addr -> ST sz (env reg) (fixAddr addr)
    CLR reg -> CLR (env reg)
    ABS sz ar reg -> ABS sz (fixRI ar) (env reg)
    NEG sz ov ar reg -> NEG sz ov (fixRI ar) (env reg)
    ADD sz ov r1 ar r2 -> ADD sz ov (env r1) (fixRI ar) (env r2)
    SADD sz sc r1 ar r2 -> SADD sz sc (env r1) (fixRI ar) (env r2)
    SUB sz ov r1 ar r2 -> SUB sz ov (env r1) (fixRI ar) (env r2)
    SSUB sz sc r1 ar r2 -> SSUB sz sc (env r1) (fixRI ar) (env r2)
    MUL sz ov r1 ar r2 -> MUL sz ov (env r1) (fixRI ar) (env r2)
    DIV sz un r1 ar r2 -> DIV sz un (env r1) (fixRI ar) (env r2)
    REM sz un r1 ar r2 -> REM sz un (env r1) (fixRI ar) (env r2)
    NOT ar reg -> NOT (fixRI ar) (env reg)
    AND r1 ar r2 -> AND (env r1) (fixRI ar) (env r2)
    ANDNOT r1 ar r2 -> ANDNOT (env r1) (fixRI ar) (env r2)
    OR r1 ar r2 -> OR (env r1) (fixRI ar) (env r2)
    ORNOT r1 ar r2 -> ORNOT (env r1) (fixRI ar) (env r2)
    XOR r1 ar r2 -> XOR (env r1) (fixRI ar) (env r2)
    XORNOT r1 ar r2 -> XORNOT (env r1) (fixRI ar) (env r2)
    SLL r1 ar r2 -> SLL (env r1) (fixRI ar) (env r2)
    SRL r1 ar r2 -> SRL (env r1) (fixRI ar) (env r2)
    SRA r1 ar r2 -> SRA (env r1) (fixRI ar) (env r2)
    ZAP r1 ar r2 -> ZAP (env r1) (fixRI ar) (env r2)
    ZAPNOT r1 ar r2 -> ZAPNOT (env r1) (fixRI ar) (env r2)
    CMP co r1 ar r2 -> CMP co (env r1) (fixRI ar) (env r2)
    FCLR reg -> FCLR (env reg)
    FABS r1 r2 -> FABS (env r1) (env r2)
    FNEG s r1 r2 -> FNEG s (env r1) (env r2)
    FADD s r1 r2 r3 -> FADD s (env r1) (env r2) (env r3)
    FDIV s r1 r2 r3 -> FDIV s (env r1) (env r2) (env r3)
    FMUL s r1 r2 r3 -> FMUL s (env r1) (env r2) (env r3)
    FSUB s r1 r2 r3 -> FSUB s (env r1) (env r2) (env r3)
    CVTxy s1 s2 r1 r2 -> CVTxy s1 s2 (env r1) (env r2)
    FCMP s co r1 r2 r3 -> FCMP s co (env r1) (env r2) (env r3)
    FMOV r1 r2 -> FMOV (env r1) (env r2)
    BI cond reg lbl -> BI cond (env reg) lbl
    BF cond reg lbl -> BF cond (env reg) lbl
    JMP reg addr hint -> JMP (env reg) (fixAddr addr) hint
    JSR reg addr i -> JSR (env reg) (fixAddr addr) i
    _ -> instr
  where
    fixAddr (AddrReg r1)       = AddrReg (env r1)
    fixAddr (AddrRegImm r1 i)  = AddrRegImm (env r1) i
    fixAddr other	       = other

    fixRI (RIReg r) = RIReg (env r)
    fixRI other	= other

#endif {- alpha_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

patchRegs instr env = case instr of
    MOV  sz src dst	-> patch2 (MOV  sz) src dst
    MOVZxL sz src dst	-> patch2 (MOVZxL sz) src dst
    MOVSxL sz src dst	-> patch2 (MOVSxL sz) src dst
    LEA  sz src dst	-> patch2 (LEA  sz) src dst
    ADD  sz src dst	-> patch2 (ADD  sz) src dst
    SUB  sz src dst	-> patch2 (SUB  sz) src dst
    IMUL sz src dst 	-> patch2 (IMUL sz) src dst
    IDIV sz src  	-> patch1 (IDIV sz) src
    AND  sz src dst	-> patch2 (AND  sz) src dst
    OR   sz src dst	-> patch2 (OR   sz) src dst
    XOR  sz src dst	-> patch2 (XOR  sz) src dst
    NOT  sz op 		-> patch1 (NOT  sz) op
    NEGI sz op		-> patch1 (NEGI sz) op
    SHL  sz imm dst 	-> patch2 (SHL  sz) imm dst
    SAR  sz imm dst 	-> patch2 (SAR  sz) imm dst
    SHR  sz imm dst 	-> patch2 (SHR  sz) imm dst
    BT   sz imm src     -> patch1 (BT sz imm) src
    TEST sz src dst	-> patch2 (TEST sz) src dst
    CMP  sz src dst	-> patch2 (CMP  sz) src dst
    PUSH sz op		-> patch1 (PUSH sz) op
    POP  sz op		-> patch1 (POP  sz) op
    SETCC cond op	-> patch1 (SETCC cond) op
    JMP op		-> patch1 JMP op

    GMOV src dst	-> GMOV (env src) (env dst)
    GLD sz src dst	-> GLD sz (lookupAddr src) (env dst)
    GST sz src dst	-> GST sz (env src) (lookupAddr dst)

    GFTOD src dst	-> GFTOD (env src) (env dst)
    GFTOI src dst	-> GFTOI (env src) (env dst)

    GDTOF src dst	-> GDTOF (env src) (env dst)
    GDTOI src dst	-> GDTOI (env src) (env dst)

    GITOF src dst	-> GITOF (env src) (env dst)
    GITOD src dst	-> GITOD (env src) (env dst)

    GADD sz s1 s2 dst	-> GADD sz (env s1) (env s2) (env dst)
    GSUB sz s1 s2 dst	-> GSUB sz (env s1) (env s2) (env dst)
    GMUL sz s1 s2 dst	-> GMUL sz (env s1) (env s2) (env dst)
    GDIV sz s1 s2 dst	-> GDIV sz (env s1) (env s2) (env dst)

    GCMP sz src1 src2	-> GCMP sz (env src1) (env src2)
    GABS sz src dst	-> GABS sz (env src) (env dst)
    GNEG sz src dst	-> GNEG sz (env src) (env dst)
    GSQRT sz src dst	-> GSQRT sz (env src) (env dst)
    GSIN sz src dst	-> GSIN sz (env src) (env dst)
    GCOS sz src dst	-> GCOS sz (env src) (env dst)
    GTAN sz src dst	-> GTAN sz (env src) (env dst)

    COMMENT _		-> instr
    SEGMENT _ 		-> instr
    LABEL _		-> instr
    ASCII _ _		-> instr
    DATA _ _		-> instr
    JXX _ _		-> instr
    CALL _		-> instr
    CLTD		-> instr
    _			-> pprPanic "patchInstr(x86)" empty

  where
    patch1 insn op      = insn (patchOp op)
    patch2 insn src dst = insn (patchOp src) (patchOp dst)

    patchOp (OpReg  reg) = OpReg (env reg)
    patchOp (OpImm  imm) = OpImm imm
    patchOp (OpAddr ea)  = OpAddr (lookupAddr ea)

    lookupAddr (ImmAddr imm off) = ImmAddr imm off
    lookupAddr (AddrBaseIndex base index disp)
      = AddrBaseIndex (lookupBase base) (lookupIndex index) disp
      where
	lookupBase Nothing       = Nothing
	lookupBase (Just r)      = Just (env r)
				 
	lookupIndex Nothing      = Nothing
	lookupIndex (Just (r,i)) = Just (env r, i)

#endif {- i386_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

patchRegs instr env = case instr of
    LD sz addr reg -> LD sz (fixAddr addr) (env reg)
    ST sz reg addr -> ST sz (env reg) (fixAddr addr)
    ADD x cc r1 ar r2 -> ADD x cc (env r1) (fixRI ar) (env r2)
    SUB x cc r1 ar r2 -> SUB x cc (env r1) (fixRI ar) (env r2)
    AND b r1 ar r2 -> AND b (env r1) (fixRI ar) (env r2)
    ANDN b r1 ar r2 -> ANDN b (env r1) (fixRI ar) (env r2)
    OR b r1 ar r2 -> OR b (env r1) (fixRI ar) (env r2)
    ORN b r1 ar r2 -> ORN b (env r1) (fixRI ar) (env r2)
    XOR b r1 ar r2 -> XOR b (env r1) (fixRI ar) (env r2)
    XNOR b r1 ar r2 -> XNOR b (env r1) (fixRI ar) (env r2)
    SLL r1 ar r2 -> SLL (env r1) (fixRI ar) (env r2)
    SRL r1 ar r2 -> SRL (env r1) (fixRI ar) (env r2)
    SRA r1 ar r2 -> SRA (env r1) (fixRI ar) (env r2)
    SETHI imm reg -> SETHI imm (env reg)
    FABS s r1 r2 -> FABS s (env r1) (env r2)
    FADD s r1 r2 r3 -> FADD s (env r1) (env r2) (env r3)
    FCMP e s r1 r2 -> FCMP e s (env r1) (env r2)
    FDIV s r1 r2 r3 -> FDIV s (env r1) (env r2) (env r3)
    FMOV s r1 r2 -> FMOV s (env r1) (env r2)
    FMUL s r1 r2 r3 -> FMUL s (env r1) (env r2) (env r3)
    FNEG s r1 r2 -> FNEG s (env r1) (env r2)
    FSQRT s r1 r2 -> FSQRT s (env r1) (env r2)
    FSUB s r1 r2 r3 -> FSUB s (env r1) (env r2) (env r3)
    FxTOy s1 s2 r1 r2 -> FxTOy s1 s2 (env r1) (env r2)
    JMP addr -> JMP (fixAddr addr)
    _ -> instr
  where
    fixAddr (AddrRegReg r1 r2) = AddrRegReg (env r1) (env r2)
    fixAddr (AddrRegImm r1 i)  = AddrRegImm (env r1) i

    fixRI (RIReg r) = RIReg (env r)
    fixRI other	= other

#endif {- sparc_TARGET_ARCH -}
\end{code}

%************************************************************************
%*									*
\subsection{@spillReg@ and @loadReg@ functions}
%*									*
%************************************************************************

Spill to memory, and load it back...

JRS, 000122: on x86, don't spill directly above the stack pointer, since 
some insn sequences (int <-> conversions) use this as a temp location.
Leave 16 bytes of slop.

\begin{code}
spillReg, loadReg :: Reg -> Reg -> InstrList

spillReg dyn (MemoryReg i pk)
  | i >= 0 -- JRS paranoia
  = let	sz = primRepToSize pk
    in
    mkUnitList (
	{-Alpha: spill below the stack pointer (?)-}
	 IF_ARCH_alpha( ST sz dyn (spRel i)

	{-I386: spill above stack pointer leaving 2 words/spill-}
	,IF_ARCH_i386 ( let loc | i < 60    = 4 + 2 * i
                                | otherwise = -2000 - 2 * i
                        in
                        if pk == FloatRep || pk == DoubleRep
                        then GST DF dyn (spRel loc)
                        else MOV sz (OpReg dyn) (OpAddr (spRel loc))

	{-SPARC: spill below frame pointer leaving 2 words/spill-}
	,IF_ARCH_sparc( ST sz dyn (fpRel (-2 * i))
        ,)))
    )
  | otherwise
  = pprPanic "spillReg:" (text "invalid spill location: " <> int i)
   
----------------------------
loadReg (MemoryReg i pk) dyn
  | i >= 0 -- JRS paranoia
  = let	sz = primRepToSize pk
    in
    mkUnitList (
	 IF_ARCH_alpha( LD  sz dyn (spRel i)
	,IF_ARCH_i386 ( let loc | i < 60    = 4 + 2 * i
                                | otherwise = -2000 - 2 * i
                        in
                        if   pk == FloatRep || pk == DoubleRep
                        then GLD DF (spRel loc) dyn
                        else MOV sz (OpAddr (spRel loc)) (OpReg dyn)
	,IF_ARCH_sparc( LD  sz (fpRel (-2 * i)) dyn
	,)))
    )
  | otherwise
  = pprPanic "loadReg:" (text "invalid spill location: " <> int i)
\end{code}
