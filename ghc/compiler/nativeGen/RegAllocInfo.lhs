%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[RegAllocInfo]{Machine-specific info used for register allocation}

The (machine-independent) allocator itself is in @AsmRegAlloc@.

\begin{code}
#include "HsVersions.h"
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
	SYN_IE(RegAssignment),
	SYN_IE(RegConflicts),
	RegFuture(..),
	RegHistory(..),
	RegInfo(..),
	RegLiveness(..),

	fstFL,
	loadReg,
	patchRegs,
	regLiveness,
	spillReg,

	SYN_IE(RegSet),
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

IMP_Ubiq(){-uitous-}
IMPORT_1_3(List(partition))

import MachMisc
import MachRegs
import MachCode		( SYN_IE(InstrList) )

import AbsCSyn		( MagicId )
import BitSet		( unitBS, mkBS, minusBS, unionBS, listBS, BitSet )
import CLabel		( pprCLabel_asm, CLabel{-instance Ord-} )
import FiniteMap	( addToFM, lookupFM, FiniteMap )
import OrdList		( mkUnitList, OrdList )
import PrimRep		( PrimRep(..) )
import Stix		( StixTree, CodeSegment )
import UniqSet		-- quite a bit of it
import Unpretty		( uppShow )
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
    MOV  sz src dst	-> usage2 src dst
    MOVZX sz src dst	-> usage2 src dst
    MOVSX sz src dst	-> usage2 src dst
    LEA  sz src dst	-> usage2 src dst
    ADD  sz src dst	-> usage2 src dst
    SUB  sz src dst	-> usage2 src dst
    IMUL sz src dst	-> usage2 src dst
    IDIV sz src		-> usage (eax:edx:opToReg src) [eax,edx]
    AND  sz src dst	-> usage2 src dst
    OR   sz src dst	-> usage2 src dst
    XOR  sz src dst	-> usage2 src dst
    NOT  sz op		-> usage1 op
    NEGI sz op		-> usage1 op
    SHL  sz imm dst	-> usage1 dst -- imm has to be an Imm
    SAR  sz imm dst	-> usage1 dst -- imm has to be an Imm
    SHR  sz imm dst	-> usage1 dst -- imm has to be an Imm
    PUSH sz op		-> usage (opToReg op) []
    POP  sz op		-> usage [] (opToReg op)
    TEST sz src dst	-> usage (opToReg src ++ opToReg dst) []
    CMP  sz src dst	-> usage (opToReg src ++ opToReg dst) []
    SETCC cond op	-> usage [] (opToReg op)
    JXX cond label	-> usage [] []
    JMP op		-> usage (opToReg op) freeRegs
    CALL imm		-> usage [] callClobberedRegs
    CLTD		-> usage [eax] [edx]
    NOP			-> usage [] []
    SAHF 		-> usage [eax] []
    FABS 		-> usage [st0] [st0]
    FADD sz src		-> usage (st0:opToReg src) [st0] -- allFPRegs
    FADDP 		-> usage [st0,st1] [st0] -- allFPRegs
    FIADD sz asrc	-> usage (addrToRegs asrc) [st0]
    FCHS 		-> usage [st0] [st0]
    FCOM sz src		-> usage (st0:opToReg src) []
    FCOS 		-> usage [st0] [st0]
    FDIV sz src 	-> usage (st0:opToReg src) [st0]
    FDIVP  		-> usage [st0,st1] [st0]
    FDIVRP 		-> usage [st0,st1] [st0]
    FIDIV sz asrc	-> usage (addrToRegs asrc) [st0]
    FDIVR sz src 	-> usage (st0:opToReg src) [st0]
    FIDIVR sz asrc	-> usage (addrToRegs asrc) [st0]
    FICOM sz asrc	-> usage (addrToRegs asrc) []
    FILD sz asrc dst	-> usage (addrToRegs asrc) [dst] -- allFPRegs
    FIST sz adst	-> usage (st0:addrToRegs adst) []
    FLD	 sz src 	-> usage (opToReg src) [st0] -- allFPRegs
    FLD1 		-> usage [] [st0] -- allFPRegs
    FLDZ 		-> usage [] [st0] -- allFPRegs
    FMUL sz src 	-> usage (st0:opToReg src) [st0]
    FMULP 	 	-> usage [st0,st1] [st0]
    FIMUL sz asrc	-> usage (addrToRegs asrc) [st0]
    FRNDINT 		-> usage [st0] [st0]
    FSIN 		-> usage [st0] [st0]
    FSQRT 		-> usage [st0] [st0]
    FST sz (OpReg r)	-> usage [st0] [r]
    FST sz dst		-> usage (st0:opToReg dst) []
    FSTP sz (OpReg r)	-> usage [st0] [r] -- allFPRegs
    FSTP sz dst		-> usage (st0:opToReg dst) [] -- allFPRegs
    FSUB sz src		-> usage (st0:opToReg src) [st0] -- allFPRegs
    FSUBR sz src	-> usage (st0:opToReg src) [st0] -- allFPRegs
    FISUB sz asrc	-> usage (addrToRegs asrc) [st0]
    FSUBP 		-> usage [st0,st1] [st0] -- allFPRegs
    FSUBRP 		-> usage [st0,st1] [st0] -- allFPRegs
    FISUBR sz asrc	-> usage (addrToRegs asrc) [st0]
    FTST 		-> usage [st0] []
    FCOMP sz op		-> usage (st0:opToReg op) [st0] -- allFPRegs
    FUCOMPP 		-> usage [st0, st1] [] --  allFPRegs
    FXCH		-> usage [st0, st1] [st0, st1]
    FNSTSW		-> usage [] [eax]
    _			-> noUsage
 where
    usage2 :: Operand -> Operand -> RegUsage
    usage2 op (OpReg reg) = usage (opToReg op) [reg]
    usage2 op (OpAddr ea) = usage (opToReg op ++ addrToRegs ea) []
    usage2 op (OpImm imm) = usage (opToReg op) []
    usage1 :: Operand -> RegUsage
    usage1 (OpReg reg)    = usage [reg] [reg]
    usage1 (OpAddr ea)    = usage (addrToRegs ea) []
    allFPRegs = [st0,st1,st2,st3,st4,st5,st6,st7]

    --callClobberedRegs = [ eax, ecx, edx ] -- according to gcc, anyway.
    callClobberedRegs = [eax]

-- General purpose register collecting functions.

    opToReg (OpReg reg)   = [reg]
    opToReg (OpImm imm)   = []
    opToReg (OpAddr  ea)  = addrToRegs ea

    addrToRegs (Addr base index _) = baseToReg base ++ indexToReg index
      where  baseToReg Nothing       = []
	     baseToReg (Just r)      = [r]
	     indexToReg Nothing      = []
	     indexToReg (Just (r,_)) = [r]
    addrToRegs (ImmAddr _ _) = []

    usage src dst = RU (mkRegSet (filter interesting src))
    	    	       (mkRegSet (filter interesting dst))

    interesting (FixedReg _) = False
    interesting _ = True

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
	    Nothing -> trace ("Missing " ++ (uppShow 80 (pprCLabel_asm lbl)) ++
			      " in future?") emptyRegSet
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
    MOVZX sz src dst	-> patch2 (MOVZX sz) src dst
    MOVSX sz src dst	-> patch2 (MOVSX sz) src dst
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
    SHL  sz imm dst 	-> patch1 (SHL  sz imm) dst
    SAR  sz imm dst 	-> patch1 (SAR  sz imm) dst
    SHR  sz imm dst 	-> patch1 (SHR  sz imm) dst
    TEST sz src dst	-> patch2 (TEST sz) src dst
    CMP  sz src dst	-> patch2 (CMP  sz) src dst
    PUSH sz op		-> patch1 (PUSH sz) op
    POP  sz op		-> patch1 (POP  sz) op
    SETCC cond op	-> patch1 (SETCC cond) op
    JMP op		-> patch1 JMP op
    FADD sz src		-> FADD sz (patchOp src)
    FIADD sz asrc	-> FIADD sz (lookupAddr asrc)
    FCOM sz src		-> patch1 (FCOM sz) src
    FDIV sz src 	-> FDIV sz (patchOp src)
    --FDIVP sz src 	-> FDIVP sz (patchOp src)
    FIDIV sz asrc	-> FIDIV sz (lookupAddr asrc)
    FDIVR sz src 	-> FDIVR sz (patchOp src)
    --FDIVRP sz src 	-> FDIVRP sz (patchOp src)
    FIDIVR sz asrc	-> FIDIVR sz (lookupAddr asrc)
    FICOM sz asrc	-> FICOM sz (lookupAddr asrc)
    FILD sz asrc dst	-> FILD sz (lookupAddr asrc) (env dst)
    FIST sz adst	-> FIST sz (lookupAddr adst)
    FLD	sz src 		-> patch1 (FLD sz) (patchOp src)
    FMUL sz src 	-> FMUL sz (patchOp src)
    --FMULP sz src 	-> FMULP sz (patchOp src)
    FIMUL sz asrc	-> FIMUL sz (lookupAddr asrc)
    FST sz dst		-> FST sz (patchOp dst)
    FSTP sz dst		-> FSTP sz (patchOp dst)
    FSUB sz src		-> FSUB sz (patchOp src)
    --FSUBP sz src	-> FSUBP sz (patchOp src)
    FISUB sz asrc	-> FISUB sz (lookupAddr asrc)
    FSUBR sz src 	-> FSUBR sz (patchOp src)
    --FSUBRP sz src 	-> FSUBRP sz (patchOp src)
    FISUBR sz asrc	-> FISUBR sz (lookupAddr asrc)
    FCOMP sz src	-> FCOMP sz (patchOp src)
    _			-> instr
  where
    patch1 insn op      = insn (patchOp op)
    patch2 insn src dst = insn (patchOp src) (patchOp dst)

    patchOp (OpReg  reg) = OpReg (env reg)
    patchOp (OpImm  imm) = OpImm imm
    patchOp (OpAddr ea)  = OpAddr (lookupAddr ea)

    lookupAddr (ImmAddr imm off) = ImmAddr imm off
    lookupAddr (Addr base index disp)
      = Addr (lookupBase base) (lookupIndex index) disp
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

\begin{code}
spillReg, loadReg :: Reg -> Reg -> InstrList

spillReg dyn (MemoryReg i pk)
  = let
	sz = primRepToSize pk
    in
    mkUnitList (
	{-Alpha: spill below the stack pointer (?)-}
	 IF_ARCH_alpha( ST sz dyn (spRel i)

	{-I386: spill below stack pointer leaving 2 words/spill-}
	,IF_ARCH_i386 ( MOV sz (OpReg dyn) (OpAddr (spRel (-2 * i)))

	{-SPARC: spill below frame pointer leaving 2 words/spill-}
	,IF_ARCH_sparc( ST sz dyn (fpRel (-2 * i))
        ,)))
    )

----------------------------
loadReg (MemoryReg i pk) dyn
  = let
	sz = primRepToSize pk
    in
    mkUnitList (
	 IF_ARCH_alpha( LD  sz dyn (spRel i)
	,IF_ARCH_i386 ( MOV sz (OpAddr (spRel (-2 * i))) (OpReg dyn)
	,IF_ARCH_sparc( LD  sz (fpRel (-2 * i)) dyn
	,)))
    )
\end{code}
