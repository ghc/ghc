%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[RegAllocInfo]{Machine-specific info used for register allocation}

The (machine-independent) allocator itself is in @AsmRegAlloc@.

\begin{code}
#include "nativeGen/NCG.h"

module RegAllocInfo (
	RegUsage(..),
	noUsage,
	regUsage,
        InsnFuture(..),
        insnFuture,

	loadReg,
	patchRegs,
	spillReg,
	findReservedRegs,

	RegSet,
        regSetFromList,
        regSetToList,
        isEmptyRegSet,
        emptyRegSet,
	eqRegSets,
	filterRegSet,
        unitRegSet,
        elemRegSet,
        unionRegSets,
        minusRegSets,
        intersectionRegSets
    ) where

#include "HsVersions.h"

import List		( sort )
import MachMisc
import MachRegs
import Stix		( DestInfo(..) )
import CLabel		( isAsmTemp, CLabel{-instance Ord-} )
import FiniteMap	( addToFM, lookupFM, FiniteMap )
import Outputable
import Constants	( rESERVED_C_STACK_BYTES )
import Unique		( Unique, Uniquable(..) )
import FastTypes

\end{code}

%************************************************************************
%*									*
\subsection{Sets of registers}
%*									*
%************************************************************************

\begin{code}

-- Blargh.  Use ghc stuff soon!  Or: perhaps that's not such a good
-- idea.  Most of these sets are either empty or very small, and it
-- might be that the overheads of the FiniteMap based set implementation
-- is a net loss.  The same might be true of FeSets.

newtype RegSet = MkRegSet [Reg]

regSetFromList xs 
   = MkRegSet (nukeDups (sort xs))
     where nukeDups :: [Reg] -> [Reg]
           nukeDups []  = []
           nukeDups [x] = [x]
           nukeDups (x:y:xys)
              = if x == y then nukeDups (y:xys)
                          else x : nukeDups (y:xys)

regSetToList   (MkRegSet xs)                 = xs
isEmptyRegSet  (MkRegSet xs)                 = null xs
emptyRegSet                                  = MkRegSet []
eqRegSets      (MkRegSet xs1) (MkRegSet xs2) = xs1 == xs2
unitRegSet x                                 = MkRegSet [x]
filterRegSet p (MkRegSet xs)                 = MkRegSet (filter p xs)

elemRegSet x (MkRegSet xs) 
   = f xs
     where
        f []     = False
        f (y:ys) | x == y    = True
                 | x < y     = False
                 | otherwise = f ys

unionRegSets (MkRegSet xs1) (MkRegSet xs2)
   = MkRegSet (f xs1 xs2)
     where
        f [] bs = bs
        f as [] = as
        f (a:as) (b:bs)
           | a < b      = a : f as (b:bs)
           | a > b      = b : f (a:as) bs
           | otherwise  = a : f as bs

minusRegSets (MkRegSet xs1) (MkRegSet xs2)
   = MkRegSet (f xs1 xs2)
     where
        f [] bs = []
        f as [] = as
        f (a:as) (b:bs)
           | a < b      = a : f as (b:bs)
           | a > b      = f (a:as) bs
           | otherwise  = f as bs

intersectionRegSets (MkRegSet xs1) (MkRegSet xs2)
   = MkRegSet (f xs1 xs2)
     where
        f [] bs = []
        f as [] = []
        f (a:as) (b:bs)
           | a < b      = f as (b:bs)
           | a > b      = f (a:as) bs
           | otherwise  = a : f as bs
\end{code}

%************************************************************************
%*									*
\subsection{@RegUsage@ type; @noUsage@ and @regUsage@ functions}
%*									*
%************************************************************************

@regUsage@ returns the sets of src and destination registers used by a
particular instruction.  Machine registers that are pre-allocated to
stgRegs are filtered out, because they are uninteresting from a
register allocation standpoint.  (We wouldn't want them to end up on
the free list!)  As far as we are concerned, the fixed registers
simply don't exist (for allocation purposes, anyway).

regUsage doesn't need to do any trickery for jumps and such.  Just
state precisely the regs read and written by that insn.  The
consequences of control flow transfers, as far as register allocation
goes, are taken care of by @insnFuture@.

\begin{code}
data RegUsage = RU RegSet RegSet

noUsage :: RegUsage
noUsage  = RU emptyRegSet emptyRegSet

regUsage :: Instr -> RegUsage

interesting (VirtualRegI _)  = True
interesting (VirtualRegF _)  = True
interesting (VirtualRegD _)  = True
interesting (RealReg i)      = isFastTrue (freeReg i)

#if alpha_TARGET_ARCH

regUsage instr = case instr of
    LD B reg addr	-> usage (regAddr addr, [reg, t9])
    LD Bu reg addr	-> usage (regAddr addr, [reg, t9])
--  LD W reg addr	-> usage (regAddr addr, [reg, t9]) : UNUSED
--  LD Wu reg addr	-> usage (regAddr addr, [reg, t9]) : UNUSED
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

#endif /* alpha_TARGET_ARCH */
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

regUsage instr = case instr of
    MOV    sz src dst	-> usageRW src dst
    MOVZxL sz src dst	-> usageRW src dst
    MOVSxL sz src dst	-> usageRW src dst
    LEA    sz src dst	-> usageRW src dst
    ADD    sz src dst	-> usageRM src dst
    SUB    sz src dst	-> usageRM src dst
    IMUL   sz src dst	-> usageRM src dst
    IMUL64    sd1 sd2   -> mkRU [sd1,sd2] [sd1,sd2]
    MUL    sz src dst	-> usageRM src dst
    IQUOT  sz src dst	-> usageRM src dst
    IREM   sz src dst	-> usageRM src dst
    QUOT   sz src dst	-> usageRM src dst
    REM    sz src dst	-> usageRM src dst
    AND    sz src dst	-> usageRM src dst
    OR     sz src dst	-> usageRM src dst
    XOR    sz src dst	-> usageRM src dst
    NOT    sz op	-> usageM op
    NEGI   sz op	-> usageM op
    SHL    sz imm dst	-> usageM dst
    SAR    sz imm dst	-> usageM dst
    SHR    sz imm dst	-> usageM dst
    BT     sz imm src	-> mkRU (use_R src) []

    PUSH   sz op	-> mkRU (use_R op) []
    POP    sz op	-> mkRU [] (def_W op)
    TEST   sz src dst	-> mkRU (use_R src ++ use_R dst) []
    CMP    sz src dst	-> mkRU (use_R src ++ use_R dst) []
    SETCC  cond op	-> mkRU [] (def_W op)
    JXX    cond lbl	-> mkRU [] []
    JMP    dsts op	-> mkRU (use_R op) []
    CALL   (Left imm)	-> mkRU [] callClobberedRegs
    CALL   (Right reg)	-> mkRU [reg] callClobberedRegs
    CLTD		-> mkRU [eax] [edx]
    NOP			-> mkRU [] []

    GMOV   src dst	-> mkRU [src] [dst]
    GLD    sz src dst	-> mkRU (use_EA src) [dst]
    GST    sz src dst	-> mkRU (src : use_EA dst) []

    GLDZ   dst		-> mkRU [] [dst]
    GLD1   dst		-> mkRU [] [dst]

    GFTOI  src dst	-> mkRU [src] [dst]
    GDTOI  src dst	-> mkRU [src] [dst]

    GITOF  src dst	-> mkRU [src] [dst]
    GITOD  src dst	-> mkRU [src] [dst]

    GADD   sz s1 s2 dst	-> mkRU [s1,s2] [dst]
    GSUB   sz s1 s2 dst	-> mkRU [s1,s2] [dst]
    GMUL   sz s1 s2 dst	-> mkRU [s1,s2] [dst]
    GDIV   sz s1 s2 dst	-> mkRU [s1,s2] [dst]

    GCMP   sz src1 src2	-> mkRU [src1,src2] []
    GABS   sz src dst	-> mkRU [src] [dst]
    GNEG   sz src dst	-> mkRU [src] [dst]
    GSQRT  sz src dst	-> mkRU [src] [dst]
    GSIN   sz src dst	-> mkRU [src] [dst]
    GCOS   sz src dst	-> mkRU [src] [dst]
    GTAN   sz src dst	-> mkRU [src] [dst]

    COMMENT _		-> noUsage
    SEGMENT _ 		-> noUsage
    LABEL   _		-> noUsage
    ASCII   _ _		-> noUsage
    DATA    _ _		-> noUsage
    DELTA   _           -> noUsage
    _			-> pprPanic "regUsage(x86)" empty

 where
    -- 2 operand form; first operand Read; second Written
    usageRW :: Operand -> Operand -> RegUsage
    usageRW op (OpReg reg) = mkRU (use_R op) [reg]
    usageRW op (OpAddr ea) = mkRU (use_R op ++ use_EA ea) []

    -- 2 operand form; first operand Read; second Modified
    usageRM :: Operand -> Operand -> RegUsage
    usageRM op (OpReg reg) = mkRU (use_R op ++ [reg]) [reg]
    usageRM op (OpAddr ea) = mkRU (use_R op ++ use_EA ea) []

    -- 1 operand form; operand Modified
    usageM :: Operand -> RegUsage
    usageM (OpReg reg)    = mkRU [reg] [reg]
    usageM (OpAddr ea)    = mkRU (use_EA ea) []

    -- Registers defd when an operand is written.
    def_W (OpReg reg)  = [reg]
    def_W (OpAddr ea)  = []

    -- Registers used when an operand is read.
    use_R (OpReg reg)  = [reg]
    use_R (OpImm imm)  = []
    use_R (OpAddr ea)  = use_EA ea

    -- Registers used to compute an effective address.
    use_EA (ImmAddr _ _)                           = []
    use_EA (AddrBaseIndex Nothing  Nothing      _) = []
    use_EA (AddrBaseIndex (Just b) Nothing      _) = [b]
    use_EA (AddrBaseIndex Nothing  (Just (i,_)) _) = [i]
    use_EA (AddrBaseIndex (Just b) (Just (i,_)) _) = [b,i]

    mkRU src dst = RU (regSetFromList (filter interesting src))
  	    	      (regSetFromList (filter interesting dst))

#endif /* i386_TARGET_ARCH */
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

regUsage instr = case instr of
    LD    sz addr reg  	-> usage (regAddr addr, [reg])
    ST    sz reg addr  	-> usage (reg : regAddr addr, [])
    ADD   x cc r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SUB   x cc r1 ar r2	-> usage (r1 : regRI ar, [r2])
    UMUL    cc r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SMUL    cc r1 ar r2	-> usage (r1 : regRI ar, [r2])
    RDY   rd            -> usage ([], [rd])
    AND   b r1 ar r2  	-> usage (r1 : regRI ar, [r2])
    ANDN  b r1 ar r2 	-> usage (r1 : regRI ar, [r2])
    OR    b r1 ar r2   	-> usage (r1 : regRI ar, [r2])
    ORN   b r1 ar r2  	-> usage (r1 : regRI ar, [r2])
    XOR   b r1 ar r2  	-> usage (r1 : regRI ar, [r2])
    XNOR  b r1 ar r2 	-> usage (r1 : regRI ar, [r2])
    SLL   r1 ar r2    	-> usage (r1 : regRI ar, [r2])
    SRL   r1 ar r2    	-> usage (r1 : regRI ar, [r2])
    SRA   r1 ar r2    	-> usage (r1 : regRI ar, [r2])
    SETHI imm reg   	-> usage ([], [reg])
    FABS  s r1 r2    	-> usage ([r1], [r2])
    FADD  s r1 r2 r3 	-> usage ([r1, r2], [r3])
    FCMP  e s r1 r2  	-> usage ([r1, r2], [])
    FDIV  s r1 r2 r3 	-> usage ([r1, r2], [r3])
    FMOV  s r1 r2    	-> usage ([r1], [r2])
    FMUL  s r1 r2 r3 	-> usage ([r1, r2], [r3])
    FNEG  s r1 r2    	-> usage ([r1], [r2])
    FSQRT s r1 r2   	-> usage ([r1], [r2])
    FSUB  s r1 r2 r3 	-> usage ([r1, r2], [r3])
    FxTOy s1 s2 r1 r2 	-> usage ([r1], [r2])

    -- We assume that all local jumps will be BI/BF.  JMP must be out-of-line.
    JMP   dst addr 	-> usage (regAddr addr, [])

    CALL  (Left imm)  n True  -> noUsage
    CALL  (Left imm)  n False -> usage (argRegs n, callClobberedRegs)
    CALL  (Right reg) n True  -> usage ([reg], [])
    CALL  (Right reg) n False -> usage (reg : (argRegs n), callClobberedRegs)

    _ 	    	    	-> noUsage
  where
    usage (src, dst) = RU (regSetFromList (filter interesting src))
    	    	    	  (regSetFromList (filter interesting dst))

    regAddr (AddrRegReg r1 r2) = [r1, r2]
    regAddr (AddrRegImm r1 _)  = [r1]

    regRI (RIReg r) = [r]
    regRI  _	= []

#endif /* sparc_TARGET_ARCH */
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if powerpc_TARGET_ARCH

regUsage instr = case instr of
    LD    sz reg addr  	-> usage (regAddr addr, [reg])
    ST    sz reg addr  	-> usage (reg : regAddr addr, [])
    STU    sz reg addr  -> usage (reg : regAddr addr, [])
    LIS   reg imm	-> usage ([], [reg])
    LI    reg imm	-> usage ([], [reg])
    MR	  reg1 reg2     -> usage ([reg2], [reg1])
    CMP   sz reg ri	-> usage (reg : regRI ri,[])
    CMPL  sz reg ri	-> usage (reg : regRI ri,[])
    BCC	  cond lbl	-> noUsage
    MTCTR reg		-> usage ([reg],[])
    BCTR  dsts		-> noUsage
    BL    imm params	-> usage (params, callClobberedRegs)
    BCTRL params	-> usage (params, callClobberedRegs)
    ADD	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    SUBF  reg1 reg2 reg3-> usage ([reg2,reg3], [reg1])
    MULLW reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    DIVW  reg1 reg2 reg3-> usage ([reg2,reg3], [reg1])
    DIVWU reg1 reg2 reg3-> usage ([reg2,reg3], [reg1])
    AND	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    OR	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    XOR	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    XORIS reg1 reg2 imm -> usage ([reg2], [reg1])
    NEG	  reg1 reg2	-> usage ([reg2], [reg1])
    NOT	  reg1 reg2	-> usage ([reg2], [reg1])
    SLW	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    SRW	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    SRAW  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    FADD  sz r1 r2 r3   -> usage ([r2,r3], [r1])
    FSUB  sz r1 r2 r3   -> usage ([r2,r3], [r1])
    FMUL  sz r1 r2 r3   -> usage ([r2,r3], [r1])
    FDIV  sz r1 r2 r3   -> usage ([r2,r3], [r1])
    FNEG  r1 r2		-> usage ([r2], [r1])
    FCMP  r1 r2		-> usage ([r1,r2], [])
    FCTIWZ r1 r2	-> usage ([r2], [r1])
    _ 	    	    	-> noUsage
  where
    usage (src, dst) = RU (regSetFromList (filter interesting src))
    	    	    	  (regSetFromList (filter interesting dst))
    regAddr (AddrRegReg r1 r2) = [r1, r2]
    regAddr (AddrRegImm r1 _)  = [r1]

    regRI (RIReg r) = [r]
    regRI  _	= []
#endif /* powerpc_TARGET_ARCH */
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
findReservedRegs :: [Instr] -> [[Reg]]
findReservedRegs instrs
#if alpha_TARGET_ARCH
  = --[[NCG_Reserved_I1, NCG_Reserved_I2,
    --  NCG_Reserved_F1, NCG_Reserved_F2]]
    error "findReservedRegs: alpha"
#endif
#if sparc_TARGET_ARCH
  = [[NCG_SpillTmp_I1, NCG_SpillTmp_I2, 
      NCG_SpillTmp_D1, NCG_SpillTmp_D2,
      NCG_SpillTmp_F1, NCG_SpillTmp_F2]]
#endif
#if i386_TARGET_ARCH
  -- We can use %fake4 and %fake5 safely for float temps.
  -- Int regs are more troublesome.  Only %ecx and %edx are
  -- definitely.  At a pinch, we also could bag %eax if there 
  -- are no ccalls, but so far we've never encountered
  -- a situation where three integer temporaries are necessary.
  -- 
  -- Because registers are in short supply on x86, we give the
  -- allocator a whole bunch of possibilities, starting with zero
  -- temporaries and working up to all that are available.  This
  -- is inefficient, but spills are pretty rare, so we don't care
  -- if the register allocator has to try half a dozen or so possibilities
  -- before getting to one that works.
  = let f1 = fake5
        f2 = fake4
        intregs_avail
           = [ecx, edx]
        possibilities
           = case intregs_avail of
                [i1] -> [ [], [i1], [f1], [i1,f1], [f1,f2], 
                          [i1,f1,f2] ]

                [i1,i2] -> [ [], [i1], [f1], [i1,i2], [i1,f1], [f1,f2],
                             [i1,i2,f1], [i1,f1,f2], [i1,i2,f1,f2] ]
    in
        possibilities
#endif
#if powerpc_TARGET_ARCH
  = [[NCG_SpillTmp_I1, NCG_SpillTmp_I2, 
      NCG_SpillTmp_D1, NCG_SpillTmp_D2]]
#endif
\end{code}

%************************************************************************
%*									*
\subsection{@InsnFuture@ type; @insnFuture@ function}
%*									*
%************************************************************************

@insnFuture@ indicates the places we could get to following the
current instruction.  This is used by the register allocator to
compute the flow edges between instructions.

\begin{code}
data InsnFuture 
   = NoFuture              -- makes a non-local jump; for the purposes of
                           -- register allocation, it exits our domain
   | Next                  -- falls through to next insn
   | Branch CLabel         -- unconditional branch to the label
   | NextOrBranch CLabel   -- conditional branch to the label
   | MultiFuture [CLabel]  -- multiple specific futures

--instance Outputable InsnFuture where
--   ppr NoFuture            = text "NoFuture"
--   ppr Next                = text "Next"
--   ppr (Branch clbl)       = text "(Branch " <> ppr clbl <> char ')'
--   ppr (NextOrBranch clbl) = text "(NextOrBranch " <> ppr clbl <> char ')'


insnFuture insn
 = case insn of

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

#endif /* alpha_TARGET_ARCH */
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

    -- conditional jump
    JXX _ clbl | isAsmTemp clbl -> NextOrBranch clbl
    JXX _ _ -> panic "insnFuture: conditional jump to non-local label"

    -- If the insn says what its dests are, use em!
    JMP (DestInfo dsts) _ -> MultiFuture dsts

    -- unconditional jump to local label
    JMP NoDestInfo (OpImm (ImmCLbl clbl)) | isAsmTemp clbl -> Branch clbl
    
    -- unconditional jump to non-local label
    JMP NoDestInfo lbl	-> NoFuture

    -- be extra-paranoid
    JMP _ _ -> panic "insnFuture(x86): JMP wierdness"

    boring	-> Next

#endif /* i386_TARGET_ARCH */
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

    -- We assume that all local jumps will be BI/BF.
    BI ALWAYS _ (ImmCLbl clbl) -> Branch clbl
    BI other  _ (ImmCLbl clbl) -> NextOrBranch clbl
    BI other  _ _ -> panic "nativeGen(sparc):insnFuture(BI)"

    BF ALWAYS _ (ImmCLbl clbl) -> Branch clbl
    BF other  _ (ImmCLbl clbl) -> NextOrBranch clbl
    BF other  _ _ -> panic "nativeGen(sparc):insnFuture(BF)"

    -- CALL(terminal) must be out-of-line.  JMP is not out-of-line
    -- iff it specifies its destinations.
    JMP NoDestInfo _      -> NoFuture  -- n.b. NoFuture == MultiFuture []
    JMP (DestInfo dsts) _ -> MultiFuture dsts

    CALL _ _ True         -> NoFuture

    boring -> Next

#endif /* sparc_TARGET_ARCH */
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if powerpc_TARGET_ARCH
    BCC ALWAYS clbl | isAsmTemp clbl -> Branch clbl
		    | otherwise -> NoFuture
    BCC _ clbl 	    | isAsmTemp clbl -> NextOrBranch clbl
    BCC _ _ -> panic "insnFuture: conditional jump to non-local label"
    
    BCTR (DestInfo dsts) -> MultiFuture dsts
    BCTR NoDestInfo -> NoFuture
    boring	-> Next
#endif /* powerpc_TARGET_ARCH */
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

#endif /* alpha_TARGET_ARCH */
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
    IMUL64  sd1 sd2     -> IMUL64 (env sd1) (env sd2)
    MUL sz src dst 	-> patch2 (MUL sz) src dst
    IQUOT sz src dst 	-> patch2 (IQUOT sz) src dst
    IREM sz src dst 	-> patch2 (IREM sz) src dst
    QUOT sz src dst 	-> patch2 (QUOT sz) src dst
    REM sz src dst 	-> patch2 (REM sz) src dst
    AND  sz src dst	-> patch2 (AND  sz) src dst
    OR   sz src dst	-> patch2 (OR   sz) src dst
    XOR  sz src dst	-> patch2 (XOR  sz) src dst
    NOT  sz op 		-> patch1 (NOT  sz) op
    NEGI sz op		-> patch1 (NEGI sz) op
    SHL  sz imm dst 	-> patch1 (SHL sz imm) dst
    SAR  sz imm dst 	-> patch1 (SAR sz imm) dst
    SHR  sz imm dst 	-> patch1 (SHR sz imm) dst
    BT   sz imm src     -> patch1 (BT  sz imm) src
    TEST sz src dst	-> patch2 (TEST sz) src dst
    CMP  sz src dst	-> patch2 (CMP  sz) src dst
    PUSH sz op		-> patch1 (PUSH sz) op
    POP  sz op		-> patch1 (POP  sz) op
    SETCC cond op	-> patch1 (SETCC cond) op
    JMP dsts op		-> patch1 (JMP dsts) op

    GMOV src dst	-> GMOV (env src) (env dst)
    GLD sz src dst	-> GLD sz (lookupAddr src) (env dst)
    GST sz src dst	-> GST sz (env src) (lookupAddr dst)

    GLDZ dst		-> GLDZ (env dst)
    GLD1 dst		-> GLD1 (env dst)

    GFTOI src dst	-> GFTOI (env src) (env dst)
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

    CALL (Left imm)	-> instr
    CALL (Right reg)	-> CALL (Right (env reg))

    COMMENT _		-> instr
    SEGMENT _ 		-> instr
    LABEL _		-> instr
    ASCII _ _		-> instr
    DATA _ _		-> instr
    DELTA _ 		-> instr
    JXX _ _		-> instr
    CLTD		-> instr
    _			-> pprPanic "patchRegs(x86)" empty

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

#endif /* i386_TARGET_ARCH */
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

patchRegs instr env = case instr of
    LD    sz addr reg   -> LD sz (fixAddr addr) (env reg)
    ST    sz reg addr   -> ST sz (env reg) (fixAddr addr)
    ADD   x cc r1 ar r2 -> ADD x cc (env r1) (fixRI ar) (env r2)
    SUB   x cc r1 ar r2 -> SUB x cc (env r1) (fixRI ar) (env r2)
    UMUL    cc r1 ar r2	-> UMUL cc (env r1) (fixRI ar) (env r2)
    SMUL    cc r1 ar r2	-> SMUL cc (env r1) (fixRI ar) (env r2)
    RDY   rd            -> RDY (env rd)
    AND   b r1 ar r2    -> AND b (env r1) (fixRI ar) (env r2)
    ANDN  b r1 ar r2    -> ANDN b (env r1) (fixRI ar) (env r2)
    OR    b r1 ar r2    -> OR b (env r1) (fixRI ar) (env r2)
    ORN   b r1 ar r2    -> ORN b (env r1) (fixRI ar) (env r2)
    XOR   b r1 ar r2    -> XOR b (env r1) (fixRI ar) (env r2)
    XNOR  b r1 ar r2    -> XNOR b (env r1) (fixRI ar) (env r2)
    SLL   r1 ar r2      -> SLL (env r1) (fixRI ar) (env r2)
    SRL   r1 ar r2      -> SRL (env r1) (fixRI ar) (env r2)
    SRA   r1 ar r2      -> SRA (env r1) (fixRI ar) (env r2)
    SETHI imm reg       -> SETHI imm (env reg)
    FABS  s r1 r2       -> FABS s (env r1) (env r2)
    FADD  s r1 r2 r3    -> FADD s (env r1) (env r2) (env r3)
    FCMP  e s r1 r2     -> FCMP e s (env r1) (env r2)
    FDIV  s r1 r2 r3    -> FDIV s (env r1) (env r2) (env r3)
    FMOV  s r1 r2       -> FMOV s (env r1) (env r2)
    FMUL  s r1 r2 r3    -> FMUL s (env r1) (env r2) (env r3)
    FNEG  s r1 r2       -> FNEG s (env r1) (env r2)
    FSQRT s r1 r2       -> FSQRT s (env r1) (env r2)
    FSUB  s r1 r2 r3    -> FSUB s (env r1) (env r2) (env r3)
    FxTOy s1 s2 r1 r2   -> FxTOy s1 s2 (env r1) (env r2)
    JMP   dsts addr     -> JMP dsts (fixAddr addr)
    CALL  (Left i) n t  -> CALL (Left i) n t
    CALL  (Right r) n t -> CALL (Right (env r)) n t
    _ -> instr
  where
    fixAddr (AddrRegReg r1 r2) = AddrRegReg (env r1) (env r2)
    fixAddr (AddrRegImm r1 i)  = AddrRegImm (env r1) i

    fixRI (RIReg r) = RIReg (env r)
    fixRI other	= other

#endif /* sparc_TARGET_ARCH */
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if powerpc_TARGET_ARCH

patchRegs instr env = case instr of
    LD    sz reg addr   -> LD sz (env reg) (fixAddr addr)
    ST    sz reg addr   -> ST sz (env reg) (fixAddr addr)
    STU    sz reg addr  -> STU sz (env reg) (fixAddr addr)
    LIS   reg imm	-> LIS (env reg) imm
    LI    reg imm	-> LI (env reg) imm
    MR	  reg1 reg2     -> MR (env reg1) (env reg2)
    CMP	  sz reg ri	-> CMP sz (env reg) (fixRI ri)
    CMPL  sz reg ri	-> CMPL sz (env reg) (fixRI ri)
    BCC	  cond lbl	-> BCC cond lbl
    MTCTR reg		-> MTCTR (env reg)
    BCTR  dsts		-> BCTR dsts
    BL    imm argRegs	-> BL imm argRegs	-- argument regs
    BCTRL argRegs	-> BCTRL argRegs 	-- cannot be remapped
    ADD	  reg1 reg2 ri	-> ADD (env reg1) (env reg2) (fixRI ri)
    SUBF  reg1 reg2 reg3-> SUBF (env reg1) (env reg2) (env reg3)
    MULLW reg1 reg2 ri	-> MULLW (env reg1) (env reg2) (fixRI ri)
    DIVW  reg1 reg2 reg3-> DIVW (env reg1) (env reg2) (env reg3)
    DIVWU reg1 reg2 reg3-> DIVWU (env reg1) (env reg2) (env reg3)
    AND	  reg1 reg2 ri	-> AND (env reg1) (env reg2) (fixRI ri)
    OR 	  reg1 reg2 ri	-> OR  (env reg1) (env reg2) (fixRI ri)
    XOR	  reg1 reg2 ri	-> XOR (env reg1) (env reg2) (fixRI ri)
    XORIS reg1 reg2 imm -> XORIS (env reg1) (env reg2) imm
    NEG	  reg1 reg2	-> NEG (env reg1) (env reg2)
    NOT	  reg1 reg2	-> NOT (env reg1) (env reg2)
    SLW	  reg1 reg2 ri	-> SLW (env reg1) (env reg2) (fixRI ri)
    SRW	  reg1 reg2 ri	-> SRW (env reg1) (env reg2) (fixRI ri)
    SRAW  reg1 reg2 ri	-> SRAW (env reg1) (env reg2) (fixRI ri)
    FADD  sz r1 r2 r3   -> FADD sz (env r1) (env r2) (env r3)
    FSUB  sz r1 r2 r3   -> FSUB sz (env r1) (env r2) (env r3)
    FMUL  sz r1 r2 r3   -> FMUL sz (env r1) (env r2) (env r3)
    FDIV  sz r1 r2 r3   -> FDIV sz (env r1) (env r2) (env r3)
    FNEG  r1 r2		-> FNEG (env r1) (env r2)
    FCMP  r1 r2		-> FCMP (env r1) (env r2)
    FCTIWZ r1 r2	-> FCTIWZ (env r1) (env r2)
    _ -> instr
  where
    fixAddr (AddrRegReg r1 r2) = AddrRegReg (env r1) (env r2)
    fixAddr (AddrRegImm r1 i)  = AddrRegImm (env r1) i

    fixRI (RIReg r) = RIReg (env r)
    fixRI other	= other
#endif /* powerpc_TARGET_ARCH */
\end{code}

%************************************************************************
%*									*
\subsection{@spillReg@ and @loadReg@ functions}
%*									*
%************************************************************************

Spill to memory, and load it back...

JRS, 000122: on x86, don't spill directly above the stack pointer,
since some insn sequences (int <-> conversions) use this as a temp
location.  Leave 8 words (ie, 64 bytes for a 64-bit arch) of slop.

\begin{code}
spillSlotSize :: Int
spillSlotSize = IF_ARCH_alpha( 8, IF_ARCH_sparc( 8, IF_ARCH_i386( 12, IF_ARCH_powerpc( 8, ))))

maxSpillSlots :: Int
maxSpillSlots = ((rESERVED_C_STACK_BYTES - 64) `div` spillSlotSize) - 1

-- convert a spill slot number to a *byte* offset, with no sign:
-- decide on a per arch basis whether you are spilling above or below
-- the C stack pointer.
spillSlotToOffset :: Int -> Int
spillSlotToOffset slot
   | slot >= 0 && slot < maxSpillSlots
   = 64 + spillSlotSize * slot
   | otherwise
   = pprPanic "spillSlotToOffset:" 
              (text "invalid spill location: " <> int slot)

vregToSpillSlot :: FiniteMap VRegUnique Int -> VRegUnique -> Int
vregToSpillSlot vreg_to_slot_map u
   = case lookupFM vreg_to_slot_map u of
        Just xx -> xx
        Nothing -> pprPanic "vregToSpillSlot: unmapped vreg" (pprVRegUnique u)


spillReg, loadReg :: FiniteMap VRegUnique Int -> Int -> Reg -> Reg -> Instr

spillReg vreg_to_slot_map delta dyn vreg
  | isVirtualReg vreg
  = let	slot_no = vregToSpillSlot vreg_to_slot_map (getVRegUnique vreg)
        off     = spillSlotToOffset slot_no
    in
	{-Alpha: spill below the stack pointer (?)-}
	 IF_ARCH_alpha( ST sz dyn (spRel (- (off `div` 8)))

	{-I386: spill above stack pointer leaving 3 words/spill-}
	,IF_ARCH_i386 ( let off_w = (off-delta) `div` 4
                        in case regClass vreg of {
                              RcInteger -> MOV L (OpReg dyn) (OpAddr (spRel off_w));
                              _         -> GST F80 dyn (spRel off_w)} {- RcFloat/RcDouble -}

	{-SPARC: spill below frame pointer leaving 2 words/spill-}
	,IF_ARCH_sparc( 
                        let{off_w = 1 + (off `div` 4);
                            sz = case regClass vreg of {
                                    RcInteger -> W;
                                    RcFloat   -> F;
                                    RcDouble  -> DF}}
                        in ST sz dyn (fpRel (- off_w))
        ,IF_ARCH_powerpc(
			let{sz = case regClass vreg of {
                                    RcInteger -> W;
                                    RcFloat   -> F;
                                    RcDouble  -> DF}}
			in ST sz dyn (AddrRegImm sp (ImmInt (off-delta)))
	,))))

   
loadReg vreg_to_slot_map delta vreg dyn
  | isVirtualReg vreg
  = let	slot_no = vregToSpillSlot vreg_to_slot_map (getVRegUnique vreg)
        off     = spillSlotToOffset slot_no
    in
	 IF_ARCH_alpha( LD  sz dyn (spRel (- (off `div` 8)))

	,IF_ARCH_i386 ( let off_w = (off-delta) `div` 4
                        in case regClass vreg of {
                              RcInteger -> MOV L (OpAddr (spRel off_w)) (OpReg dyn);
                              _         -> GLD F80 (spRel off_w) dyn} {- RcFloat/RcDouble -}

	,IF_ARCH_sparc( 
                        let{off_w = 1 + (off `div` 4);
                            sz = case regClass vreg of {
                                   RcInteger -> W;
                                   RcFloat   -> F;
                                   RcDouble  -> DF}}
                        in LD sz (fpRel (- off_w)) dyn
        ,IF_ARCH_powerpc(
			let{sz = case regClass vreg of {
                                    RcInteger -> W;
                                    RcFloat   -> F;
                                    RcDouble  -> DF}}
			in LD sz dyn (AddrRegImm sp (ImmInt (off-delta)))
	,))))
\end{code}
