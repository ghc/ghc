%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module AsmCodeGen ( nativeCodeGen ) where

#include "HsVersions.h"
#include "nativeGen/NCG.h"

import IO		( Handle )
import List		( intersperse )

import MachMisc
import MachRegs
import MachCode
import PprMach

import AbsCStixGen	( genCodeAbstractC )
import AbsCSyn		( AbstractC, MagicId )
import AbsCUtils	( mkAbsCStmtList )
import AsmRegAlloc	( runRegAllocate )
import PrimOp		( commutableOp, PrimOp(..) )
import RegAllocInfo	( findReservedRegs )
import Stix		( StixTree(..), StixReg(..), 
                          pprStixTrees, pprStixTree, CodeSegment(..),
                          stixCountTempUses, stixSubst,
                          NatM, initNat, mapNat,
                          NatM_State, mkNatM_State,
                          uniqOfNatM_State, deltaOfNatM_State )
import PrimRep		( isFloatingRep, PrimRep(..) )
import UniqSupply	( returnUs, thenUs, mapUs, initUs, 
                          initUs_, UniqSM, UniqSupply,
			  lazyThenUs, lazyMapUs )
import MachMisc		( IF_ARCH_i386(i386_insert_ffrees,) )

import OrdList		( fromOL, concatOL )
import Outputable	

\end{code}

The 96/03 native-code generator has machine-independent and
machine-dependent modules (those \tr{#include}'ing \tr{NCG.h}).

This module (@AsmCodeGen@) is the top-level machine-independent
module.  It uses @AbsCStixGen.genCodeAbstractC@ to produce @StixTree@s
(defined in module @Stix@), using support code from @StixInfo@ (info
tables), @StixPrim@ (primitive operations), @StixMacro@ (Abstract C
macros), and @StixInteger@ (GMP arbitrary-precision operations).

Before entering machine-dependent land, we do some machine-independent
@genericOpt@imisations (defined below) on the @StixTree@s.

We convert to the machine-specific @Instr@ datatype with
@stmt2Instrs@, assuming an ``infinite'' supply of registers.  We then
use a machine-independent register allocator (@runRegAllocate@) to
rejoin reality.  Obviously, @runRegAllocate@ has machine-specific
helper functions (see about @RegAllocInfo@ below).

The machine-dependent bits break down as follows:
\begin{description}
\item[@MachRegs@:]  Everything about the target platform's machine
    registers (and immediate operands, and addresses, which tend to
    intermingle/interact with registers).

\item[@MachMisc@:]  Includes the @Instr@ datatype (possibly should
    have a module of its own), plus a miscellany of other things
    (e.g., @targetDoubleSize@, @smStablePtrTable@, ...)

\item[@MachCode@:]  @stmt2Instrs@ is where @Stix@ stuff turns into
    machine instructions.

\item[@PprMach@:] @pprInstr@ turns an @Instr@ into text (well, really
    an @Doc@).

\item[@RegAllocInfo@:] In the register allocator, we manipulate
    @MRegsState@s, which are @BitSet@s, one bit per machine register.
    When we want to say something about a specific machine register
    (e.g., ``it gets clobbered by this instruction''), we set/unset
    its bit.  Obviously, we do this @BitSet@ thing for efficiency
    reasons.

    The @RegAllocInfo@ module collects together the machine-specific
    info needed to do register allocation.
\end{description}

So, here we go:

\begin{code}
nativeCodeGen :: AbstractC -> UniqSupply -> (SDoc, SDoc)
nativeCodeGen absC us
   = let absCstmts         = mkAbsCStmtList absC
         (sdoc_pairs, us1) = initUs us (lazyMapUs absCtoNat absCstmts)
         stix_sdocs        = map fst sdoc_pairs
         insn_sdocs        = map snd sdoc_pairs

         insn_sdoc         = my_vcat insn_sdocs
         stix_sdoc         = vcat stix_sdocs

#        if DEBUG
         my_trace m x = trace m x
         my_vcat sds = vcat (intersperse (char ' ' 
                                          $$ ptext SLIT("# ___stg_split_marker")
                                          $$ char ' ') 
                                          sds)
#        else
         my_vcat sds = vcat sds
         my_trace m x = x
#        endif
     in  
         my_trace "nativeGen: begin" 
                  (stix_sdoc, insn_sdoc)


absCtoNat :: AbstractC -> UniqSM (SDoc, SDoc)
absCtoNat absC
   = genCodeAbstractC absC                `thenUs` \ stixRaw ->
     genericOpt stixRaw                   `bind`   \ stixOpt ->
     genMachCode stixOpt                  `thenUs` \ pre_regalloc ->
     regAlloc pre_regalloc                `bind`   \ almost_final ->
     x86fp_kludge almost_final            `bind`   \ final_mach_code ->
     vcat (map pprInstr final_mach_code)  `bind`   \ final_sdoc ->
     pprStixTrees stixOpt                 `bind`   \ stix_sdoc ->
     returnUs (stix_sdoc, final_sdoc)
     where
        bind f x = x f

        x86fp_kludge :: [Instr] -> [Instr]
        x86fp_kludge = IF_ARCH_i386(i386_insert_ffrees,id)

        regAlloc :: InstrBlock -> [Instr]
        regAlloc = runRegAllocate allocatableRegs findReservedRegs
\end{code}

Top level code generator for a chunk of stix code.  For this part of
the computation, we switch from the UniqSM monad to the NatM monad.
The latter carries not only a Unique, but also an Int denoting the
current C stack pointer offset in the generated code; this is needed
for creating correct spill offsets on architectures which don't offer,
or for which it would be prohibitively expensive to employ, a frame
pointer register.  Viz, x86.

The offset is measured in bytes, and indicates the difference between
the current (simulated) C stack-ptr and the value it was at the
beginning of the block.  For stacks which grow down, this value should
be either zero or negative.

Switching between the two monads whilst carrying along the same Unique
supply breaks abstraction.  Is that bad?

\begin{code}
genMachCode :: [StixTree] -> UniqSM InstrBlock

genMachCode stmts initial_us
  = let initial_st         = mkNatM_State initial_us 0
        (blocks, final_st) = initNat initial_st 
                                     (mapNat stmt2Instrs stmts)
        instr_list         = concatOL blocks
        final_us           = uniqOfNatM_State final_st
        final_delta        = deltaOfNatM_State final_st
    in
        if   final_delta == 0
        then (instr_list, final_us)
        else pprPanic "genMachCode: nonzero final delta"
                      (int final_delta)
\end{code}

%************************************************************************
%*									*
\subsection[NCOpt]{The Generic Optimiser}
%*									*
%************************************************************************

This is called between translating Abstract C to its Tree and actually
using the Native Code Generator to generate the annotations.  It's a
chance to do some strength reductions.

** Remember these all have to be machine independent ***

Note that constant-folding should have already happened, but we might
have introduced some new opportunities for constant-folding wrt
address manipulations.

\begin{code}
genericOpt :: [StixTree] -> [StixTree]
genericOpt = map stixConFold . stixPeep



stixPeep :: [StixTree] -> [StixTree]

-- This transformation assumes that the temp assigned to in t1
-- is not assigned to in t2; for otherwise the target of the
-- second assignment would be substituted for, giving nonsense
-- code.  As far as I can see, StixTemps are only ever assigned
-- to once.  It would be nice to be sure!

stixPeep ( t1@(StAssign pka (StReg (StixTemp u pk)) rhs)
         : t2
         : ts )
   | stixCountTempUses u t2 == 1
     && sum (map (stixCountTempUses u) ts) == 0
   = 
#    ifdef DEBUG
     trace ("nativeGen: inlining " ++ showSDoc (pprStixTree rhs))
#    endif
           (stixPeep (stixSubst u rhs t2 : ts))

stixPeep (t1:t2:ts) = t1 : stixPeep (t2:ts)
stixPeep [t1]       = [t1]
stixPeep []         = []

-- disable stix inlining until we figure out how to fix the
-- latent bugs in the register allocator which are exposed by
-- the inliner.
--stixPeep = id
\end{code}

For most nodes, just optimize the children.

\begin{code}
stixConFold :: StixTree -> StixTree

stixConFold (StInd pk addr) = StInd pk (stixConFold addr)

stixConFold (StAssign pk dst src)
  = StAssign pk (stixConFold dst) (stixConFold src)

stixConFold (StJump addr) = StJump (stixConFold addr)

stixConFold (StCondJump addr test)
  = StCondJump addr (stixConFold test)

stixConFold (StCall fn cconv pk args)
  = StCall fn cconv pk (map stixConFold args)
\end{code}

Fold indices together when the types match:
\begin{code}
stixConFold (StIndex pk (StIndex pk' base off) off')
  | pk == pk'
  = StIndex pk (stixConFold base)
    	       (stixConFold (StPrim IntAddOp [off, off']))

stixConFold (StIndex pk base off)
  = StIndex pk (stixConFold base) (stixConFold off)
\end{code}

For PrimOps, we first optimize the children, and then we try our hand
at some constant-folding.

\begin{code}
stixConFold (StPrim op args) = stixPrimFold op (map stixConFold args)
\end{code}

Replace register leaves with appropriate StixTrees for the given
target.

\begin{code}
stixConFold leaf@(StReg (StixMagicId id))
  = case (stgReg id) of
    	Always tree -> stixConFold tree
    	Save _      -> leaf

stixConFold other = other
\end{code}

Now, try to constant-fold the PrimOps.  The arguments have already
been optimized and folded.

\begin{code}
stixPrimFold
    :: PrimOp	    	-- The operation from an StPrim
    -> [StixTree]   	-- The optimized arguments
    -> StixTree

stixPrimFold op arg@[StInt x]
  = case op of
    	IntNegOp -> StInt (-x)
    	_ -> StPrim op arg

stixPrimFold op args@[StInt x, StInt y]
  = case op of
    	CharGtOp -> StInt (if x > y  then 1 else 0)
    	CharGeOp -> StInt (if x >= y then 1 else 0)
    	CharEqOp -> StInt (if x == y then 1 else 0)
    	CharNeOp -> StInt (if x /= y then 1 else 0)
    	CharLtOp -> StInt (if x < y  then 1 else 0)
    	CharLeOp -> StInt (if x <= y then 1 else 0)
    	IntAddOp -> StInt (x + y)
    	IntSubOp -> StInt (x - y)
    	IntMulOp -> StInt (x * y)
    	IntQuotOp -> StInt (x `quot` y)
    	IntRemOp -> StInt (x `rem` y)
    	IntGtOp -> StInt (if x > y  then 1 else 0)
    	IntGeOp -> StInt (if x >= y then 1 else 0)
    	IntEqOp -> StInt (if x == y then 1 else 0)
    	IntNeOp -> StInt (if x /= y then 1 else 0)
    	IntLtOp -> StInt (if x < y  then 1 else 0)
    	IntLeOp -> StInt (if x <= y then 1 else 0)
	-- ToDo: WordQuotOp, WordRemOp.
    	_ -> StPrim op args
\end{code}

When possible, shift the constants to the right-hand side, so that we
can match for strength reductions.  Note that the code generator will
also assume that constants have been shifted to the right when
possible.

\begin{code}
stixPrimFold op [x@(StInt _), y] | commutableOp op = stixPrimFold op [y, x]
\end{code}

We can often do something with constants of 0 and 1 ...

\begin{code}
stixPrimFold op args@[x, y@(StInt 0)]
  = case op of
    	IntAddOp -> x
    	IntSubOp -> x
    	IntMulOp -> y
    	AndOp  	 -> y
    	OrOp   	 -> x
    	XorOp  	 -> x
    	SllOp  	 -> x
    	SrlOp  	 -> x
    	ISllOp 	 -> x
    	ISraOp 	 -> x
    	ISrlOp 	 -> x
        IntNeOp  | is_comparison -> x
    	_	 -> StPrim op args
    where
       is_comparison
          = case x of
               StPrim opp [_, _] -> opp `elem` comparison_ops
               _                 -> False

stixPrimFold op args@[x, y@(StInt 1)]
  = case op of
    	IntMulOp  -> x
    	IntQuotOp -> x
    	IntRemOp  -> StInt 0
    	_	  -> StPrim op args
\end{code}

Now look for multiplication/division by powers of 2 (integers).

\begin{code}
stixPrimFold op args@[x, y@(StInt n)]
  = case op of
    	IntMulOp -> case exactLog2 n of
	    Nothing -> StPrim op args
    	    Just p  -> StPrim ISllOp [x, StInt p]
    	IntQuotOp -> case exactLog2 n of
	    Nothing -> StPrim op args
    	    Just p  -> StPrim ISrlOp [x, StInt p]
    	_ -> StPrim op args
\end{code}

Anything else is just too hard.

\begin{code}
stixPrimFold op args = StPrim op args
\end{code}

\begin{code}
comparison_ops
   = [ CharGtOp  , CharGeOp  , CharEqOp  , CharNeOp  , CharLtOp  , CharLeOp,
       IntGtOp   , IntGeOp   , IntEqOp   , IntNeOp   , IntLtOp   , IntLeOp,
       WordGtOp  , WordGeOp  , WordEqOp  , WordNeOp  , WordLtOp  , WordLeOp,
       AddrGtOp  , AddrGeOp  , AddrEqOp  , AddrNeOp  , AddrLtOp  , AddrLeOp,
       FloatGtOp , FloatGeOp , FloatEqOp , FloatNeOp , FloatLtOp , FloatLeOp,
       DoubleGtOp, DoubleGeOp, DoubleEqOp, DoubleNeOp, DoubleLtOp, DoubleLeOp
     ]
\end{code}