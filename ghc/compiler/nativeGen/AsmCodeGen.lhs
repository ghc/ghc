%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module AsmCodeGen ( writeRealAsm, dumpRealAsm ) where

#include "HsVersions.h"

import IO		( Handle )

import MachMisc
import MachRegs
import MachCode
import PprMach

import AbsCStixGen	( genCodeAbstractC )
import AbsCSyn		( AbstractC, MagicId )
import AsmRegAlloc	( runRegAllocate )
import OrdList		( OrdList )
import PrimOp		( commutableOp, PrimOp(..) )
import RegAllocInfo	( mkMRegsState, MRegsState )
import Stix		( StixTree(..), StixReg(..) )
import UniqSupply	( returnUs, thenUs, mapUs, initUs, UniqSM, UniqSupply )
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
writeRealAsm :: Handle -> AbstractC -> UniqSupply -> IO ()
writeRealAsm handle absC us
  = -- _scc_ "writeRealAsm" 
    printForAsm handle (initUs us (runNCG absC))

dumpRealAsm :: AbstractC -> UniqSupply -> SDoc
dumpRealAsm absC us = initUs us (runNCG absC)

runNCG absC
  = genCodeAbstractC absC	`thenUs` \ treelists ->
    let
	stix = map (map genericOpt) treelists
    in
    codeGen stix
\end{code}

@codeGen@ is the top-level code-generation function:
\begin{code}
codeGen :: [[StixTree]] -> UniqSM SDoc

codeGen trees
  = mapUs genMachCode trees	`thenUs` \ dynamic_codes ->
    let
	static_instrs = scheduleMachCode dynamic_codes
    in
    returnUs (vcat (map pprInstr static_instrs))
\end{code}

Top level code generator for a chunk of stix code:
\begin{code}
genMachCode :: [StixTree] -> UniqSM InstrList

genMachCode stmts
  = mapUs stmt2Instrs stmts    	    	`thenUs` \ blocks ->
    returnUs (foldr (.) id blocks asmVoid)
\end{code}

The next bit does the code scheduling.  The scheduler must also deal
with register allocation of temporaries.  Much parallelism can be
exposed via the OrdList, but more might occur, so further analysis
might be needed.

\begin{code}
scheduleMachCode :: [InstrList] -> [Instr]

scheduleMachCode
  = concat . map (runRegAllocate freeRegsState reservedRegs)
  where
    freeRegsState = mkMRegsState (extractMappedRegNos freeRegs)
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
genericOpt :: StixTree -> StixTree
\end{code}

For most nodes, just optimize the children.

\begin{code}
genericOpt (StInd pk addr) = StInd pk (genericOpt addr)

genericOpt (StAssign pk dst src)
  = StAssign pk (genericOpt dst) (genericOpt src)

genericOpt (StJump addr) = StJump (genericOpt addr)

genericOpt (StCondJump addr test)
  = StCondJump addr (genericOpt test)

genericOpt (StCall fn cconv pk args)
  = StCall fn cconv pk (map genericOpt args)
\end{code}

Fold indices together when the types match:
\begin{code}
genericOpt (StIndex pk (StIndex pk' base off) off')
  | pk == pk'
  = StIndex pk (genericOpt base)
    	       (genericOpt (StPrim IntAddOp [off, off']))

genericOpt (StIndex pk base off)
  = StIndex pk (genericOpt base) (genericOpt off)
\end{code}

For PrimOps, we first optimize the children, and then we try our hand
at some constant-folding.

\begin{code}
genericOpt (StPrim op args) = primOpt op (map genericOpt args)
\end{code}

Replace register leaves with appropriate StixTrees for the given
target.

\begin{code}
genericOpt leaf@(StReg (StixMagicId id))
  = case (stgReg id) of
    	Always tree -> genericOpt tree
    	Save _      -> leaf

genericOpt other = other
\end{code}

Now, try to constant-fold the PrimOps.  The arguments have already
been optimized and folded.

\begin{code}
primOpt
    :: PrimOp	    	-- The operation from an StPrim
    -> [StixTree]   	-- The optimized arguments
    -> StixTree

primOpt op arg@[StInt x]
  = case op of
    	IntNegOp -> StInt (-x)
    	IntAbsOp -> StInt (abs x)
    	_ -> StPrim op arg

primOpt op args@[StInt x, StInt y]
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
primOpt op [x@(StInt _), y] | commutableOp op = primOpt op [y, x]
\end{code}

We can often do something with constants of 0 and 1 ...

\begin{code}
primOpt op args@[x, y@(StInt 0)]
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
    	_	 -> StPrim op args

primOpt op args@[x, y@(StInt 1)]
  = case op of
    	IntMulOp  -> x
    	IntQuotOp -> x
    	IntRemOp  -> StInt 0
    	_	  -> StPrim op args
\end{code}

Now look for multiplication/division by powers of 2 (integers).

\begin{code}
primOpt op args@[x, y@(StInt n)]
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
primOpt op args = StPrim op args
\end{code}
