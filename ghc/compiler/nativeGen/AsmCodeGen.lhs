%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module AsmCodeGen ( nativeCodeGen ) where

#include "HsVersions.h"
#include "NCG.h"

import MachMisc
import MachRegs
import MachCode
import PprMach

import AbsCStixGen	( genCodeAbstractC )
import AbsCSyn		( AbstractC, MagicId(..) )
import AbsCUtils	( mkAbsCStmtList, magicIdPrimRep )
import AsmRegAlloc	( runRegAllocate )
import MachOp		( MachOp(..), isCommutableMachOp, isComparisonMachOp )
import RegAllocInfo	( findReservedRegs )
import Stix		( StixReg(..), StixStmt(..), StixExpr(..), StixVReg(..),
                          pprStixStmts, pprStixStmt, 
                          stixStmt_CountTempUses, stixStmt_Subst,
                          liftStrings,
                          initNat, 
                          mkNatM_State,
                          uniqOfNatM_State, deltaOfNatM_State,
			  importsOfNatM_State )
import UniqSupply	( returnUs, thenUs, initUs, 
                          UniqSM, UniqSupply,
			  lazyMapUs )
import MachMisc		( IF_ARCH_i386(i386_insert_ffrees,) )
#if darwin_TARGET_OS
import PprMach		( pprDyldSymbolStub )
import List		( group, sort )
#endif

import qualified Pretty
import Outputable
import FastString

-- DEBUGGING ONLY
--import OrdList

#ifdef NCG_DEBUG
import List		( intersperse )
#endif
\end{code}

The 96/03 native-code generator has machine-independent and
machine-dependent modules (those \tr{#include}'ing \tr{NCG.h}).

This module (@AsmCodeGen@) is the top-level machine-independent
module.  It uses @AbsCStixGen.genCodeAbstractC@ to produce @StixTree@s
(defined in module @Stix@), using support code from @StixPrim@
(primitive operations), @StixMacro@ (Abstract C macros), and
@StixInteger@ (GMP arbitrary-precision operations).

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
nativeCodeGen :: AbstractC -> UniqSupply -> (SDoc, Pretty.Doc)
nativeCodeGen absC us
   = let absCstmts         = mkAbsCStmtList absC
         (results, us1)    = initUs us (lazyMapUs absCtoNat absCstmts)
         stix_sdocs        = [ stix | (stix, insn, imports) <- results ]
         insn_sdocs        = [ insn | (stix, insn, imports) <- results ]
         imports           = [ imports | (stix, insn, imports) <- results ]

         insn_sdoc         = my_vcat insn_sdocs IF_OS_darwin(Pretty.$$ dyld_stubs,)
         stix_sdoc         = vcat stix_sdocs

#if darwin_TARGET_OS
	 -- Generate "symbol stubs" for all external symbols that might
	 -- come from a dynamic library.

         dyld_stubs         = Pretty.vcat $  map pprDyldSymbolStub $
					     map head $ group $ sort $ concat imports
#endif

#        ifdef NCG_DEBUG
         my_trace m x = trace m x
         my_vcat sds = Pretty.vcat (
                          intersperse (
                             Pretty.char ' ' 
                                Pretty.$$ Pretty.ptext SLIT("# ___ncg_debug_marker")
                                Pretty.$$ Pretty.char ' '
                          ) 
                          sds
                       )
#        else
         my_vcat sds = Pretty.vcat sds
         my_trace m x = x
#        endif
     in
         my_trace "nativeGen: begin"
                  (stix_sdoc, insn_sdoc)


absCtoNat :: AbstractC -> UniqSM (SDoc, Pretty.Doc, [FastString])
absCtoNat absC
   = _scc_ "genCodeAbstractC" genCodeAbstractC absC        `thenUs` \ stixRaw ->
     _scc_ "genericOpt"       genericOpt stixRaw           `bind`   \ stixOpt ->
     _scc_ "liftStrings"      liftStrings stixOpt          `thenUs` \ stixLifted ->
     _scc_ "genMachCode"      genMachCode stixLifted       `thenUs` \ (pre_regalloc, imports) ->
     _scc_ "regAlloc"         regAlloc pre_regalloc        `bind`   \ almost_final ->
     _scc_ "x86fp_kludge"     x86fp_kludge almost_final    `bind`   \ final_mach_code ->
     _scc_ "vcat"     Pretty.vcat (map pprInstr final_mach_code)  `bind`   \ final_sdoc ->
     _scc_ "pprStixTrees"     pprStixStmts stixOpt         `bind`   \ stix_sdoc ->
     returnUs ({-\_ -> Pretty.vcat (map pprInstr almost_final),-}
               stix_sdoc, final_sdoc, imports)
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
genMachCode :: [StixStmt] -> UniqSM (InstrBlock, [FastString])

genMachCode stmts initial_us
  = let initial_st             = mkNatM_State initial_us 0
        (instr_list, final_st) = initNat initial_st (stmtsToInstrs stmts)
        final_us               = uniqOfNatM_State final_st
        final_delta            = deltaOfNatM_State final_st
	final_imports          = importsOfNatM_State final_st
    in
        if   final_delta == 0
        then ((instr_list, final_imports), final_us)
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
genericOpt :: [StixStmt] -> [StixStmt]
genericOpt = map stixStmt_ConFold . stixPeep



stixPeep :: [StixStmt] -> [StixStmt]

-- This transformation assumes that the temp assigned to in t1
-- is not assigned to in t2; for otherwise the target of the
-- second assignment would be substituted for, giving nonsense
-- code.  As far as I can see, StixTemps are only ever assigned
-- to once.  It would be nice to be sure!

stixPeep ( t1@(StAssignReg pka (StixTemp (StixVReg u pk)) rhs)
         : t2
         : ts )
   | stixStmt_CountTempUses u t2 == 1
     && sum (map (stixStmt_CountTempUses u) ts) == 0
   = 
#    ifdef NCG_DEBUG
     trace ("nativeGen: inlining " ++ showSDoc (pprStixExpr rhs))
#    endif
           (stixPeep (stixStmt_Subst u rhs t2 : ts))

stixPeep (t1:t2:ts) = t1 : stixPeep (t2:ts)
stixPeep [t1]       = [t1]
stixPeep []         = []
\end{code}

For most nodes, just optimize the children.

\begin{code}
stixExpr_ConFold :: StixExpr -> StixExpr
stixStmt_ConFold :: StixStmt -> StixStmt

stixStmt_ConFold stmt
   = case stmt of
        StAssignReg pk reg@(StixTemp _) src
           -> StAssignReg pk reg (stixExpr_ConFold src)
        StAssignReg pk reg@(StixMagicId mid) src
           -- Replace register leaves with appropriate StixTrees for 
           -- the given target. MagicIds which map to a reg on this arch are left unchanged. 
           -- Assigning to BaseReg is always illegal, so we check for that.
           -> case mid of { 
                 BaseReg -> panic "stixStmt_ConFold: assignment to BaseReg";
                 other ->
                 case get_MagicId_reg_or_addr mid of
                    Left  realreg 
                       -> StAssignReg pk reg (stixExpr_ConFold src)
                    Right baseRegAddr 
                       -> stixStmt_ConFold (StAssignMem pk baseRegAddr src)
              }
        StAssignMem pk addr src
           -> StAssignMem pk (stixExpr_ConFold addr) (stixExpr_ConFold src)
        StVoidable expr
           -> StVoidable (stixExpr_ConFold expr)
        StJump dsts addr
           -> StJump dsts (stixExpr_ConFold addr)
        StCondJump addr test
           -> let test_opt = stixExpr_ConFold test
              in 
              if  manifestlyZero test_opt
              then StComment (mkFastString ("deleted: " ++ showSDoc (pprStixStmt stmt)))
              else StCondJump addr (stixExpr_ConFold test)
        StData pk datas
           -> StData pk (map stixExpr_ConFold datas)
        other
           -> other
     where
        manifestlyZero (StInt 0) = True
        manifestlyZero other     = False

stixExpr_ConFold expr
   = case expr of
        StInd pk addr
           -> StInd pk (stixExpr_ConFold addr)
        StCall fn cconv pk args
           -> StCall fn cconv pk (map stixExpr_ConFold args)
        StIndex pk (StIndex pk' base off) off'
           -- Fold indices together when the types match:
           |  pk == pk'
           -> StIndex pk (stixExpr_ConFold base)
                         (stixExpr_ConFold (StMachOp MO_Nat_Add [off, off']))
        StIndex pk base off
           -> StIndex pk (stixExpr_ConFold base) (stixExpr_ConFold off)

        StMachOp mop args
           -- For PrimOps, we first optimize the children, and then we try 
           -- our hand at some constant-folding.
           -> stixMachOpFold mop (map stixExpr_ConFold args)
        StReg (StixMagicId mid)
           -- Replace register leaves with appropriate StixTrees for 
           -- the given target.  MagicIds which map to a reg on this arch are left unchanged. 
           -- For the rest, BaseReg is taken to mean the address of the reg table 
           -- in MainCapability, and for all others we generate an indirection to 
           -- its location in the register table.
           -> case get_MagicId_reg_or_addr mid of
                 Left  realreg -> expr
                 Right baseRegAddr 
                    -> case mid of 
                          BaseReg -> stixExpr_ConFold baseRegAddr
                          other   -> stixExpr_ConFold (StInd (magicIdPrimRep mid) baseRegAddr)
        other
           -> other
\end{code}

Now, try to constant-fold the PrimOps.  The arguments have already
been optimized and folded.

\begin{code}
stixMachOpFold
    :: MachOp	    	-- The operation from an StMachOp
    -> [StixExpr]   	-- The optimized arguments
    -> StixExpr

stixMachOpFold mop arg@[StInt x]
  = case mop of
    	MO_NatS_Neg -> StInt (-x)
    	other       -> StMachOp mop arg

stixMachOpFold mop args@[StInt x, StInt y]
  = case mop of
    	MO_32U_Gt   -> StInt (if x > y  then 1 else 0)
    	MO_32U_Ge   -> StInt (if x >= y then 1 else 0)
    	MO_32U_Eq   -> StInt (if x == y then 1 else 0)
    	MO_32U_Ne   -> StInt (if x /= y then 1 else 0)
    	MO_32U_Lt   -> StInt (if x < y  then 1 else 0)
    	MO_32U_Le   -> StInt (if x <= y then 1 else 0)
    	MO_Nat_Add  -> StInt (x + y)
    	MO_Nat_Sub  -> StInt (x - y)
    	MO_NatS_Mul -> StInt (x * y)
    	MO_NatS_Quot | y /= 0 -> StInt (x `quot` y)
    	MO_NatS_Rem  | y /= 0 -> StInt (x `rem` y)
    	MO_NatS_Gt  -> StInt (if x > y  then 1 else 0)
    	MO_NatS_Ge  -> StInt (if x >= y then 1 else 0)
    	MO_Nat_Eq   -> StInt (if x == y then 1 else 0)
    	MO_Nat_Ne   -> StInt (if x /= y then 1 else 0)
    	MO_NatS_Lt  -> StInt (if x < y  then 1 else 0)
    	MO_NatS_Le  -> StInt (if x <= y then 1 else 0)
        MO_Nat_Shl  | y >= 0 && y < 32 -> do_shl x y
    	other       -> StMachOp mop args
    where
       do_shl :: Integer -> Integer -> StixExpr
       do_shl v 0         = StInt v
       do_shl v n | n > 0 = do_shl (v*2) (n-1)
\end{code}

When possible, shift the constants to the right-hand side, so that we
can match for strength reductions.  Note that the code generator will
also assume that constants have been shifted to the right when
possible.

\begin{code}
stixMachOpFold op [x@(StInt _), y] | isCommutableMachOp op 
   = stixMachOpFold op [y, x]
\end{code}

We can often do something with constants of 0 and 1 ...

\begin{code}
stixMachOpFold mop args@[x, y@(StInt 0)]
  = case mop of
    	MO_Nat_Add  -> x
    	MO_Nat_Sub  -> x
    	MO_NatS_Mul -> y
    	MO_NatU_Mul -> y
    	MO_Nat_And  -> y
    	MO_Nat_Or   -> x
    	MO_Nat_Xor  -> x
    	MO_Nat_Shl  -> x
    	MO_Nat_Shr  -> x
    	MO_Nat_Sar  -> x
        MO_Nat_Ne | x_is_comparison -> x
    	other       -> StMachOp mop args
    where
       x_is_comparison
          = case x of
               StMachOp mopp [_, _] -> isComparisonMachOp mopp
               _                    -> False

stixMachOpFold mop args@[x, y@(StInt 1)]
  = case mop of
    	MO_NatS_Mul  -> x
    	MO_NatU_Mul  -> x
    	MO_NatS_Quot -> x
    	MO_NatU_Quot -> x
    	MO_NatS_Rem  -> StInt 0
    	MO_NatU_Rem  -> StInt 0
    	other        -> StMachOp mop args
\end{code}

Now look for multiplication/division by powers of 2 (integers).

\begin{code}
stixMachOpFold mop args@[x, y@(StInt n)]
  = case mop of
    	MO_NatS_Mul 
           -> case exactLog2 n of
                 Nothing -> unchanged
                 Just p  -> StMachOp MO_Nat_Shl [x, StInt p]
    	MO_NatS_Quot 
           -> case exactLog2 n of
                 Nothing -> unchanged
                 Just p  -> StMachOp MO_Nat_Shr [x, StInt p]
    	other 
           -> unchanged
    where
       unchanged = StMachOp mop args
\end{code}

Anything else is just too hard.

\begin{code}
stixMachOpFold mop args = StMachOp mop args
\end{code}
