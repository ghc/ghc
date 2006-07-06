-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1993-2004
-- 
-- This is the top-level module in the native code generator.
--
-- -----------------------------------------------------------------------------

\begin{code}
module AsmCodeGen ( nativeCodeGen ) where

#include "HsVersions.h"
#include "NCG.h"

import MachInstrs
import MachRegs
import MachCodeGen
import PprMach
import RegisterAlloc
import RegAllocInfo	( jumpDests )
import NCGMonad
import PositionIndependentCode

import Cmm
import CmmOpt		( cmmMiniInline, cmmMachOpFold )
import PprCmm		( pprStmt, pprCmms )
import MachOp
import CLabel           ( CLabel, mkSplitMarkerLabel, mkAsmTempLabel )
#if powerpc_TARGET_ARCH
import CLabel           ( mkRtsCodeLabel )
#endif

import UniqFM
import Unique		( Unique, getUnique )
import UniqSupply
import FastTypes
import List		( groupBy, sortBy )
import CLabel           ( pprCLabel )
import ErrUtils		( dumpIfSet_dyn )
import DynFlags		( DynFlags, DynFlag(..), dopt )
import StaticFlags	( opt_Static, opt_PIC )

import Digraph
import qualified Pretty
import Outputable
import FastString

-- DEBUGGING ONLY
--import OrdList

#ifdef NCG_DEBUG
import List		( intersperse )
#endif

import DATA_INT
import DATA_WORD
import DATA_BITS
import GLAEXTS

{-
The native-code generator has machine-independent and
machine-dependent modules.

This module ("AsmCodeGen") is the top-level machine-independent
module.  Before entering machine-dependent land, we do some
machine-independent optimisations (defined below) on the
'CmmStmts's.

We convert to the machine-specific 'Instr' datatype with
'cmmCodeGen', assuming an infinite supply of registers.  We then use
a machine-independent register allocator ('regAlloc') to rejoin
reality.  Obviously, 'regAlloc' has machine-specific helper
functions (see about "RegAllocInfo" below).

Finally, we order the basic blocks of the function so as to minimise
the number of jumps between blocks, by utilising fallthrough wherever
possible.

The machine-dependent bits break down as follows:

  * ["MachRegs"]  Everything about the target platform's machine
    registers (and immediate operands, and addresses, which tend to
    intermingle/interact with registers).

  * ["MachInstrs"]  Includes the 'Instr' datatype (possibly should
    have a module of its own), plus a miscellany of other things
    (e.g., 'targetDoubleSize', 'smStablePtrTable', ...)

  * ["MachCodeGen"]  is where 'Cmm' stuff turns into
    machine instructions.

  * ["PprMach"] 'pprInstr' turns an 'Instr' into text (well, really
    a 'Doc').

  * ["RegAllocInfo"] In the register allocator, we manipulate
    'MRegsState's, which are 'BitSet's, one bit per machine register.
    When we want to say something about a specific machine register
    (e.g., ``it gets clobbered by this instruction''), we set/unset
    its bit.  Obviously, we do this 'BitSet' thing for efficiency
    reasons.

    The 'RegAllocInfo' module collects together the machine-specific
    info needed to do register allocation.

   * ["RegisterAlloc"] The (machine-independent) register allocator.
-}

-- -----------------------------------------------------------------------------
-- Top-level of the native codegen

-- NB. We *lazilly* compile each block of code for space reasons.

nativeCodeGen :: DynFlags -> [Cmm] -> UniqSupply -> IO Pretty.Doc
nativeCodeGen dflags cmms us
  = let (res, _) = initUs us $
	   cgCmm (concat (map add_split cmms))

	cgCmm :: [CmmTop] -> UniqSM (Cmm, Pretty.Doc, [CLabel])
	cgCmm tops = 
	   lazyMapUs (cmmNativeGen dflags) tops  `thenUs` \ results -> 
	   case unzip3 results of { (cmms,docs,imps) ->
	   returnUs (Cmm cmms, my_vcat docs, concat imps)
	   }
    in 
    case res of { (ppr_cmms, insn_sdoc, imports) -> do
    dumpIfSet_dyn dflags Opt_D_dump_opt_cmm "Optimised Cmm" (pprCmms [ppr_cmms])
    return (insn_sdoc Pretty.$$ dyld_stubs imports
#if HAVE_SUBSECTIONS_VIA_SYMBOLS
                -- On recent versions of Darwin, the linker supports
                -- dead-stripping of code and data on a per-symbol basis.
                -- There's a hack to make this work in PprMach.pprNatCmmTop.
            Pretty.$$ Pretty.text ".subsections_via_symbols"
#endif
#if HAVE_GNU_NONEXEC_STACK
                -- On recent GNU ELF systems one can mark an object file
                -- as not requiring an executable stack. If all objects
                -- linked into a program have this note then the program
                -- will not use an executable stack, which is good for
                -- security. GHC generated code does not need an executable
                -- stack so add the note in:
            Pretty.$$ Pretty.text ".section .note.GNU-stack,\"\",@progbits"
#endif
            )
   }

  where

    add_split (Cmm tops)
	| dopt Opt_SplitObjs dflags = split_marker : tops
	| otherwise		    = tops

    split_marker = CmmProc [] mkSplitMarkerLabel [] []

     	 -- Generate "symbol stubs" for all external symbols that might
	 -- come from a dynamic library.
{-    dyld_stubs imps = Pretty.vcat $ map pprDyldSymbolStub $
				    map head $ group $ sort imps-}
				    
	-- (Hack) sometimes two Labels pretty-print the same, but have
	-- different uniques; so we compare their text versions...
    dyld_stubs imps 
        | needImportedSymbols
          = Pretty.vcat $
            (pprGotDeclaration :) $
            map (pprImportedSymbol . fst . head) $
            groupBy (\(_,a) (_,b) -> a == b) $
            sortBy (\(_,a) (_,b) -> compare a b) $
            map doPpr $
            imps
        | otherwise
          = Pretty.empty
        
        where doPpr lbl = (lbl, Pretty.render $ pprCLabel lbl astyle)
              astyle = mkCodeStyle AsmStyle

#ifndef NCG_DEBUG
    my_vcat sds = Pretty.vcat sds
#else
    my_vcat sds = Pretty.vcat (
                      intersperse (
                         Pretty.char ' ' 
                            Pretty.$$ Pretty.ptext SLIT("# ___ncg_debug_marker")
                            Pretty.$$ Pretty.char ' '
                      ) 
                      sds
                   )
#endif


-- Complete native code generation phase for a single top-level chunk
-- of Cmm.

cmmNativeGen :: DynFlags -> CmmTop -> UniqSM (CmmTop, Pretty.Doc, [CLabel])
cmmNativeGen dflags cmm
   = {-# SCC "fixAssigns"       #-} 
 	fixAssignsTop cmm	     `thenUs` \ fixed_cmm ->
     {-# SCC "genericOpt"       #-} 
	cmmToCmm fixed_cmm           `bind`   \ (cmm, imports) ->
        (if dopt Opt_D_dump_opt_cmm dflags  -- space leak avoidance
	   then cmm 
	   else CmmData Text [])     `bind`   \ ppr_cmm ->
     {-# SCC "genMachCode"      #-}
	genMachCode cmm              `thenUs` \ (pre_regalloc, lastMinuteImports) ->
     {-# SCC "regAlloc"         #-}
	mapUs regAlloc pre_regalloc `thenUs`   \ with_regs ->
     {-# SCC "sequenceBlocks"   #-}
	map sequenceTop with_regs    `bind`   \ sequenced ->
     {-# SCC "x86fp_kludge"     #-}
	map x86fp_kludge sequenced   `bind`   \ final_mach_code ->
     {-# SCC "vcat"             #-}
	Pretty.vcat (map pprNatCmmTop final_mach_code)  `bind`   \ final_sdoc ->

        returnUs (ppr_cmm, final_sdoc Pretty.$$ Pretty.text "", lastMinuteImports ++ imports)
     where
        x86fp_kludge :: NatCmmTop -> NatCmmTop
        x86fp_kludge top@(CmmData _ _) = top
#if i386_TARGET_ARCH
        x86fp_kludge top@(CmmProc info lbl params code) = 
		CmmProc info lbl params (map bb_i386_insert_ffrees code)
		where
		  bb_i386_insert_ffrees (BasicBlock id instrs) =
			BasicBlock id (i386_insert_ffrees instrs)
#else
        x86fp_kludge top =  top
#endif

-- -----------------------------------------------------------------------------
-- Sequencing the basic blocks

-- Cmm BasicBlocks are self-contained entities: they always end in a
-- jump, either non-local or to another basic block in the same proc.
-- In this phase, we attempt to place the basic blocks in a sequence
-- such that as many of the local jumps as possible turn into
-- fallthroughs.

sequenceTop :: NatCmmTop -> NatCmmTop
sequenceTop top@(CmmData _ _) = top
sequenceTop (CmmProc info lbl params blocks) = 
  CmmProc info lbl params (sequenceBlocks blocks)

-- The algorithm is very simple (and stupid): we make a graph out of
-- the blocks where there is an edge from one block to another iff the
-- first block ends by jumping to the second.  Then we topologically
-- sort this graph.  Then traverse the list: for each block, we first
-- output the block, then if it has an out edge, we move the
-- destination of the out edge to the front of the list, and continue.

sequenceBlocks :: [NatBasicBlock] -> [NatBasicBlock]
sequenceBlocks [] = []
sequenceBlocks (entry:blocks) = 
  seqBlocks (mkNode entry : reverse (flattenSCCs (sccBlocks blocks)))
  -- the first block is the entry point ==> it must remain at the start.

sccBlocks :: [NatBasicBlock] -> [SCC (NatBasicBlock,Unique,[Unique])]
sccBlocks blocks = stronglyConnCompR (map mkNode blocks)

getOutEdges :: [Instr] -> [Unique]
getOutEdges instrs = case jumpDests (last instrs) [] of
			[one] -> [getUnique one]
			_many -> []
		-- we're only interested in the last instruction of
		-- the block, and only if it has a single destination.

mkNode block@(BasicBlock id instrs) = (block, getUnique id, getOutEdges instrs)

seqBlocks [] = []
seqBlocks ((block,_,[]) : rest)
  = block : seqBlocks rest
seqBlocks ((block@(BasicBlock id instrs),_,[next]) : rest)
  | can_fallthrough = BasicBlock id (init instrs) : seqBlocks rest'
  | otherwise       = block : seqBlocks rest'
  where
	(can_fallthrough, rest') = reorder next [] rest
	  -- TODO: we should do a better job for cycles; try to maximise the
	  -- fallthroughs within a loop.
seqBlocks _ = panic "AsmCodegen:seqBlocks"

reorder id accum [] = (False, reverse accum)
reorder id accum (b@(block,id',out) : rest)
  | id == id'  = (True, (block,id,out) : reverse accum ++ rest)
  | otherwise  = reorder id (b:accum) rest

-- -----------------------------------------------------------------------------
-- Instruction selection

-- Native code instruction selection for a chunk of stix code.  For
-- this part of the computation, we switch from the UniqSM monad to
-- the NatM monad.  The latter carries not only a Unique, but also an
-- Int denoting the current C stack pointer offset in the generated
-- code; this is needed for creating correct spill offsets on
-- architectures which don't offer, or for which it would be
-- prohibitively expensive to employ, a frame pointer register.  Viz,
-- x86.

-- The offset is measured in bytes, and indicates the difference
-- between the current (simulated) C stack-ptr and the value it was at
-- the beginning of the block.  For stacks which grow down, this value
-- should be either zero or negative.

-- Switching between the two monads whilst carrying along the same
-- Unique supply breaks abstraction.  Is that bad?

genMachCode :: CmmTop -> UniqSM ([NatCmmTop], [CLabel])

genMachCode cmm_top initial_us
  = let initial_st             = mkNatM_State initial_us 0
        (new_tops, final_st)   = initNat initial_st (cmmTopCodeGen cmm_top)
        final_us               = natm_us final_st
        final_delta            = natm_delta final_st
	final_imports          = natm_imports final_st
    in
        if   final_delta == 0
        then ((new_tops, final_imports), final_us)
        else pprPanic "genMachCode: nonzero final delta"
                      (int final_delta)

-- -----------------------------------------------------------------------------
-- Fixup assignments to global registers so that they assign to 
-- locations within the RegTable, if appropriate.

-- Note that we currently don't fixup reads here: they're done by
-- the generic optimiser below, to avoid having two separate passes
-- over the Cmm.

fixAssignsTop :: CmmTop -> UniqSM CmmTop
fixAssignsTop top@(CmmData _ _) = returnUs top
fixAssignsTop (CmmProc info lbl params blocks) =
  mapUs fixAssignsBlock blocks `thenUs` \ blocks' ->
  returnUs (CmmProc info lbl params blocks')

fixAssignsBlock :: CmmBasicBlock -> UniqSM CmmBasicBlock
fixAssignsBlock (BasicBlock id stmts) =
  fixAssigns stmts `thenUs` \ stmts' ->
  returnUs (BasicBlock id stmts')

fixAssigns :: [CmmStmt] -> UniqSM [CmmStmt]
fixAssigns stmts =
  mapUs fixAssign stmts `thenUs` \ stmtss ->
  returnUs (concat stmtss)

fixAssign :: CmmStmt -> UniqSM [CmmStmt]
fixAssign (CmmAssign (CmmGlobal BaseReg) src)
   = panic "cmmStmtConFold: assignment to BaseReg";

fixAssign (CmmAssign (CmmGlobal reg) src)
  | Left  realreg <- reg_or_addr
  = returnUs [CmmAssign (CmmGlobal reg) src]
  | Right baseRegAddr <- reg_or_addr
  = returnUs [CmmStore baseRegAddr src]
           -- Replace register leaves with appropriate StixTrees for
           -- the given target. GlobalRegs which map to a reg on this
           -- arch are left unchanged.  Assigning to BaseReg is always
           -- illegal, so we check for that.
  where
	reg_or_addr = get_GlobalReg_reg_or_addr reg

fixAssign (CmmCall target results args vols)
  = mapAndUnzipUs fixResult results `thenUs` \ (results',stores) ->
    returnUs (caller_save ++
	      CmmCall target results' args vols :
	      caller_restore ++
	      concat stores)
  where
	-- we also save/restore any caller-saves STG registers here
	(caller_save, caller_restore) = callerSaveVolatileRegs vols

	fixResult g@(CmmGlobal reg,hint) = 
	  case get_GlobalReg_reg_or_addr reg of
		Left realreg -> returnUs (g, [])
		Right baseRegAddr ->
		    getUniqueUs `thenUs` \ uq ->
		    let local = CmmLocal (LocalReg uq (globalRegRep reg)) in
		    returnUs ((local,hint), 
			      [CmmStore baseRegAddr (CmmReg local)])
	fixResult other =
	  returnUs (other,[])

fixAssign other_stmt = returnUs [other_stmt]

-- -----------------------------------------------------------------------------
-- Generic Cmm optimiser

{-
Here we do:

  (a) Constant folding
  (b) Simple inlining: a temporary which is assigned to and then
      used, once, can be shorted.
  (c) Replacement of references to GlobalRegs which do not have
      machine registers by the appropriate memory load (eg.
      Hp ==>  *(BaseReg + 34) ).
  (d) Position independent code and dynamic linking
        (i)  introduce the appropriate indirections
             and position independent refs
        (ii) compile a list of imported symbols

Ideas for other things we could do (ToDo):

  - shortcut jumps-to-jumps
  - eliminate dead code blocks
  - simple CSE: if an expr is assigned to a temp, then replace later occs of
    that expr with the temp, until the expr is no longer valid (can push through
    temp assignments, and certain assigns to mem...)
-}

cmmToCmm :: CmmTop -> (CmmTop, [CLabel])
cmmToCmm top@(CmmData _ _) = (top, [])
cmmToCmm (CmmProc info lbl params blocks) = runCmmOpt $ do
  blocks' <- mapM cmmBlockConFold (cmmMiniInline blocks)
  return $ CmmProc info lbl params blocks'

newtype CmmOptM a = CmmOptM ([CLabel] -> (# a, [CLabel] #))

instance Monad CmmOptM where
  return x = CmmOptM $ \imports -> (# x,imports #)
  (CmmOptM f) >>= g =
    CmmOptM $ \imports ->
                case f imports of
                  (# x, imports' #) ->
                    case g x of
                      CmmOptM g' -> g' imports'

addImportCmmOpt :: CLabel -> CmmOptM ()
addImportCmmOpt lbl = CmmOptM $ \imports -> (# (), lbl:imports #)

runCmmOpt :: CmmOptM a -> (a, [CLabel])
runCmmOpt (CmmOptM f) = case f [] of
                        (# result, imports #) -> (result, imports)

cmmBlockConFold :: CmmBasicBlock -> CmmOptM CmmBasicBlock
cmmBlockConFold (BasicBlock id stmts) = do
  stmts' <- mapM cmmStmtConFold stmts
  return $ BasicBlock id stmts'

cmmStmtConFold stmt
   = case stmt of
        CmmAssign reg src
           -> do src' <- cmmExprConFold False src
                 return $ case src' of
		   CmmReg reg' | reg == reg' -> CmmNop
		   new_src -> CmmAssign reg new_src

        CmmStore addr src
           -> do addr' <- cmmExprConFold False addr
                 src'  <- cmmExprConFold False src
                 return $ CmmStore addr' src'

        CmmJump addr regs
           -> do addr' <- cmmExprConFold True addr
                 return $ CmmJump addr' regs

	CmmCall target regs args vols
	   -> do target' <- case target of
			      CmmForeignCall e conv -> do
			        e' <- cmmExprConFold True e
			        return $ CmmForeignCall e' conv
			      other -> return other
                 args' <- mapM (\(arg, hint) -> do
                                  arg' <- cmmExprConFold False arg
                                  return (arg', hint)) args
	         return $ CmmCall target' regs args' vols

        CmmCondBranch test dest
           -> do test' <- cmmExprConFold False test
	         return $ case test' of
		   CmmLit (CmmInt 0 _) -> 
		     CmmComment (mkFastString ("deleted: " ++ 
					showSDoc (pprStmt stmt)))

		   CmmLit (CmmInt n _) -> CmmBranch dest
		   other -> CmmCondBranch test' dest

	CmmSwitch expr ids
	   -> do expr' <- cmmExprConFold False expr
	         return $ CmmSwitch expr' ids

        other
           -> return other


cmmExprConFold isJumpTarget expr
   = case expr of
        CmmLoad addr rep
           -> do addr' <- cmmExprConFold False addr
                 return $ CmmLoad addr' rep

        CmmMachOp mop args
           -- For MachOps, we first optimize the children, and then we try 
           -- our hand at some constant-folding.
           -> do args' <- mapM (cmmExprConFold False) args
                 return $ cmmMachOpFold mop args'

        CmmLit (CmmLabel lbl)
           -> cmmMakeDynamicReference addImportCmmOpt isJumpTarget lbl
        CmmLit (CmmLabelOff lbl off)
           -> do dynRef <- cmmMakeDynamicReference addImportCmmOpt isJumpTarget lbl
                 return $ cmmMachOpFold (MO_Add wordRep) [
                     dynRef,
                     (CmmLit $ CmmInt (fromIntegral off) wordRep)
                   ]

#if powerpc_TARGET_ARCH
           -- On powerpc (non-PIC), it's easier to jump directly to a label than
           -- to use the register table, so we replace these registers
           -- with the corresponding labels:
        CmmReg (CmmGlobal GCEnter1)
          | not opt_PIC
          -> cmmExprConFold isJumpTarget $
             CmmLit (CmmLabel (mkRtsCodeLabel SLIT( "__stg_gc_enter_1"))) 
        CmmReg (CmmGlobal GCFun)
          | not opt_PIC
          -> cmmExprConFold isJumpTarget $
             CmmLit (CmmLabel (mkRtsCodeLabel SLIT( "__stg_gc_fun")))
#endif

        CmmReg (CmmGlobal mid)
           -- Replace register leaves with appropriate StixTrees for
           -- the given target.  MagicIds which map to a reg on this
           -- arch are left unchanged.  For the rest, BaseReg is taken
           -- to mean the address of the reg table in MainCapability,
           -- and for all others we generate an indirection to its
           -- location in the register table.
           -> case get_GlobalReg_reg_or_addr mid of
                 Left  realreg -> return expr
                 Right baseRegAddr 
                    -> case mid of 
                          BaseReg -> cmmExprConFold False baseRegAddr
                          other   -> cmmExprConFold False (CmmLoad baseRegAddr 
							(globalRegRep mid))
	   -- eliminate zero offsets
	CmmRegOff reg 0
	   -> cmmExprConFold False (CmmReg reg)

        CmmRegOff (CmmGlobal mid) offset
           -- RegOf leaves are just a shorthand form. If the reg maps
           -- to a real reg, we keep the shorthand, otherwise, we just
           -- expand it and defer to the above code. 
           -> case get_GlobalReg_reg_or_addr mid of
                Left  realreg -> return expr
                Right baseRegAddr
                   -> cmmExprConFold False (CmmMachOp (MO_Add wordRep) [
                                        CmmReg (CmmGlobal mid),
                                        CmmLit (CmmInt (fromIntegral offset)
                                                       wordRep)])
        other
           -> return other

-- -----------------------------------------------------------------------------
-- Utils

bind f x = x $! f

\end{code}

