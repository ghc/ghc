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
	map regAlloc pre_regalloc    `bind`   \ with_regs ->
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
    returnUs (CmmCall target results' args vols : concat stores)
  where
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
  blocks' <- mapM cmmBlockConFold (cmmPeep blocks)
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
-- MachOp constant folder

-- Now, try to constant-fold the MachOps.  The arguments have already
-- been optimized and folded.

cmmMachOpFold
    :: MachOp	    	-- The operation from an CmmMachOp
    -> [CmmExpr]   	-- The optimized arguments
    -> CmmExpr

cmmMachOpFold op arg@[CmmLit (CmmInt x rep)]
  = case op of
      MO_S_Neg r -> CmmLit (CmmInt (-x) rep)
      MO_Not r   -> CmmLit (CmmInt (complement x) rep)

	-- these are interesting: we must first narrow to the 
	-- "from" type, in order to truncate to the correct size.
	-- The final narrow/widen to the destination type
	-- is implicit in the CmmLit.
      MO_S_Conv from to
	   | isFloatingRep to -> CmmLit (CmmFloat (fromInteger x) to)
	   | otherwise        -> CmmLit (CmmInt (narrowS from x) to)
      MO_U_Conv from to -> CmmLit (CmmInt (narrowU from x) to)

      _ -> panic "cmmMachOpFold: unknown unary op"


-- Eliminate conversion NOPs
cmmMachOpFold (MO_S_Conv rep1 rep2) [x] | rep1 == rep2 = x
cmmMachOpFold (MO_U_Conv rep1 rep2) [x] | rep1 == rep2 = x

-- Eliminate nested conversions where possible
cmmMachOpFold conv_outer args@[CmmMachOp conv_inner [x]]
  | Just (rep1,rep2,signed1) <- isIntConversion conv_inner,
    Just (_,   rep3,signed2) <- isIntConversion conv_outer
  = case () of
	-- widen then narrow to the same size is a nop
      _ | rep1 < rep2 && rep1 == rep3 -> x
	-- Widen then narrow to different size: collapse to single conversion
	-- but remember to use the signedness from the widening, just in case
	-- the final conversion is a widen.
	| rep1 < rep2 && rep2 > rep3 ->
	    cmmMachOpFold (intconv signed1 rep1 rep3) [x]
	-- Nested widenings: collapse if the signedness is the same
	| rep1 < rep2 && rep2 < rep3 && signed1 == signed2 ->
	    cmmMachOpFold (intconv signed1 rep1 rep3) [x]
	-- Nested narrowings: collapse
	| rep1 > rep2 && rep2 > rep3 ->
	    cmmMachOpFold (MO_U_Conv rep1 rep3) [x]
	| otherwise ->
	    CmmMachOp conv_outer args
  where
	isIntConversion (MO_U_Conv rep1 rep2) 
	  | not (isFloatingRep rep1) && not (isFloatingRep rep2) 
	  = Just (rep1,rep2,False)
	isIntConversion (MO_S_Conv rep1 rep2)
	  | not (isFloatingRep rep1) && not (isFloatingRep rep2) 
	  = Just (rep1,rep2,True)
	isIntConversion _ = Nothing

	intconv True  = MO_S_Conv
	intconv False = MO_U_Conv

-- ToDo: a narrow of a load can be collapsed into a narrow load, right?
-- but what if the architecture only supports word-sized loads, should
-- we do the transformation anyway?

cmmMachOpFold mop args@[CmmLit (CmmInt x xrep), CmmLit (CmmInt y _)]
  = case mop of
	-- for comparisons: don't forget to narrow the arguments before
	-- comparing, since they might be out of range.
    	MO_Eq r   -> CmmLit (CmmInt (if x_u == y_u then 1 else 0) wordRep)
    	MO_Ne r   -> CmmLit (CmmInt (if x_u /= y_u then 1 else 0) wordRep)

    	MO_U_Gt r -> CmmLit (CmmInt (if x_u >  y_u then 1 else 0) wordRep)
    	MO_U_Ge r -> CmmLit (CmmInt (if x_u >= y_u then 1 else 0) wordRep)
    	MO_U_Lt r -> CmmLit (CmmInt (if x_u <  y_u then 1 else 0) wordRep)
    	MO_U_Le r -> CmmLit (CmmInt (if x_u <= y_u then 1 else 0) wordRep)

    	MO_S_Gt r -> CmmLit (CmmInt (if x_s >  y_s then 1 else 0) wordRep) 
    	MO_S_Ge r -> CmmLit (CmmInt (if x_s >= y_s then 1 else 0) wordRep)
    	MO_S_Lt r -> CmmLit (CmmInt (if x_s <  y_s then 1 else 0) wordRep)
    	MO_S_Le r -> CmmLit (CmmInt (if x_s <= y_s then 1 else 0) wordRep)

    	MO_Add r -> CmmLit (CmmInt (x + y) r)
    	MO_Sub r -> CmmLit (CmmInt (x - y) r)
    	MO_Mul r -> CmmLit (CmmInt (x * y) r)
    	MO_S_Quot r | y /= 0 -> CmmLit (CmmInt (x `quot` y) r)
    	MO_S_Rem  r | y /= 0 -> CmmLit (CmmInt (x `rem` y) r)

	MO_And   r -> CmmLit (CmmInt (x .&. y) r)
	MO_Or    r -> CmmLit (CmmInt (x .|. y) r)
	MO_Xor   r -> CmmLit (CmmInt (x `xor` y) r)

        MO_Shl   r -> CmmLit (CmmInt (x `shiftL` fromIntegral y) r)
        MO_U_Shr r -> CmmLit (CmmInt (x_u `shiftR` fromIntegral y) r)
        MO_S_Shr r -> CmmLit (CmmInt (x `shiftR` fromIntegral y) r)

	other      -> CmmMachOp mop args

   where
	x_u = narrowU xrep x
	y_u = narrowU xrep y
	x_s = narrowS xrep x
	y_s = narrowS xrep y
	

-- When possible, shift the constants to the right-hand side, so that we
-- can match for strength reductions.  Note that the code generator will
-- also assume that constants have been shifted to the right when
-- possible.

cmmMachOpFold op [x@(CmmLit _), y]
   | not (isLit y) && isCommutableMachOp op 
   = cmmMachOpFold op [y, x]

-- Turn (a+b)+c into a+(b+c) where possible.  Because literals are
-- moved to the right, it is more likely that we will find
-- opportunities for constant folding when the expression is
-- right-associated.
--
-- ToDo: this appears to introduce a quadratic behaviour due to the
-- nested cmmMachOpFold.  Can we fix this?
--
-- Why do we check isLit arg1?  If arg1 is a lit, it means that arg2
-- is also a lit (otherwise arg1 would be on the right).  If we
-- put arg1 on the left of the rearranged expression, we'll get into a
-- loop:  (x1+x2)+x3 => x1+(x2+x3)  => (x2+x3)+x1 => x2+(x3+x1) ...
--
cmmMachOpFold mop1 [CmmMachOp mop2 [arg1,arg2], arg3]
   | mop1 == mop2 && isAssociativeMachOp mop1 && not (isLit arg1)
   = cmmMachOpFold mop1 [arg1, cmmMachOpFold mop2 [arg2,arg3]]

-- Make a RegOff if we can
cmmMachOpFold (MO_Add _) [CmmReg reg, CmmLit (CmmInt n rep)]
  = CmmRegOff reg (fromIntegral (narrowS rep n))
cmmMachOpFold (MO_Add _) [CmmRegOff reg off, CmmLit (CmmInt n rep)]
  = CmmRegOff reg (off + fromIntegral (narrowS rep n))
cmmMachOpFold (MO_Sub _) [CmmReg reg, CmmLit (CmmInt n rep)]
  = CmmRegOff reg (- fromIntegral (narrowS rep n))
cmmMachOpFold (MO_Sub _) [CmmRegOff reg off, CmmLit (CmmInt n rep)]
  = CmmRegOff reg (off - fromIntegral (narrowS rep n))

-- Fold label(+/-)offset into a CmmLit where possible

cmmMachOpFold (MO_Add _) [CmmLit (CmmLabel lbl), CmmLit (CmmInt i rep)]
  = CmmLit (CmmLabelOff lbl (fromIntegral (narrowU rep i)))
cmmMachOpFold (MO_Add _) [CmmLit (CmmInt i rep), CmmLit (CmmLabel lbl)]
  = CmmLit (CmmLabelOff lbl (fromIntegral (narrowU rep i)))
cmmMachOpFold (MO_Sub _) [CmmLit (CmmLabel lbl), CmmLit (CmmInt i rep)]
  = CmmLit (CmmLabelOff lbl (fromIntegral (negate (narrowU rep i))))

-- We can often do something with constants of 0 and 1 ...

cmmMachOpFold mop args@[x, y@(CmmLit (CmmInt 0 _))]
  = case mop of
    	MO_Add   r -> x
    	MO_Sub   r -> x
    	MO_Mul   r -> y
    	MO_And   r -> y
    	MO_Or    r -> x
    	MO_Xor   r -> x
    	MO_Shl   r -> x
    	MO_S_Shr r -> x
    	MO_U_Shr r -> x
        MO_Ne    r | isComparisonExpr x -> x
	MO_Eq    r | Just x' <- maybeInvertConditionalExpr x -> x'
	MO_U_Gt  r | isComparisonExpr x -> x
	MO_S_Gt  r | isComparisonExpr x -> x
	MO_U_Lt  r | isComparisonExpr x -> CmmLit (CmmInt 0 wordRep)
	MO_S_Lt  r | isComparisonExpr x -> CmmLit (CmmInt 0 wordRep)
	MO_U_Ge  r | isComparisonExpr x -> CmmLit (CmmInt 1 wordRep)
	MO_S_Ge  r | isComparisonExpr x -> CmmLit (CmmInt 1 wordRep)
	MO_U_Le  r | Just x' <- maybeInvertConditionalExpr x -> x'
	MO_S_Le  r | Just x' <- maybeInvertConditionalExpr x -> x'
    	other    -> CmmMachOp mop args

cmmMachOpFold mop args@[x, y@(CmmLit (CmmInt 1 rep))]
  = case mop of
    	MO_Mul    r -> x
    	MO_S_Quot r -> x
    	MO_U_Quot r -> x
    	MO_S_Rem  r -> CmmLit (CmmInt 0 rep)
    	MO_U_Rem  r -> CmmLit (CmmInt 0 rep)
        MO_Ne    r | Just x' <- maybeInvertConditionalExpr x -> x'
	MO_Eq    r | isComparisonExpr x -> x
	MO_U_Lt  r | Just x' <- maybeInvertConditionalExpr x -> x'
	MO_S_Lt  r | Just x' <- maybeInvertConditionalExpr x -> x'
	MO_U_Gt  r | isComparisonExpr x -> CmmLit (CmmInt 0 wordRep)
	MO_S_Gt  r | isComparisonExpr x -> CmmLit (CmmInt 0 wordRep)
	MO_U_Le  r | isComparisonExpr x -> CmmLit (CmmInt 1 wordRep)
	MO_S_Le  r | isComparisonExpr x -> CmmLit (CmmInt 1 wordRep)
	MO_U_Ge  r | isComparisonExpr x -> x
	MO_S_Ge  r | isComparisonExpr x -> x
    	other       -> CmmMachOp mop args

-- Now look for multiplication/division by powers of 2 (integers).

cmmMachOpFold mop args@[x, y@(CmmLit (CmmInt n _))]
  = case mop of
    	MO_Mul rep
           -> case exactLog2 n of
                 Nothing -> unchanged
                 Just p  -> CmmMachOp (MO_Shl rep) [x, CmmLit (CmmInt p rep)]
    	MO_S_Quot rep
           -> case exactLog2 n of
                 Nothing -> unchanged
                 Just p  -> CmmMachOp (MO_S_Shr rep) [x, CmmLit (CmmInt p rep)]
    	other 
           -> unchanged
    where
       unchanged = CmmMachOp mop args

-- Anything else is just too hard.

cmmMachOpFold mop args = CmmMachOp mop args

-- -----------------------------------------------------------------------------
-- exactLog2

-- This algorithm for determining the $\log_2$ of exact powers of 2 comes
-- from GCC.  It requires bit manipulation primitives, and we use GHC
-- extensions.  Tough.
-- 
-- Used to be in MachInstrs --SDM.
-- ToDo: remove use of unboxery --SDM.

w2i x = word2Int# x
i2w x = int2Word# x

exactLog2 :: Integer -> Maybe Integer
exactLog2 x
  = if (x <= 0 || x >= 2147483648) then
       Nothing
    else
       case iUnbox (fromInteger x) of { x# ->
       if (w2i ((i2w x#) `and#` (i2w (0# -# x#))) /=# x#) then
	  Nothing
       else
	  Just (toInteger (iBox (pow2 x#)))
       }
  where
    pow2 x# | x# ==# 1# = 0#
            | otherwise = 1# +# pow2 (w2i (i2w x# `shiftRL#` 1#))


-- -----------------------------------------------------------------------------
-- widening / narrowing

narrowU :: MachRep -> Integer -> Integer
narrowU I8  x = fromIntegral (fromIntegral x :: Word8)
narrowU I16 x = fromIntegral (fromIntegral x :: Word16)
narrowU I32 x = fromIntegral (fromIntegral x :: Word32)
narrowU I64 x = fromIntegral (fromIntegral x :: Word64)
narrowU _ _ = panic "narrowTo"

narrowS :: MachRep -> Integer -> Integer
narrowS I8  x = fromIntegral (fromIntegral x :: Int8)
narrowS I16 x = fromIntegral (fromIntegral x :: Int16)
narrowS I32 x = fromIntegral (fromIntegral x :: Int32)
narrowS I64 x = fromIntegral (fromIntegral x :: Int64)
narrowS _ _ = panic "narrowTo"

-- -----------------------------------------------------------------------------
-- The mini-inliner

-- This pass inlines assignments to temporaries that are used just
-- once in the very next statement only.  Generalising this would be
-- quite difficult (have to take into account aliasing of memory
-- writes, and so on), but at the moment it catches a number of useful
-- cases and lets the code generator generate much better code.

-- NB. This assumes that temporaries are single-assignment.

cmmPeep :: [CmmBasicBlock] -> [CmmBasicBlock]
cmmPeep blocks = map do_inline blocks 
  where 
	blockUses (BasicBlock _ stmts)
	 = foldr (plusUFM_C (+)) emptyUFM (map getStmtUses stmts)

	uses = foldr (plusUFM_C (+)) emptyUFM (map blockUses blocks)

	do_inline (BasicBlock id stmts)
	 = BasicBlock id (cmmMiniInline uses stmts)


cmmMiniInline :: UniqFM Int -> [CmmStmt] -> [CmmStmt]
cmmMiniInline uses [] = []
cmmMiniInline uses (stmt@(CmmAssign (CmmLocal (LocalReg u _)) expr) : stmts)
  | Just 1 <- lookupUFM uses u,
    Just stmts' <- lookForInline u expr stmts
  = 
#ifdef NCG_DEBUG
     trace ("nativeGen: inlining " ++ showSDoc (pprStmt stmt)) $
#endif
     cmmMiniInline uses stmts'

cmmMiniInline uses (stmt:stmts)
  = stmt : cmmMiniInline uses stmts


-- Try to inline a temporary assignment.  We can skip over assignments to
-- other tempoararies, because we know that expressions aren't side-effecting
-- and temporaries are single-assignment.
lookForInline u expr (stmt@(CmmAssign (CmmLocal (LocalReg u' _)) rhs) : rest)
  | u /= u' 
  = case lookupUFM (getExprUses rhs) u of
	Just 1 -> Just (inlineStmt u expr stmt : rest)
	_other -> case lookForInline u expr rest of
		     Nothing    -> Nothing
		     Just stmts -> Just (stmt:stmts)

lookForInline u expr (CmmNop : rest)
  = lookForInline u expr rest

lookForInline u expr (stmt:stmts)
  = case lookupUFM (getStmtUses stmt) u of
	Just 1 -> Just (inlineStmt u expr stmt : stmts)
	_other -> Nothing

-- -----------------------------------------------------------------------------
-- Boring Cmm traversals for collecting usage info and substitutions.

getStmtUses :: CmmStmt -> UniqFM Int
getStmtUses (CmmAssign _ e) = getExprUses e
getStmtUses (CmmStore e1 e2) = plusUFM_C (+) (getExprUses e1) (getExprUses e2)
getStmtUses (CmmCall target _ es _)
   = plusUFM_C (+) (uses target) (getExprsUses (map fst es))
   where uses (CmmForeignCall e _) = getExprUses e
	 uses _ = emptyUFM
getStmtUses (CmmCondBranch e _) = getExprUses e
getStmtUses (CmmSwitch e _) = getExprUses e
getStmtUses (CmmJump e _) = getExprUses e
getStmtUses _ = emptyUFM

getExprUses :: CmmExpr -> UniqFM Int
getExprUses (CmmReg (CmmLocal (LocalReg u _))) = unitUFM u 1
getExprUses (CmmRegOff (CmmLocal (LocalReg u _)) _) = unitUFM u 1
getExprUses (CmmLoad e _) = getExprUses e
getExprUses (CmmMachOp _ es) = getExprsUses es
getExprUses _other = emptyUFM

getExprsUses es = foldr (plusUFM_C (+)) emptyUFM (map getExprUses es)

inlineStmt :: Unique -> CmmExpr -> CmmStmt -> CmmStmt
inlineStmt u a (CmmAssign r e) = CmmAssign r (inlineExpr u a e)
inlineStmt u a (CmmStore e1 e2) = CmmStore (inlineExpr u a e1) (inlineExpr u a e2)
inlineStmt u a (CmmCall target regs es vols)
   = CmmCall (infn target) regs es' vols
   where infn (CmmForeignCall fn cconv) = CmmForeignCall fn cconv
	 infn (CmmPrim p) = CmmPrim p
	 es' = [ (inlineExpr u a e, hint) | (e,hint) <- es ]
inlineStmt u a (CmmCondBranch e d) = CmmCondBranch (inlineExpr u a e) d
inlineStmt u a (CmmSwitch e d) = CmmSwitch (inlineExpr u a e) d
inlineStmt u a (CmmJump e d) = CmmJump (inlineExpr u a e) d
inlineStmt u a other_stmt = other_stmt

inlineExpr :: Unique -> CmmExpr -> CmmExpr -> CmmExpr
inlineExpr u a e@(CmmReg (CmmLocal (LocalReg u' _)))
  | u == u' = a
  | otherwise = e
inlineExpr u a e@(CmmRegOff (CmmLocal (LocalReg u' rep)) off)
  | u == u' = CmmMachOp (MO_Add rep) [a, CmmLit (CmmInt (fromIntegral off) rep)]
  | otherwise = e
inlineExpr u a (CmmLoad e rep) = CmmLoad (inlineExpr u a e) rep
inlineExpr u a (CmmMachOp op es) = CmmMachOp op (map (inlineExpr u a) es)
inlineExpr u a other_expr = other_expr

-- -----------------------------------------------------------------------------
-- Utils

bind f x = x $! f

isLit (CmmLit _) = True
isLit _          = False

isComparisonExpr :: CmmExpr -> Bool
isComparisonExpr (CmmMachOp op _) = isComparisonMachOp op
isComparisonExpr _other 	    = False

maybeInvertConditionalExpr :: CmmExpr -> Maybe CmmExpr
maybeInvertConditionalExpr (CmmMachOp op args) 
  | Just op' <- maybeInvertComparison op = Just (CmmMachOp op' args)
maybeInvertConditionalExpr _ = Nothing
\end{code}

