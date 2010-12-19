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
#include "nativeGen/NCG.h"


#if   alpha_TARGET_ARCH
import Alpha.CodeGen
import Alpha.Regs
import Alpha.RegInfo
import Alpha.Instr

#elif i386_TARGET_ARCH || x86_64_TARGET_ARCH
import X86.CodeGen
import X86.Regs
import X86.Instr
import X86.Ppr

#elif sparc_TARGET_ARCH
import SPARC.CodeGen
import SPARC.CodeGen.Expand
import SPARC.Regs
import SPARC.Instr
import SPARC.Ppr
import SPARC.ShortcutJump

#elif powerpc_TARGET_ARCH
import PPC.CodeGen
import PPC.Cond
import PPC.Regs
import PPC.RegInfo
import PPC.Instr
import PPC.Ppr

#else
#error "AsmCodeGen: unknown architecture"

#endif

import RegAlloc.Liveness
import qualified RegAlloc.Linear.Main		as Linear

import qualified GraphColor			as Color
import qualified RegAlloc.Graph.Main		as Color
import qualified RegAlloc.Graph.Stats		as Color
import qualified RegAlloc.Graph.TrivColorable	as Color

import TargetReg
import Platform
import Instruction
import PIC
import Reg
import NCGMonad

import BlockId
import CgUtils		( fixStgRegisters )
import Cmm
import CmmOpt		( cmmMiniInline, cmmMachOpFold )
import PprCmm
import CLabel

import UniqFM
import Unique		( Unique, getUnique )
import UniqSupply
import DynFlags
import StaticFlags
import Util
import Config

import Digraph
import qualified Pretty
import BufWrite
import Outputable
import FastString
import UniqSet
import ErrUtils
import Module

-- DEBUGGING ONLY
--import OrdList

import Data.List
import Data.Maybe
import Control.Monad
import System.IO
import Distribution.System

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

--------------------
nativeCodeGen :: DynFlags -> Handle -> UniqSupply -> [RawCmm] -> IO ()
nativeCodeGen dflags h us cmms
 = do
	let split_cmms	= concat $ map add_split cmms

        -- BufHandle is a performance hack.  We could hide it inside
        -- Pretty if it weren't for the fact that we do lots of little
        -- printDocs here (in order to do codegen in constant space).
        bufh <- newBufHandle h
 	(imports, prof) <- cmmNativeGens dflags bufh us split_cmms [] [] 0
        bFlush bufh

	let (native, colorStats, linearStats)
		= unzip3 prof

	-- dump native code
	dumpIfSet_dyn dflags
		Opt_D_dump_asm "Asm code"
		(vcat $ map (docToSDoc . pprNatCmmTop) $ concat native)

	-- dump global NCG stats for graph coloring allocator
	(case concat $ catMaybes colorStats of
	  []	-> return ()
	  stats	-> do	
	   	-- build the global register conflict graph
		let graphGlobal	
			= foldl Color.union Color.initGraph
			$ [ Color.raGraph stat
				| stat@Color.RegAllocStatsStart{} <- stats]
	   
	   	dumpSDoc dflags Opt_D_dump_asm_stats "NCG stats"
			$ Color.pprStats stats graphGlobal

		dumpIfSet_dyn dflags
			Opt_D_dump_asm_conflicts "Register conflict graph"
			$ Color.dotGraph 
				targetRegDotColor 
				(Color.trivColorable 
					targetVirtualRegSqueeze 
					targetRealRegSqueeze)
			$ graphGlobal)


	-- dump global NCG stats for linear allocator
	(case concat $ catMaybes linearStats of
		[]	-> return ()
		stats	-> dumpSDoc dflags Opt_D_dump_asm_stats "NCG stats"
				$ Linear.pprStats (concat native) stats)

	-- write out the imports
	Pretty.printDoc Pretty.LeftMode h
		$ makeImportsDoc dflags (concat imports)

	return	()

 where	add_split (Cmm tops)
		| dopt Opt_SplitObjs dflags = split_marker : tops
		| otherwise		    = tops

	split_marker = CmmProc [] mkSplitMarkerLabel [] (ListGraph [])


-- | Do native code generation on all these cmms.
--
cmmNativeGens :: DynFlags
              -> BufHandle
              -> UniqSupply
              -> [RawCmmTop]
              -> [[CLabel]]
              -> [ ([NatCmmTop Instr],
                   Maybe [Color.RegAllocStats Instr],
                   Maybe [Linear.RegAllocStats]) ]
              -> Int
              -> IO ( [[CLabel]],
                      [([NatCmmTop Instr],
                      Maybe [Color.RegAllocStats Instr],
                      Maybe [Linear.RegAllocStats])] )

cmmNativeGens _ _ _ [] impAcc profAcc _
	= return (reverse impAcc, reverse profAcc)

cmmNativeGens dflags h us (cmm : cmms) impAcc profAcc count
 = do
 	(us', native, imports, colorStats, linearStats)
		<- cmmNativeGen dflags us cmm count

	Pretty.bufLeftRender h
		$ {-# SCC "pprNativeCode" #-} Pretty.vcat $ map pprNatCmmTop native

           -- carefully evaluate this strictly.  Binding it with 'let'
           -- and then using 'seq' doesn't work, because the let
           -- apparently gets inlined first.
	lsPprNative <- return $!
		if  dopt Opt_D_dump_asm       dflags
	         || dopt Opt_D_dump_asm_stats dflags
			then native
			else []

	count' <- return $! count + 1;

	-- force evaulation all this stuff to avoid space leaks
	seqString (showSDoc $ vcat $ map ppr imports) `seq` return ()

	cmmNativeGens dflags h us' cmms
			(imports : impAcc)
			((lsPprNative, colorStats, linearStats) : profAcc)
			count'

 where	seqString []		= ()
	seqString (x:xs)	= x `seq` seqString xs `seq` ()


-- | Complete native code generation phase for a single top-level chunk of Cmm.
--	Dumping the output of each stage along the way.
--	Global conflict graph and NGC stats
cmmNativeGen 
	:: DynFlags
	-> UniqSupply
	-> RawCmmTop					-- ^ the cmm to generate code for
	-> Int						-- ^ sequence number of this top thing
	-> IO	( UniqSupply
		, [NatCmmTop Instr]			-- native code
		, [CLabel]				-- things imported by this cmm
		, Maybe [Color.RegAllocStats Instr]	-- stats for the coloring register allocator
		, Maybe [Linear.RegAllocStats])		-- stats for the linear register allocators

cmmNativeGen dflags us cmm count
 = do

	-- rewrite assignments to global regs
 	let fixed_cmm =
		{-# SCC "fixStgRegisters" #-}
		fixStgRegisters cmm

	-- cmm to cmm optimisations
	let (opt_cmm, imports) =
		{-# SCC "cmmToCmm" #-}
		cmmToCmm dflags fixed_cmm

	dumpIfSet_dyn dflags
		Opt_D_dump_opt_cmm "Optimised Cmm"
		(pprCmm $ Cmm [opt_cmm])

	-- generate native code from cmm
	let ((native, lastMinuteImports), usGen) =
		{-# SCC "genMachCode" #-}
		initUs us $ genMachCode dflags opt_cmm

	dumpIfSet_dyn dflags
		Opt_D_dump_asm_native "Native code"
		(vcat $ map (docToSDoc . pprNatCmmTop) native)

	-- tag instructions with register liveness information
	let (withLiveness, usLive) =
		{-# SCC "regLiveness" #-}
		initUs usGen 
			$ mapUs regLiveness 
			$ map natCmmTopToLive native

	dumpIfSet_dyn dflags
		Opt_D_dump_asm_liveness "Liveness annotations added"
		(vcat $ map ppr withLiveness)
		
	-- allocate registers
	(alloced, usAlloc, ppr_raStatsColor, ppr_raStatsLinear) <-
	 if ( dopt Opt_RegsGraph dflags
	   || dopt Opt_RegsIterative dflags)
	  then do
	  	-- the regs usable for allocation
		let (alloc_regs :: UniqFM (UniqSet RealReg))
			= foldr (\r -> plusUFM_C unionUniqSets
					$ unitUFM (targetClassOfRealReg r) (unitUniqSet r))
				emptyUFM
			$ allocatableRegs

		-- do the graph coloring register allocation
		let ((alloced, regAllocStats), usAlloc)
			= {-# SCC "RegAlloc" #-}
			  initUs usLive
			  $ Color.regAlloc
				dflags
				alloc_regs
				(mkUniqSet [0..maxSpillSlots])
				withLiveness

		-- dump out what happened during register allocation
		dumpIfSet_dyn dflags
			Opt_D_dump_asm_regalloc "Registers allocated"
			(vcat $ map (docToSDoc . pprNatCmmTop) alloced)

		dumpIfSet_dyn dflags
			Opt_D_dump_asm_regalloc_stages "Build/spill stages"
			(vcat 	$ map (\(stage, stats)
					-> text "# --------------------------"
					$$ text "#  cmm " <> int count <> text " Stage " <> int stage
					$$ ppr stats)
				$ zip [0..] regAllocStats)

		let mPprStats =
			if dopt Opt_D_dump_asm_stats dflags
			 then Just regAllocStats else Nothing

		-- force evaluation of the Maybe to avoid space leak
		mPprStats `seq` return ()

		return	( alloced, usAlloc
			, mPprStats
			, Nothing)

	  else do
	  	-- do linear register allocation
		let ((alloced, regAllocStats), usAlloc) 
			= {-# SCC "RegAlloc" #-}
  			  initUs usLive
 			  $ liftM unzip
			  $ mapUs Linear.regAlloc withLiveness

		dumpIfSet_dyn dflags
			Opt_D_dump_asm_regalloc "Registers allocated"
			(vcat $ map (docToSDoc . pprNatCmmTop) alloced)

		let mPprStats =
			if dopt Opt_D_dump_asm_stats dflags
			 then Just (catMaybes regAllocStats) else Nothing

		-- force evaluation of the Maybe to avoid space leak
		mPprStats `seq` return ()

		return	( alloced, usAlloc
			, Nothing
			, mPprStats)

	---- shortcut branches
	let shorted	=
	 	{-# SCC "shortcutBranches" #-}
	 	shortcutBranches dflags alloced

	---- sequence blocks
	let sequenced	=
	 	{-# SCC "sequenceBlocks" #-}
	 	map sequenceTop shorted

	---- x86fp_kludge
	let kludged =
#if i386_TARGET_ARCH
	 	{-# SCC "x86fp_kludge" #-}
	 	map x86fp_kludge sequenced
#else
		sequenced
#endif

	---- expansion of SPARC synthetic instrs
#if sparc_TARGET_ARCH
	let expanded = 
		{-# SCC "sparc_expand" #-}
		map expandTop kludged

	dumpIfSet_dyn dflags
		Opt_D_dump_asm_expanded "Synthetic instructions expanded"
		(vcat $ map (docToSDoc . pprNatCmmTop) expanded)
#else
	let expanded = 
		kludged
#endif

	return 	( usAlloc
		, expanded
		, lastMinuteImports ++ imports
		, ppr_raStatsColor
		, ppr_raStatsLinear)


#if i386_TARGET_ARCH
x86fp_kludge :: NatCmmTop Instr -> NatCmmTop Instr
x86fp_kludge top@(CmmData _ _) = top
x86fp_kludge (CmmProc info lbl params (ListGraph code)) = 
	CmmProc info lbl params (ListGraph $ i386_insert_ffrees code)
#endif


-- | Build a doc for all the imports.
--
makeImportsDoc :: DynFlags -> [CLabel] -> Pretty.Doc
makeImportsDoc dflags imports
 = dyld_stubs imports

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
#if !defined(darwin_TARGET_OS)
                -- And just because every other compiler does, lets stick in
		-- an identifier directive: .ident "GHC x.y.z"
	    Pretty.$$ let compilerIdent = Pretty.text "GHC" Pretty.<+>
	                                  Pretty.text cProjectVersion
                       in Pretty.text ".ident" Pretty.<+>
                          Pretty.doubleQuotes compilerIdent
#endif

 where
	-- Generate "symbol stubs" for all external symbols that might
	-- come from a dynamic library.
	dyld_stubs :: [CLabel] -> Pretty.Doc
{-      dyld_stubs imps = Pretty.vcat $ map pprDyldSymbolStub $
				    map head $ group $ sort imps-}

	arch	= platformArch	$ targetPlatform dflags
	os	= platformOS	$ targetPlatform dflags
	
	-- (Hack) sometimes two Labels pretty-print the same, but have
	-- different uniques; so we compare their text versions...
	dyld_stubs imps
		| needImportedSymbols arch os
		= Pretty.vcat $
			(pprGotDeclaration arch os :) $
			map ( pprImportedSymbol arch os . fst . head) $
			groupBy (\(_,a) (_,b) -> a == b) $
			sortBy (\(_,a) (_,b) -> compare a b) $
			map doPpr $
			imps
		| otherwise
		= Pretty.empty

	doPpr lbl = (lbl, Pretty.render $ pprCLabel lbl astyle)
	astyle = mkCodeStyle AsmStyle


-- -----------------------------------------------------------------------------
-- Sequencing the basic blocks

-- Cmm BasicBlocks are self-contained entities: they always end in a
-- jump, either non-local or to another basic block in the same proc.
-- In this phase, we attempt to place the basic blocks in a sequence
-- such that as many of the local jumps as possible turn into
-- fallthroughs.

sequenceTop 
	:: NatCmmTop Instr
	-> NatCmmTop Instr

sequenceTop top@(CmmData _ _) = top
sequenceTop (CmmProc info lbl params (ListGraph blocks)) = 
  CmmProc info lbl params (ListGraph $ makeFarBranches $ sequenceBlocks blocks)

-- The algorithm is very simple (and stupid): we make a graph out of
-- the blocks where there is an edge from one block to another iff the
-- first block ends by jumping to the second.  Then we topologically
-- sort this graph.  Then traverse the list: for each block, we first
-- output the block, then if it has an out edge, we move the
-- destination of the out edge to the front of the list, and continue.

-- FYI, the classic layout for basic blocks uses postorder DFS; this
-- algorithm is implemented in cmm/ZipCfg.hs (NR 6 Sep 2007).

sequenceBlocks 
	:: Instruction instr
	=> [NatBasicBlock instr] 
	-> [NatBasicBlock instr]

sequenceBlocks [] = []
sequenceBlocks (entry:blocks) = 
  seqBlocks (mkNode entry : reverse (flattenSCCs (sccBlocks blocks)))
  -- the first block is the entry point ==> it must remain at the start.


sccBlocks 
	:: Instruction instr
	=> [NatBasicBlock instr] 
	-> [SCC ( NatBasicBlock instr
		, Unique
		, [Unique])]

sccBlocks blocks = stronglyConnCompFromEdgedVerticesR (map mkNode blocks)

-- we're only interested in the last instruction of
-- the block, and only if it has a single destination.
getOutEdges 
	:: Instruction instr
	=> [instr] -> [Unique]

getOutEdges instrs 
	= case jumpDestsOfInstr (last instrs) of
		[one] -> [getUnique one]
		_many -> []

mkNode :: (Instruction t)
       => GenBasicBlock t
       -> (GenBasicBlock t, Unique, [Unique])
mkNode block@(BasicBlock id instrs) = (block, getUnique id, getOutEdges instrs)

seqBlocks :: (Eq t) => [(GenBasicBlock t1, t, [t])] -> [GenBasicBlock t1]
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

reorder :: (Eq a) => a -> [(t, a, t1)] -> [(t, a, t1)] -> (Bool, [(t, a, t1)])
reorder  _ accum [] = (False, reverse accum)
reorder id accum (b@(block,id',out) : rest)
  | id == id'  = (True, (block,id,out) : reverse accum ++ rest)
  | otherwise  = reorder id (b:accum) rest


-- -----------------------------------------------------------------------------
-- Making far branches

-- Conditional branches on PowerPC are limited to +-32KB; if our Procs get too
-- big, we have to work around this limitation.

makeFarBranches 
	:: [NatBasicBlock Instr] 
	-> [NatBasicBlock Instr]

#if powerpc_TARGET_ARCH
makeFarBranches blocks
    | last blockAddresses < nearLimit = blocks
    | otherwise = zipWith handleBlock blockAddresses blocks
    where
        blockAddresses = scanl (+) 0 $ map blockLen blocks
        blockLen (BasicBlock _ instrs) = length instrs
        
        handleBlock addr (BasicBlock id instrs)
                = BasicBlock id (zipWith makeFar [addr..] instrs)
        
        makeFar _ (BCC ALWAYS tgt) = BCC ALWAYS tgt
        makeFar addr (BCC cond tgt)
            | abs (addr - targetAddr) >= nearLimit
            = BCCFAR cond tgt
            | otherwise
            = BCC cond tgt
            where Just targetAddr = lookupUFM blockAddressMap tgt
        makeFar _ other            = other
        
        nearLimit = 7000 -- 8192 instructions are allowed; let's keep some
                         -- distance, as we have a few pseudo-insns that are
                         -- pretty-printed as multiple instructions,
                         -- and it's just not worth the effort to calculate
                         -- things exactly
        
        blockAddressMap = listToUFM $ zip (map blockId blocks) blockAddresses
#else
makeFarBranches = id
#endif

-- -----------------------------------------------------------------------------
-- Shortcut branches

shortcutBranches 
	:: DynFlags 
	-> [NatCmmTop Instr] 
	-> [NatCmmTop Instr]

shortcutBranches dflags tops
  | optLevel dflags < 1 = tops    -- only with -O or higher
  | otherwise           = map (apply_mapping mapping) tops'
  where
    (tops', mappings) = mapAndUnzip build_mapping tops
    mapping = foldr plusUFM emptyUFM mappings

build_mapping :: GenCmmTop d t (ListGraph Instr)
              -> (GenCmmTop d t (ListGraph Instr), UniqFM JumpDest)
build_mapping top@(CmmData _ _) = (top, emptyUFM)
build_mapping (CmmProc info lbl params (ListGraph []))
  = (CmmProc info lbl params (ListGraph []), emptyUFM)
build_mapping (CmmProc info lbl params (ListGraph (head:blocks)))
  = (CmmProc info lbl params (ListGraph (head:others)), mapping)
        -- drop the shorted blocks, but don't ever drop the first one,
        -- because it is pointed to by a global label.
  where
    -- find all the blocks that just consist of a jump that can be
    -- shorted.
    -- Don't completely eliminate loops here -- that can leave a dangling jump!
    (_, shortcut_blocks, others) = foldl split (emptyBlockSet, [], []) blocks
    split (s, shortcut_blocks, others) b@(BasicBlock id [insn])
        | Just (DestBlockId dest) <- canShortcut insn,
          (elemBlockSet dest s) || dest == id -- loop checks
        = (s, shortcut_blocks, b : others)
    split (s, shortcut_blocks, others) (BasicBlock id [insn])
        | Just dest <- canShortcut insn
        = (extendBlockSet s id, (id,dest) : shortcut_blocks, others)
    split (s, shortcut_blocks, others) other = (s, shortcut_blocks, other : others)


    -- build a mapping from BlockId to JumpDest for shorting branches
    mapping = foldl add emptyUFM shortcut_blocks
    add ufm (id,dest) = addToUFM ufm id dest
    
apply_mapping :: UniqFM JumpDest
              -> GenCmmTop CmmStatic h (ListGraph Instr)
              -> GenCmmTop CmmStatic h (ListGraph Instr)
apply_mapping ufm (CmmData sec statics) 
  = CmmData sec (map (shortcutStatic (lookupUFM ufm)) statics)
  -- we need to get the jump tables, so apply the mapping to the entries
  -- of a CmmData too.
apply_mapping ufm (CmmProc info lbl params (ListGraph blocks))
  = CmmProc info lbl params (ListGraph $ map short_bb blocks)
  where
    short_bb (BasicBlock id insns) = BasicBlock id $! map short_insn insns
    short_insn i = shortcutJump (lookupUFM ufm) i
                 -- shortcutJump should apply the mapping repeatedly,
                 -- just in case we can short multiple branches.

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

genMachCode 
	:: DynFlags 
	-> RawCmmTop 
	-> UniqSM 
		( [NatCmmTop Instr]
		, [CLabel])

genMachCode dflags cmm_top
  = do	{ initial_us <- getUs
	; let initial_st           = mkNatM_State initial_us 0 dflags
	      (new_tops, final_st) = initNat initial_st (cmmTopCodeGen dflags cmm_top)
	      final_delta          = natm_delta final_st
	      final_imports        = natm_imports final_st
	; if   final_delta == 0
          then return (new_tops, final_imports)
          else pprPanic "genMachCode: nonzero final delta" (int final_delta)
    }


-- -----------------------------------------------------------------------------
-- Generic Cmm optimiser

{-
Here we do:

  (a) Constant folding
  (b) Simple inlining: a temporary which is assigned to and then
      used, once, can be shorted.
  (c) Position independent code and dynamic linking
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

cmmToCmm :: DynFlags -> RawCmmTop -> (RawCmmTop, [CLabel])
cmmToCmm _ top@(CmmData _ _) = (top, [])
cmmToCmm dflags (CmmProc info lbl params (ListGraph blocks)) = runCmmOpt dflags $ do
  blocks' <- mapM cmmBlockConFold (cmmMiniInline blocks)
  return $ CmmProc info lbl params (ListGraph blocks')

newtype CmmOptM a = CmmOptM (([CLabel], DynFlags) -> (# a, [CLabel] #))

instance Monad CmmOptM where
  return x = CmmOptM $ \(imports, _) -> (# x,imports #)
  (CmmOptM f) >>= g =
    CmmOptM $ \(imports, dflags) ->
                case f (imports, dflags) of
                  (# x, imports' #) ->
                    case g x of
                      CmmOptM g' -> g' (imports', dflags)

addImportCmmOpt :: CLabel -> CmmOptM ()
addImportCmmOpt lbl = CmmOptM $ \(imports, _dflags) -> (# (), lbl:imports #)

getDynFlagsCmmOpt :: CmmOptM DynFlags
getDynFlagsCmmOpt = CmmOptM $ \(imports, dflags) -> (# dflags, imports #)

runCmmOpt :: DynFlags -> CmmOptM a -> (a, [CLabel])
runCmmOpt dflags (CmmOptM f) = case f ([], dflags) of
                        (# result, imports #) -> (result, imports)

cmmBlockConFold :: CmmBasicBlock -> CmmOptM CmmBasicBlock
cmmBlockConFold (BasicBlock id stmts) = do
  stmts' <- mapM cmmStmtConFold stmts
  return $ BasicBlock id stmts'

cmmStmtConFold :: CmmStmt -> CmmOptM CmmStmt
cmmStmtConFold stmt
   = case stmt of
        CmmAssign reg src
           -> do src' <- cmmExprConFold DataReference src
                 return $ case src' of
		   CmmReg reg' | reg == reg' -> CmmNop
		   new_src -> CmmAssign reg new_src

        CmmStore addr src
           -> do addr' <- cmmExprConFold DataReference addr
                 src'  <- cmmExprConFold DataReference src
                 return $ CmmStore addr' src'

        CmmJump addr regs
           -> do addr' <- cmmExprConFold JumpReference addr
                 return $ CmmJump addr' regs

	CmmCall target regs args srt returns
	   -> do target' <- case target of
			      CmmCallee e conv -> do
			        e' <- cmmExprConFold CallReference e
			        return $ CmmCallee e' conv
			      other -> return other
                 args' <- mapM (\(CmmHinted arg hint) -> do
                                  arg' <- cmmExprConFold DataReference arg
                                  return (CmmHinted arg' hint)) args
	         return $ CmmCall target' regs args' srt returns

        CmmCondBranch test dest
           -> do test' <- cmmExprConFold DataReference test
	         return $ case test' of
		   CmmLit (CmmInt 0 _) -> 
		     CmmComment (mkFastString ("deleted: " ++ 
					showSDoc (pprStmt stmt)))

		   CmmLit (CmmInt _ _) -> CmmBranch dest
		   _other -> CmmCondBranch test' dest

	CmmSwitch expr ids
	   -> do expr' <- cmmExprConFold DataReference expr
	         return $ CmmSwitch expr' ids

        other
           -> return other


cmmExprConFold :: ReferenceKind -> CmmExpr -> CmmOptM CmmExpr
cmmExprConFold referenceKind expr
   = case expr of
        CmmLoad addr rep
           -> do addr' <- cmmExprConFold DataReference addr
                 return $ CmmLoad addr' rep

        CmmMachOp mop args
           -- For MachOps, we first optimize the children, and then we try 
           -- our hand at some constant-folding.
           -> do args' <- mapM (cmmExprConFold DataReference) args
                 return $ cmmMachOpFold mop args'

        CmmLit (CmmLabel lbl)
           -> do
		dflags <- getDynFlagsCmmOpt
		cmmMakeDynamicReference dflags addImportCmmOpt referenceKind lbl
        CmmLit (CmmLabelOff lbl off)
           -> do
		 dflags <- getDynFlagsCmmOpt
		 dynRef <- cmmMakeDynamicReference dflags addImportCmmOpt referenceKind lbl
                 return $ cmmMachOpFold (MO_Add wordWidth) [
                     dynRef,
                     (CmmLit $ CmmInt (fromIntegral off) wordWidth)
                   ]

        -- On powerpc (non-PIC), it's easier to jump directly to a label than
        -- to use the register table, so we replace these registers
        -- with the corresponding labels:
        CmmReg (CmmGlobal EagerBlackholeInfo)
          | cTargetArch == PPC && not opt_PIC
          -> cmmExprConFold referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "__stg_EAGER_BLACKHOLE_info")))
        CmmReg (CmmGlobal GCEnter1)
          | cTargetArch == PPC && not opt_PIC
          -> cmmExprConFold referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "__stg_gc_enter_1"))) 
        CmmReg (CmmGlobal GCFun)
          | cTargetArch == PPC && not opt_PIC
          -> cmmExprConFold referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "__stg_gc_fun")))

        other
           -> return other

\end{code}

