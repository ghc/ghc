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


import qualified X86.CodeGen
import qualified X86.Regs
import qualified X86.Instr
import qualified X86.Ppr

import qualified SPARC.CodeGen
import qualified SPARC.Regs
import qualified SPARC.Instr
import qualified SPARC.Ppr
import qualified SPARC.ShortcutJump
import qualified SPARC.CodeGen.Expand

import qualified PPC.CodeGen
import qualified PPC.Cond
import qualified PPC.Regs
import qualified PPC.RegInfo
import qualified PPC.Instr
import qualified PPC.Ppr

import RegAlloc.Liveness
import qualified RegAlloc.Linear.Main		as Linear

import qualified GraphColor			as Color
import qualified RegAlloc.Graph.Main		as Color
import qualified RegAlloc.Graph.Stats		as Color
import qualified RegAlloc.Graph.TrivColorable	as Color

import TargetReg
import Platform
import Config
import Instruction
import PIC
import Reg
import NCGMonad

import BlockId
import CgUtils		( fixStgRegisters )
import OldCmm
import CmmOpt		( cmmEliminateDeadBlocks, cmmMiniInline, cmmMachOpFold )
import OldPprCmm
import CLabel

import UniqFM
import Unique		( Unique, getUnique )
import UniqSupply
import DynFlags
import StaticFlags
import Util

import BasicTypes       ( Alignment )
import Digraph
import Pretty (Doc)
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

data NcgImpl statics instr jumpDest = NcgImpl {
    cmmTopCodeGen             :: RawCmmTop -> NatM [NatCmmTop statics instr],
    generateJumpTableForInstr :: instr -> Maybe (NatCmmTop statics instr),
    getJumpDestBlockId        :: jumpDest -> Maybe BlockId,
    canShortcut               :: instr -> Maybe jumpDest,
    shortcutStatics           :: (BlockId -> Maybe jumpDest) -> statics -> statics,
    shortcutJump              :: (BlockId -> Maybe jumpDest) -> instr -> instr,
    pprNatCmmTop              :: Platform -> NatCmmTop statics instr -> Doc,
    maxSpillSlots             :: Int,
    allocatableRegs           :: [RealReg],
    ncg_x86fp_kludge          :: [NatCmmTop statics instr] -> [NatCmmTop statics instr],
    ncgExpandTop              :: [NatCmmTop statics instr] -> [NatCmmTop statics instr],
    ncgMakeFarBranches        :: [NatBasicBlock instr] -> [NatBasicBlock instr]
    }

--------------------
nativeCodeGen :: DynFlags -> Handle -> UniqSupply -> [RawCmm] -> IO ()
nativeCodeGen dflags h us cmms
 = let nCG' :: (Outputable statics, Outputable instr, Instruction instr) => NcgImpl statics instr jumpDest -> IO ()
       nCG' ncgImpl = nativeCodeGen' dflags ncgImpl h us cmms
       x86NcgImpl = NcgImpl {
                         cmmTopCodeGen             = X86.CodeGen.cmmTopCodeGen
                        ,generateJumpTableForInstr = X86.CodeGen.generateJumpTableForInstr
                        ,getJumpDestBlockId        = X86.Instr.getJumpDestBlockId
                        ,canShortcut               = X86.Instr.canShortcut
                        ,shortcutStatics           = X86.Instr.shortcutStatics
                        ,shortcutJump              = X86.Instr.shortcutJump
                        ,pprNatCmmTop              = X86.Ppr.pprNatCmmTop
                        ,maxSpillSlots             = X86.Instr.maxSpillSlots
                        ,allocatableRegs           = X86.Regs.allocatableRegs
                        ,ncg_x86fp_kludge          = id
                        ,ncgExpandTop              = id
                        ,ncgMakeFarBranches        = id
                    }
   in case platformArch $ targetPlatform dflags of
                 ArchX86    -> nCG' (x86NcgImpl { ncg_x86fp_kludge = map x86fp_kludge })
                 ArchX86_64 -> nCG' x86NcgImpl
                 ArchPPC ->
                     nCG' $ NcgImpl {
                          cmmTopCodeGen             = PPC.CodeGen.cmmTopCodeGen
                         ,generateJumpTableForInstr = PPC.CodeGen.generateJumpTableForInstr
                         ,getJumpDestBlockId        = PPC.RegInfo.getJumpDestBlockId
                         ,canShortcut               = PPC.RegInfo.canShortcut
                         ,shortcutStatics           = PPC.RegInfo.shortcutStatics
                         ,shortcutJump              = PPC.RegInfo.shortcutJump
                         ,pprNatCmmTop              = PPC.Ppr.pprNatCmmTop
                         ,maxSpillSlots             = PPC.Instr.maxSpillSlots
                         ,allocatableRegs           = PPC.Regs.allocatableRegs
                         ,ncg_x86fp_kludge          = id
                         ,ncgExpandTop              = id
                         ,ncgMakeFarBranches        = makeFarBranches
                     }
                 ArchSPARC ->
                     nCG' $ NcgImpl {
                          cmmTopCodeGen             = SPARC.CodeGen.cmmTopCodeGen
                         ,generateJumpTableForInstr = SPARC.CodeGen.generateJumpTableForInstr
                         ,getJumpDestBlockId        = SPARC.ShortcutJump.getJumpDestBlockId
                         ,canShortcut               = SPARC.ShortcutJump.canShortcut
                         ,shortcutStatics           = SPARC.ShortcutJump.shortcutStatics
                         ,shortcutJump              = SPARC.ShortcutJump.shortcutJump
                         ,pprNatCmmTop              = SPARC.Ppr.pprNatCmmTop
                         ,maxSpillSlots             = SPARC.Instr.maxSpillSlots
                         ,allocatableRegs           = SPARC.Regs.allocatableRegs
                         ,ncg_x86fp_kludge          = id
                         ,ncgExpandTop              = map SPARC.CodeGen.Expand.expandTop
                         ,ncgMakeFarBranches        = id
                     }
                 ArchARM ->
                     panic "nativeCodeGen: No NCG for ARM"
                 ArchPPC_64 ->
                     panic "nativeCodeGen: No NCG for PPC 64"
                 ArchUnknown ->
                     panic "nativeCodeGen: No NCG for unknown arch"

nativeCodeGen' :: (Outputable statics, Outputable instr, Instruction instr)
               => DynFlags
               -> NcgImpl statics instr jumpDest
               -> Handle -> UniqSupply -> [RawCmm] -> IO ()
nativeCodeGen' dflags ncgImpl h us cmms
 = do
	let split_cmms	= concat $ map add_split cmms
        -- BufHandle is a performance hack.  We could hide it inside
        -- Pretty if it weren't for the fact that we do lots of little
        -- printDocs here (in order to do codegen in constant space).
        bufh <- newBufHandle h
 	(imports, prof) <- cmmNativeGens dflags ncgImpl bufh us split_cmms [] [] 0
        bFlush bufh

	let (native, colorStats, linearStats)
		= unzip3 prof

	-- dump native code
	dumpIfSet_dyn dflags
		Opt_D_dump_asm "Asm code"
		(vcat $ map (docToSDoc . pprNatCmmTop ncgImpl (targetPlatform dflags)) $ concat native)

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

	split_marker = CmmProc Nothing mkSplitMarkerLabel (ListGraph [])


-- | Do native code generation on all these cmms.
--
cmmNativeGens :: (Outputable statics, Outputable instr, Instruction instr)
              => DynFlags
              -> NcgImpl statics instr jumpDest
              -> BufHandle
              -> UniqSupply
              -> [RawCmmTop]
              -> [[CLabel]]
              -> [ ([NatCmmTop statics instr],
                   Maybe [Color.RegAllocStats statics instr],
                   Maybe [Linear.RegAllocStats]) ]
              -> Int
              -> IO ( [[CLabel]],
                      [([NatCmmTop statics instr],
                      Maybe [Color.RegAllocStats statics instr],
                      Maybe [Linear.RegAllocStats])] )

cmmNativeGens _ _ _ _ [] impAcc profAcc _
	= return (reverse impAcc, reverse profAcc)

cmmNativeGens dflags ncgImpl h us (cmm : cmms) impAcc profAcc count
 = do
 	(us', native, imports, colorStats, linearStats)
		<- cmmNativeGen dflags ncgImpl us cmm count

	Pretty.bufLeftRender h
		$ {-# SCC "pprNativeCode" #-} Pretty.vcat $ map (pprNatCmmTop ncgImpl (targetPlatform dflags)) native

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

	cmmNativeGens dflags ncgImpl
            h us' cmms
			(imports : impAcc)
			((lsPprNative, colorStats, linearStats) : profAcc)
			count'

 where	seqString []		= ()
	seqString (x:xs)	= x `seq` seqString xs `seq` ()


-- | Complete native code generation phase for a single top-level chunk of Cmm.
--	Dumping the output of each stage along the way.
--	Global conflict graph and NGC stats
cmmNativeGen
	:: (Outputable statics, Outputable instr, Instruction instr)
    => DynFlags
    -> NcgImpl statics instr jumpDest
	-> UniqSupply
	-> RawCmmTop					-- ^ the cmm to generate code for
	-> Int						-- ^ sequence number of this top thing
	-> IO	( UniqSupply
		, [NatCmmTop statics instr]	            -- native code
		, [CLabel]			            -- things imported by this cmm
		, Maybe [Color.RegAllocStats statics instr] -- stats for the coloring register allocator
		, Maybe [Linear.RegAllocStats])		    -- stats for the linear register allocators

cmmNativeGen dflags ncgImpl us cmm count
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
		initUs us $ genMachCode dflags (cmmTopCodeGen ncgImpl) opt_cmm

	dumpIfSet_dyn dflags
		Opt_D_dump_asm_native "Native code"
		(vcat $ map (docToSDoc . pprNatCmmTop ncgImpl (targetPlatform dflags)) native)

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
			$ allocatableRegs ncgImpl

		-- do the graph coloring register allocation
		let ((alloced, regAllocStats), usAlloc)
			= {-# SCC "RegAlloc" #-}
			  initUs usLive
			  $ Color.regAlloc
				dflags
				alloc_regs
				(mkUniqSet [0 .. maxSpillSlots ncgImpl])
				withLiveness

		-- dump out what happened during register allocation
		dumpIfSet_dyn dflags
			Opt_D_dump_asm_regalloc "Registers allocated"
			(vcat $ map (docToSDoc . pprNatCmmTop ncgImpl (targetPlatform dflags)) alloced)

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
			  $ mapUs (Linear.regAlloc dflags) withLiveness

		dumpIfSet_dyn dflags
			Opt_D_dump_asm_regalloc "Registers allocated"
			(vcat $ map (docToSDoc . pprNatCmmTop ncgImpl (targetPlatform dflags)) alloced)

		let mPprStats =
			if dopt Opt_D_dump_asm_stats dflags
			 then Just (catMaybes regAllocStats) else Nothing

		-- force evaluation of the Maybe to avoid space leak
		mPprStats `seq` return ()

		return	( alloced, usAlloc
			, Nothing
			, mPprStats)

        ---- x86fp_kludge.  This pass inserts ffree instructions to clear
        ---- the FPU stack on x86.  The x86 ABI requires that the FPU stack
        ---- is clear, and library functions can return odd results if it
        ---- isn't.
        ----
        ---- NB. must happen before shortcutBranches, because that
        ---- generates JXX_GBLs which we can't fix up in x86fp_kludge.
        let kludged = {-# SCC "x86fp_kludge" #-} ncg_x86fp_kludge ncgImpl alloced

        ---- generate jump tables
	let tabled	=
		{-# SCC "generateJumpTables" #-}
                generateJumpTables ncgImpl kludged

	---- shortcut branches
	let shorted	=
	 	{-# SCC "shortcutBranches" #-}
	 	shortcutBranches dflags ncgImpl tabled

	---- sequence blocks
	let sequenced	=
	 	{-# SCC "sequenceBlocks" #-}
	 	map (sequenceTop ncgImpl) shorted

        ---- expansion of SPARC synthetic instrs
	let expanded = 
		{-# SCC "sparc_expand" #-}
                ncgExpandTop ncgImpl sequenced

	dumpIfSet_dyn dflags
		Opt_D_dump_asm_expanded "Synthetic instructions expanded"
		(vcat $ map (docToSDoc . pprNatCmmTop ncgImpl (targetPlatform dflags)) expanded)

	return 	( usAlloc
		, expanded
		, lastMinuteImports ++ imports
		, ppr_raStatsColor
		, ppr_raStatsLinear)


x86fp_kludge :: NatCmmTop (Alignment, CmmStatics) X86.Instr.Instr -> NatCmmTop (Alignment, CmmStatics) X86.Instr.Instr
x86fp_kludge top@(CmmData _ _) = top
x86fp_kludge (CmmProc info lbl (ListGraph code)) = 
	CmmProc info lbl (ListGraph $ X86.Instr.i386_insert_ffrees code)


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
                -- And just because every other compiler does, lets stick in
		-- an identifier directive: .ident "GHC x.y.z"
            Pretty.$$ let compilerIdent = Pretty.text "GHC" Pretty.<+>
	                                  Pretty.text cProjectVersion
                       in Pretty.text ".ident" Pretty.<+>
                          Pretty.doubleQuotes compilerIdent

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

	doPpr lbl = (lbl, renderWithStyle (pprCLabel lbl) astyle)
	astyle = mkCodeStyle AsmStyle


-- -----------------------------------------------------------------------------
-- Sequencing the basic blocks

-- Cmm BasicBlocks are self-contained entities: they always end in a
-- jump, either non-local or to another basic block in the same proc.
-- In this phase, we attempt to place the basic blocks in a sequence
-- such that as many of the local jumps as possible turn into
-- fallthroughs.

sequenceTop 
	:: Instruction instr
    => NcgImpl statics instr jumpDest -> NatCmmTop statics instr -> NatCmmTop statics instr

sequenceTop _       top@(CmmData _ _) = top
sequenceTop ncgImpl (CmmProc info lbl (ListGraph blocks)) = 
  CmmProc info lbl (ListGraph $ ncgMakeFarBranches ncgImpl $ sequenceBlocks blocks)

-- The algorithm is very simple (and stupid): we make a graph out of
-- the blocks where there is an edge from one block to another iff the
-- first block ends by jumping to the second.  Then we topologically
-- sort this graph.  Then traverse the list: for each block, we first
-- output the block, then if it has an out edge, we move the
-- destination of the out edge to the front of the list, and continue.

-- FYI, the classic layout for basic blocks uses postorder DFS; this
-- algorithm is implemented in Hoopl.

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
	:: [NatBasicBlock PPC.Instr.Instr] 
	-> [NatBasicBlock PPC.Instr.Instr]
makeFarBranches blocks
    | last blockAddresses < nearLimit = blocks
    | otherwise = zipWith handleBlock blockAddresses blocks
    where
        blockAddresses = scanl (+) 0 $ map blockLen blocks
        blockLen (BasicBlock _ instrs) = length instrs
        
        handleBlock addr (BasicBlock id instrs)
                = BasicBlock id (zipWith makeFar [addr..] instrs)
        
        makeFar _ (PPC.Instr.BCC PPC.Cond.ALWAYS tgt) = PPC.Instr.BCC PPC.Cond.ALWAYS tgt
        makeFar addr (PPC.Instr.BCC cond tgt)
            | abs (addr - targetAddr) >= nearLimit
            = PPC.Instr.BCCFAR cond tgt
            | otherwise
            = PPC.Instr.BCC cond tgt
            where Just targetAddr = lookupUFM blockAddressMap tgt
        makeFar _ other            = other
        
        nearLimit = 7000 -- 8192 instructions are allowed; let's keep some
                         -- distance, as we have a few pseudo-insns that are
                         -- pretty-printed as multiple instructions,
                         -- and it's just not worth the effort to calculate
                         -- things exactly
        
        blockAddressMap = listToUFM $ zip (map blockId blocks) blockAddresses

-- -----------------------------------------------------------------------------
-- Generate jump tables

-- Analyzes all native code and generates data sections for all jump
-- table instructions.
generateJumpTables
	:: NcgImpl statics instr jumpDest
    -> [NatCmmTop statics instr] -> [NatCmmTop statics instr]
generateJumpTables ncgImpl xs = concatMap f xs
    where f p@(CmmProc _ _ (ListGraph xs)) = p : concatMap g xs
          f p = [p]
          g (BasicBlock _ xs) = catMaybes (map (generateJumpTableForInstr ncgImpl) xs)

-- -----------------------------------------------------------------------------
-- Shortcut branches

shortcutBranches
	:: DynFlags
    -> NcgImpl statics instr jumpDest
	-> [NatCmmTop statics instr] 
	-> [NatCmmTop statics instr]

shortcutBranches dflags ncgImpl tops
  | optLevel dflags < 1 = tops    -- only with -O or higher
  | otherwise           = map (apply_mapping ncgImpl mapping) tops'
  where
    (tops', mappings) = mapAndUnzip (build_mapping ncgImpl) tops
    mapping = foldr plusUFM emptyUFM mappings

build_mapping :: NcgImpl statics instr jumpDest
              -> GenCmmTop d t (ListGraph instr)
              -> (GenCmmTop d t (ListGraph instr), UniqFM jumpDest)
build_mapping _ top@(CmmData _ _) = (top, emptyUFM)
build_mapping _ (CmmProc info lbl (ListGraph []))
  = (CmmProc info lbl (ListGraph []), emptyUFM)
build_mapping ncgImpl (CmmProc info lbl (ListGraph (head:blocks)))
  = (CmmProc info lbl (ListGraph (head:others)), mapping)
        -- drop the shorted blocks, but don't ever drop the first one,
        -- because it is pointed to by a global label.
  where
    -- find all the blocks that just consist of a jump that can be
    -- shorted.
    -- Don't completely eliminate loops here -- that can leave a dangling jump!
    (_, shortcut_blocks, others) = foldl split (emptyBlockSet, [], []) blocks
    split (s, shortcut_blocks, others) b@(BasicBlock id [insn])
        | Just jd <- canShortcut ncgImpl insn,
          Just dest <- getJumpDestBlockId ncgImpl jd,
          (setMember dest s) || dest == id -- loop checks
        = (s, shortcut_blocks, b : others)
    split (s, shortcut_blocks, others) (BasicBlock id [insn])
        | Just dest <- canShortcut ncgImpl insn
        = (setInsert id s, (id,dest) : shortcut_blocks, others)
    split (s, shortcut_blocks, others) other = (s, shortcut_blocks, other : others)


    -- build a mapping from BlockId to JumpDest for shorting branches
    mapping = foldl add emptyUFM shortcut_blocks
    add ufm (id,dest) = addToUFM ufm id dest
    
apply_mapping :: NcgImpl statics instr jumpDest
              -> UniqFM jumpDest
              -> GenCmmTop statics h (ListGraph instr)
              -> GenCmmTop statics h (ListGraph instr)
apply_mapping ncgImpl ufm (CmmData sec statics)
  = CmmData sec (shortcutStatics ncgImpl (lookupUFM ufm) statics)
apply_mapping ncgImpl ufm (CmmProc info lbl (ListGraph blocks))
  = CmmProc info lbl (ListGraph $ map short_bb blocks)
  where
    short_bb (BasicBlock id insns) = BasicBlock id $! map short_insn insns
    short_insn i = shortcutJump ncgImpl (lookupUFM ufm) i
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
        -> (RawCmmTop -> NatM [NatCmmTop statics instr])
	-> RawCmmTop 
	-> UniqSM 
		( [NatCmmTop statics instr]
		, [CLabel])

genMachCode dflags cmmTopCodeGen cmm_top
  = do	{ initial_us <- getUs
	; let initial_st           = mkNatM_State initial_us 0 dflags
	      (new_tops, final_st) = initNat initial_st (cmmTopCodeGen cmm_top)
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
  (d) Some arch-specific optimizations

(a) and (b) will be moving to the new Hoopl pipeline, however, (c) and
(d) are only needed by the native backend and will continue to live
here.

Ideas for other things we could do (put these in Hoopl please!):

  - shortcut jumps-to-jumps
  - simple CSE: if an expr is assigned to a temp, then replace later occs of
    that expr with the temp, until the expr is no longer valid (can push through
    temp assignments, and certain assigns to mem...)
-}

cmmToCmm :: DynFlags -> RawCmmTop -> (RawCmmTop, [CLabel])
cmmToCmm _ top@(CmmData _ _) = (top, [])
cmmToCmm dflags (CmmProc info lbl (ListGraph blocks)) = runCmmOpt dflags $ do
  blocks' <- mapM cmmBlockConFold (cmmMiniInline (cmmEliminateDeadBlocks blocks))
  return $ CmmProc info lbl (ListGraph blocks')

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

-- This does three optimizations, but they're very quick to check, so we don't
-- bother turning them off even when the Hoopl code is active.  Since
-- this is on the old Cmm representation, we can't reuse the code either:
--  * reg = reg      --> nop
--  * if 0 then jump --> nop
--  * if 1 then jump --> jump
-- We might be tempted to skip this step entirely of not opt_PIC, but
-- there is some PowerPC code for the non-PIC case, which would also
-- have to be separated.
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
cmmExprConFold referenceKind expr = do
    dflags <- getDynFlagsCmmOpt
    -- Skip constant folding if new code generator is running
    -- (this optimization is done in Hoopl)
    let expr' = if dopt Opt_TryNewCodeGen dflags
                    then expr
                    else cmmExprCon expr
    cmmExprNative referenceKind expr'

cmmExprCon :: CmmExpr -> CmmExpr
cmmExprCon (CmmLoad addr rep) = CmmLoad (cmmExprCon addr) rep
cmmExprCon (CmmMachOp mop args) = cmmMachOpFold mop (map cmmExprCon args)
cmmExprCon other = other

-- handles both PIC and non-PIC cases... a very strange mixture
-- of things to do.
cmmExprNative :: ReferenceKind -> CmmExpr -> CmmOptM CmmExpr
cmmExprNative referenceKind expr = do
     dflags <- getDynFlagsCmmOpt
     let arch = platformArch (targetPlatform dflags)
     case expr of
        CmmLoad addr rep
           -> do addr' <- cmmExprNative DataReference addr
                 return $ CmmLoad addr' rep

        CmmMachOp mop args
           -> do args' <- mapM (cmmExprNative DataReference) args
                 return $ CmmMachOp mop args'

        CmmLit (CmmLabel lbl)
           -> do
                cmmMakeDynamicReference dflags addImportCmmOpt referenceKind lbl
        CmmLit (CmmLabelOff lbl off)
           -> do
                 dynRef <- cmmMakeDynamicReference dflags addImportCmmOpt referenceKind lbl
                 -- need to optimize here, since it's late
                 return $ cmmMachOpFold (MO_Add wordWidth) [
                     dynRef,
                     (CmmLit $ CmmInt (fromIntegral off) wordWidth)
                   ]

        -- On powerpc (non-PIC), it's easier to jump directly to a label than
        -- to use the register table, so we replace these registers
        -- with the corresponding labels:
        CmmReg (CmmGlobal EagerBlackholeInfo)
          | arch == ArchPPC && not opt_PIC
          -> cmmExprNative referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "__stg_EAGER_BLACKHOLE_info")))
        CmmReg (CmmGlobal GCEnter1)
          | arch == ArchPPC && not opt_PIC
          -> cmmExprNative referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "__stg_gc_enter_1"))) 
        CmmReg (CmmGlobal GCFun)
          | arch == ArchPPC && not opt_PIC
          -> cmmExprNative referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "__stg_gc_fun")))

        other
           -> return other

\end{code}

