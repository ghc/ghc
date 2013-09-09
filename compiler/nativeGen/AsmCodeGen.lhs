-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1993-2004
--
-- This is the top-level module in the native code generator.
--
-- -----------------------------------------------------------------------------

\begin{code}
{-# LANGUAGE GADTs #-}
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
import qualified PPC.Regs
import qualified PPC.RegInfo
import qualified PPC.Instr
import qualified PPC.Ppr

import RegAlloc.Liveness
import qualified RegAlloc.Linear.Main           as Linear

import qualified GraphColor                     as Color
import qualified RegAlloc.Graph.Main            as Color
import qualified RegAlloc.Graph.Stats           as Color
import qualified RegAlloc.Graph.TrivColorable   as Color

import TargetReg
import Platform
import Config
import Instruction
import PIC
import Reg
import NCGMonad

import BlockId
import CgUtils          ( fixStgRegisters )
import Cmm
import CmmUtils
import Hoopl
import CmmOpt           ( cmmMachOpFold )
import PprCmm
import CLabel

import UniqFM
import UniqSupply
import DynFlags
import Util

import BasicTypes       ( Alignment )
import Digraph
import qualified Pretty
import BufWrite
import Outputable
import FastString
import UniqSet
import ErrUtils
import Module
import Stream (Stream)
import qualified Stream

-- DEBUGGING ONLY
--import OrdList

import Data.List
import Data.Maybe
import Control.Exception
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
    a 'SDoc').

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
    cmmTopCodeGen             :: RawCmmDecl -> NatM [NatCmmDecl statics instr],
    generateJumpTableForInstr :: instr -> Maybe (NatCmmDecl statics instr),
    getJumpDestBlockId        :: jumpDest -> Maybe BlockId,
    canShortcut               :: instr -> Maybe jumpDest,
    shortcutStatics           :: (BlockId -> Maybe jumpDest) -> statics -> statics,
    shortcutJump              :: (BlockId -> Maybe jumpDest) -> instr -> instr,
    pprNatCmmDecl             :: NatCmmDecl statics instr -> SDoc,
    maxSpillSlots             :: Int,
    allocatableRegs           :: [RealReg],
    ncg_x86fp_kludge          :: [NatCmmDecl statics instr] -> [NatCmmDecl statics instr],
    ncgExpandTop              :: [NatCmmDecl statics instr] -> [NatCmmDecl statics instr],
    ncgAllocMoreStack         :: Int -> NatCmmDecl statics instr -> UniqSM (NatCmmDecl statics instr),
    ncgMakeFarBranches        :: BlockEnv CmmStatics -> [NatBasicBlock instr] -> [NatBasicBlock instr]
    }

--------------------
nativeCodeGen :: DynFlags -> Module -> Handle -> UniqSupply
              -> Stream IO RawCmmGroup ()
              -> IO UniqSupply
nativeCodeGen dflags this_mod h us cmms
 = let platform = targetPlatform dflags
       nCG' :: (Outputable statics, Outputable instr, Instruction instr)
            => NcgImpl statics instr jumpDest -> IO UniqSupply
       nCG' ncgImpl = nativeCodeGen' dflags this_mod ncgImpl h us cmms
   in case platformArch platform of
      ArchX86     -> nCG' (x86NcgImpl    dflags)
      ArchX86_64  -> nCG' (x86_64NcgImpl dflags)
      ArchPPC     -> nCG' (ppcNcgImpl    dflags)
      ArchSPARC   -> nCG' (sparcNcgImpl  dflags)
      ArchARM {}  -> panic "nativeCodeGen: No NCG for ARM"
      ArchPPC_64  -> panic "nativeCodeGen: No NCG for PPC 64"
      ArchAlpha   -> panic "nativeCodeGen: No NCG for Alpha"
      ArchMipseb  -> panic "nativeCodeGen: No NCG for mipseb"
      ArchMipsel  -> panic "nativeCodeGen: No NCG for mipsel"
      ArchUnknown -> panic "nativeCodeGen: No NCG for unknown arch"
      ArchJavaScript -> panic "nativeCodeGen: No NCG for JavaScript"

x86NcgImpl :: DynFlags -> NcgImpl (Alignment, CmmStatics) X86.Instr.Instr X86.Instr.JumpDest
x86NcgImpl dflags
 = (x86_64NcgImpl dflags) { ncg_x86fp_kludge = map x86fp_kludge }

x86_64NcgImpl :: DynFlags -> NcgImpl (Alignment, CmmStatics) X86.Instr.Instr X86.Instr.JumpDest
x86_64NcgImpl dflags
 = NcgImpl {
        cmmTopCodeGen             = X86.CodeGen.cmmTopCodeGen
       ,generateJumpTableForInstr = X86.CodeGen.generateJumpTableForInstr dflags
       ,getJumpDestBlockId        = X86.Instr.getJumpDestBlockId
       ,canShortcut               = X86.Instr.canShortcut
       ,shortcutStatics           = X86.Instr.shortcutStatics
       ,shortcutJump              = X86.Instr.shortcutJump
       ,pprNatCmmDecl             = X86.Ppr.pprNatCmmDecl
       ,maxSpillSlots             = X86.Instr.maxSpillSlots dflags
       ,allocatableRegs           = X86.Regs.allocatableRegs platform
       ,ncg_x86fp_kludge          = id
       ,ncgAllocMoreStack         = X86.Instr.allocMoreStack platform
       ,ncgExpandTop              = id
       ,ncgMakeFarBranches        = const id
   }
    where platform = targetPlatform dflags

ppcNcgImpl :: DynFlags -> NcgImpl CmmStatics PPC.Instr.Instr PPC.RegInfo.JumpDest
ppcNcgImpl dflags
 = NcgImpl {
        cmmTopCodeGen             = PPC.CodeGen.cmmTopCodeGen
       ,generateJumpTableForInstr = PPC.CodeGen.generateJumpTableForInstr dflags
       ,getJumpDestBlockId        = PPC.RegInfo.getJumpDestBlockId
       ,canShortcut               = PPC.RegInfo.canShortcut
       ,shortcutStatics           = PPC.RegInfo.shortcutStatics
       ,shortcutJump              = PPC.RegInfo.shortcutJump
       ,pprNatCmmDecl             = PPC.Ppr.pprNatCmmDecl
       ,maxSpillSlots             = PPC.Instr.maxSpillSlots dflags
       ,allocatableRegs           = PPC.Regs.allocatableRegs platform
       ,ncg_x86fp_kludge          = id
       ,ncgAllocMoreStack         = PPC.Instr.allocMoreStack platform
       ,ncgExpandTop              = id
       ,ncgMakeFarBranches        = PPC.Instr.makeFarBranches
   }
    where platform = targetPlatform dflags

sparcNcgImpl :: DynFlags -> NcgImpl CmmStatics SPARC.Instr.Instr SPARC.ShortcutJump.JumpDest
sparcNcgImpl dflags
 = NcgImpl {
        cmmTopCodeGen             = SPARC.CodeGen.cmmTopCodeGen
       ,generateJumpTableForInstr = SPARC.CodeGen.generateJumpTableForInstr dflags
       ,getJumpDestBlockId        = SPARC.ShortcutJump.getJumpDestBlockId
       ,canShortcut               = SPARC.ShortcutJump.canShortcut
       ,shortcutStatics           = SPARC.ShortcutJump.shortcutStatics
       ,shortcutJump              = SPARC.ShortcutJump.shortcutJump
       ,pprNatCmmDecl             = SPARC.Ppr.pprNatCmmDecl
       ,maxSpillSlots             = SPARC.Instr.maxSpillSlots dflags
       ,allocatableRegs           = SPARC.Regs.allocatableRegs
       ,ncg_x86fp_kludge          = id
       ,ncgAllocMoreStack         = noAllocMoreStack
       ,ncgExpandTop              = map SPARC.CodeGen.Expand.expandTop
       ,ncgMakeFarBranches        = const id
   }

--
-- Allocating more stack space for spilling is currently only
-- supported for the linear register allocator on x86/x86_64, the rest
-- default to the panic below.  To support allocating extra stack on
-- more platforms provide a definition of ncgAllocMoreStack.
--
noAllocMoreStack :: Int -> NatCmmDecl statics instr -> UniqSM (NatCmmDecl statics instr)
noAllocMoreStack amount _
  = panic $   "Register allocator: out of stack slots (need " ++ show amount ++ ")\n"
        ++  "   If you are trying to compile SHA1.hs from the crypto library then this\n"
        ++  "   is a known limitation in the linear allocator.\n"
        ++  "\n"
        ++  "   Try enabling the graph colouring allocator with -fregs-graph instead."
        ++  "   You can still file a bug report if you like.\n"


type NativeGenAcc statics instr
        = ([[CLabel]],
           [([NatCmmDecl statics instr],
             Maybe [Color.RegAllocStats statics instr],
             Maybe [Linear.RegAllocStats])])

nativeCodeGen' :: (Outputable statics, Outputable instr, Instruction instr)
               => DynFlags
               -> Module
               -> NcgImpl statics instr jumpDest
               -> Handle
               -> UniqSupply
               -> Stream IO RawCmmGroup ()
               -> IO UniqSupply
nativeCodeGen' dflags this_mod ncgImpl h us cmms
 = do
        let split_cmms  = Stream.map add_split cmms
        -- BufHandle is a performance hack.  We could hide it inside
        -- Pretty if it weren't for the fact that we do lots of little
        -- printDocs here (in order to do codegen in constant space).
        bufh <- newBufHandle h
        (ngs, us') <- cmmNativeGenStream dflags this_mod ncgImpl bufh us split_cmms ([], [])
        finishNativeGen dflags ncgImpl bufh ngs

        return us'

 where  add_split tops
                | gopt Opt_SplitObjs dflags = split_marker : tops
                | otherwise                 = tops

        split_marker = CmmProc mapEmpty mkSplitMarkerLabel []
                               (ofBlockList (panic "split_marker_entry") [])


finishNativeGen :: Instruction instr
                => DynFlags
                -> NcgImpl statics instr jumpDest
                -> BufHandle
                -> NativeGenAcc statics instr
                -> IO ()
finishNativeGen dflags ncgImpl bufh@(BufHandle _ _ h) (imports, prof)
 = do
        bFlush bufh

        let platform = targetPlatform dflags
        let (native, colorStats, linearStats)
                = unzip3 prof

        -- dump native code
        dumpIfSet_dyn dflags
                Opt_D_dump_asm "Asm code"
                (vcat $ map (pprNatCmmDecl ncgImpl) $ concat native)

        -- dump global NCG stats for graph coloring allocator
        (case concat $ catMaybes colorStats of
          []    -> return ()
          stats -> do
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
                                (targetRegDotColor platform)
                                (Color.trivColorable platform
                                        (targetVirtualRegSqueeze platform)
                                        (targetRealRegSqueeze platform))
                        $ graphGlobal)


        -- dump global NCG stats for linear allocator
        (case concat $ catMaybes linearStats of
                []      -> return ()
                stats   -> dumpSDoc dflags Opt_D_dump_asm_stats "NCG stats"
                                $ Linear.pprStats (concat native) stats)

        -- write out the imports
        Pretty.printDoc Pretty.LeftMode (pprCols dflags) h
                $ withPprStyleDoc dflags (mkCodeStyle AsmStyle)
                $ makeImportsDoc dflags (concat imports)

cmmNativeGenStream :: (Outputable statics, Outputable instr, Instruction instr)
              => DynFlags
              -> Module
              -> NcgImpl statics instr jumpDest
              -> BufHandle
              -> UniqSupply
              -> Stream IO RawCmmGroup ()
              -> NativeGenAcc statics instr
              -> IO (NativeGenAcc statics instr, UniqSupply)

cmmNativeGenStream dflags this_mod ncgImpl h us cmm_stream ngs@(impAcc, profAcc)
 = do r <- Stream.runStream cmm_stream
      case r of
          Left () ->
              return ((reverse impAcc, reverse profAcc) , us)
          Right (cmms, cmm_stream') -> do
              (ngs',us') <- cmmNativeGens dflags this_mod ncgImpl h us cmms ngs 0
              cmmNativeGenStream dflags this_mod ncgImpl h us' cmm_stream' ngs'

-- | Do native code generation on all these cmms.
--
cmmNativeGens :: (Outputable statics, Outputable instr, Instruction instr)
              => DynFlags
              -> Module
              -> NcgImpl statics instr jumpDest
              -> BufHandle
              -> UniqSupply
              -> [RawCmmDecl]
              -> NativeGenAcc statics instr
              -> Int
              -> IO (NativeGenAcc statics instr, UniqSupply)

cmmNativeGens _ _ _ _ us [] ngs _
        = return (ngs, us)

cmmNativeGens dflags this_mod ncgImpl h us (cmm : cmms) (impAcc, profAcc) count
 = do
        (us', native, imports, colorStats, linearStats)
                <- {-# SCC "cmmNativeGen" #-} cmmNativeGen dflags this_mod ncgImpl us cmm count

        {-# SCC "pprNativeCode" #-} Pretty.bufLeftRender h
                $ withPprStyleDoc dflags (mkCodeStyle AsmStyle)
                $ vcat $ map (pprNatCmmDecl ncgImpl) native

        let !lsPprNative =
                if  dopt Opt_D_dump_asm       dflags
                 || dopt Opt_D_dump_asm_stats dflags
                        then native
                        else []

        let !count' = count + 1

        -- force evaluation all this stuff to avoid space leaks
        {-# SCC "seqString" #-} evaluate $ seqString (showSDoc dflags $ vcat $ map ppr imports)

        cmmNativeGens dflags this_mod ncgImpl h
            us' cmms ((imports : impAcc),
                      ((lsPprNative, colorStats, linearStats) : profAcc))
                     count'

 where  seqString []            = ()
        seqString (x:xs)        = x `seq` seqString xs


-- | Complete native code generation phase for a single top-level chunk of Cmm.
--      Dumping the output of each stage along the way.
--      Global conflict graph and NGC stats
cmmNativeGen
        :: (Outputable statics, Outputable instr, Instruction instr)
    => DynFlags
    -> Module
    -> NcgImpl statics instr jumpDest
        -> UniqSupply
        -> RawCmmDecl                                   -- ^ the cmm to generate code for
        -> Int                                          -- ^ sequence number of this top thing
        -> IO   ( UniqSupply
                , [NatCmmDecl statics instr]                -- native code
                , [CLabel]                                  -- things imported by this cmm
                , Maybe [Color.RegAllocStats statics instr] -- stats for the coloring register allocator
                , Maybe [Linear.RegAllocStats])             -- stats for the linear register allocators

cmmNativeGen dflags this_mod ncgImpl us cmm count
 = do
        let platform = targetPlatform dflags

        -- rewrite assignments to global regs
        let fixed_cmm =
                {-# SCC "fixStgRegisters" #-}
                fixStgRegisters dflags cmm

        -- cmm to cmm optimisations
        let (opt_cmm, imports) =
                {-# SCC "cmmToCmm" #-}
                cmmToCmm dflags this_mod fixed_cmm

        dumpIfSet_dyn dflags
                Opt_D_dump_opt_cmm "Optimised Cmm"
                (pprCmmGroup [opt_cmm])

        -- generate native code from cmm
        let ((native, lastMinuteImports), usGen) =
                {-# SCC "genMachCode" #-}
                initUs us $ genMachCode dflags this_mod (cmmTopCodeGen ncgImpl) opt_cmm

        dumpIfSet_dyn dflags
                Opt_D_dump_asm_native "Native code"
                (vcat $ map (pprNatCmmDecl ncgImpl) native)

        -- tag instructions with register liveness information
        let (withLiveness, usLive) =
                {-# SCC "regLiveness" #-}
                initUs usGen
                        $ mapM (regLiveness platform)
                        $ map natCmmTopToLive native

        dumpIfSet_dyn dflags
                Opt_D_dump_asm_liveness "Liveness annotations added"
                (vcat $ map ppr withLiveness)

        -- allocate registers
        (alloced, usAlloc, ppr_raStatsColor, ppr_raStatsLinear) <-
         if ( gopt Opt_RegsGraph dflags
           || gopt Opt_RegsIterative dflags)
          then do
                -- the regs usable for allocation
                let (alloc_regs :: UniqFM (UniqSet RealReg))
                        = foldr (\r -> plusUFM_C unionUniqSets
                                        $ unitUFM (targetClassOfRealReg platform r) (unitUniqSet r))
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
                        (vcat $ map (pprNatCmmDecl ncgImpl) alloced)

                dumpIfSet_dyn dflags
                        Opt_D_dump_asm_regalloc_stages "Build/spill stages"
                        (vcat   $ map (\(stage, stats)
                                        -> text "# --------------------------"
                                        $$ text "#  cmm " <> int count <> text " Stage " <> int stage
                                        $$ ppr stats)
                                $ zip [0..] regAllocStats)

                let mPprStats =
                        if dopt Opt_D_dump_asm_stats dflags
                         then Just regAllocStats else Nothing

                -- force evaluation of the Maybe to avoid space leak
                mPprStats `seq` return ()

                return  ( alloced, usAlloc
                        , mPprStats
                        , Nothing)

          else do
                -- do linear register allocation
                let reg_alloc proc = do
                       (alloced, maybe_more_stack, ra_stats) <-
                               Linear.regAlloc dflags proc
                       case maybe_more_stack of
                         Nothing -> return ( alloced, ra_stats )
                         Just amount -> do
                           alloced' <- ncgAllocMoreStack ncgImpl amount alloced
                           return (alloced', ra_stats )

                let ((alloced, regAllocStats), usAlloc)
                        = {-# SCC "RegAlloc" #-}
                          initUs usLive
                          $ liftM unzip
                          $ mapM reg_alloc withLiveness

                dumpIfSet_dyn dflags
                        Opt_D_dump_asm_regalloc "Registers allocated"
                        (vcat $ map (pprNatCmmDecl ncgImpl) alloced)

                let mPprStats =
                        if dopt Opt_D_dump_asm_stats dflags
                         then Just (catMaybes regAllocStats) else Nothing

                -- force evaluation of the Maybe to avoid space leak
                mPprStats `seq` return ()

                return  ( alloced, usAlloc
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
        let tabled      =
                {-# SCC "generateJumpTables" #-}
                generateJumpTables ncgImpl kludged

        ---- shortcut branches
        let shorted     =
                {-# SCC "shortcutBranches" #-}
                shortcutBranches dflags ncgImpl tabled

        ---- sequence blocks
        let sequenced   =
                {-# SCC "sequenceBlocks" #-}
                map (sequenceTop ncgImpl) shorted

        ---- expansion of SPARC synthetic instrs
        let expanded =
                {-# SCC "sparc_expand" #-}
                ncgExpandTop ncgImpl sequenced

        dumpIfSet_dyn dflags
                Opt_D_dump_asm_expanded "Synthetic instructions expanded"
                (vcat $ map (pprNatCmmDecl ncgImpl) expanded)

        return  ( usAlloc
                , expanded
                , lastMinuteImports ++ imports
                , ppr_raStatsColor
                , ppr_raStatsLinear)


x86fp_kludge :: NatCmmDecl (Alignment, CmmStatics) X86.Instr.Instr -> NatCmmDecl (Alignment, CmmStatics) X86.Instr.Instr
x86fp_kludge top@(CmmData _ _) = top
x86fp_kludge (CmmProc info lbl live (ListGraph code)) =
        CmmProc info lbl live (ListGraph $ X86.Instr.i386_insert_ffrees code)


-- | Build a doc for all the imports.
--
makeImportsDoc :: DynFlags -> [CLabel] -> SDoc
makeImportsDoc dflags imports
 = dyld_stubs imports
            $$
            -- On recent versions of Darwin, the linker supports
            -- dead-stripping of code and data on a per-symbol basis.
            -- There's a hack to make this work in PprMach.pprNatCmmDecl.
            (if platformHasSubsectionsViaSymbols platform
             then text ".subsections_via_symbols"
             else empty)
            $$
                -- On recent GNU ELF systems one can mark an object file
                -- as not requiring an executable stack. If all objects
                -- linked into a program have this note then the program
                -- will not use an executable stack, which is good for
                -- security. GHC generated code does not need an executable
                -- stack so add the note in:
            (if platformHasGnuNonexecStack platform
             then text ".section .note.GNU-stack,\"\",@progbits"
             else empty)
            $$
                -- And just because every other compiler does, lets stick in
                -- an identifier directive: .ident "GHC x.y.z"
            (if platformHasIdentDirective platform
             then let compilerIdent = text "GHC" <+> text cProjectVersion
                   in text ".ident" <+> doubleQuotes compilerIdent
             else empty)

 where
        platform = targetPlatform dflags
        arch = platformArch platform
        os   = platformOS   platform

        -- Generate "symbol stubs" for all external symbols that might
        -- come from a dynamic library.
        dyld_stubs :: [CLabel] -> SDoc
{-      dyld_stubs imps = vcat $ map pprDyldSymbolStub $
                                    map head $ group $ sort imps-}
        -- (Hack) sometimes two Labels pretty-print the same, but have
        -- different uniques; so we compare their text versions...
        dyld_stubs imps
                | needImportedSymbols dflags arch os
                = vcat $
                        (pprGotDeclaration dflags arch os :) $
                        map ( pprImportedSymbol dflags platform . fst . head) $
                        groupBy (\(_,a) (_,b) -> a == b) $
                        sortBy (\(_,a) (_,b) -> compare a b) $
                        map doPpr $
                        imps
                | otherwise
                = empty

        doPpr lbl = (lbl, renderWithStyle dflags (pprCLabel platform lbl) astyle)
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
    => NcgImpl statics instr jumpDest -> NatCmmDecl statics instr -> NatCmmDecl statics instr

sequenceTop _       top@(CmmData _ _) = top
sequenceTop ncgImpl (CmmProc info lbl live (ListGraph blocks)) =
  CmmProc info lbl live (ListGraph $ ncgMakeFarBranches ncgImpl info $ sequenceBlocks info blocks)

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
        => BlockEnv i
        -> [NatBasicBlock instr]
        -> [NatBasicBlock instr]

sequenceBlocks _ [] = []
sequenceBlocks infos (entry:blocks) =
  seqBlocks infos (mkNode entry : reverse (flattenSCCs (sccBlocks blocks)))
  -- the first block is the entry point ==> it must remain at the start.


sccBlocks
        :: Instruction instr
        => [NatBasicBlock instr]
        -> [SCC ( NatBasicBlock instr
                , BlockId
                , [BlockId])]

sccBlocks blocks = stronglyConnCompFromEdgedVerticesR (map mkNode blocks)

-- we're only interested in the last instruction of
-- the block, and only if it has a single destination.
getOutEdges
        :: Instruction instr
        => [instr] -> [BlockId]

getOutEdges instrs
        = case jumpDestsOfInstr (last instrs) of
                [one] -> [one]
                _many -> []

mkNode :: (Instruction t)
       => GenBasicBlock t
       -> (GenBasicBlock t, BlockId, [BlockId])
mkNode block@(BasicBlock id instrs) = (block, id, getOutEdges instrs)

seqBlocks :: BlockEnv i -> [(GenBasicBlock t1, BlockId, [BlockId])]
                        -> [GenBasicBlock t1]
seqBlocks _ [] = []
seqBlocks infos ((block,_,[]) : rest)
  = block : seqBlocks infos rest
seqBlocks infos ((block@(BasicBlock id instrs),_,[next]) : rest)
  | can_fallthrough = BasicBlock id (init instrs) : seqBlocks infos rest'
  | otherwise       = block : seqBlocks infos rest'
  where
        can_fallthrough = not (mapMember next infos) && can_reorder
        (can_reorder, rest') = reorder next [] rest
          -- TODO: we should do a better job for cycles; try to maximise the
          -- fallthroughs within a loop.
seqBlocks _ _ = panic "AsmCodegen:seqBlocks"

reorder :: (Eq a) => a -> [(t, a, t1)] -> [(t, a, t1)] -> (Bool, [(t, a, t1)])
reorder  _ accum [] = (False, reverse accum)
reorder id accum (b@(block,id',out) : rest)
  | id == id'  = (True, (block,id,out) : reverse accum ++ rest)
  | otherwise  = reorder id (b:accum) rest


-- -----------------------------------------------------------------------------
-- Generate jump tables

-- Analyzes all native code and generates data sections for all jump
-- table instructions.
generateJumpTables
        :: NcgImpl statics instr jumpDest
        -> [NatCmmDecl statics instr] -> [NatCmmDecl statics instr]
generateJumpTables ncgImpl xs = concatMap f xs
    where f p@(CmmProc _ _ _ (ListGraph xs)) = p : concatMap g xs
          f p = [p]
          g (BasicBlock _ xs) = catMaybes (map (generateJumpTableForInstr ncgImpl) xs)

-- -----------------------------------------------------------------------------
-- Shortcut branches

shortcutBranches
        :: DynFlags
    -> NcgImpl statics instr jumpDest
        -> [NatCmmDecl statics instr]
        -> [NatCmmDecl statics instr]

shortcutBranches dflags ncgImpl tops
  | optLevel dflags < 1 = tops    -- only with -O or higher
  | otherwise           = map (apply_mapping ncgImpl mapping) tops'
  where
    (tops', mappings) = mapAndUnzip (build_mapping ncgImpl) tops
    mapping = foldr plusUFM emptyUFM mappings

build_mapping :: NcgImpl statics instr jumpDest
              -> GenCmmDecl d (BlockEnv t) (ListGraph instr)
              -> (GenCmmDecl d (BlockEnv t) (ListGraph instr), UniqFM jumpDest)
build_mapping _ top@(CmmData _ _) = (top, emptyUFM)
build_mapping _ (CmmProc info lbl live (ListGraph []))
  = (CmmProc info lbl live (ListGraph []), emptyUFM)
build_mapping ncgImpl (CmmProc info lbl live (ListGraph (head:blocks)))
  = (CmmProc info lbl live (ListGraph (head:others)), mapping)
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
          not (has_info id),
          (setMember dest s) || dest == id -- loop checks
        = (s, shortcut_blocks, b : others)
    split (s, shortcut_blocks, others) (BasicBlock id [insn])
        | Just dest <- canShortcut ncgImpl insn,
          not (has_info id)
        = (setInsert id s, (id,dest) : shortcut_blocks, others)
    split (s, shortcut_blocks, others) other = (s, shortcut_blocks, other : others)

    -- do not eliminate blocks that have an info table
    has_info l = mapMember l info

    -- build a mapping from BlockId to JumpDest for shorting branches
    mapping = foldl add emptyUFM shortcut_blocks
    add ufm (id,dest) = addToUFM ufm id dest

apply_mapping :: NcgImpl statics instr jumpDest
              -> UniqFM jumpDest
              -> GenCmmDecl statics h (ListGraph instr)
              -> GenCmmDecl statics h (ListGraph instr)
apply_mapping ncgImpl ufm (CmmData sec statics)
  = CmmData sec (shortcutStatics ncgImpl (lookupUFM ufm) statics)
apply_mapping ncgImpl ufm (CmmProc info lbl live (ListGraph blocks))
  = CmmProc info lbl live (ListGraph $ map short_bb blocks)
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
        -> Module
        -> (RawCmmDecl -> NatM [NatCmmDecl statics instr])
        -> RawCmmDecl
        -> UniqSM
                ( [NatCmmDecl statics instr]
                , [CLabel])

genMachCode dflags this_mod cmmTopCodeGen cmm_top
  = do  { initial_us <- getUs
        ; let initial_st           = mkNatM_State initial_us 0 dflags this_mod
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
  (c) Position independent code and dynamic linking
        (i)  introduce the appropriate indirections
             and position independent refs
        (ii) compile a list of imported symbols
  (d) Some arch-specific optimizations

(a) will be moving to the new Hoopl pipeline, however, (c) and
(d) are only needed by the native backend and will continue to live
here.

Ideas for other things we could do (put these in Hoopl please!):

  - shortcut jumps-to-jumps
  - simple CSE: if an expr is assigned to a temp, then replace later occs of
    that expr with the temp, until the expr is no longer valid (can push through
    temp assignments, and certain assigns to mem...)
-}

cmmToCmm :: DynFlags -> Module -> RawCmmDecl -> (RawCmmDecl, [CLabel])
cmmToCmm _ _ top@(CmmData _ _) = (top, [])
cmmToCmm dflags this_mod (CmmProc info lbl live graph)
    = runCmmOpt dflags this_mod $
      do blocks' <- mapM cmmBlockConFold (toBlockList graph)
         return $ CmmProc info lbl live (ofBlockList (g_entry graph) blocks')

newtype CmmOptM a = CmmOptM (DynFlags -> Module -> [CLabel] -> (# a, [CLabel] #))

instance Monad CmmOptM where
  return x = CmmOptM $ \_ _ imports -> (# x, imports #)
  (CmmOptM f) >>= g =
    CmmOptM $ \dflags this_mod imports ->
                case f dflags this_mod imports of
                  (# x, imports' #) ->
                    case g x of
                      CmmOptM g' -> g' dflags this_mod imports'

instance CmmMakeDynamicReferenceM CmmOptM where
    addImport = addImportCmmOpt
    getThisModule = CmmOptM $ \_ this_mod imports -> (# this_mod, imports #)

addImportCmmOpt :: CLabel -> CmmOptM ()
addImportCmmOpt lbl = CmmOptM $ \_ _ imports -> (# (), lbl:imports #)

instance HasDynFlags CmmOptM where
    getDynFlags = CmmOptM $ \dflags _ imports -> (# dflags, imports #)

runCmmOpt :: DynFlags -> Module -> CmmOptM a -> (a, [CLabel])
runCmmOpt dflags this_mod (CmmOptM f) = case f dflags this_mod [] of
                        (# result, imports #) -> (result, imports)

cmmBlockConFold :: CmmBlock -> CmmOptM CmmBlock
cmmBlockConFold block = do
  let (entry, middle, last) = blockSplit block
      stmts = blockToList middle
  stmts' <- mapM cmmStmtConFold stmts
  last' <- cmmStmtConFold last
  return $ blockJoin entry (blockFromList stmts') last'

-- This does three optimizations, but they're very quick to check, so we don't
-- bother turning them off even when the Hoopl code is active.  Since
-- this is on the old Cmm representation, we can't reuse the code either:
--  * reg = reg      --> nop
--  * if 0 then jump --> nop
--  * if 1 then jump --> jump
-- We might be tempted to skip this step entirely of not Opt_PIC, but
-- there is some PowerPC code for the non-PIC case, which would also
-- have to be separated.
cmmStmtConFold :: CmmNode e x -> CmmOptM (CmmNode e x)
cmmStmtConFold stmt
   = case stmt of
        CmmAssign reg src
           -> do src' <- cmmExprConFold DataReference src
                 return $ case src' of
                   CmmReg reg' | reg == reg' -> CmmComment (fsLit "nop")
                   new_src -> CmmAssign reg new_src

        CmmStore addr src
           -> do addr' <- cmmExprConFold DataReference addr
                 src'  <- cmmExprConFold DataReference src
                 return $ CmmStore addr' src'

        CmmCall { cml_target = addr }
           -> do addr' <- cmmExprConFold JumpReference addr
                 return $ stmt { cml_target = addr' }

        CmmUnsafeForeignCall target regs args
           -> do target' <- case target of
                              ForeignTarget e conv -> do
                                e' <- cmmExprConFold CallReference e
                                return $ ForeignTarget e' conv
                              PrimTarget _ ->
                                return target
                 args' <- mapM (cmmExprConFold DataReference) args
                 return $ CmmUnsafeForeignCall target' regs args'

        CmmCondBranch test true false
           -> do test' <- cmmExprConFold DataReference test
                 return $ case test' of
                   CmmLit (CmmInt 0 _) -> CmmBranch false
                   CmmLit (CmmInt _ _) -> CmmBranch true
                   _other -> CmmCondBranch test' true false

        CmmSwitch expr ids
           -> do expr' <- cmmExprConFold DataReference expr
                 return $ CmmSwitch expr' ids

        other
           -> return other

cmmExprConFold :: ReferenceKind -> CmmExpr -> CmmOptM CmmExpr
cmmExprConFold referenceKind expr = do
    dflags <- getDynFlags

    -- With -O1 and greater, the cmmSink pass does constant-folding, so
    -- we don't need to do it again here.
    let expr' = if optLevel dflags >= 1
                    then expr
                    else cmmExprCon dflags expr

    cmmExprNative referenceKind expr'

cmmExprCon :: DynFlags -> CmmExpr -> CmmExpr
cmmExprCon dflags (CmmLoad addr rep) = CmmLoad (cmmExprCon dflags addr) rep
cmmExprCon dflags (CmmMachOp mop args)
    = cmmMachOpFold dflags mop (map (cmmExprCon dflags) args)
cmmExprCon _ other = other

-- handles both PIC and non-PIC cases... a very strange mixture
-- of things to do.
cmmExprNative :: ReferenceKind -> CmmExpr -> CmmOptM CmmExpr
cmmExprNative referenceKind expr = do
     dflags <- getDynFlags
     let platform = targetPlatform dflags
         arch = platformArch platform
     case expr of
        CmmLoad addr rep
           -> do addr' <- cmmExprNative DataReference addr
                 return $ CmmLoad addr' rep

        CmmMachOp mop args
           -> do args' <- mapM (cmmExprNative DataReference) args
                 return $ CmmMachOp mop args'

        CmmLit (CmmBlock id)
           -> cmmExprNative referenceKind (CmmLit (CmmLabel (infoTblLbl id)))
           -- we must convert block Ids to CLabels here, because we
           -- might have to do the PIC transformation.  Hence we must
           -- not modify BlockIds beyond this point.

        CmmLit (CmmLabel lbl)
           -> do
                cmmMakeDynamicReference dflags referenceKind lbl
        CmmLit (CmmLabelOff lbl off)
           -> do
                 dynRef <- cmmMakeDynamicReference dflags referenceKind lbl
                 -- need to optimize here, since it's late
                 return $ cmmMachOpFold dflags (MO_Add (wordWidth dflags)) [
                     dynRef,
                     (CmmLit $ CmmInt (fromIntegral off) (wordWidth dflags))
                   ]

        -- On powerpc (non-PIC), it's easier to jump directly to a label than
        -- to use the register table, so we replace these registers
        -- with the corresponding labels:
        CmmReg (CmmGlobal EagerBlackholeInfo)
          | arch == ArchPPC && not (gopt Opt_PIC dflags)
          -> cmmExprNative referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "__stg_EAGER_BLACKHOLE_info")))
        CmmReg (CmmGlobal GCEnter1)
          | arch == ArchPPC && not (gopt Opt_PIC dflags)
          -> cmmExprNative referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "__stg_gc_enter_1")))
        CmmReg (CmmGlobal GCFun)
          | arch == ArchPPC && not (gopt Opt_PIC dflags)
          -> cmmExprNative referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "__stg_gc_fun")))

        other
           -> return other

\end{code}

