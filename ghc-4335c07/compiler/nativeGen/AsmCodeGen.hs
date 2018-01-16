-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1993-2004
--
-- This is the top-level module in the native code generator.
--
-- -----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns, CPP, GADTs, ScopedTypeVariables, UnboxedTuples #-}

module AsmCodeGen (
                    -- * Module entry point
                    nativeCodeGen

                    -- * Test-only exports: see trac #12744
                    -- used by testGraphNoSpills, which needs to access
                    -- the register allocator intermediate data structures
                    -- cmmNativeGen emits
                  , cmmNativeGen
                  , NcgImpl(..)
                  , x86NcgImpl
                  ) where

#include "HsVersions.h"
#include "nativeGen/NCG.h"


import GhcPrelude

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

import AsmUtils
import TargetReg
import Platform
import Config
import Instruction
import PIC
import Reg
import NCGMonad
import Dwarf
import Debug

import BlockId
import CgUtils          ( fixStgRegisters )
import Cmm
import CmmUtils
import Hoopl.Collections
import Hoopl.Label
import Hoopl.Block
import CmmOpt           ( cmmMachOpFold )
import PprCmm
import CLabel

import UniqFM
import UniqSupply
import DynFlags
import Util
import Unique

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
import Data.Ord         ( comparing )
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
    ncgMakeFarBranches        :: LabelMap CmmStatics
                              -> [NatBasicBlock instr] -> [NatBasicBlock instr],
    extractUnwindPoints       :: [instr] -> [UnwindPoint]
    -- ^ given the instruction sequence of a block, produce a list of
    -- the block's 'UnwindPoint's
    -- See Note [What is this unwinding business?] in Debug
    -- and Note [Unwinding information in the NCG] in this module.
    }

--------------------
nativeCodeGen :: DynFlags -> Module -> ModLocation -> Handle -> UniqSupply
              -> Stream IO RawCmmGroup ()
              -> IO UniqSupply
nativeCodeGen dflags this_mod modLoc h us cmms
 = let platform = targetPlatform dflags
       nCG' :: (Outputable statics, Outputable instr, Instruction instr)
            => NcgImpl statics instr jumpDest -> IO UniqSupply
       nCG' ncgImpl = nativeCodeGen' dflags this_mod modLoc ncgImpl h us cmms
   in case platformArch platform of
      ArchX86       -> nCG' (x86NcgImpl    dflags)
      ArchX86_64    -> nCG' (x86_64NcgImpl dflags)
      ArchPPC       -> nCG' (ppcNcgImpl    dflags)
      ArchSPARC     -> nCG' (sparcNcgImpl  dflags)
      ArchSPARC64   -> panic "nativeCodeGen: No NCG for SPARC64"
      ArchARM {}    -> panic "nativeCodeGen: No NCG for ARM"
      ArchARM64     -> panic "nativeCodeGen: No NCG for ARM64"
      ArchPPC_64 _  -> nCG' (ppcNcgImpl    dflags)
      ArchAlpha     -> panic "nativeCodeGen: No NCG for Alpha"
      ArchMipseb    -> panic "nativeCodeGen: No NCG for mipseb"
      ArchMipsel    -> panic "nativeCodeGen: No NCG for mipsel"
      ArchUnknown   -> panic "nativeCodeGen: No NCG for unknown arch"
      ArchJavaScript-> panic "nativeCodeGen: No NCG for JavaScript"

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
       ,extractUnwindPoints       = X86.CodeGen.extractUnwindPoints
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
       ,extractUnwindPoints       = const []
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
       ,extractUnwindPoints       = const []
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


-- | Data accumulated during code generation. Mostly about statistics,
-- but also collects debug data for DWARF generation.
data NativeGenAcc statics instr
  = NGS { ngs_imports     :: ![[CLabel]]
        , ngs_natives     :: ![[NatCmmDecl statics instr]]
             -- ^ Native code generated, for statistics. This might
             -- hold a lot of data, so it is important to clear this
             -- field as early as possible if it isn't actually
             -- required.
        , ngs_colorStats  :: ![[Color.RegAllocStats statics instr]]
        , ngs_linearStats :: ![[Linear.RegAllocStats]]
        , ngs_labels      :: ![Label]
        , ngs_debug       :: ![DebugBlock]
        , ngs_dwarfFiles  :: !DwarfFiles
        , ngs_unwinds     :: !(LabelMap [UnwindPoint])
             -- ^ see Note [Unwinding information in the NCG]
             -- and Note [What is this unwinding business?] in Debug.
        }

{-
Note [Unwinding information in the NCG]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unwind information is a type of metadata which allows a debugging tool
to reconstruct the values of machine registers at the time a procedure was
entered. For the most part, the production of unwind information is handled by
the Cmm stage, where it is represented by CmmUnwind nodes.

Unfortunately, the Cmm stage doesn't know everything necessary to produce
accurate unwinding information. For instance, the x86-64 calling convention
requires that the stack pointer be aligned to 16 bytes, which in turn means that
GHC must sometimes add padding to $sp prior to performing a foreign call. When
this happens unwind information must be updated accordingly.
For this reason, we make the NCG backends responsible for producing
unwinding tables (with the extractUnwindPoints function in NcgImpl).

We accumulate the produced unwind tables over CmmGroups in the ngs_unwinds
field of NativeGenAcc. This is a label map which contains an entry for each
procedure, containing a list of unwinding points (e.g. a label and an associated
unwinding table).

See also Note [What is this unwinding business?] in Debug.
-}

nativeCodeGen' :: (Outputable statics, Outputable instr, Instruction instr)
               => DynFlags
               -> Module -> ModLocation
               -> NcgImpl statics instr jumpDest
               -> Handle
               -> UniqSupply
               -> Stream IO RawCmmGroup ()
               -> IO UniqSupply
nativeCodeGen' dflags this_mod modLoc ncgImpl h us cmms
 = do
        -- BufHandle is a performance hack.  We could hide it inside
        -- Pretty if it weren't for the fact that we do lots of little
        -- printDocs here (in order to do codegen in constant space).
        bufh <- newBufHandle h
        let ngs0 = NGS [] [] [] [] [] [] emptyUFM mapEmpty
        (ngs, us') <- cmmNativeGenStream dflags this_mod modLoc ncgImpl bufh us
                                         cmms ngs0
        finishNativeGen dflags modLoc bufh us' ngs

finishNativeGen :: Instruction instr
                => DynFlags
                -> ModLocation
                -> BufHandle
                -> UniqSupply
                -> NativeGenAcc statics instr
                -> IO UniqSupply
finishNativeGen dflags modLoc bufh@(BufHandle _ _ h) us ngs
 = do
        -- Write debug data and finish
        let emitDw = debugLevel dflags > 0 && not (gopt Opt_SplitObjs dflags)
        us' <- if not emitDw then return us else do
          (dwarf, us') <- dwarfGen dflags modLoc us (ngs_debug ngs)
          emitNativeCode dflags bufh dwarf
          return us'
        bFlush bufh

        -- dump global NCG stats for graph coloring allocator
        let stats = concat (ngs_colorStats ngs)
        when (not (null stats)) $ do

          -- build the global register conflict graph
          let graphGlobal
                  = foldl Color.union Color.initGraph
                  $ [ Color.raGraph stat
                          | stat@Color.RegAllocStatsStart{} <- stats]

          dump_stats (Color.pprStats stats graphGlobal)

          let platform = targetPlatform dflags
          dumpIfSet_dyn dflags
                  Opt_D_dump_asm_conflicts "Register conflict graph"
                  $ Color.dotGraph
                          (targetRegDotColor platform)
                          (Color.trivColorable platform
                                  (targetVirtualRegSqueeze platform)
                                  (targetRealRegSqueeze platform))
                  $ graphGlobal


        -- dump global NCG stats for linear allocator
        let linearStats = concat (ngs_linearStats ngs)
        when (not (null linearStats)) $
          dump_stats (Linear.pprStats (concat (ngs_natives ngs)) linearStats)

        -- write out the imports
        printSDocLn Pretty.LeftMode dflags h (mkCodeStyle AsmStyle)
                $ makeImportsDoc dflags (concat (ngs_imports ngs))
        return us'
  where
    dump_stats = dumpSDoc dflags alwaysQualify Opt_D_dump_asm_stats "NCG stats"

cmmNativeGenStream :: (Outputable statics, Outputable instr, Instruction instr)
              => DynFlags
              -> Module -> ModLocation
              -> NcgImpl statics instr jumpDest
              -> BufHandle
              -> UniqSupply
              -> Stream IO RawCmmGroup ()
              -> NativeGenAcc statics instr
              -> IO (NativeGenAcc statics instr, UniqSupply)

cmmNativeGenStream dflags this_mod modLoc ncgImpl h us cmm_stream ngs
 = do r <- Stream.runStream cmm_stream
      case r of
        Left () ->
          return (ngs { ngs_imports = reverse $ ngs_imports ngs
                      , ngs_natives = reverse $ ngs_natives ngs
                      , ngs_colorStats = reverse $ ngs_colorStats ngs
                      , ngs_linearStats = reverse $ ngs_linearStats ngs
                      },
                  us)
        Right (cmms, cmm_stream') -> do

          -- Generate debug information
          let debugFlag = debugLevel dflags > 0
              !ndbgs | debugFlag = cmmDebugGen modLoc cmms
                     | otherwise = []
              dbgMap = debugToMap ndbgs

          -- Insert split marker, generate native code
          let splitObjs = gopt Opt_SplitObjs dflags
              split_marker = CmmProc mapEmpty mkSplitMarkerLabel [] $
                             ofBlockList (panic "split_marker_entry") []
              cmms' | splitObjs  = split_marker : cmms
                    | otherwise  = cmms
          (ngs',us') <- cmmNativeGens dflags this_mod modLoc ncgImpl h
                                             dbgMap us cmms' ngs 0

          -- Link native code information into debug blocks
          -- See Note [What is this unwinding business?] in Debug.
          let !ldbgs = cmmDebugLink (ngs_labels ngs') (ngs_unwinds ngs') ndbgs
          dumpIfSet_dyn dflags Opt_D_dump_debug "Debug Infos"
            (vcat $ map ppr ldbgs)

          -- Emit & clear DWARF information when generating split
          -- object files, as we need it to land in the same object file
          -- When using split sections, note that we do not split the debug
          -- info but emit all the info at once in finishNativeGen.
          (ngs'', us'') <-
            if debugFlag && splitObjs
            then do (dwarf, us'') <- dwarfGen dflags modLoc us ldbgs
                    emitNativeCode dflags h dwarf
                    return (ngs' { ngs_debug = []
                                 , ngs_dwarfFiles = emptyUFM
                                 , ngs_labels = [] },
                            us'')
            else return (ngs' { ngs_debug  = ngs_debug ngs' ++ ldbgs
                              , ngs_labels = [] },
                         us')

          cmmNativeGenStream dflags this_mod modLoc ncgImpl h us''
              cmm_stream' ngs''

-- | Do native code generation on all these cmms.
--
cmmNativeGens :: forall statics instr jumpDest.
                 (Outputable statics, Outputable instr, Instruction instr)
              => DynFlags
              -> Module -> ModLocation
              -> NcgImpl statics instr jumpDest
              -> BufHandle
              -> LabelMap DebugBlock
              -> UniqSupply
              -> [RawCmmDecl]
              -> NativeGenAcc statics instr
              -> Int
              -> IO (NativeGenAcc statics instr, UniqSupply)

cmmNativeGens dflags this_mod modLoc ncgImpl h dbgMap = go
  where
    go :: UniqSupply -> [RawCmmDecl]
       -> NativeGenAcc statics instr -> Int
       -> IO (NativeGenAcc statics instr, UniqSupply)

    go us [] ngs !_ =
        return (ngs, us)

    go us (cmm : cmms) ngs count = do
        let fileIds = ngs_dwarfFiles ngs
        (us', fileIds', native, imports, colorStats, linearStats, unwinds)
          <- {-# SCC "cmmNativeGen" #-}
             cmmNativeGen dflags this_mod modLoc ncgImpl us fileIds dbgMap
                          cmm count

        -- Generate .file directives for every new file that has been
        -- used. Note that it is important that we generate these in
        -- ascending order, as Clang's 3.6 assembler complains.
        let newFileIds = sortBy (comparing snd) $
                         nonDetEltsUFM $ fileIds' `minusUFM` fileIds
            -- See Note [Unique Determinism and code generation]
            pprDecl (f,n) = text "\t.file " <> ppr n <+>
                            doubleQuotes (ftext f)

        emitNativeCode dflags h $ vcat $
          map pprDecl newFileIds ++
          map (pprNatCmmDecl ncgImpl) native

        -- force evaluation all this stuff to avoid space leaks
        {-# SCC "seqString" #-} evaluate $ seqString (showSDoc dflags $ vcat $ map ppr imports)

        let !labels' = if debugLevel dflags > 0
                       then cmmDebugLabels isMetaInstr native else []
            !natives' = if dopt Opt_D_dump_asm_stats dflags
                        then native : ngs_natives ngs else []

            mCon = maybe id (:)
            ngs' = ngs{ ngs_imports     = imports : ngs_imports ngs
                      , ngs_natives     = natives'
                      , ngs_colorStats  = colorStats `mCon` ngs_colorStats ngs
                      , ngs_linearStats = linearStats `mCon` ngs_linearStats ngs
                      , ngs_labels      = ngs_labels ngs ++ labels'
                      , ngs_dwarfFiles  = fileIds'
                      , ngs_unwinds     = ngs_unwinds ngs `mapUnion` unwinds
                      }
        go us' cmms ngs' (count + 1)

    seqString []            = ()
    seqString (x:xs)        = x `seq` seqString xs


emitNativeCode :: DynFlags -> BufHandle -> SDoc -> IO ()
emitNativeCode dflags h sdoc = do

        {-# SCC "pprNativeCode" #-} bufLeftRenderSDoc dflags h
                                      (mkCodeStyle AsmStyle) sdoc

        -- dump native code
        dumpIfSet_dyn dflags
                Opt_D_dump_asm "Asm code"
                sdoc

-- | Complete native code generation phase for a single top-level chunk of Cmm.
--      Dumping the output of each stage along the way.
--      Global conflict graph and NGC stats
cmmNativeGen
        :: (Outputable statics, Outputable instr, Instruction instr)
    => DynFlags
    -> Module -> ModLocation
    -> NcgImpl statics instr jumpDest
        -> UniqSupply
        -> DwarfFiles
        -> LabelMap DebugBlock
        -> RawCmmDecl                                   -- ^ the cmm to generate code for
        -> Int                                          -- ^ sequence number of this top thing
        -> IO   ( UniqSupply
                , DwarfFiles
                , [NatCmmDecl statics instr]                -- native code
                , [CLabel]                                  -- things imported by this cmm
                , Maybe [Color.RegAllocStats statics instr] -- stats for the coloring register allocator
                , Maybe [Linear.RegAllocStats]              -- stats for the linear register allocators
                , LabelMap [UnwindPoint]                    -- unwinding information for blocks
                )

cmmNativeGen dflags this_mod modLoc ncgImpl us fileIds dbgMap cmm count
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
        let ((native, lastMinuteImports, fileIds'), usGen) =
                {-# SCC "genMachCode" #-}
                initUs us $ genMachCode dflags this_mod modLoc
                                        (cmmTopCodeGen ncgImpl)
                                        fileIds dbgMap opt_cmm

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
           || gopt Opt_RegsIterative dflags )
          then do
                -- the regs usable for allocation
                let (alloc_regs :: UniqFM (UniqSet RealReg))
                        = foldr (\r -> plusUFM_C unionUniqSets
                                        $ unitUFM (targetClassOfRealReg platform r) (unitUniqSet r))
                                emptyUFM
                        $ allocatableRegs ncgImpl

                -- do the graph coloring register allocation
                let ((alloced, regAllocStats), usAlloc)
                        = {-# SCC "RegAlloc-color" #-}
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
                        = {-# SCC "RegAlloc-linear" #-}
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

        -- generate unwinding information from cmm
        let unwinds :: BlockMap [UnwindPoint]
            unwinds =
                {-# SCC "unwindingInfo" #-}
                foldl' addUnwind mapEmpty expanded
              where
                addUnwind acc proc =
                    acc `mapUnion` computeUnwinding dflags ncgImpl proc

        return  ( usAlloc
                , fileIds'
                , expanded
                , lastMinuteImports ++ imports
                , ppr_raStatsColor
                , ppr_raStatsLinear
                , unwinds )


x86fp_kludge :: NatCmmDecl (Alignment, CmmStatics) X86.Instr.Instr -> NatCmmDecl (Alignment, CmmStatics) X86.Instr.Instr
x86fp_kludge top@(CmmData _ _) = top
x86fp_kludge (CmmProc info lbl live (ListGraph code)) =
        CmmProc info lbl live (ListGraph $ X86.Instr.i386_insert_ffrees code)

-- | Compute unwinding tables for the blocks of a procedure
computeUnwinding :: Instruction instr
                 => DynFlags -> NcgImpl statics instr jumpDest
                 -> NatCmmDecl statics instr
                    -- ^ the native code generated for the procedure
                 -> LabelMap [UnwindPoint]
                    -- ^ unwinding tables for all points of all blocks of the
                    -- procedure
computeUnwinding dflags _ _
  | debugLevel dflags == 0         = mapEmpty
computeUnwinding _ _ (CmmData _ _) = mapEmpty
computeUnwinding _ ncgImpl (CmmProc _ _ _ (ListGraph blks)) =
    -- In general we would need to push unwinding information down the
    -- block-level call-graph to ensure that we fully account for all
    -- relevant register writes within a procedure.
    --
    -- However, the only unwinding information that we care about in GHC is for
    -- Sp. The fact that CmmLayoutStack already ensures that we have unwind
    -- information at the beginning of every block means that there is no need
    -- to perform this sort of push-down.
    mapFromList [ (blk_lbl, extractUnwindPoints ncgImpl instrs)
                | BasicBlock blk_lbl instrs <- blks ]

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
             else Outputable.empty)
            $$
                -- On recent GNU ELF systems one can mark an object file
                -- as not requiring an executable stack. If all objects
                -- linked into a program have this note then the program
                -- will not use an executable stack, which is good for
                -- security. GHC generated code does not need an executable
                -- stack so add the note in:
            (if platformHasGnuNonexecStack platform
             then text ".section .note.GNU-stack,\"\"," <> sectionType "progbits"
             else Outputable.empty)
            $$
                -- And just because every other compiler does, let's stick in
                -- an identifier directive: .ident "GHC x.y.z"
            (if platformHasIdentDirective platform
             then let compilerIdent = text "GHC" <+> text cProjectVersion
                   in text ".ident" <+> doubleQuotes compilerIdent
             else Outputable.empty)

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
                = Outputable.empty

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
        => LabelMap i
        -> [NatBasicBlock instr]
        -> [NatBasicBlock instr]

sequenceBlocks _ [] = []
sequenceBlocks infos (entry:blocks) =
  seqBlocks infos (mkNode entry : reverse (flattenSCCs (sccBlocks blocks)))
  -- the first block is the entry point ==> it must remain at the start.


sccBlocks
        :: Instruction instr
        => [NatBasicBlock instr]
        -> [SCC (Node BlockId (NatBasicBlock instr))]

sccBlocks blocks = stronglyConnCompFromEdgedVerticesUniqR (map mkNode blocks)

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
       -> Node BlockId (GenBasicBlock t)
mkNode block@(BasicBlock id instrs) = DigraphNode block id (getOutEdges instrs)

seqBlocks :: LabelMap i -> [Node BlockId (GenBasicBlock t1)]
                        -> [GenBasicBlock t1]
seqBlocks infos blocks = placeNext pullable0 todo0
  where
    -- pullable: Blocks that are not yet placed
    -- todo:     Original order of blocks, to be followed if we have no good
    --           reason not to;
    --           may include blocks that have already been placed, but then
    --           these are not in pullable
    pullable0 = listToUFM [ (i,(b,n)) | DigraphNode b i n <- blocks ]
    todo0     = map node_key blocks

    placeNext _ [] = []
    placeNext pullable (i:rest)
        | Just (block, pullable') <- lookupDeleteUFM pullable i
        = place pullable' rest block
        | otherwise
        -- We already placed this block, so ignore
        = placeNext pullable rest

    place pullable todo (block,[])
                          = block : placeNext pullable todo
    place pullable todo (block@(BasicBlock id instrs),[next])
        | mapMember next infos
        = block : placeNext pullable todo
        | Just (nextBlock, pullable') <- lookupDeleteUFM pullable next
        = BasicBlock id (init instrs) : place pullable' todo nextBlock
        | otherwise
        = block : placeNext pullable todo
    place _ _ (_,tooManyNextNodes)
        = pprPanic "seqBlocks" (ppr tooManyNextNodes)


lookupDeleteUFM :: Uniquable key => UniqFM elt -> key -> Maybe (elt, UniqFM elt)
lookupDeleteUFM m k = do -- Maybe monad
    v <- lookupUFM m k
    return (v, delFromUFM m k)

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
    mapping = plusUFMList mappings

build_mapping :: NcgImpl statics instr jumpDest
              -> GenCmmDecl d (LabelMap t) (ListGraph instr)
              -> (GenCmmDecl d (LabelMap t) (ListGraph instr), UniqFM jumpDest)
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
    (_, shortcut_blocks, others) =
        foldl split (setEmpty :: LabelSet, [], []) blocks
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
    mapping = foldl' add emptyUFM shortcut_blocks
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

-- Along with the stack pointer offset, we also carry along a LabelMap of
-- DebugBlocks, which we read to generate .location directives.
--
-- Switching between the two monads whilst carrying along the same
-- Unique supply breaks abstraction.  Is that bad?

genMachCode
        :: DynFlags
        -> Module -> ModLocation
        -> (RawCmmDecl -> NatM [NatCmmDecl statics instr])
        -> DwarfFiles
        -> LabelMap DebugBlock
        -> RawCmmDecl
        -> UniqSM
                ( [NatCmmDecl statics instr]
                , [CLabel]
                , DwarfFiles)

genMachCode dflags this_mod modLoc cmmTopCodeGen fileIds dbgMap cmm_top
  = do  { initial_us <- getUniqueSupplyM
        ; let initial_st           = mkNatM_State initial_us 0 dflags this_mod
                                                  modLoc fileIds dbgMap
              (new_tops, final_st) = initNat initial_st (cmmTopCodeGen cmm_top)
              final_delta          = natm_delta final_st
              final_imports        = natm_imports final_st
        ; if   final_delta == 0
          then return (new_tops, final_imports, natm_fileid final_st)
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

instance Functor CmmOptM where
    fmap = liftM

instance Applicative CmmOptM where
    pure x = CmmOptM $ \_ _ imports -> (# x, imports #)
    (<*>) = ap

instance Monad CmmOptM where
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

        CmmCondBranch test true false likely
           -> do test' <- cmmExprConFold DataReference test
                 return $ case test' of
                   CmmLit (CmmInt 0 _) -> CmmBranch false
                   CmmLit (CmmInt _ _) -> CmmBranch true
                   _other -> CmmCondBranch test' true false likely

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
          | arch == ArchPPC && not (positionIndependent dflags)
          -> cmmExprNative referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsUnitId (fsLit "__stg_EAGER_BLACKHOLE_info")))
        CmmReg (CmmGlobal GCEnter1)
          | arch == ArchPPC && not (positionIndependent dflags)
          -> cmmExprNative referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsUnitId (fsLit "__stg_gc_enter_1")))
        CmmReg (CmmGlobal GCFun)
          | arch == ArchPPC && not (positionIndependent dflags)
          -> cmmExprNative referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsUnitId (fsLit "__stg_gc_fun")))

        other
           -> return other
