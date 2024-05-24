-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1993-2004
--
--
-- -----------------------------------------------------------------------------

-- | Note [Native code generator]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The native-code generator has machine-independent and
-- machine-dependent modules.
--
-- This module ("GHC.CmmToAsm") is the top-level machine-independent
-- module.  Before entering machine-dependent land, we do some
-- machine-independent optimisations (defined below) on the
-- 'CmmStmts's. (Which ideally would be folded into CmmOpt ...)
--
-- We convert to the machine-specific 'Instr' datatype with
-- 'cmmCodeGen', assuming an infinite supply of registers.  We then use
-- a (mostly) machine-independent register allocator to rejoin
-- reality.  Obviously, 'regAlloc' has machine-specific helper
-- functions (see the used register allocator for details).
--
-- Finally, we order the basic blocks of the function so as to minimise
-- the number of jumps between blocks, by utilising fallthrough wherever
-- possible.
--
-- The machine-dependent bits are generally contained under
--  GHC/CmmToAsm/<Arch>/* and generally breaks down as follows:
--
--   * "Regs": Everything about the target platform's machine
--     registers (and immediate operands, and addresses, which tend to
--     intermingle/interact with registers).
--
--   * "Instr":  Includes the 'Instr' datatype plus a miscellany of other things
--     (e.g., 'targetDoubleSize', 'smStablePtrTable', ...)
--
--   * "CodeGen":  is where 'Cmm' stuff turns into
--     machine instructions.
--
--   * "Ppr": 'pprInstr' turns an 'Instr' into text (well, really
--     a 'SDoc').
--
-- The register allocators lives under GHC.CmmToAsm.Reg.*, there is both a Linear and a Graph
-- based register allocator. Both of which have their own notes describing them. They
-- are mostly platform independent but there are some platform specific files
-- encoding architecture details under Reg/<Allocator>/<Arch.hs>
--
-- -}
--
module GHC.CmmToAsm
   ( nativeCodeGen

   -- * Test-only exports: see #12744
   -- used by testGraphNoSpills, which needs to access
   -- the register allocator intermediate data structures
   -- cmmNativeGen emits
   , cmmNativeGen
   , NcgImpl(..)
   )
where

import GHC.Prelude hiding (head)

import qualified GHC.CmmToAsm.X86   as X86
import qualified GHC.CmmToAsm.PPC   as PPC
import qualified GHC.CmmToAsm.AArch64 as AArch64
import qualified GHC.CmmToAsm.Wasm as Wasm32

import GHC.CmmToAsm.Reg.Liveness
import qualified GHC.CmmToAsm.Reg.Linear                as Linear

import qualified GHC.Data.Graph.Color                   as Color
import qualified GHC.CmmToAsm.Reg.Graph                 as Color
import qualified GHC.CmmToAsm.Reg.Graph.Stats           as Color
import qualified GHC.CmmToAsm.Reg.Graph.TrivColorable   as Color

import GHC.Utils.Asm
import GHC.CmmToAsm.Reg.Target
import GHC.Platform
import GHC.CmmToAsm.BlockLayout as BlockLayout
import GHC.Settings.Config
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.PIC
import GHC.Platform.Reg
import GHC.Platform.Reg.Class (RegClass)
import GHC.CmmToAsm.Monad
import GHC.CmmToAsm.CFG
import GHC.CmmToAsm.Dwarf
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types
import GHC.Cmm.DebugBlock

import GHC.Cmm.BlockId
import GHC.StgToCmm.CgUtils ( fixStgRegisters )
import GHC.Cmm
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.GenericOpt
import GHC.Cmm.CLabel

import GHC.Types.Unique.FM
import GHC.Types.Unique.Supply
import GHC.Driver.DynFlags
import GHC.Driver.Ppr
import GHC.Utils.Misc
import GHC.Utils.Logger

import GHC.Utils.BufHandle
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Utils.Error
import GHC.Utils.Exception (evaluate)
import GHC.Utils.Constants (debugIsOn)

import GHC.Data.FastString
import GHC.Types.Unique.Set
import GHC.Unit
import GHC.Data.Stream (Stream)
import qualified GHC.Data.Stream as Stream
import GHC.Settings

import Data.List (sortBy)
import Data.List.NonEmpty (groupAllWith, head)
import Data.Maybe
import Data.Ord         ( comparing )
import Control.Monad
import System.IO
import System.Directory ( getCurrentDirectory )

--------------------
nativeCodeGen :: forall a . Logger -> ToolSettings -> NCGConfig -> ModLocation -> Handle -> UniqSupply
              -> Stream IO RawCmmGroup a
              -> IO a
nativeCodeGen logger ts config modLoc h us cmms
 = let platform = ncgPlatform config
       nCG' :: ( OutputableP Platform statics, Outputable jumpDest, Instruction instr)
            => NcgImpl statics instr jumpDest -> IO a
       nCG' ncgImpl = nativeCodeGen' logger config modLoc ncgImpl h us cmms
   in case platformArch platform of
      ArchX86       -> nCG' (X86.ncgX86     config)
      ArchX86_64    -> nCG' (X86.ncgX86_64  config)
      ArchPPC       -> nCG' (PPC.ncgPPC     config)
      ArchPPC_64 _  -> nCG' (PPC.ncgPPC     config)
      ArchS390X     -> panic "nativeCodeGen: No NCG for S390X"
      ArchARM {}    -> panic "nativeCodeGen: No NCG for ARM"
      ArchAArch64   -> nCG' (AArch64.ncgAArch64 config)
      ArchAlpha     -> panic "nativeCodeGen: No NCG for Alpha"
      ArchMipseb    -> panic "nativeCodeGen: No NCG for mipseb"
      ArchMipsel    -> panic "nativeCodeGen: No NCG for mipsel"
      ArchRISCV64   -> panic "nativeCodeGen: No NCG for RISCV64"
      ArchLoongArch64->panic "nativeCodeGen: No NCG for LoongArch64"
      ArchUnknown   -> panic "nativeCodeGen: No NCG for unknown arch"
      ArchJavaScript-> panic "nativeCodeGen: No NCG for JavaScript"
      ArchWasm32    -> Wasm32.ncgWasm config logger platform ts us modLoc h cmms

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
             -- and Note [What is this unwinding business?] in "GHC.Cmm.DebugBlock".
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

See also Note [What is this unwinding business?] in "GHC.Cmm.DebugBlock".
-}

nativeCodeGen' :: (OutputableP Platform statics, Outputable jumpDest, Instruction instr)
               => Logger
               -> NCGConfig
               -> ModLocation
               -> NcgImpl statics instr jumpDest
               -> Handle
               -> UniqSupply
               -> Stream IO RawCmmGroup a
               -> IO a
nativeCodeGen' logger config modLoc ncgImpl h us cmms
 = do
        -- BufHandle is a performance hack.  We could hide it inside
        -- Pretty if it weren't for the fact that we do lots of little
        -- printDocs here (in order to do codegen in constant space).
        bufh <- newBufHandle h
        let ngs0 = NGS [] [] [] [] [] [] emptyUFM mapEmpty
        (ngs, us', a) <- cmmNativeGenStream logger config modLoc ncgImpl bufh us
                                         cmms ngs0
        _ <- finishNativeGen logger config modLoc bufh us' ngs
        return a

finishNativeGen :: Instruction instr
                => Logger
                -> NCGConfig
                -> ModLocation
                -> BufHandle
                -> UniqSupply
                -> NativeGenAcc statics instr
                -> IO UniqSupply
finishNativeGen logger config modLoc bufh us ngs
 = withTimingSilent logger (text "NCG") (`seq` ()) $ do
        -- Write debug data and finish
        us' <- if not (ncgDwarfEnabled config)
                  then return us
                  else do
                     compPath <- getCurrentDirectory
                     let (dwarf_h, us') = dwarfGen compPath config modLoc us (ngs_debug ngs)
                         (dwarf_s, _)   = dwarfGen compPath config modLoc us (ngs_debug ngs)
                     emitNativeCode logger config bufh dwarf_h dwarf_s
                     return us'

        -- dump global NCG stats for graph coloring allocator
        let stats = concat (ngs_colorStats ngs)
        unless (null stats) $ do

          -- build the global register conflict graph
          let graphGlobal
                  = foldl' Color.union Color.initGraph
                  $ [ Color.raGraph stat
                          | stat@Color.RegAllocStatsStart{} <- stats]

          dump_stats (Color.pprStats stats graphGlobal)

          let platform = ncgPlatform config
          putDumpFileMaybe logger
                  Opt_D_dump_asm_conflicts "Register conflict graph"
                  FormatText
                  $ Color.dotGraph
                          (targetRegDotColor platform)
                          (Color.trivColorable platform
                                  (targetVirtualRegSqueeze platform)
                                  (targetRealRegSqueeze platform))
                  $ graphGlobal


        -- dump global NCG stats for linear allocator
        let linearStats = concat (ngs_linearStats ngs)
        unless (null linearStats) $
          dump_stats (Linear.pprStats (concat (ngs_natives ngs)) linearStats)

        -- write out the imports
        let ctx = ncgAsmContext config
        bPutHDoc bufh ctx $ makeImportsDoc config (concat (ngs_imports ngs))
        bFlush bufh

        return us'
  where
    dump_stats = logDumpFile logger (mkDumpStyle alwaysQualify)
                   Opt_D_dump_asm_stats "NCG stats"
                   FormatText

cmmNativeGenStream :: forall statics jumpDest instr a . (OutputableP Platform statics, Outputable jumpDest, Instruction instr)
              => Logger
              -> NCGConfig
              -> ModLocation
              -> NcgImpl statics instr jumpDest
              -> BufHandle
              -> UniqSupply
              -> Stream.Stream IO RawCmmGroup a
              -> NativeGenAcc statics instr
              -> IO (NativeGenAcc statics instr, UniqSupply, a)

cmmNativeGenStream logger config modLoc ncgImpl h us cmm_stream ngs
 = loop us (Stream.runStream cmm_stream) ngs
  where
    ncglabel = text "NCG"
    loop :: UniqSupply
              -> Stream.StreamS IO RawCmmGroup a
              -> NativeGenAcc statics instr
              -> IO (NativeGenAcc statics instr, UniqSupply, a)
    loop us s ngs =
      case s of
        Stream.Done a ->
          return (ngs { ngs_imports = reverse $ ngs_imports ngs
                      , ngs_natives = reverse $ ngs_natives ngs
                      , ngs_colorStats = reverse $ ngs_colorStats ngs
                      , ngs_linearStats = reverse $ ngs_linearStats ngs
                      },
                  us,
                  a)
        Stream.Effect m -> m >>= \cmm_stream' -> loop us cmm_stream' ngs
        Stream.Yield cmms cmm_stream' -> do
          (us', ngs'') <-
            withTimingSilent logger
                ncglabel (\(a, b) -> a `seq` b `seq` ()) $ do
              -- Generate debug information
              let !ndbgs | ncgDwarfEnabled config = cmmDebugGen modLoc cmms
                         | otherwise              = []
                  dbgMap = debugToMap ndbgs

              -- Generate native code
              (ngs',us') <- cmmNativeGens logger config ncgImpl h
                                          dbgMap us cmms ngs 0

              -- Link native code information into debug blocks
              -- See Note [What is this unwinding business?] in "GHC.Cmm.DebugBlock".
              let !ldbgs = cmmDebugLink (ngs_labels ngs') (ngs_unwinds ngs') ndbgs
                  platform = ncgPlatform config
              unless (null ldbgs) $
                putDumpFileMaybe logger Opt_D_dump_debug "Debug Infos" FormatText
                  (vcat $ map (pdoc platform) ldbgs)

              -- Accumulate debug information for emission in finishNativeGen.
              let ngs'' = ngs' { ngs_debug = ngs_debug ngs' ++ ldbgs, ngs_labels = [] }
              return (us', ngs'')

          loop us' cmm_stream' ngs''


-- | Do native code generation on all these cmms.
--
cmmNativeGens :: forall statics instr jumpDest.
                 (OutputableP Platform statics, Outputable jumpDest, Instruction instr)
              => Logger
              -> NCGConfig
              -> NcgImpl statics instr jumpDest
              -> BufHandle
              -> LabelMap DebugBlock
              -> UniqSupply
              -> [RawCmmDecl]
              -> NativeGenAcc statics instr
              -> Int
              -> IO (NativeGenAcc statics instr, UniqSupply)

cmmNativeGens logger config ncgImpl h dbgMap = go
  where
    go :: UniqSupply -> [RawCmmDecl]
       -> NativeGenAcc statics instr -> Int
       -> IO (NativeGenAcc statics instr, UniqSupply)

    go us [] ngs !_ =
        return (ngs, us)

    go us (cmm : cmms) ngs count = do
        let fileIds = ngs_dwarfFiles ngs
        (us', fileIds', native, imports, colorStats, linearStats, unwinds, mcfg)
          <- {-# SCC "cmmNativeGen" #-}
             cmmNativeGen logger ncgImpl us fileIds dbgMap
                          cmm count

        -- Generate .file directives for every new file that has been
        -- used. Note that it is important that we generate these in
        -- ascending order, as Clang's 3.6 assembler complains.
        let newFileIds = sortBy (comparing snd) $
                         nonDetEltsUFM $ fileIds' `minusUFM` fileIds
            -- See Note [Unique Determinism and code generation]
            pprDecl (f,n) = line $ text "\t.file " <> int n <+>
                                   pprFilePathString (unpackFS f)

        -- see Note [pprNatCmmDeclS and pprNatCmmDeclH] in GHC.CmmToAsm.Monad
        emitNativeCode logger config h
          (vcat $
           map pprDecl newFileIds ++
           map (pprNatCmmDeclH ncgImpl) native)
          (vcat $
           map pprDecl newFileIds ++
           map (pprNatCmmDeclS ncgImpl) native)

        -- force evaluation all this stuff to avoid space leaks
        let platform = ncgPlatform config
        {-# SCC "seqString" #-} evaluate $ seqList (showSDocUnsafe $ vcat $ map (pprAsmLabel platform) imports) ()

        let !labels' = if ncgDwarfEnabled config
                       then cmmDebugLabels is_valid_label isMetaInstr native else []
            is_valid_label
              -- filter dead labels: asm-shortcutting may remove some blocks
              -- (#22792)
              | Just cfg <- mcfg = hasNode cfg
              | otherwise        = const True

            !natives' = if logHasDumpFlag logger Opt_D_dump_asm_stats
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


-- see Note [pprNatCmmDeclS and pprNatCmmDeclH] in GHC.CmmToAsm.Monad
emitNativeCode :: Logger -> NCGConfig -> BufHandle -> HDoc -> SDoc -> IO ()
emitNativeCode logger config h hdoc sdoc = do
        let ctx = ncgAsmContext config
        {-# SCC "pprNativeCode" #-} bPutHDoc h ctx hdoc

        -- dump native code
        putDumpFileMaybe logger
                Opt_D_dump_asm "Asm code" FormatASM
                sdoc

-- | Complete native code generation phase for a single top-level chunk of Cmm.
--      Dumping the output of each stage along the way.
--      Global conflict graph and NGC stats
cmmNativeGen
    :: forall statics instr jumpDest. (Instruction instr, OutputableP Platform statics, Outputable jumpDest)
    => Logger
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
                , Maybe CFG                                 -- final CFG
                )

cmmNativeGen logger ncgImpl us fileIds dbgMap cmm count
 = do
        let config   = ncgConfig ncgImpl
        let platform = ncgPlatform config
        let weights  = ncgCfgWeights config

        let proc_name = case cmm of
                (CmmProc _ entry_label _ _) -> pprAsmLabel platform entry_label
                _                           -> text "DataChunk"

        -- rewrite assignments to global regs
        let fixed_cmm =
                {-# SCC "fixStgRegisters" #-}
                fixStgRegisters platform cmm

        -- cmm to cmm optimisations
        let (opt_cmm, imports) =
                {-# SCC "cmmToCmm" #-}
                cmmToCmm config fixed_cmm

        putDumpFileMaybe logger
                Opt_D_dump_opt_cmm "Optimised Cmm" FormatCMM
                (pprCmmGroup platform [opt_cmm])

        let cmmCfg = {-# SCC "getCFG" #-}
                     getCfgProc platform weights opt_cmm

        -- generate native code from cmm
        let ((native, lastMinuteImports, fileIds', nativeCfgWeights), usGen) =
                {-# SCC "genMachCode" #-}
                initUs us $ genMachCode config
                                        (cmmTopCodeGen ncgImpl)
                                        fileIds dbgMap opt_cmm cmmCfg

        putDumpFileMaybe logger
                Opt_D_dump_asm_native "Native code" FormatASM
                (vcat $ map (pprNatCmmDeclS ncgImpl) native)

        maybeDumpCfg logger (Just nativeCfgWeights) "CFG Weights - Native" proc_name

        -- tag instructions with register liveness information
        -- also drops dead code. We don't keep the cfg in sync on
        -- some backends, so don't use it there.
        let livenessCfg = if ncgEnableDeadCodeElimination config
                                then Just nativeCfgWeights
                                else Nothing
        let (withLiveness, usLive) =
                {-# SCC "regLiveness" #-}
                initUs usGen
                        $ mapM (cmmTopLiveness livenessCfg platform) native

        putDumpFileMaybe logger
                Opt_D_dump_asm_liveness "Liveness annotations added"
                FormatCMM
                (vcat $ map (pprLiveCmmDecl platform) withLiveness)

        -- ROMES:TODO: RENAME VIRTUAL REGISTERS DETERMINISTICALLY

        -- allocate registers
        (alloced, usAlloc, ppr_raStatsColor, ppr_raStatsLinear, raStats, stack_updt_blks) <-
         if ( ncgRegsGraph config || ncgRegsIterative config )
          then do
                -- the regs usable for allocation
                let (alloc_regs :: UniqFM RegClass (UniqSet RealReg))
                        = foldr (\r -> plusUFM_C unionUniqSets
                                        $ unitUFM (targetClassOfRealReg platform r) (unitUniqSet r))
                                emptyUFM
                        $ allocatableRegs ncgImpl

                -- do the graph coloring register allocation
                let ((alloced, maybe_more_stack, regAllocStats), usAlloc)
                        = {-# SCC "RegAlloc-color" #-}
                          initUs usLive
                          $ Color.regAlloc
                                config
                                alloc_regs
                                (mkUniqSet [0 .. maxSpillSlots ncgImpl])
                                (maxSpillSlots ncgImpl)
                                withLiveness
                                livenessCfg

                let ((alloced', stack_updt_blks), usAlloc')
                        = initUs usAlloc $
                                case maybe_more_stack of
                                Nothing     -> return (alloced, [])
                                Just amount -> do
                                    (alloced',stack_updt_blks) <- unzip <$>
                                                (mapM ((ncgAllocMoreStack ncgImpl) amount) alloced)
                                    return (alloced', concat stack_updt_blks )


                -- dump out what happened during register allocation
                putDumpFileMaybe logger
                        Opt_D_dump_asm_regalloc "Registers allocated"
                        FormatCMM
                        (vcat $ map (pprNatCmmDeclS ncgImpl) alloced)

                putDumpFileMaybe logger
                        Opt_D_dump_asm_regalloc_stages "Build/spill stages"
                        FormatText
                        (vcat   $ map (\(stage, stats)
                                        -> text "# --------------------------"
                                        $$ text "#  cmm " <> int count <> text " Stage " <> int stage
                                        $$ ppr (fmap (pprInstr platform) stats))
                                $ zip [0..] regAllocStats)

                let mPprStats =
                        if logHasDumpFlag logger Opt_D_dump_asm_stats
                         then Just regAllocStats else Nothing

                -- force evaluation of the Maybe to avoid space leak
                mPprStats `seq` return ()

                return  ( alloced', usAlloc'
                        , mPprStats
                        , Nothing
                        , [], stack_updt_blks)

          else do

                -- do linear register allocation
                let reg_alloc proc = do
                       (alloced, maybe_more_stack, ra_stats) <-
                               Linear.regAlloc config proc
                       case maybe_more_stack of
                         Nothing -> return ( alloced, ra_stats, [] )
                         Just amount -> do
                           (alloced',stack_updt_blks) <-
                               ncgAllocMoreStack ncgImpl amount alloced
                           return (alloced', ra_stats, stack_updt_blks )

                let ((alloced, regAllocStats, stack_updt_blks), usAlloc)
                        = {-# SCC "RegAlloc-linear" #-}
                          initUs usLive
                          $ liftM unzip3
                          $ mapM reg_alloc withLiveness

                putDumpFileMaybe logger
                        Opt_D_dump_asm_regalloc "Registers allocated"
                        FormatCMM
                        (vcat $ map (pprNatCmmDeclS ncgImpl) alloced)

                let mPprStats =
                        if logHasDumpFlag logger Opt_D_dump_asm_stats
                         then Just (catMaybes regAllocStats) else Nothing

                -- force evaluation of the Maybe to avoid space leak
                mPprStats `seq` return ()

                return  ( alloced, usAlloc
                        , Nothing
                        , mPprStats, (catMaybes regAllocStats)
                        , concat stack_updt_blks )

        -- Fixupblocks the register allocator inserted (from, regMoves, to)
        let cfgRegAllocUpdates :: [(BlockId,BlockId,BlockId)]
            cfgRegAllocUpdates = (concatMap Linear.ra_fixupList raStats)

        let cfgWithFixupBlks =
                (\cfg -> addNodesBetween weights cfg cfgRegAllocUpdates) <$> livenessCfg

        -- Insert stack update blocks
        let postRegCFG =
                pure (foldl' (\m (from,to) -> addImmediateSuccessor weights from to m ))
                     <*> cfgWithFixupBlks
                     <*> pure stack_updt_blks

        ---- generate jump tables
        let tabled      =
                {-# SCC "generateJumpTables" #-}
                generateJumpTables ncgImpl alloced

        when (not $ null nativeCfgWeights) $ putDumpFileMaybe logger
                Opt_D_dump_cfg_weights "CFG Update information"
                FormatText
                ( text "stack:" <+> ppr stack_updt_blks $$
                  text "linearAlloc:" <+> ppr cfgRegAllocUpdates )

        ---- shortcut branches
        let (shorted, postShortCFG)     =
                {-# SCC "shortcutBranches" #-}
                shortcutBranches config ncgImpl tabled postRegCFG

        let optimizedCFG :: Maybe CFG
            optimizedCFG =
                optimizeCFG (ncgCmmStaticPred config) weights cmm <$!> postShortCFG

        maybeDumpCfg logger optimizedCFG "CFG Weights - Final" proc_name

        --TODO: Partially check validity of the cfg.
        let getBlks (CmmProc _info _lbl _live (ListGraph blocks)) = blocks
            getBlks _ = []

        when ( ncgEnableDeadCodeElimination config &&
                (ncgAsmLinting config || debugIsOn )) $ do
                let blocks = concatMap getBlks shorted
                let labels = setFromList $ fmap blockId blocks :: LabelSet
                let cfg = fromJust optimizedCFG
                return $! seq (sanityCheckCfg cfg labels $
                                text "cfg not in lockstep") ()

        ---- sequence blocks
        -- sequenced :: [NatCmmDecl statics instr]
        let (sequenced, us_seq) =
                        {-# SCC "sequenceBlocks" #-}
                        initUs usAlloc $ mapM (BlockLayout.sequenceTop
                                ncgImpl optimizedCFG)
                            shorted

        massert (checkLayout shorted sequenced)

        let branchOpt :: [NatCmmDecl statics instr]
            branchOpt =
                {-# SCC "invertCondBranches" #-}
                map invert sequenced
              where
                invertConds :: LabelMap RawCmmStatics -> [NatBasicBlock instr]
                            -> [NatBasicBlock instr]
                invertConds = invertCondBranches ncgImpl optimizedCFG
                invert top@CmmData {} = top
                invert (CmmProc info lbl live (ListGraph blocks)) =
                    CmmProc info lbl live (ListGraph $ invertConds info blocks)

        -- generate unwinding information from cmm
        let unwinds :: BlockMap [UnwindPoint]
            unwinds =
                {-# SCC "unwindingInfo" #-}
                foldl' addUnwind mapEmpty branchOpt
              where
                addUnwind acc proc =
                    acc `mapUnion` computeUnwinding config ncgImpl proc

        return  ( us_seq
                , fileIds'
                , branchOpt
                , lastMinuteImports ++ imports
                , ppr_raStatsColor
                , ppr_raStatsLinear
                , unwinds
                , optimizedCFG
                )

maybeDumpCfg :: Logger -> Maybe CFG -> String -> SDoc -> IO ()
maybeDumpCfg _logger Nothing _ _ = return ()
maybeDumpCfg logger (Just cfg) msg proc_name
        | null cfg = return ()
        | otherwise
        = putDumpFileMaybe logger
                Opt_D_dump_cfg_weights msg
                FormatText
                (proc_name <> char ':' $$ pprEdgeWeights cfg)

-- | Make sure all blocks we want the layout algorithm to place have been placed.
checkLayout :: [NatCmmDecl statics instr] -> [NatCmmDecl statics instr]
            -> Bool
checkLayout procsUnsequenced procsSequenced =
        assertPpr (setNull diff) (text "Block sequencing dropped blocks:" <> ppr diff)
        True
  where
        blocks1 = foldl' (setUnion) setEmpty $
                        map getBlockIds procsUnsequenced :: LabelSet
        blocks2 = foldl' (setUnion) setEmpty $
                        map getBlockIds procsSequenced
        diff = setDifference blocks1 blocks2

        getBlockIds (CmmData _ _) = setEmpty
        getBlockIds (CmmProc _ _ _ (ListGraph blocks)) =
                setFromList $ map blockId blocks

-- | Compute unwinding tables for the blocks of a procedure
computeUnwinding :: Instruction instr
                 => NCGConfig
                 -> NcgImpl statics instr jumpDest
                 -> NatCmmDecl statics instr
                    -- ^ the native code generated for the procedure
                 -> LabelMap [UnwindPoint]
                    -- ^ unwinding tables for all points of all blocks of the
                    -- procedure
computeUnwinding config _ _
  | not (ncgComputeUnwinding config) = mapEmpty
computeUnwinding _ _ (CmmData _ _)   = mapEmpty
computeUnwinding _ ncgImpl (CmmProc _ _ _ (ListGraph blks)) =
    -- In general we would need to push unwinding information down the
    -- block-level call-graph to ensure that we fully account for all
    -- relevant register writes within a procedure.
    --
    -- However, the only unwinding information that we care about in GHC is for
    -- Sp. The fact that GHC.Cmm.LayoutStack already ensures that we have unwind
    -- information at the beginning of every block means that there is no need
    -- to perform this sort of push-down.
    mapFromList [ (blk_lbl, extractUnwindPoints ncgImpl instrs)
                | BasicBlock blk_lbl instrs <- blks ]

-- | Build a doc for all the imports.
--
makeImportsDoc :: NCGConfig -> [CLabel] -> HDoc
makeImportsDoc config imports
 = dyld_stubs imports
            $$
            -- On recent versions of Darwin, the linker supports
            -- dead-stripping of code and data on a per-symbol basis.
            -- There's a hack to make this work in PprMach.pprNatCmmDecl.
            (if platformHasSubsectionsViaSymbols platform
             then line $ text ".subsections_via_symbols"
             else Outputable.empty)
            $$
                -- On recent GNU ELF systems one can mark an object file
                -- as not requiring an executable stack. If all objects
                -- linked into a program have this note then the program
                -- will not use an executable stack, which is good for
                -- security. GHC generated code does not need an executable
                -- stack so add the note in:
            (if platformHasGnuNonexecStack platform
             then line $ text ".section .note.GNU-stack,\"\"," <> sectionType platform "progbits"
             else Outputable.empty)
            $$
                -- And just because every other compiler does, let's stick in
                -- an identifier directive: .ident "GHC x.y.z"
            (if platformHasIdentDirective platform
             then let compilerIdent = text "GHC" <+> text cProjectVersion
                   in line $ text ".ident" <+> doubleQuotes compilerIdent
             else Outputable.empty)

 where
        platform = ncgPlatform config

        -- Generate "symbol stubs" for all external symbols that might
        -- come from a dynamic library.
        dyld_stubs :: [CLabel] -> HDoc
        -- (Hack) sometimes two Labels pretty-print the same, but have
        -- different uniques; so we compare their text versions...
        dyld_stubs imps
                | needImportedSymbols config
                = vcat $
                        (pprGotDeclaration config :) $
                        fmap (pprImportedSymbol config . fst . head) $
                        groupAllWith snd $
                        map doPpr $
                        imps
                | otherwise
                = Outputable.empty

        doPpr lbl = (lbl, showSDocOneLine
                              (ncgAsmContext config)
                              (pprAsmLabel platform lbl))

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
          g (BasicBlock _ xs) = mapMaybe (generateJumpTableForInstr ncgImpl) xs

-- -----------------------------------------------------------------------------
-- Shortcut branches

-- Note [No asm-shortcutting on Darwin]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Asm-shortcutting may produce relative references to symbols defined in
-- other compilation units. This is not something that MachO relocations
-- support (see #21972). For this reason we disable the optimisation on Darwin.
-- We do so in the backend without a warning since this flag is enabled by
-- `-O2`.
--
-- Another way to address this issue would be to rather implement a
-- PLT-relocatable jump-table strategy. However, this would only benefit Darwin
-- and does not seem worth the effort as this optimisation generally doesn't
-- offer terribly great benefits.

shortcutBranches
        :: forall statics instr jumpDest. (Outputable jumpDest)
        => NCGConfig
        -> NcgImpl statics instr jumpDest
        -> [NatCmmDecl statics instr]
        -> Maybe CFG
        -> ([NatCmmDecl statics instr],Maybe CFG)

shortcutBranches config ncgImpl tops weights
  | ncgEnableShortcutting config
    -- See Note [No asm-shortcutting on Darwin]
  , not $ osMachOTarget $ platformOS $ ncgPlatform config
  = ( map (apply_mapping ncgImpl mapping) tops'
    , shortcutWeightMap mappingBid <$!> weights )
  | otherwise
  = (tops, weights)
  where
    (tops', mappings) = mapAndUnzip (build_mapping ncgImpl) tops
    mapping = mapUnions mappings :: LabelMap jumpDest
    mappingBid = fmap (getJumpDestBlockId ncgImpl) mapping

build_mapping :: forall instr t d statics jumpDest.
                 NcgImpl statics instr jumpDest
              -> GenCmmDecl d (LabelMap t) (ListGraph instr)
              -> (GenCmmDecl d (LabelMap t) (ListGraph instr)
                 ,LabelMap jumpDest)
build_mapping _ top@(CmmData _ _) = (top, mapEmpty)
build_mapping _ (CmmProc info lbl live (ListGraph []))
  = (CmmProc info lbl live (ListGraph []), mapEmpty)
build_mapping ncgImpl (CmmProc info lbl live (ListGraph (head:blocks)))
  = (CmmProc info lbl live (ListGraph (head:others)), mapping)
        -- drop the shorted blocks, but don't ever drop the first one,
        -- because it is pointed to by a global label.
  where
    -- find all the blocks that just consist of a jump that can be
    -- shorted.
    -- Don't completely eliminate loops here -- that can leave a dangling jump!
    shortcut_blocks :: [(BlockId, jumpDest)]
    (_, shortcut_blocks, others) =
        foldl' split (setEmpty :: LabelSet, [], []) blocks
    split (s, shortcut_blocks, others) b@(BasicBlock id [insn])
        | Just jd <- canShortcut ncgImpl insn
        , Just dest <- getJumpDestBlockId ncgImpl jd
        , not (has_info id)
        , (setMember dest s) || dest == id -- loop checks
        = (s, shortcut_blocks, b : others)
    split (s, shortcut_blocks, others) (BasicBlock id [insn])
        | Just dest <- canShortcut ncgImpl insn
        , not (has_info id)
        = (setInsert id s, (id,dest) : shortcut_blocks, others)
    split (s, shortcut_blocks, others) other = (s, shortcut_blocks, other : others)

    -- do not eliminate blocks that have an info table
    has_info l = mapMember l info

    -- build a mapping from BlockId to JumpDest for shorting branches
    mapping = mapFromList shortcut_blocks

apply_mapping :: NcgImpl statics instr jumpDest
              -> LabelMap jumpDest
              -> GenCmmDecl statics h (ListGraph instr)
              -> GenCmmDecl statics h (ListGraph instr)
apply_mapping ncgImpl ufm (CmmData sec statics)
  = CmmData sec (shortcutStatics ncgImpl (\bid -> mapLookup bid ufm) statics)
apply_mapping ncgImpl ufm (CmmProc info lbl live (ListGraph blocks))
  = CmmProc info lbl live (ListGraph $ map short_bb blocks)
  where
    short_bb (BasicBlock id insns) = BasicBlock id $! map short_insn insns
    short_insn i = shortcutJump ncgImpl (\bid -> mapLookup bid ufm) i
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
        :: NCGConfig
        -> (RawCmmDecl -> NatM [NatCmmDecl statics instr])
        -> DwarfFiles
        -> LabelMap DebugBlock
        -> RawCmmDecl
        -> CFG
        -> UniqSM
                ( [NatCmmDecl statics instr]
                , [CLabel]
                , DwarfFiles
                , CFG
                )

genMachCode config cmmTopCodeGen fileIds dbgMap cmm_top cmm_cfg
  = do  { initial_us <- getUniqueSupplyM
        ; let initial_st           = mkNatM_State initial_us 0 config
                                                  fileIds dbgMap cmm_cfg
              (new_tops, final_st) = initNat initial_st (cmmTopCodeGen cmm_top)
              final_delta          = natm_delta final_st
              final_imports        = natm_imports final_st
              final_cfg            = natm_cfg final_st
        ; if   final_delta == 0
          then return (new_tops, final_imports
                      , natm_fileid final_st, final_cfg)
          else pprPanic "genMachCode: nonzero final delta" (int final_delta)
    }
