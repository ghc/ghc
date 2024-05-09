module GHC.Driver.Config.CmmToAsm
  ( initNCGConfig
  )
where

import GHC.Prelude

import GHC.Driver.DynFlags

import GHC.Platform
import GHC.Unit.Types (Module)
import GHC.CmmToAsm.Config
import GHC.Utils.Outputable
import GHC.CmmToAsm.BlockLayout

-- | Initialize the native code generator configuration from the DynFlags
initNCGConfig :: DynFlags -> Module -> NCGConfig
initNCGConfig dflags this_mod = NCGConfig
   { ncgPlatform              = targetPlatform dflags
   , ncgThisModule            = this_mod
   , ncgAsmContext            = initSDocContext dflags PprCode
   , ncgProcAlignment         = cmmProcAlignment dflags
   , ncgExternalDynamicRefs   = gopt Opt_ExternalDynamicRefs dflags
   -- no PIC on wasm32 for now
   , ncgPIC                   = positionIndependent dflags && not (platformArch (targetPlatform dflags) == ArchWasm32)
   , ncgInlineThresholdMemcpy = fromIntegral $ maxInlineMemcpyInsns dflags
   , ncgInlineThresholdMemset = fromIntegral $ maxInlineMemsetInsns dflags
   , ncgSplitSections         = gopt Opt_SplitSections dflags
   , ncgRegsIterative         = gopt Opt_RegsIterative dflags
   , ncgRegsGraph             = gopt Opt_RegsGraph dflags
   , ncgAsmLinting            = gopt Opt_DoAsmLinting dflags
   , ncgCfgWeights            = cfgWeights dflags
   , ncgCfgBlockLayout        = gopt Opt_CfgBlocklayout dflags
   , ncgCfgWeightlessLayout   = gopt Opt_WeightlessBlocklayout dflags

     -- When constant-folding is enabled, the cmmSink pass does constant-folding, so
     -- we don't need to do it again in the native code generator.
   , ncgDoConstantFolding     = not (gopt Opt_CoreConstantFolding dflags || gopt Opt_CmmSink dflags)

   , ncgDumpRegAllocStages    = dopt Opt_D_dump_asm_regalloc_stages dflags
   , ncgDumpAsmStats          = dopt Opt_D_dump_asm_stats dflags
   , ncgDumpAsmConflicts      = dopt Opt_D_dump_asm_conflicts dflags
   , ncgBmiVersion            = case platformArch (targetPlatform dflags) of
                                 ArchX86_64 -> bmiVersion dflags
                                 ArchX86    -> bmiVersion dflags
                                 _          -> Nothing

     -- We assume  SSE1 and SSE2 operations are available on both
     -- x86 and x86_64. Historically we didn't default to SSE2 and
     -- SSE1 on x86, which results in defacto nondeterminism for how
     -- rounding behaves in the associated x87 floating point instructions
     -- because variations in the spill/fpu stack placement of arguments for
     -- operations would change the precision and final result of what
     -- would otherwise be the same expressions with respect to single or
     -- double precision IEEE floating point computations.
   , ncgSseVersion =
      let v | sseVersion dflags < Just SSE2 = Just SSE2
            | otherwise                     = sseVersion dflags
      in case platformArch (targetPlatform dflags) of
            ArchX86_64 -> v
            ArchX86    -> v
            _          -> Nothing
   , ncgAvxEnabled = isAvxEnabled dflags

   , ncgDwarfEnabled        = osElfTarget (platformOS (targetPlatform dflags)) && debugLevel dflags > 0 && platformArch (targetPlatform dflags) /= ArchAArch64
   , ncgDwarfUnwindings     = osElfTarget (platformOS (targetPlatform dflags)) && debugLevel dflags > 0
   , ncgDwarfStripBlockInfo = osElfTarget (platformOS (targetPlatform dflags)) && debugLevel dflags < 2 -- We strip out block information when running with -g0 or -g1.
   , ncgDwarfSourceNotes    = osElfTarget (platformOS (targetPlatform dflags)) && debugLevel dflags > 2 -- We produce GHC-specific source-note DIEs only with -g3
   , ncgExposeInternalSymbols = gopt Opt_ExposeInternalSymbols dflags
   , ncgCmmStaticPred       = gopt Opt_CmmStaticPred dflags
   , ncgEnableShortcutting  = gopt Opt_AsmShortcutting dflags
   , ncgComputeUnwinding    = debugLevel dflags > 0
   , ncgEnableDeadCodeElimination = not (gopt Opt_InfoTableMap dflags)
                                     -- Disable when -finfo-table-map is on (#20428)
                                     && backendMaintainsCfg (targetPlatform dflags)
                                     -- Enable if the platform maintains the CFG
   }
