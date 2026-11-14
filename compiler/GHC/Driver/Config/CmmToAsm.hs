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
   -- BUG: the value is not checked, so one that is not a power of 2 or is over
   -- 512 makes mkAlignment panic in X86's, RV64's and LA64's pprProcAlignment,
   -- on x86, x86-64, RISC-V and LoongArch, where the flag could be rejected;
   -- AArch64's and PowerPC's printers do not use the field, so there
   -- -fproc-alignment has no effect.
   , ncgProcAlignment         = cmmProcAlignment dflags
   , ncgExternalDynamicRefs   = gopt Opt_ExternalDynamicRefs dflags
   , ncgPIC                   = positionIndependent dflags
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
   , ncgCpuFeatures           = targetCpuFeatures dflags
   , ncgLa664Enabled          = isLa664Enabled dflags

   , ncgDwarfEnabled        = osElfTarget (platformOS (targetPlatform dflags)) && debugLevel dflags > 0 && platformArch (targetPlatform dflags) /= ArchAArch64
   , ncgDwarfUnwindings     = osElfTarget (platformOS (targetPlatform dflags)) && debugLevel dflags > 0
   , ncgDwarfStripBlockInfo = osElfTarget (platformOS (targetPlatform dflags)) && debugLevel dflags < 2 -- We strip out block information when running with -g0 or -g1.
   , ncgDwarfSourceNotes    = osElfTarget (platformOS (targetPlatform dflags)) && debugLevel dflags > 2 -- We produce GHC-specific source-note DIEs only with -g3
   , ncgExposeInternalSymbols = gopt Opt_ExposeInternalSymbols dflags
   , ncgCmmStaticPred       = gopt Opt_CmmStaticPred dflags
   , ncgEnableShortcutting  = gopt Opt_AsmShortcutting dflags
   , ncgEnableInterModuleFarJumps = gopt Opt_InterModuleFarJumps dflags
   , ncgComputeUnwinding    = debugLevel dflags > 0
   , ncgEnableDeadCodeElimination = not (gopt Opt_InfoTableMap dflags)
                                     -- TODO #28170: off under -finfo-table-map
                                     -- since 55a6377a5d, a workaround for
                                     -- #20428, and with it, from the liveness
                                     -- pass on, every use of the CFG that can
                                     -- change the code (see livenessCfg in
                                     -- GHC.CmmToAsm)
                                     && backendMaintainsCfg (targetPlatform dflags)
                                     -- Enable if the platform maintains the CFG
   }
