module GHC.Driver.Config.Cmm.Builder
  ( initCmmBuilderConfig
  ) where

import GHC.Cmm.Builder.Config

import GHC.Driver.Config.StgToCmm.Ticky
--import GHC.Driver.Backend
import GHC.Driver.Session
--import GHC.Platform
--import GHC.Platform.Profile
import GHC.Utils.Outputable

--import Data.Maybe
import Prelude

initCmmBuilderConfig :: DynFlags -> CmmBuilderConfig
initCmmBuilderConfig dflags = CmmBuilderConfig
  -- settings
  { cmmBuilderProfile       = profile
  -- , cmmBuilderThisModule    = mod
  -- , cmmBuilderTmpDir        = tmpDir          dflags
  , cmmBuilderContext       = initSDocContext dflags defaultDumpStyle
  , cmmBuilderEmitDebugInfo = debugLevel      dflags > 0
  -- , cmmBuilderBinBlobThresh = b_blob
  -- , cmmBuilderMaxInlAllocSize = maxInlineAllocSize           dflags
  -- -- flags
  , cmmBuilderTickyCfg      = initCmmTickyConfig             dflags
  -- , cmmBuilderLoopification = gopt Opt_Loopification         dflags
  -- , cmmBuilderAlignCheck    = gopt Opt_AlignmentSanitisation dflags
  -- , cmmBuilderOptHpc        = gopt Opt_Hpc                   dflags
  -- , cmmBuilderFastPAPCalls  = gopt Opt_FastPAPCalls          dflags
  , cmmBuilderSCCProfiling  = sccProfilingEnabled            dflags
  , cmmBuilderEagerBlackHole = gopt Opt_EagerBlackHoling     dflags
  , cmmBuilderInfoTableMap  = gopt Opt_InfoTableMap          dflags
  , cmmBuilderOmitYields    = gopt Opt_OmitYields            dflags
  -- , cmmBuilderOmitIfPragmas = gopt Opt_OmitInterfacePragmas  dflags
  -- , cmmBuilderPIC           = gopt Opt_PIC                   dflags
  -- , cmmBuilderPIE           = gopt Opt_PIE                   dflags
  -- , cmmBuilderExtDynRefs    = gopt Opt_ExternalDynamicRefs   dflags
  -- , cmmBuilderDoBoundsCheck = gopt Opt_DoBoundsChecking      dflags
  -- , cmmBuilderDoTagCheck    = gopt Opt_DoTagInferenceChecks  dflags
  -- -- backend flags
  -- , cmmBuilderAllowBigArith             = not ncg
  -- , cmmBuilderAllowQuotRemInstr         = ncg  && (x86ish || ppc)
  -- , cmmBuilderAllowQuotRem2             = (ncg && (x86ish || ppc)) || llvm
  -- , cmmBuilderAllowExtendedAddSubInstrs = (ncg && (x86ish || ppc)) || llvm
  -- , cmmBuilderAllowIntMul2Instr         = (ncg && x86ish) || llvm
  -- -- SIMD flags
  -- , cmmBuilderVecInstrsErr  = vec_err
  -- , cmmBuilderAvx           = isAvxEnabled                   dflags
  -- , cmmBuilderAvx2          = isAvx2Enabled                  dflags
  -- , cmmBuilderAvx512f       = isAvx512fEnabled               dflags
  -- , cmmBuilderTickyAP       = gopt Opt_Ticky_AP dflags
  } where profile  = targetProfile dflags
          -- platform = profilePlatform profile
          -- bk_end  = backend dflags
          -- ncg     = bk_end == NCG
          -- llvm    = bk_end == LLVM
          -- b_blob  = if not ncg then Nothing else binBlobThreshold dflags
          -- x86ish  = case platformArch platform of
          --             ArchX86    -> True
          --             ArchX86_64 -> True
          --             _          -> False
          -- ppc     = case platformArch platform of
          --             ArchPPC      -> True
          --             ArchPPC_64 _ -> True
          --             _            -> False
          -- vec_err = case backend dflags of
          --             LLVM -> Nothing
          --             _    -> Just (unlines ["SIMD vector instructions require the LLVM back-end.", "Please use -fllvm."])
