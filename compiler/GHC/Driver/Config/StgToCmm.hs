module GHC.Driver.Config.StgToCmm
  ( initStgToCmmConfig
  ) where

import GHC.StgToCmm.Config

import GHC.Driver.Backend
import GHC.Driver.Session
import GHC.Platform
import GHC.Platform.Profile
import GHC.Unit.Module
import GHC.Utils.Outputable

import Data.Maybe
import Prelude

initStgToCmmConfig :: DynFlags -> Module -> StgToCmmConfig
initStgToCmmConfig dflags mod = StgToCmmConfig
  -- settings
  { stgToCmmProfile       = profile
  , stgToCmmThisModule    = mod
  , stgToCmmTmpDir        = tmpDir          dflags
  , stgToCmmContext       = initSDocContext dflags defaultDumpStyle
  , stgToCmmDebugLevel    = debugLevel      dflags
  , stgToCmmBinBlobThresh = binBlobThreshold               dflags
  , stgToCmmMaxInlAllocSize = maxInlineAllocSize           dflags
  -- ticky options
  , stgToCmmDoTicky       = gopt Opt_Ticky                 dflags
  , stgToCmmTickyAllocd   = gopt Opt_Ticky_Allocd          dflags
  , stgToCmmTickyLNE      = gopt Opt_Ticky_LNE             dflags
  , stgToCmmTickyDynThunk = gopt Opt_Ticky_Dyn_Thunk       dflags
  -- flags
  , stgToCmmLoopification = gopt Opt_Loopification         dflags
  , stgToCmmAlignCheck    = gopt Opt_AlignmentSanitisation dflags
  , stgToCmmOptHpc        = gopt Opt_Hpc                   dflags
  , stgToCmmFastPAPCalls  = gopt Opt_FastPAPCalls          dflags
  , stgToCmmSCCProfiling  = sccProfilingEnabled            dflags
  , stgToCmmEagerBlackHole = gopt Opt_EagerBlackHoling     dflags
  , stgToCmmInfoTableMap  = gopt Opt_InfoTableMap          dflags
  , stgToCmmOmitYields    = gopt Opt_OmitYields            dflags
  , stgToCmmOmitIfPragmas = gopt Opt_OmitInterfacePragmas  dflags
  , stgToCmmPIC           = gopt Opt_PIC                   dflags
  , stgToCmmPIE           = gopt Opt_PIE                   dflags
  , stgToCmmExtDynRefs    = gopt Opt_ExternalDynamicRefs   dflags
  , stgToCmmDoBoundsCheck = gopt Opt_DoBoundsChecking      dflags
  -- backend flags
  , stgToCmmAllowBigArith             = not ncg
  , stgToCmmAllowQuotRemInstr         = ncg  && (x86ish || ppc)
  , stgToCmmAllowQuotRem2             = (ncg && (x86ish || ppc)) || llvm
  , stgToCmmAllowExtendedAddSubInstrs = (ncg && (x86ish || ppc)) || llvm
  , stgToCmmAllowIntMul2Instr         = (ncg && x86ish) || llvm
  , stgToCmmAllowFabsInstrs           = (ncg && (x86ish || ppc || aarch64)) || llvm
  -- SIMD flags
  , stgToCmmVecInstrsErr  = vec_err
  , stgToCmmAvx           = isAvxEnabled                   dflags
  , stgToCmmAvx2          = isAvx2Enabled                  dflags
  , stgToCmmAvx512f       = isAvx512fEnabled               dflags
  } where profile  = targetProfile dflags
          platform = profilePlatform profile
          bk_end  = backend dflags
          ncg     = bk_end == NCG
          llvm    = bk_end == LLVM
          x86ish  = case platformArch platform of
                      ArchX86    -> True
                      ArchX86_64 -> True
                      _          -> False
          ppc     = case platformArch platform of
                      ArchPPC      -> True
                      ArchPPC_64 _ -> True
                      _            -> False
          aarch64 = platformArch platform == ArchAArch64
          vec_err = case backend dflags of
                      LLVM -> Nothing
                      _    -> Just (unlines ["SIMD vector instructions require the LLVM back-end.", "Please use -fllvm."])
