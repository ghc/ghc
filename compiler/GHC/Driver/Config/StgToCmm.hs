module GHC.Driver.Config.StgToCmm
  ( initStgToCmmConfig
  ) where

import GHC.StgToCmm.Config

import GHC.Driver.Config.Cmm.Builder
import GHC.Driver.Backend
import GHC.Driver.Session
import GHC.Platform
import GHC.Platform.Profile
import GHC.Utils.Error
import GHC.Unit.Module
import GHC.Utils.Outputable

import Data.Maybe
import Prelude

initStgToCmmConfig :: DynFlags -> Module -> StgToCmmConfig
initStgToCmmConfig dflags mod = StgToCmmConfig
  -- settings
  { stgToCmmBuilderCfg    = initCmmBuilderConfig dflags
  , stgToCmmThisModule    = mod
  , stgToCmmTmpDir        = tmpDir          dflags
  , stgToCmmContext       = initSDocContext dflags defaultDumpStyle
  , stgToCmmBinBlobThresh = b_blob
  , stgToCmmMaxInlAllocSize = maxInlineAllocSize           dflags
  -- flags
  , stgToCmmLoopification = gopt Opt_Loopification         dflags
  , stgToCmmAlignCheck    = gopt Opt_AlignmentSanitisation dflags
  , stgToCmmOptHpc        = gopt Opt_Hpc                   dflags
  , stgToCmmFastPAPCalls  = gopt Opt_FastPAPCalls          dflags
  , stgToCmmOmitIfPragmas = gopt Opt_OmitInterfacePragmas  dflags
  , stgToCmmPIC           = gopt Opt_PIC                   dflags
  , stgToCmmPIE           = gopt Opt_PIE                   dflags
  , stgToCmmExtDynRefs    = gopt Opt_ExternalDynamicRefs   dflags
  , stgToCmmDoBoundsCheck = gopt Opt_DoBoundsChecking      dflags
  , stgToCmmDoTagCheck    = gopt Opt_DoTagInferenceChecks  dflags
  -- backend flags
  , stgToCmmAllowBigArith             = not ncg
  , stgToCmmAllowQuotRemInstr         = ncg  && (x86ish || ppc)
  , stgToCmmAllowQuotRem2             = (ncg && (x86ish || ppc)) || llvm
  , stgToCmmAllowExtendedAddSubInstrs = (ncg && (x86ish || ppc)) || llvm
  , stgToCmmAllowIntMul2Instr         = (ncg && x86ish) || llvm
  -- SIMD flags
  , stgToCmmVecInstrsErr  = vec_err
  , stgToCmmAvx           = isAvxEnabled                   dflags
  , stgToCmmAvx2          = isAvx2Enabled                  dflags
  , stgToCmmAvx512f       = isAvx512fEnabled               dflags
  , stgToCmmTickyAP       = gopt Opt_Ticky_AP dflags
  } where profile  = targetProfile dflags
          platform = profilePlatform profile
          bk_end  = backend dflags
          b_blob  = if not ncg then Nothing else binBlobThreshold dflags
          (ncg, llvm) = case backendPrimitiveImplementation bk_end of
                          GenericPrimitives -> (False, False)
                          NcgPrimitives -> (True, False)
                          LlvmPrimitives -> (False, True)
          x86ish  = case platformArch platform of
                      ArchX86    -> True
                      ArchX86_64 -> True
                      _          -> False
          ppc     = case platformArch platform of
                      ArchPPC      -> True
                      ArchPPC_64 _ -> True
                      _            -> False
          vec_err = case backendSimdValidity (backend dflags) of
                      IsValid -> Nothing
                      NotValid msg -> Just msg
