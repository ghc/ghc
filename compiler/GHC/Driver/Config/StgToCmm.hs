{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module GHC.Driver.Config.StgToCmm
  ( initStgToCmmConfig
  ) where

import GHC.Prelude.Basic

import GHC.StgToCmm.Config

import GHC.Cmm.MachOp ( FMASign(..))
import GHC.Driver.Backend
import GHC.Driver.Session
import GHC.Platform
import GHC.Platform.Profile
import GHC.Utils.Error
import GHC.Unit.Module
import GHC.Utils.Outputable

initStgToCmmConfig :: DynFlags -> Module -> StgToCmmConfig
initStgToCmmConfig dflags mod = StgToCmmConfig
  -- settings
  { stgToCmmProfile       = profile
  , stgToCmmThisModule    = mod
  , stgToCmmTmpDir        = tmpDir          dflags
  , stgToCmmContext       = initSDocContext dflags defaultDumpStyle
  , stgToCmmEmitDebugInfo = debugLevel      dflags > 0
  , stgToCmmBinBlobThresh = b_blob
  , stgToCmmMaxInlAllocSize = maxInlineAllocSize           dflags
  -- ticky options
  , stgToCmmDoTicky       = gopt Opt_Ticky                 dflags
  , stgToCmmTickyAllocd   = gopt Opt_Ticky_Allocd          dflags
  , stgToCmmTickyLNE      = gopt Opt_Ticky_LNE             dflags
  , stgToCmmTickyDynThunk = gopt Opt_Ticky_Dyn_Thunk       dflags
  , stgToCmmTickyTag      = gopt Opt_Ticky_Tag             dflags
  -- flags
  , stgToCmmLoopification = gopt Opt_Loopification         dflags
  , stgToCmmAlignCheck    = gopt Opt_AlignmentSanitisation dflags
  , stgToCmmOptHpc        = gopt Opt_Hpc                   dflags
  , stgToCmmFastPAPCalls  = gopt Opt_FastPAPCalls          dflags
  , stgToCmmSCCProfiling  = sccProfilingEnabled            dflags
  , stgToCmmEagerBlackHole = gopt Opt_EagerBlackHoling     dflags
  , stgToCmmOrigThunkInfo = gopt Opt_OrigThunkInfo         dflags
  , stgToCmmInfoTableMap  = gopt Opt_InfoTableMap          dflags
  , stgToCmmInfoTableMapWithFallback = gopt Opt_InfoTableMapWithFallback dflags
  , stgToCmmInfoTableMapWithStack = gopt Opt_InfoTableMapWithStack dflags
  , stgToCmmOmitYields    = gopt Opt_OmitYields            dflags
  , stgToCmmOmitIfPragmas = gopt Opt_OmitInterfacePragmas  dflags
  , stgToCmmPIC           = gopt Opt_PIC                   dflags
  , stgToCmmPIE           = gopt Opt_PIE                   dflags
  , stgToCmmExtDynRefs    = gopt Opt_ExternalDynamicRefs   dflags
  , stgToCmmDoBoundsCheck = gopt Opt_DoBoundsChecking      dflags
  , stgToCmmDoTagCheck    = gopt Opt_DoTagInferenceChecks  dflags
  -- backend flags
  , stgToCmmAllowBigArith             = not ncg || platformArch platform == ArchWasm32 || platformArch platform == ArchX86
  , stgToCmmAllowBigQuot              = not ncg || platformArch platform == ArchWasm32
  , stgToCmmAllowQuotRemInstr         = ncg  && (x86ish || ppc)
  , stgToCmmAllowQuotRem2             = (ncg && (x86ish || ppc)) || llvm
  , stgToCmmAllowExtendedAddSubInstrs = (ncg && (x86ish || ppc)) || llvm
  , stgToCmmAllowFMAInstr =
      if
        | not (isFmaEnabled dflags)
        || not (ncg || llvm)
        -- If we're not using the native code generator or LLVM,
        -- fall back to the generic implementation.
        || platformArch platform == ArchWasm32
        -- WASM doesn't support native FMA instructions (at the time of writing).
        -> const False

        -- FNMSub and FNMAdd have different semantics on PowerPC,
        -- so we avoid using them.
        | ppc
        -> \ case { FMAdd -> True; FMSub -> True; _ -> False }

        | otherwise
        -> const True

  , stgToCmmAllowIntMul2Instr         = (ncg && (x86ish || aarch64)) || llvm
  , stgToCmmAllowWordMul2Instr        = (ncg && (x86ish || ppc || aarch64)) || llvm
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
                          JSPrimitives      -> (False, False)
                          NcgPrimitives     -> (True, False)
                          LlvmPrimitives    -> (False, True)
          aarch64 = case platformArch platform of
                      ArchAArch64  -> True
                      _            -> False
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
