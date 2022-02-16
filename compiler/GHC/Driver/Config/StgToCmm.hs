module GHC.Driver.Config.StgToCmm
  ( initStgToCmmConfig
  , PrimitiveImplementation(..)
  ) where

import GHC.StgToCmm.Config

import GHC.Driver.Backend.Types
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
  { stgToCmmProfile       = profile
  , stgToCmmThisModule    = mod
  , stgToCmmTmpDir        = tmpDir          dflags
  , stgToCmmContext       = initSDocContext dflags defaultDumpStyle
  , stgToCmmDebugLevel    = debugLevel      dflags
  , stgToCmmBinBlobThresh = bin_blob_thresh
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
  , stgToCmmDoTagCheck    = gopt Opt_DoTagInferenceChecks  dflags
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
          aarch64 = platformArch platform == ArchAArch64
          vec_err = case backendSimdValidity (backend dflags) of
                      IsValid -> Nothing
                      NotValid msg -> Just msg
          bin_blob_thresh = if backendSupportsEmbeddedBlobs (backend dflags) then
                                binBlobThreshold dflags
                            else
                                0 -- suppress them entirely



-- | This enumeration type specifies how the back end wishes GHC's
-- primitives to be implemented.  (Module "GHC.StgToCmm.Prim" provides
-- a generic implementation of every primitive, but some primitives,
-- like `IntQuotRemOp`, can be implemented more efficiently by
-- certain back ends on certain platforms.  For example, by using a
-- machine instruction that simultaneously computes quotient and remainder.)
--
-- For the meaning of each alternative, consult
-- "GHC.Driver.Config.StgToCmm".  (In a perfect world, type
-- `PrimitiveImplementation` would be defined there, in the module
-- that determines its meaning.  But I could not figure out how to do
-- it without mutual recursion across module boundaries.)

data PrimitiveImplementation
    = LlvmPrimitives
    | NcgPrimitives
    | GenericPrimitives
  deriving Show
