-- | The stg to cmm code generator configuration

module GHC.StgToCmm.Config
  ( StgToCmmConfig(..)
  , stgToCmmProfile
  , stgToCmmPlatform
  , stgToCmmEmitDebugInfo
  , stgToCmmSCCProfiling
  , stgToCmmEagerBlackHole
  , stgToCmmInfoTableMap
  , stgToCmmOmitYields
  ) where

import GHC.Platform
import GHC.Platform.Profile
import GHC.Platform.Profile.Class
import GHC.Cmm.Builder.Config ( CmmBuilderConfig(..) )
import GHC.StgToCmm.Ticky.Config
import GHC.Unit.Module
import GHC.Utils.Outputable
import GHC.Utils.TmpFs

import GHC.Prelude


-- This config is static and contains information only passed *downwards* by StgToCmm.Monad
data StgToCmmConfig = StgToCmmConfig
  { stgToCmmBuilderCfg    :: !CmmBuilderConfig   -- ^ Config for building C-- in general
  ----------------------------- General Settings --------------------------------
  , stgToCmmThisModule    :: Module              -- ^ The module being compiled. This field kept lazy for
                                                 -- Cmm/Parser.y which preloads it with a panic
  , stgToCmmTmpDir        :: !TempDir            -- ^ Temp Dir for files used in compilation
  , stgToCmmContext       :: !SDocContext        -- ^ Context for StgToCmm phase
  , stgToCmmBinBlobThresh :: !(Maybe Word)       -- ^ Threshold at which Binary literals (e.g. strings)
                                                 -- are either dumped to a file and a CmmFileEmbed literal
                                                 -- is emitted (over threshold), or become a CmmString
                                                 -- Literal (under or at threshold). CmmFileEmbed is only supported
                                                 -- with the NCG, thus a Just means two things: We have a threshold,
                                                 -- and will be using the NCG. Conversely, a Nothing implies we are not
                                                 -- using NCG and disables CmmFileEmbed. See Note
                                                 -- [Embedding large binary blobs] in GHC.CmmToAsm.Ppr, and
                                                 -- @cgTopBinding@ in GHC.StgToCmm.
  , stgToCmmMaxInlAllocSize :: !Int              -- ^ Max size, in bytes, of inline array allocations.
  ---------------------------------- Flags --------------------------------------
  , stgToCmmLoopification  :: !Bool              -- ^ Loopification enabled (cf @-floopification@)
  , stgToCmmAlignCheck     :: !Bool              -- ^ Insert alignment check (cf @-falignment-sanitisation@)
  , stgToCmmOptHpc         :: !Bool              -- ^ perform code generation for code coverage
  , stgToCmmFastPAPCalls   :: !Bool              -- ^
  , stgToCmmOmitIfPragmas  :: !Bool              -- ^ true means don't generate interface programs (implied by -O0)
  , stgToCmmPIC            :: !Bool              -- ^ true if @-fPIC@
  , stgToCmmPIE            :: !Bool              -- ^ true if @-fPIE@
  , stgToCmmExtDynRefs     :: !Bool              -- ^ true if @-fexternal-dynamic-refs@, meaning generate
                                                 -- code for linking against dynamic libraries
  , stgToCmmDoBoundsCheck  :: !Bool              -- ^ decides whether to check array bounds in StgToCmm.Prim
                                                 -- or not
  , stgToCmmDoTagCheck     :: !Bool              -- ^ Verify tag inference predictions.
  ------------------------------ Backend Flags ----------------------------------
  , stgToCmmAllowBigArith             :: !Bool   -- ^ Allowed to emit larger than native size arithmetic (only LLVM and C backends)
  , stgToCmmAllowQuotRemInstr         :: !Bool   -- ^ Allowed to generate QuotRem instructions
  , stgToCmmAllowQuotRem2             :: !Bool   -- ^ Allowed to generate QuotRem
  , stgToCmmAllowExtendedAddSubInstrs :: !Bool   -- ^ Allowed to generate AddWordC, SubWordC, Add2, etc.
  , stgToCmmAllowIntMul2Instr         :: !Bool   -- ^ Allowed to generate IntMul2 instruction
  , stgToCmmTickyAP                   :: !Bool   -- ^ Disable use of precomputed standard thunks.
  ------------------------------ SIMD flags ------------------------------------
  -- Each of these flags checks vector compatibility with the backend requested
  -- during compilation. In essence, this means checking for @-fllvm@ which is
  -- the only backend that currently allows SIMD instructions, see
  -- Ghc.StgToCmm.Prim.checkVecCompatibility for these flags only call site.
  , stgToCmmVecInstrsErr   :: Maybe String       -- ^ Error (if any) to raise when vector instructions are
                                                 -- used, see @StgToCmm.Prim.checkVecCompatibility@
  , stgToCmmAvx            :: !Bool              -- ^ check for Advanced Vector Extensions
  , stgToCmmAvx2           :: !Bool              -- ^ check for Advanced Vector Extensions 2
  , stgToCmmAvx512f        :: !Bool              -- ^ check for Advanced Vector 512-bit Extensions
  }

stgToCmmProfile :: StgToCmmConfig -> Profile
stgToCmmProfile = platformProfile . stgToCmmBuilderCfg

stgToCmmPlatform :: StgToCmmConfig -> Platform
stgToCmmPlatform = profilePlatform . stgToCmmProfile

stgToCmmEmitDebugInfo :: StgToCmmConfig -> Bool
stgToCmmEmitDebugInfo = cmmBuilderEmitDebugInfo . stgToCmmBuilderCfg

stgToCmmSCCProfiling :: StgToCmmConfig -> Bool
stgToCmmSCCProfiling = cmmBuilderSCCProfiling . stgToCmmBuilderCfg

stgToCmmEagerBlackHole :: StgToCmmConfig -> Bool
stgToCmmEagerBlackHole = cmmBuilderEagerBlackHole . stgToCmmBuilderCfg

stgToCmmInfoTableMap :: StgToCmmConfig -> Bool
stgToCmmInfoTableMap = cmmBuilderInfoTableMap . stgToCmmBuilderCfg

stgToCmmOmitYields :: StgToCmmConfig -> Bool
stgToCmmOmitYields = cmmBuilderOmitYields . stgToCmmBuilderCfg


instance ContainsPlatformProfile StgToCmmConfig where
  platformProfile = stgToCmmProfile

instance ContainsCmmTickyConfig StgToCmmConfig where
  cmmTickyConfig = cmmTickyConfig . stgToCmmBuilderCfg
