-- | The stg to cmm code generator configuration

module GHC.Cmm.Builder.Config
  ( CmmBuilderConfig(..)
  , cmmBuilderPlatform
  ) where

import GHC.Platform
import GHC.Platform.Profile
import GHC.Platform.Profile.Class
import GHC.StgToCmm.Ticky.Config
import GHC.Utils.Outputable
--import GHC.Utils.TmpFs

import GHC.Prelude


-- This config is static and contains information only passed *downwards* by StgToCmm.Monad
data CmmBuilderConfig = CmmBuilderConfig
  ----------------------------- General Settings --------------------------------
  { cmmBuilderProfile       :: !Profile            -- ^ Current profile
  , cmmBuilderContext       :: !SDocContext        -- ^ Context for C-- building
  , cmmBuilderEmitDebugInfo :: !Bool               -- ^ Whether we wish to print debug messages
  ---------------------------------- Flags --------------------------------------
  , cmmBuilderTickyCfg       :: !CmmTickyConfig    -- ^ Flags related to ticky
  -- , cmmBuilderLoopification  :: !Bool              -- ^ Loopification enabled (cf @-floopification@)
  -- , cmmBuilderAlignCheck     :: !Bool              -- ^ Insert alignment check (cf @-falignment-sanitisation@)
  -- , cmmBuilderOptHpc         :: !Bool              -- ^ perform code generation for code coverage
  -- , cmmBuilderFastPAPCalls   :: !Bool              -- ^
  , cmmBuilderSCCProfiling   :: !Bool              -- ^ Check if cost-centre profiling is enabled
  , cmmBuilderEagerBlackHole :: !Bool              -- ^
  , cmmBuilderInfoTableMap   :: !Bool              -- ^ true means generate C Stub for IPE map, See note [Mapping
                                                 -- Info Tables to Source Positions]
  , cmmBuilderOmitYields     :: !Bool              -- ^ true means omit heap checks when no allocation is performed
  -- , cmmBuilderOmitIfPragmas  :: !Bool              -- ^ true means don't generate interface programs (implied by -O0)
  -- , cmmBuilderPIC            :: !Bool              -- ^ true if @-fPIC@
  -- , cmmBuilderPIE            :: !Bool              -- ^ true if @-fPIE@
  -- , cmmBuilderExtDynRefs     :: !Bool              -- ^ true if @-fexternal-dynamic-refs@, meaning generate
  --                                                -- code for linking against dynamic libraries
  -- , cmmBuilderDoBoundsCheck  :: !Bool              -- ^ decides whether to check array bounds in StgToCmm.Prim
  --                                                -- or not
  -- , cmmBuilderDoTagCheck     :: !Bool              -- ^ Verify tag inference predictions.
  -- ------------------------------ Backend Flags ----------------------------------
  -- , cmmBuilderAllowBigArith             :: !Bool   -- ^ Allowed to emit larger than native size arithmetic (only LLVM and C backends)
  -- , cmmBuilderAllowQuotRemInstr         :: !Bool   -- ^ Allowed to generate QuotRem instructions
  -- , cmmBuilderAllowQuotRem2             :: !Bool   -- ^ Allowed to generate QuotRem
  -- , cmmBuilderAllowExtendedAddSubInstrs :: !Bool   -- ^ Allowed to generate AddWordC, SubWordC, Add2, etc.
  -- , cmmBuilderAllowIntMul2Instr         :: !Bool   -- ^ Allowed to generate IntMul2 instruction
  -- , cmmBuilderTickyAP                   :: !Bool   -- ^ Disable use of precomputed standard thunks.
  -- ------------------------------ SIMD flags ------------------------------------
  -- -- Each of these flags checks vector compatibility with the backend requested
  -- -- during compilation. In essence, this means checking for @-fllvm@ which is
  -- -- the only backend that currently allows SIMD instructions, see
  -- -- Ghc.StgToCmm.Prim.checkVecCompatibility for these flags only call site.
  -- , cmmBuilderVecInstrsErr   :: Maybe String       -- ^ Error (if any) to raise when vector instructions are
  --                                                -- used, see @StgToCmm.Prim.checkVecCompatibility@
  -- , cmmBuilderAvx            :: !Bool              -- ^ check for Advanced Vector Extensions
  -- , cmmBuilderAvx2           :: !Bool              -- ^ check for Advanced Vector Extensions 2
  -- , cmmBuilderAvx512f        :: !Bool              -- ^ check for Advanced Vector 512-bit Extensions
  }


cmmBuilderPlatform :: CmmBuilderConfig -> Platform
cmmBuilderPlatform = profilePlatform . cmmBuilderProfile


instance ContainsPlatformProfile CmmBuilderConfig where
  platformProfile = cmmBuilderProfile

instance ContainsCmmTickyConfig CmmBuilderConfig where
  cmmTickyConfig = cmmBuilderTickyCfg
