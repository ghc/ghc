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

import GHC.Prelude


-- This config is static and contains information only passed *downwards* by StgToCmm.Monad
data CmmBuilderConfig = CmmBuilderConfig
  ----------------------------- General Settings --------------------------------
  { cmmBuilderProfile       :: !Profile            -- ^ Current profile
  , cmmBuilderContext       :: !SDocContext        -- ^ Context for C-- building
  , cmmBuilderEmitDebugInfo :: !Bool               -- ^ Whether we wish to print debug messages
  ---------------------------------- Flags --------------------------------------
  , cmmBuilderTickyCfg       :: !CmmTickyConfig    -- ^ Flags related to ticky
  , cmmBuilderSCCProfiling   :: !Bool              -- ^ Check if cost-centre profiling is enabled
  , cmmBuilderEagerBlackHole :: !Bool              -- ^
  , cmmBuilderInfoTableMap   :: !Bool              -- ^ true means generate C Stub for IPE map, See note [Mapping
                                                 -- Info Tables to Source Positions]
  , cmmBuilderOmitYields     :: !Bool              -- ^ true means omit heap checks when no allocation is performed
  }


cmmBuilderPlatform :: CmmBuilderConfig -> Platform
cmmBuilderPlatform = profilePlatform . cmmBuilderProfile


instance ContainsPlatformProfile CmmBuilderConfig where
  platformProfile = cmmBuilderProfile

instance ContainsCmmTickyConfig CmmBuilderConfig where
  cmmTickyConfig = cmmBuilderTickyCfg
