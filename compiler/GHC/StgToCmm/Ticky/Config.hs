-- | The stg to cmm code generator configuration

module GHC.StgToCmm.Ticky.Config
  ( CmmTickyConfig(..)
  , ContainsCmmTickyConfig(..)
  ) where

import GHC.Prelude

-- This config is static and contains information only passed *downwards* by StgToCmm.Monad
data CmmTickyConfig = CmmTickyConfig
  { cmmTickyEnable    :: !Bool              -- ^ Ticky profiling enabled (cf @-ticky@)
  , cmmTickyAllocd    :: !Bool              -- ^ True indicates ticky prof traces allocs of each named
                                            -- thing in addition to allocs _by_ that thing
  , cmmTickyLNE       :: !Bool              -- ^ True indicates ticky uses name-specific counters for
                                            -- join-points (let-no-escape)
  , cmmTickyDynThunk  :: !Bool              -- ^ True indicates ticky uses name-specific counters for
                                            -- dynamic thunks
  , cmmTickyTag       :: !Bool              -- ^ True indicates ticky will count number of avoided tag checks by tag inference.
  }


class ContainsCmmTickyConfig c where
  cmmTickyConfig :: c -> CmmTickyConfig
