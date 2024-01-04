{-# LANGUAGE DerivingStrategies #-}
module GHC.Core.LateCC.Types where

import GHC.Prelude

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import qualified Data.Set as S

import GHC.Data.FastString
import GHC.Types.CostCentre
import GHC.Types.CostCentre.State
import GHC.Unit.Types

-- | Late cost centre insertion environment
data LateCCEnv = LateCCEnv
  { lateCCEnv_module :: !Module
    -- ^ Current module
  , lateCCEnv_file :: Maybe FastString
    -- ^ Current file, if we have one
  , lateCCEnv_countEntries:: !Bool
    -- ^ Whether the inserted cost centers should count entries
  , lateCCEnv_collectCCs  :: !Bool
    -- ^ Whether to collect the cost centres we insert. See
    -- Note [Collecting late cost centres]
  }

-- | Late cost centre insertion state, indexed by some extra state type that an
-- insertion method may require.
data LateCCState s = LateCCState
    { lateCCState_ccs :: !(S.Set CostCentre)
      -- ^ Cost centres that have been inserted
    , lateCCState_ccState :: !CostCentreState
      -- ^ Per-module state tracking for cost centre indices
    , lateCCState_extra :: !s
    }

-- | Late cost centre insertion monad
type LateCCM s = ReaderT LateCCEnv (State (LateCCState s))
