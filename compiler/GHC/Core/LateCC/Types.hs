-- | Types related to late cost center insertion
module GHC.Core.LateCC.Types
  ( LateCCConfig(..)
  , LateCCBindSpec(..)
  , LateCCEnv(..)
  , LateCCState(..)
  , initLateCCState
  , LateCCM
  ) where

import GHC.Prelude

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import qualified Data.Set as S

import GHC.Data.FastString
import GHC.Types.CostCentre
import GHC.Types.CostCentre.State
import GHC.Unit.Types

-- | Late cost center insertion configuration.
--
-- Specifies whether cost centers are added to overloaded function call sites
-- and/or top-level bindings, and which top-level bindings they are added to.
-- Also holds the cost center insertion environment.
data LateCCConfig =
      LateCCConfig
        { lateCCConfig_whichBinds :: !LateCCBindSpec
        , lateCCConfig_overloadedCalls :: !Bool
        , lateCCConfig_env :: !LateCCEnv
        }

-- | The types of top-level bindings we support adding cost centers to.
data LateCCBindSpec =
      LateCCNone
    | LateCCBinds
    | LateCCOverloadedBinds

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

-- | The empty late cost centre insertion state
initLateCCState :: s -> LateCCState s
initLateCCState s =
    LateCCState
      { lateCCState_ccState = newCostCentreState
      , lateCCState_ccs = mempty
      , lateCCState_extra = s
      }

-- | Late cost centre insertion monad
type LateCCM s = ReaderT LateCCEnv (State (LateCCState s))
