{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GHC.Types.CostCentre.State
   ( CostCentreState
   , newCostCentreState
   , CostCentreIndex
   , unCostCentreIndex
   , getCCIndex
   )
where

import GHC.Prelude
import GHC.Data.FastString
import GHC.Data.FastString.Env

import Data.Data
import GHC.Utils.Binary

-- | Per-module state for tracking cost centre indices.
--
-- See documentation of 'GHC.Types.CostCentre.cc_flavour' for more details.
newtype CostCentreState = CostCentreState (FastStringEnv Int)

-- | Initialize cost centre state.
newCostCentreState :: CostCentreState
newCostCentreState = CostCentreState emptyFsEnv

-- | An index into a given cost centre module,name,flavour set
newtype CostCentreIndex = CostCentreIndex { unCostCentreIndex :: Int }
  deriving (Eq, Ord, Data, Binary)

-- | Get a new index for a given cost centre name.
getCCIndex :: FastString
           -> CostCentreState
           -> (CostCentreIndex, CostCentreState)
getCCIndex nm (CostCentreState m) =
    (CostCentreIndex idx, CostCentreState m')
  where
    m_idx = lookupFsEnv m nm
    idx = maybe 0 id m_idx
    m' = extendFsEnv m nm (idx + 1)
