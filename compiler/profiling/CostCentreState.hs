{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CostCentreState ( CostCentreState, newCostCentreState
                       , CostCentreIndex, unCostCentreIndex, getCCIndex
                       ) where

import GhcPrelude
import FastString
import FastStringEnv

import Data.Data
import Binary

-- | Per-module state for tracking cost centre indices.
--
-- See documentation of 'CostCentre.cc_flavour' for more details.
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
