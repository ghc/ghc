-- | External unit database
module GHC.Unit.External.DB
  ( ExtUnitDB (..)
  , UnitInfoMap
  , WiringMap
  )
where

import GHC.Unit.Types
import GHC.Unit.Info

import Data.Map (Map)

type UnitInfoMap = Map UnitId UnitInfo

type WiringMap = Map UnitId UnitId

type UnwiringMap = Map UnitId UnitId


-- | Consolidated database of external units
--
-- Usually created from a *stack* of unit databases on disk.
data ExtUnitDB = ExtUnitDB
  { unitInfoMap :: UnitInfoMap
    -- ^ Information about units

  , unitWiringMap :: WiringMap
    -- ^ A mapping from database unit keys to wired in unit ids.
    -- Units that are not in the map have UnitKey = UnitId.

  , unitUnwiringMap :: UnwiringMap
    -- ^ A mapping from wired in unit ids to unit keys from the database.
    -- Units that are not in the map have UnitKey = UnitId.
  }


