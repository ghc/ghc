module GHC.Unit.Env
    ( UnitEnv (..)
    , preloadUnitsInfo
    , preloadUnitsInfo'
    )
where

import GHC.Prelude

import GHC.Unit.State
import GHC.Unit.Home
import GHC.Unit.Types

import GHC.Platform
import GHC.Settings
import GHC.Data.Maybe

data UnitEnv = UnitEnv
    { ue_units     :: !UnitState      -- ^ Units
    , ue_home_unit :: !HomeUnit       -- ^ Home unit
    , ue_platform  :: !Platform       -- ^ Platform
    , ue_namever   :: !GhcNameVersion -- ^ GHC name/version (used for dynamic library suffix)
    }

-- -----------------------------------------------------------------------------
-- Extracting information from the packages in scope

-- Many of these functions take a list of packages: in those cases,
-- the list is expected to contain the "dependent packages",
-- i.e. those packages that were found to be depended on by the
-- current module/program.  These can be auto or non-auto packages, it
-- doesn't really matter.  The list is always combined with the list
-- of preload (command-line) packages to determine which packages to
-- use.

-- | Lookup 'UnitInfo' for every preload unit from the UnitState, for every unit
-- used to instantiate the home unit, and for every unit explicitly passed in
-- the given list of UnitId.
preloadUnitsInfo' :: UnitEnv -> [UnitId] -> MaybeErr UnitErr [UnitInfo]
preloadUnitsInfo' unit_env ids0 = all_infos
  where
    home_unit  = ue_home_unit unit_env
    unit_state = ue_units     unit_env
    ids      = ids0 ++ inst_ids
    inst_ids
       -- An indefinite package will have insts to HOLE,
       -- which is not a real package. Don't look it up.
       -- Fixes #14525
       | isHomeUnitIndefinite home_unit = []
       | otherwise = map (toUnitId . moduleUnit . snd) (homeUnitInstantiations home_unit)
    pkg_map = unitInfoMap unit_state
    preload = preloadUnits unit_state

    all_pkgs  = closeUnitDeps' pkg_map preload (ids `zip` repeat Nothing)
    all_infos = map (unsafeLookupUnitId unit_state) <$> all_pkgs


-- | Lookup 'UnitInfo' for every preload unit from the UnitState and for every
-- unit used to instantiate the home unit.
preloadUnitsInfo :: UnitEnv -> MaybeErr UnitErr [UnitInfo]
preloadUnitsInfo unit_env = preloadUnitsInfo' unit_env []
