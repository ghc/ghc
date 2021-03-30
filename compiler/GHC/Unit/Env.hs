module GHC.Unit.Env
    ( UnitEnv (..)
    , initUnitEnv
    , unsafeGetHomeUnit
    , updateHpt
    , preloadUnitsInfo
    , preloadUnitsInfo'
    )
where

import GHC.Prelude

import GHC.Unit.External
import GHC.Unit.State
import GHC.Unit.Home
import GHC.Unit.Types
import GHC.Unit.Home.ModInfo

import GHC.Platform
import GHC.Settings
import GHC.Data.Maybe
import GHC.Utils.Panic.Plain

data UnitEnv = UnitEnv
    { ue_units     :: !UnitState
        -- ^ External units

    , ue_unit_dbs :: !(Maybe [UnitDatabase UnitId])
        -- ^ Stack of unit databases for the target platform.
        --
        -- This field is populated with the result of `initUnits`.
        --
        -- 'Nothing' means the databases have never been read from disk.
        --
        -- Usually we don't reload the databases from disk if they are
        -- cached, even if the database flags changed!

    , ue_eps :: {-# UNPACK #-} !ExternalUnitCache
        -- ^ Information about the currently loaded external packages.
        -- This is mutable because packages will be demand-loaded during
        -- a compilation run as required.

    , ue_home_unit :: !(Maybe HomeUnit)
        -- ^ Home unit

    , ue_hpt :: !HomePackageTable
        -- ^ The home package table describes already-compiled
        -- home-package modules, /excluding/ the module we
        -- are compiling right now.
        -- (In one-shot mode the current module is the only
        -- home-package module, so hsc_HPT is empty.  All other
        -- modules count as \"external-package\" modules.
        -- However, even in GHCi mode, hi-boot interfaces are
        -- demand-loaded into the external-package table.)
        --
        -- 'hsc_HPT' is not mutable because we only demand-load
        -- external packages; the home package is eagerly
        -- loaded, module by module, by the compilation manager.
        --
        -- The HPT may contain modules compiled earlier by @--make@
        -- but not actually below the current module in the dependency
        -- graph.
        --
        -- (This changes a previous invariant: changed Jan 05.)

    , ue_platform  :: !Platform
        -- ^ Platform

    , ue_namever   :: !GhcNameVersion
        -- ^ GHC name/version (used for dynamic library suffix)
    }

initUnitEnv :: GhcNameVersion -> Platform -> IO UnitEnv
initUnitEnv namever platform = do
  eps <- initExternalUnitCache
  return $ UnitEnv
    { ue_units     = emptyUnitState
    , ue_unit_dbs  = Nothing
    , ue_eps       = eps
    , ue_home_unit = Nothing
    , ue_hpt       = emptyHomePackageTable
    , ue_platform  = platform
    , ue_namever   = namever
    }

-- | Get home-unit
--
-- Unsafe because the home-unit may not be set
unsafeGetHomeUnit :: UnitEnv -> HomeUnit
unsafeGetHomeUnit ue = case ue_home_unit ue of
  Nothing -> panic "unsafeGetHomeUnit: No home unit"
  Just h  -> h

updateHpt :: (HomePackageTable -> HomePackageTable) -> UnitEnv -> UnitEnv
updateHpt f ue = ue { ue_hpt = f (ue_hpt ue) }

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
    unit_state = ue_units unit_env
    ids      = ids0 ++ inst_ids
    inst_ids = case ue_home_unit unit_env of
      Nothing -> []
      Just home_unit
       -- An indefinite package will have insts to HOLE,
       -- which is not a real package. Don't look it up.
       -- Fixes #14525
       | isHomeUnitIndefinite home_unit -> []
       | otherwise -> map (toUnitId . moduleUnit . snd) (homeUnitInstantiations home_unit)
    pkg_map = unitInfoMap unit_state
    preload = preloadUnits unit_state

    all_pkgs  = closeUnitDeps' pkg_map preload (ids `zip` repeat Nothing)
    all_infos = map (unsafeLookupUnitId unit_state) <$> all_pkgs


-- | Lookup 'UnitInfo' for every preload unit from the UnitState and for every
-- unit used to instantiate the home unit.
preloadUnitsInfo :: UnitEnv -> MaybeErr UnitErr [UnitInfo]
preloadUnitsInfo unit_env = preloadUnitsInfo' unit_env []
