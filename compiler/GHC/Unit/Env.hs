{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A 'UnitEnv' provides the complete interface into everything that is loaded
-- into a GHC session, including the 'HomeUnitGraph' for mapping home units to their
-- 'HomePackageTable's (which store information about all home modules), and
-- the 'ExternalPackageState' which provides access to all external packages
-- loaded.
--
-- This module is meant to be imported as @UnitEnv@ when calling @insertHpt@:
--
-- @
-- import GHC.Unit.Env (UnitEnv, HomeUnitGraph, HomeUnitEnv)
-- import GHC.Unit.Env as UnitEnv
-- @
--
-- Here is an overview of how the UnitEnv, ModuleGraph, HUG, HPT, and EPS interact:
--
-- @
-- ┌────────────────┐┌────────────────────┐┌───────────┐
-- │HomePackageTable││ExternalPackageState││ModuleGraph│
-- └┬───────────────┘└┬───────────────────┘└┬──────────┘
-- ┌▽────────────┐    │                     │
-- │HomeUnitGraph│    │                     │
-- └┬────────────┘    │                     │
-- ┌▽─────────────────▽─────────────────────▽┐
-- │UnitEnv                                  │
-- └┬─────────────-──────────────────────────┘
--  │
--  │
-- ┌▽──────────────────────────────────────▽┐
-- │HscEnv                                  │
-- └────────────────────────────────────────┘
-- @
--
-- The 'UnitEnv' references the 'HomeUnitGraph' (with all the home unit
-- modules), the 'ExternalPackageState' (information about all
-- non-home/external units), and the 'ModuleGraph' (which describes the
-- relationship between the modules being compiled).
-- The 'HscEnv' references this 'UnitEnv'.
-- The 'HomeUnitGraph' has one 'HomePackageTable' for every unit.
module GHC.Unit.Env
    ( UnitEnv (..)
    , initUnitEnv
    , ueEPS -- Not really needed, get directly type families and rule base!
    , updateHug
    -- * Unit Env helper functions
    , ue_currentHomeUnitEnv
    , ue_hpt
    , ue_setActiveUnit
    , ue_currentUnit
    , ue_findHomeUnitEnv
    , ue_unitHomeUnit
    , ue_unitHomeUnit_maybe
    , ue_updateHomeUnitEnv
    , ue_all_home_unit_ids
    , ue_unsafeHomeUnit

    -- * HUG Re-export
    , HomeUnitGraph
    , HomeUnitEnv (..)

    -- * Invariants
    , assertUnitEnvInvariant
    -- * Preload units info
    , preloadUnitsInfo
    , preloadUnitsInfo'
    -- * Home Module functions
    , isUnitEnvInstalledModule

    --------------------------------------------------------------------------------
    -- WIP above
    --------------------------------------------------------------------------------

    -- * Operations on the UnitEnv
    , renameUnitId

    -- ** Modifying the current active home unit
    , insertHpt
    , ue_setFlags

    -- * Queries

    -- ** Queries on the current active home unit
    , ue_homeUnitState
    , ue_unit_dbs
    , ue_homeUnit
    , ue_unitFlags

    -- ** Reachability
    , ue_transitiveHomeDeps

    --------------------------------------------------------------------------------
    -- Harder queries for the whole UnitEnv
    --------------------------------------------------------------------------------

    -- ** Instances, rules, type fams, annotations, etc..
    --
    -- | The @hug@ prefix means the function returns only things found in home
    -- units.
    , hugCompleteSigs
    , hugAllInstances
    , hugAllAnns

    -- * Legacy API
    --
    -- | This API is deprecated!
    , ue_units
    )
where

import GHC.Prelude
import qualified Data.Set as Set

import GHC.Unit.External
import GHC.Unit.State
import GHC.Unit.Home
import GHC.Unit.Types
import GHC.Unit.Home.ModInfo
import GHC.Unit.Home.PackageTable
import GHC.Unit.Home.Graph (HomeUnitGraph, HomeUnitEnv)
import qualified GHC.Unit.Home.Graph as HUG
import GHC.Unit.Module.Graph

import GHC.Platform
import GHC.Settings
import GHC.Data.Maybe
import GHC.Utils.Misc (HasDebugCallStack)
import GHC.Driver.DynFlags
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Types.Annotations
import GHC.Types.CompleteMatch
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv

--------------------------------------------------------------------------------
-- The hard queries
--------------------------------------------------------------------------------

-- | Find all the instance declarations (of classes and families) from
-- the Home Package Table filtered by the provided predicate function.
hugAllInstances :: UnitEnv -> IO (InstEnv, [FamInst])
hugAllInstances = HUG.allInstances . ue_home_unit_graph

-- | Find all the annotations in all home units
hugAllAnns :: UnitEnv -> IO AnnEnv
hugAllAnns = HUG.allAnns . ue_home_unit_graph

-- | Get all 'CompleteMatches' (arising from COMPLETE pragmas) present across
-- all home units.
hugCompleteSigs :: UnitEnv -> IO CompleteMatches
hugCompleteSigs = HUG.allCompleteSigs . ue_home_unit_graph

--------------------------------------------------------------------------------
-- UnitEnv
--------------------------------------------------------------------------------

data UnitEnv = UnitEnv
    { ue_eps :: {-# UNPACK #-} !ExternalUnitCache
        -- ^ Information about the currently loaded external packages.
        -- This is mutable because packages will be demand-loaded during
        -- a compilation run as required.

    , ue_current_unit    :: UnitId

    , ue_module_graph    :: ModuleGraph
        -- ^ The module graph of the current session
        -- See Note [Downsweep and the ModuleGraph] for when this is constructed.

    , ue_home_unit_graph :: !HomeUnitGraph
        -- See Note [Multiple Home Units]

    , ue_platform  :: !Platform
        -- ^ Platform

    , ue_namever   :: !GhcNameVersion
        -- ^ GHC name/version (used for dynamic library suffix)
    }

ueEPS :: UnitEnv -> IO ExternalPackageState
ueEPS = eucEPS . ue_eps

initUnitEnv :: UnitId -> HomeUnitGraph -> GhcNameVersion -> Platform -> IO UnitEnv
initUnitEnv cur_unit hug namever platform = do
  eps <- initExternalUnitCache
  return $ UnitEnv
    { ue_eps             = eps
    , ue_home_unit_graph = hug
    , ue_module_graph    = emptyMG
    , ue_current_unit    = cur_unit
    , ue_platform        = platform
    , ue_namever         = namever
    }

updateHug :: (HomeUnitGraph -> HomeUnitGraph) -> UnitEnv -> UnitEnv
updateHug = ue_updateHUG

-- -----------------------------------------------------------------------------
-- Extracting information from the packages in scope
-- -----------------------------------------------------------------------------

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
    unit_state = HUG.homeUnitEnv_units (ue_currentHomeUnitEnv unit_env)
    ids      = ids0 ++ inst_ids
    inst_ids = case ue_homeUnit unit_env of
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

-- -- | Test if the module comes from the home unit
isUnitEnvInstalledModule :: UnitEnv -> InstalledModule -> Bool
isUnitEnvInstalledModule ue m = maybe False (`isHomeInstalledModule` m) hu
  where
    hu = ue_unitHomeUnit_maybe (moduleUnit m) ue

-- -------------------------------------------------------
-- Operations on arbitrary elements of the home unit graph
-- -------------------------------------------------------

ue_findHomeUnitEnv :: HasDebugCallStack => UnitId -> UnitEnv -> HomeUnitEnv
ue_findHomeUnitEnv uid e = case HUG.lookupHugUnitId uid (ue_home_unit_graph e) of
  Nothing -> pprPanic "Unit unknown to the internal unit environment"
              $  text "unit (" <> ppr uid <> text ")"
              $$ ppr (HUG.allUnits (ue_home_unit_graph e))
  Just hue -> hue

-- -------------------------------------------------------
-- Query and modify UnitState of active unit in HomeUnitEnv
-- -------------------------------------------------------

ue_homeUnitState :: HasDebugCallStack => UnitEnv -> UnitState
ue_homeUnitState = HUG.homeUnitEnv_units . ue_currentHomeUnitEnv

ue_unit_dbs :: UnitEnv ->  Maybe [UnitDatabase UnitId]
ue_unit_dbs = HUG.homeUnitEnv_unit_dbs . ue_currentHomeUnitEnv

-- -------------------------------------------------------
-- Query and modify Home Package Table in HomeUnitEnv
-- -------------------------------------------------------

-- | Get the /current home unit/'s package table
ue_hpt :: HasDebugCallStack => UnitEnv -> HomePackageTable
ue_hpt = HUG.homeUnitEnv_hpt . ue_currentHomeUnitEnv

-- | Inserts a 'HomeModInfo' at the given 'ModuleName' on the
-- 'HomePackageTable' of the /current unit/ being compiled.
insertHpt :: HasDebugCallStack => HomeModInfo -> UnitEnv -> IO ()
insertHpt hmi e = do
  HUG.addHomeModInfoToHug hmi (ue_home_unit_graph e)

ue_updateHUG :: HasDebugCallStack => (HomeUnitGraph -> HomeUnitGraph) -> UnitEnv -> UnitEnv
ue_updateHUG f e = ue_updateUnitHUG f e

ue_updateUnitHUG :: HasDebugCallStack => (HomeUnitGraph -> HomeUnitGraph) -> UnitEnv -> UnitEnv
ue_updateUnitHUG f ue_env = ue_env { ue_home_unit_graph = f (ue_home_unit_graph ue_env)}

-- -------------------------------------------------------
-- Query and modify DynFlags in HomeUnitEnv
-- -------------------------------------------------------

ue_unitFlags :: HasDebugCallStack => UnitId -> UnitEnv -> DynFlags
ue_unitFlags uid ue_env = HUG.homeUnitEnv_dflags $ ue_findHomeUnitEnv uid ue_env

-- | Sets the 'DynFlags' of the /current unit/ being compiled to the given ones
ue_setFlags :: HasDebugCallStack => DynFlags -> UnitEnv -> UnitEnv
ue_setFlags dflags env =
  env
    { ue_home_unit_graph = HUG.updateUnitFlags
                            (ue_currentUnit env)
                            (const dflags)
                            (ue_home_unit_graph env)
    }

-- -------------------------------------------------------
-- Query and modify home units in HomeUnitEnv
-- -------------------------------------------------------

ue_homeUnit :: UnitEnv -> Maybe HomeUnit
ue_homeUnit = HUG.homeUnitEnv_home_unit . ue_currentHomeUnitEnv

ue_unsafeHomeUnit :: UnitEnv -> HomeUnit
ue_unsafeHomeUnit ue = case ue_homeUnit ue of
  Nothing -> panic "ue_unsafeHomeUnit: No home unit"
  Just h  -> h

ue_unitHomeUnit :: UnitId -> UnitEnv -> HomeUnit
ue_unitHomeUnit uid = expectJust . ue_unitHomeUnit_maybe uid

ue_unitHomeUnit_maybe :: UnitId -> UnitEnv -> Maybe HomeUnit
ue_unitHomeUnit_maybe uid ue_env =
  HUG.homeUnitEnv_home_unit =<< HUG.lookupHugUnitId uid (ue_home_unit_graph ue_env)

-- -------------------------------------------------------
-- Query and modify the currently active unit
-- -------------------------------------------------------

ue_currentHomeUnitEnv :: HasDebugCallStack => UnitEnv -> HomeUnitEnv
ue_currentHomeUnitEnv e =
  case HUG.lookupHugUnitId (ue_currentUnit e) (ue_home_unit_graph e) of
    Just unitEnv -> unitEnv
    Nothing -> pprPanic "packageNotFound" $
      (ppr $ ue_currentUnit e) $$ ppr (HUG.allUnits (ue_home_unit_graph e))

ue_setActiveUnit :: UnitId -> UnitEnv -> UnitEnv
ue_setActiveUnit u ue_env = assertUnitEnvInvariant $ ue_env
  { ue_current_unit = u
  }

ue_currentUnit :: UnitEnv -> UnitId
ue_currentUnit = ue_current_unit


ue_updateHomeUnitEnv :: (HomeUnitEnv -> HomeUnitEnv) -> UnitId -> UnitEnv -> UnitEnv
ue_updateHomeUnitEnv f uid e = e
  { ue_home_unit_graph = HUG.unitEnv_adjust f uid $ ue_home_unit_graph e
  }

ue_all_home_unit_ids :: UnitEnv -> Set.Set UnitId
ue_all_home_unit_ids = HUG.allUnits . ue_home_unit_graph

-- | Rename a unit id in the internal unit env.
--
-- @'renameUnitId' oldUnit newUnit UnitEnv@, it is assumed that the 'oldUnit' exists in the home units map,
-- otherwise we panic.
-- The 'DynFlags' associated with the home unit will have its field 'homeUnitId' set to 'newUnit'.
renameUnitId :: HasDebugCallStack => UnitId -> UnitId -> UnitEnv -> UnitEnv
renameUnitId oldUnit newUnit unitEnv =
  case HUG.renameUnitId oldUnit newUnit (ue_home_unit_graph unitEnv) of
    Nothing ->
      pprPanic "Tried to rename unit, but it didn't exist"
                $ text "Rename old unit \"" <> ppr oldUnit <> text "\" to \""<> ppr newUnit <> text "\""
                $$ nest 2 (ppr $ HUG.allUnits (ue_home_unit_graph unitEnv))
    Just newHug ->
      let
        activeUnit :: UnitId
        !activeUnit = if ue_currentUnit unitEnv == oldUnit
                  then newUnit
                  else ue_currentUnit unitEnv

      in
      unitEnv
        { ue_current_unit = activeUnit
        , ue_home_unit_graph =
            HUG.updateUnitFlags
              newUnit
              (\df -> df{ homeUnitId_ = newUnit })
              newHug
        }

-- ---------------------------------------------
-- Transitive closure
-- ---------------------------------------------

ue_transitiveHomeDeps :: UnitId -> UnitEnv -> [UnitId]
ue_transitiveHomeDeps uid e =
  case HUG.transitiveHomeDeps uid (ue_home_unit_graph e) of
    Nothing -> pprPanic "Unit unknown to the internal unit environment"
                $  text "unit (" <> ppr uid <> text ")"
                $$ ppr (HUG.allUnits $ ue_home_unit_graph e)
    Just deps -> deps

-- ---------------------------------------------
-- Asserts to enforce invariants for the UnitEnv
-- ---------------------------------------------

-- FIXME: Shouldn't this be a proper assertion only used in debug mode?
assertUnitEnvInvariant :: HasDebugCallStack => UnitEnv -> UnitEnv
assertUnitEnvInvariant u =
  case HUG.lookupHugUnitId (ue_current_unit u) (ue_home_unit_graph u) of
    Just _ -> u
    Nothing ->
      pprPanic "invariant" (ppr (ue_current_unit u) $$ ppr (HUG.allUnits (ue_home_unit_graph u)))

-- -----------------------------------------------------------------------------
-- Pretty output functions
-- -----------------------------------------------------------------------------

-- pprUnitEnvGraph :: UnitEnv -> IO SDoc
-- pprUnitEnvGraph env = do
--   hugDoc <- HUG.pprHomeUnitGraph $ ue_home_unit_graph env
--   return $ text "pprInternalUnitMap" $$ nest 2 hugDoc

{-
Note [Multiple Home Units]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic idea of multiple home units is quite simple. Instead of allowing one
home unit, you can multiple home units

The flow:

1. Dependencies between units are specified between each other in the normal manner,
   a unit is identified by the -this-unit-id flag and dependencies specified by
   the normal -package-id flag.
2. Downsweep is augmented to know to know how to look for dependencies in any home unit.
3. The rest of the compiler is modified appropriately to offset paths to the right places.
4. --make mode can parallelise between home units and multiple units are allowed to produce linkables.

Closure Property
----------------

You must perform a clean cut of the dependency graph.

> Any dependency which is not a home unit must not (transitively) depend on a home unit.

For example, if you have three packages p, q and r, then if p depends on q which
depends on r then it is illegal to load both p and r as home units but not q,
because q is a dependency of the home unit p which depends on another home unit r.

Offsetting Paths
----------------

The main complication to the implementation is to do with offsetting paths appropriately.
For a long time it has been assumed that GHC will execute in the top-directory for a unit,
normally where the .cabal file is and all paths are interpreted relative to there.
When you have multiple home units then it doesn't make sense to pick one of these
units to choose as the base-unit, and you can't robustly change directories when
using parallelism.

Therefore there is an option `-working-directory`, which tells GHC where the relative
paths for each unit should be interpreted relative to. For example, if you specify
`-working-dir a -ib`, then GHC will offset the relative path `b`, by `a`, and look for
source files in `a/b`. The same thing happens for any path passed on the command line.

A non-exhaustive list is

* -i
* -I
* -odir/-hidir/-outputdir/-stubdir/-hiedir
* Target files passed on the command line

There is also a template-haskell function, makeRelativeToProject, which uses the `-working-directory` option
in order to allow users to offset their own relative paths.

-}

--------------------------------------------------------------------------------
-- * Legacy API
--------------------------------------------------------------------------------

{-# DEPRECATED ue_units "Renamed to ue_homeUnitState because of confusion between units(tate) and unit(s) plural" #-}
ue_units :: HasDebugCallStack => UnitEnv -> UnitState
ue_units = ue_homeUnitState

