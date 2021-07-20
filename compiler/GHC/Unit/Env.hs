{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module GHC.Unit.Env
    ( UnitEnv (..)
    , initUnitEnv
    , unsafeGetHomeUnit
    , updateHug
    , updateHpt
    -- * Unit Env helper functions
    , ue_units
    , ue_currentHomeUnitEnv
    , ue_setUnits
    , ue_setUnitFlags
    , ue_unit_dbs
    , ue_setUnitDbs
    , ue_hpt
    , ue_homeUnit
    , ue_unsafeHomeUnit
    , ue_setFlags
    , ue_setActiveUnit
    , ue_currentUnit
    , ue_findHomeUnitEnv
    , ue_updateHomeUnitEnv
    , ue_unitHomeUnit
    , ue_unitFlags
    , ue_renameUnitId
    , ue_transitiveHomeDeps
    -- * HomeUnitEnv
    , HomeUnitGraph
    , HomeUnitEnv (..)
    , mkHomeUnitEnv
    , lookupHugByModule
    , hugElts
    , lookupHug
    , addHomeModInfoToHug
    -- * UnitEnvGraph
    , UnitEnvGraph (..)
    , unitEnv_insert
    , unitEnv_delete
    , unitEnv_adjust
    , unitEnv_new
    , unitEnv_singleton
    , unitEnv_map
    , unitEnv_member
    , unitEnv_lookup_maybe
    , unitEnv_lookup
    , unitEnv_keys
    , unitEnv_elts
    , unitEnv_hpts
    , unitEnv_foldWithKey
    , unitEnv_mapWithKey
    -- * Invariants
    , assertUnitEnvInvariant
    -- * Preload units info
    , preloadUnitsInfo
    , preloadUnitsInfo'
    -- * Home Module functions
    , isUnitEnvInstalledModule )
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Utils.Misc (HasDebugCallStack)
import GHC.Driver.Session
import GHC.Utils.Outputable
import GHC.Utils.Panic (pprPanic)
import GHC.Unit.Module.ModIface
import GHC.Unit.Module
import qualified Data.Set as Set

data UnitEnv = UnitEnv
    { ue_eps :: {-# UNPACK #-} !ExternalUnitCache
        -- ^ Information about the currently loaded external packages.
        -- This is mutable because packages will be demand-loaded during
        -- a compilation run as required.

    , ue_current_unit    :: UnitId

    , ue_home_unit_graph :: !HomeUnitGraph
        -- See Note [Multiple Home Units]

    , ue_platform  :: !Platform
        -- ^ Platform

    , ue_namever   :: !GhcNameVersion
        -- ^ GHC name/version (used for dynamic library suffix)
    }

initUnitEnv :: UnitId -> HomeUnitGraph -> GhcNameVersion -> Platform -> IO UnitEnv
initUnitEnv cur_unit hug namever platform = do
  eps <- initExternalUnitCache
  return $ UnitEnv
    { ue_eps             = eps
    , ue_home_unit_graph = hug
    , ue_current_unit    = cur_unit
    , ue_platform        = platform
    , ue_namever         = namever
    }

-- | Get home-unit
--
-- Unsafe because the home-unit may not be set
unsafeGetHomeUnit :: UnitEnv -> HomeUnit
unsafeGetHomeUnit ue = ue_unsafeHomeUnit ue

updateHpt :: (HomePackageTable -> HomePackageTable) -> UnitEnv -> UnitEnv
updateHpt = ue_updateHPT

updateHug :: (HomeUnitGraph -> HomeUnitGraph) -> UnitEnv -> UnitEnv
updateHug = ue_updateHUG

ue_transitiveHomeDeps :: UnitId -> UnitEnv -> [UnitId]
ue_transitiveHomeDeps uid unit_env = Set.toList (loop Set.empty [uid])
  where
    loop acc [] = acc
    loop acc (uid:uids)
      | uid `Set.member` acc = loop acc uids
      | otherwise =
        let hue = homeUnitDepends (homeUnitEnv_units (ue_findHomeUnitEnv uid unit_env))
        in loop (Set.insert uid acc) (hue ++ uids)


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

-- -----------------------------------------------------------------------------

data HomeUnitEnv = HomeUnitEnv
  { homeUnitEnv_units     :: !UnitState
      -- ^ External units

  , homeUnitEnv_unit_dbs :: !(Maybe [UnitDatabase UnitId])
      -- ^ Stack of unit databases for the target platform.
      --
      -- This field is populated with the result of `initUnits`.
      --
      -- 'Nothing' means the databases have never been read from disk.
      --
      -- Usually we don't reload the databases from disk if they are
      -- cached, even if the database flags changed!

  , homeUnitEnv_dflags :: DynFlags
    -- ^ The dynamic flag settings
  , homeUnitEnv_hpt :: HomePackageTable
    -- ^ The home package table describes already-compiled
    -- home-package modules, /excluding/ the module we
    -- are compiling right now.
    -- (In one-shot mode the current module is the only
    -- home-package module, so homeUnitEnv_hpt is empty.  All other
    -- modules count as \"external-package\" modules.
    -- However, even in GHCi mode, hi-boot interfaces are
    -- demand-loaded into the external-package table.)
    --
    -- 'homeUnitEnv_hpt' is not mutable because we only demand-load
    -- external packages; the home package is eagerly
    -- loaded, module by module, by the compilation manager.
    --
    -- The HPT may contain modules compiled earlier by @--make@
    -- but not actually below the current module in the dependency
    -- graph.
    --
    -- (This changes a previous invariant: changed Jan 05.)

  , homeUnitEnv_home_unit :: !(Maybe HomeUnit)
    -- ^ Home-unit
  }

instance Outputable HomeUnitEnv where
  ppr hug = pprHPT (homeUnitEnv_hpt hug)

homeUnitEnv_unsafeHomeUnit :: HomeUnitEnv -> HomeUnit
homeUnitEnv_unsafeHomeUnit hue = case homeUnitEnv_home_unit hue of
  Nothing -> panic "homeUnitEnv_unsafeHomeUnit: No home unit"
  Just h  -> h

mkHomeUnitEnv :: DynFlags -> HomePackageTable -> Maybe HomeUnit -> HomeUnitEnv
mkHomeUnitEnv dflags hpt home_unit = HomeUnitEnv
  { homeUnitEnv_units = emptyUnitState
  , homeUnitEnv_unit_dbs = Nothing
  , homeUnitEnv_dflags = dflags
  , homeUnitEnv_hpt = hpt
  , homeUnitEnv_home_unit = home_unit
  }

-- | Test if the module comes from the home unit
isUnitEnvInstalledModule :: UnitEnv -> InstalledModule -> Bool
isUnitEnvInstalledModule ue m = maybe False (`isHomeInstalledModule` m) hu
  where
    hu = ue_unitHomeUnit_maybe (moduleUnit m) ue


type HomeUnitGraph = UnitEnvGraph HomeUnitEnv

lookupHugByModule :: Module -> HomeUnitGraph -> Maybe HomeModInfo
lookupHugByModule mod hug
  | otherwise = do
      env <- (unitEnv_lookup_maybe (toUnitId $ moduleUnit mod) hug)
      lookupHptByModule (homeUnitEnv_hpt env) mod

hugElts :: HomeUnitGraph -> [(UnitId, HomeUnitEnv)]
hugElts hug = unitEnv_elts hug

addHomeModInfoToHug :: HomeModInfo -> HomeUnitGraph -> HomeUnitGraph
addHomeModInfoToHug hmi hug = unitEnv_alter go hmi_unit hug
  where
    hmi_mod :: Module
    hmi_mod = mi_module (hm_iface hmi)

    hmi_unit = toUnitId (moduleUnit hmi_mod)
    _hmi_mn   = moduleName hmi_mod

    go :: Maybe HomeUnitEnv -> Maybe HomeUnitEnv
    go Nothing = pprPanic "addHomeInfoToHug" (ppr hmi_mod)
    go (Just hue) = Just (updateHueHpt (addHomeModInfoToHpt hmi) hue)

updateHueHpt :: (HomePackageTable -> HomePackageTable) -> HomeUnitEnv -> HomeUnitEnv
updateHueHpt f hue = hue { homeUnitEnv_hpt = f (homeUnitEnv_hpt hue)}


lookupHug :: HomeUnitGraph -> UnitId -> ModuleName -> Maybe HomeModInfo
lookupHug hug uid mod = unitEnv_lookup_maybe uid hug >>= flip lookupHpt mod . homeUnitEnv_hpt


instance Outputable (UnitEnvGraph HomeUnitEnv) where
  ppr g = ppr [(k, length (homeUnitEnv_hpt  hue)) | (k, hue) <- (unitEnv_elts g)]


type UnitEnvGraphKey = UnitId

newtype UnitEnvGraph v = UnitEnvGraph
  { unitEnv_graph :: Map UnitEnvGraphKey v
  } deriving (Functor, Foldable, Traversable)

unitEnv_insert :: UnitEnvGraphKey -> v -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_insert unitId env unitEnv = unitEnv
  { unitEnv_graph = Map.insert unitId env (unitEnv_graph unitEnv)
  }

unitEnv_delete :: UnitEnvGraphKey -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_delete uid unitEnv =
    unitEnv
      { unitEnv_graph = Map.delete uid (unitEnv_graph unitEnv)
      }

unitEnv_adjust :: (v -> v) -> UnitEnvGraphKey -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_adjust f uid unitEnv = unitEnv
  { unitEnv_graph = Map.adjust f uid (unitEnv_graph unitEnv)
  }

unitEnv_alter :: (Maybe v -> Maybe v) -> UnitEnvGraphKey -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_alter f uid unitEnv = unitEnv
  { unitEnv_graph = Map.alter f uid (unitEnv_graph unitEnv)
  }

unitEnv_mapWithKey :: (UnitEnvGraphKey -> v -> b) -> UnitEnvGraph v -> UnitEnvGraph b
unitEnv_mapWithKey f (UnitEnvGraph u) = UnitEnvGraph $ Map.mapWithKey f u

unitEnv_new :: Map UnitEnvGraphKey v -> UnitEnvGraph v
unitEnv_new m =
  UnitEnvGraph
    { unitEnv_graph = m
    }

unitEnv_singleton :: UnitEnvGraphKey -> v -> UnitEnvGraph v
unitEnv_singleton active m = UnitEnvGraph
  { unitEnv_graph = Map.singleton active m
  }

unitEnv_map :: (v -> v) -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_map f m = m { unitEnv_graph = Map.map f (unitEnv_graph m)}

unitEnv_member :: UnitEnvGraphKey -> UnitEnvGraph v -> Bool
unitEnv_member u env = Map.member u (unitEnv_graph env)

unitEnv_lookup_maybe :: UnitEnvGraphKey -> UnitEnvGraph v -> Maybe v
unitEnv_lookup_maybe u env = Map.lookup u (unitEnv_graph env)

unitEnv_lookup :: UnitEnvGraphKey -> UnitEnvGraph v -> v
unitEnv_lookup u env = fromJust $ unitEnv_lookup_maybe u env

unitEnv_keys :: UnitEnvGraph v -> Set.Set UnitEnvGraphKey
unitEnv_keys env = Map.keysSet (unitEnv_graph env)

unitEnv_elts :: UnitEnvGraph v -> [(UnitEnvGraphKey, v)]
unitEnv_elts env = Map.toList (unitEnv_graph env)

unitEnv_hpts :: UnitEnvGraph HomeUnitEnv -> [HomePackageTable]
unitEnv_hpts env = map homeUnitEnv_hpt (Map.elems (unitEnv_graph env))

unitEnv_foldWithKey :: (b -> UnitEnvGraphKey -> a -> b) -> b -> UnitEnvGraph a -> b
unitEnv_foldWithKey f z (UnitEnvGraph g)= Map.foldlWithKey' f z g

-- -------------------------------------------------------
-- Query and modify UnitState in HomeUnitEnv
-- -------------------------------------------------------

ue_units :: HasDebugCallStack => UnitEnv -> UnitState
ue_units = homeUnitEnv_units . ue_currentHomeUnitEnv

ue_setUnits :: UnitState -> UnitEnv -> UnitEnv
ue_setUnits units ue = ue_updateHomeUnitEnv f (ue_currentUnit ue) ue
  where
    f hue = hue { homeUnitEnv_units = units  }

ue_unit_dbs :: UnitEnv ->  Maybe [UnitDatabase UnitId]
ue_unit_dbs = homeUnitEnv_unit_dbs . ue_currentHomeUnitEnv

ue_setUnitDbs :: Maybe [UnitDatabase UnitId] -> UnitEnv -> UnitEnv
ue_setUnitDbs unit_dbs ue = ue_updateHomeUnitEnv f (ue_currentUnit ue) ue
  where
    f hue = hue { homeUnitEnv_unit_dbs = unit_dbs  }

-- -------------------------------------------------------
-- Query and modify Home Package Table in HomeUnitEnv
-- -------------------------------------------------------

ue_hpt :: HasDebugCallStack => UnitEnv -> HomePackageTable
ue_hpt = homeUnitEnv_hpt . ue_currentHomeUnitEnv

ue_updateHPT :: HasDebugCallStack => (HomePackageTable -> HomePackageTable) -> UnitEnv -> UnitEnv
ue_updateHPT f e = ue_updateUnitHPT f (ue_currentUnit e) e

ue_updateHUG :: HasDebugCallStack => (HomeUnitGraph -> HomeUnitGraph) -> UnitEnv -> UnitEnv
ue_updateHUG f e = ue_updateUnitHUG f e

ue_updateUnitHPT :: HasDebugCallStack => (HomePackageTable -> HomePackageTable) -> UnitId -> UnitEnv -> UnitEnv
ue_updateUnitHPT f uid ue_env = ue_updateHomeUnitEnv update uid ue_env
  where
    update unitEnv = unitEnv { homeUnitEnv_hpt = f $ homeUnitEnv_hpt unitEnv }

ue_updateUnitHUG :: HasDebugCallStack => (HomeUnitGraph -> HomeUnitGraph) -> UnitEnv -> UnitEnv
ue_updateUnitHUG f ue_env = ue_env { ue_home_unit_graph = f (ue_home_unit_graph ue_env)}

-- -------------------------------------------------------
-- Query and modify DynFlags in HomeUnitEnv
-- -------------------------------------------------------

ue_setFlags :: HasDebugCallStack => DynFlags -> UnitEnv -> UnitEnv
ue_setFlags dflags ue_env = ue_setUnitFlags (ue_currentUnit ue_env) dflags ue_env

ue_setUnitFlags :: HasDebugCallStack => UnitId -> DynFlags -> UnitEnv -> UnitEnv
ue_setUnitFlags uid dflags e =
  ue_updateUnitFlags (const dflags) uid e

ue_unitFlags :: HasDebugCallStack => UnitId -> UnitEnv -> DynFlags
ue_unitFlags uid ue_env = homeUnitEnv_dflags $ ue_findHomeUnitEnv uid ue_env

ue_updateUnitFlags :: HasDebugCallStack => (DynFlags -> DynFlags) -> UnitId -> UnitEnv -> UnitEnv
ue_updateUnitFlags f uid e = ue_updateHomeUnitEnv update uid e
  where
    update hue = hue { homeUnitEnv_dflags = f $ homeUnitEnv_dflags hue }

-- -------------------------------------------------------
-- Query and modify home units in HomeUnitEnv
-- -------------------------------------------------------

ue_homeUnit :: UnitEnv -> Maybe HomeUnit
ue_homeUnit = homeUnitEnv_home_unit . ue_currentHomeUnitEnv

ue_unsafeHomeUnit :: UnitEnv -> HomeUnit
ue_unsafeHomeUnit ue = case ue_homeUnit ue of
  Nothing -> panic "unsafeGetHomeUnit: No home unit"
  Just h  -> h

ue_unitHomeUnit_maybe :: UnitId -> UnitEnv -> Maybe HomeUnit
ue_unitHomeUnit_maybe uid ue_env =
  homeUnitEnv_unsafeHomeUnit <$> (ue_findHomeUnitEnv_maybe uid ue_env)

ue_unitHomeUnit :: UnitId -> UnitEnv -> HomeUnit
ue_unitHomeUnit uid ue_env = homeUnitEnv_unsafeHomeUnit $ ue_findHomeUnitEnv uid ue_env


-- -------------------------------------------------------
-- Query and modify the currently active unit
-- -------------------------------------------------------

ue_currentHomeUnitEnv :: HasDebugCallStack => UnitEnv -> HomeUnitEnv
ue_currentHomeUnitEnv e =
  case ue_findHomeUnitEnv_maybe (ue_currentUnit e) e of
    Just unitEnv -> unitEnv
    Nothing -> pprPanic "packageNotFound" $
      (ppr $ ue_currentUnit e) $$ ppr (ue_home_unit_graph e)

ue_setActiveUnit :: UnitId -> UnitEnv -> UnitEnv
ue_setActiveUnit u ue_env = assertUnitEnvInvariant $ ue_env
  { ue_current_unit = u
  }

ue_currentUnit :: UnitEnv -> UnitId
ue_currentUnit = ue_current_unit

-- -------------------------------------------------------
-- Operations on arbitrary elements of the home unit graph
-- -------------------------------------------------------

ue_findHomeUnitEnv_maybe :: UnitId -> UnitEnv -> Maybe HomeUnitEnv
ue_findHomeUnitEnv_maybe uid e =
  unitEnv_lookup_maybe uid (ue_home_unit_graph e)

ue_findHomeUnitEnv :: HasDebugCallStack => UnitId -> UnitEnv -> HomeUnitEnv
ue_findHomeUnitEnv uid e = case unitEnv_lookup_maybe uid (ue_home_unit_graph e) of
  Nothing -> pprPanic "Unit unknown to the internal unit environment"
              $  text "unit (" <> ppr uid <> text ")"
              $$ pprUnitEnvGraph e
  Just hue -> hue

ue_updateHomeUnitEnv :: (HomeUnitEnv -> HomeUnitEnv) -> UnitId -> UnitEnv -> UnitEnv
ue_updateHomeUnitEnv f uid e = e
  { ue_home_unit_graph = unitEnv_adjust f uid $ ue_home_unit_graph e
  }


-- | Rename a unit id in the internal unit env.
--
-- @'ue_renameUnitId' oldUnit newUnit UnitEnv@, it is assumed that the 'oldUnit' exists in the map,
-- otherwise we panic.
-- The 'DynFlags' associated with the home unit will have its field 'homeUnitId' set to 'newUnit'.
ue_renameUnitId :: HasDebugCallStack => UnitId -> UnitId -> UnitEnv -> UnitEnv
ue_renameUnitId oldUnit newUnit unitEnv = case ue_findHomeUnitEnv_maybe oldUnit unitEnv of
  Nothing ->
    pprPanic "Tried to rename unit, but it didn't exist"
              $ text "Rename old unit \"" <> ppr oldUnit <> text "\" to \""<> ppr newUnit <> text "\""
              $$ nest 2 (pprUnitEnvGraph unitEnv)
  Just oldEnv ->
    let
      activeUnit :: UnitId
      !activeUnit = if ue_currentUnit unitEnv == oldUnit
                then newUnit
                else ue_currentUnit unitEnv

      newInternalUnitEnv = oldEnv
        { homeUnitEnv_dflags = (homeUnitEnv_dflags oldEnv)
            { homeUnitId_ = newUnit
            }
        }
    in
    unitEnv
      { ue_current_unit = activeUnit
      , ue_home_unit_graph =
          unitEnv_insert newUnit newInternalUnitEnv
          $ unitEnv_delete oldUnit
          $ ue_home_unit_graph unitEnv
          }

-- ---------------------------------------------
-- Asserts to enforce invariants for the UnitEnv
-- ---------------------------------------------

assertUnitEnvInvariant :: HasDebugCallStack => UnitEnv -> UnitEnv
assertUnitEnvInvariant u =
  if ue_current_unit u `unitEnv_member` ue_home_unit_graph u
    then u
    else pprPanic "invariant" (ppr (ue_current_unit u) $$ ppr (ue_home_unit_graph u))

-- -----------------------------------------------------------------------------
-- Pretty output functions
-- -----------------------------------------------------------------------------

pprUnitEnvGraph :: UnitEnv -> SDoc
pprUnitEnvGraph env = text "pprInternalUnitMap"
  $$ nest 2 (pprHomeUnitGraph $ ue_home_unit_graph env)

pprHomeUnitGraph :: HomeUnitGraph -> SDoc
pprHomeUnitGraph unitEnv = vcat (map (\(k, v) -> pprHomeUnitEnv k v) $ Map.assocs $ unitEnv_graph unitEnv)

pprHomeUnitEnv :: UnitId -> HomeUnitEnv -> SDoc
pprHomeUnitEnv uid env =
  ppr uid <+> text "(flags:" <+> ppr (homeUnitId_ $ homeUnitEnv_dflags env) <> text "," <+> ppr (fmap homeUnitId $ homeUnitEnv_home_unit env) <> text ")" <+> text "->"
  $$ nest 4 (pprHPT $ homeUnitEnv_hpt env)

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
3. The rest of the compiler is modified appropiately to offset paths to the right places.
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

The main complication to the implementation is to do with offsetting paths appropiately.
For a long time it has been assumed that GHC will execute in the top-directory for a unit,
normally where the .cabal file is and all paths are interpreted relative to there.
When you have multiple home units then it doesn't make sense to pick one of these
units to choose as the base-unit, and you can't robustly change directories when
using parralelism.

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
