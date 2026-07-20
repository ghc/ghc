-- | The 'UnitIndex' is a 'UnitEnv' wide data structure that shares
-- external unit information across the 'UnitState' of all home units
-- (e.g., 'HomeUnitEnv') in a particular 'UnitEnv'.
--
-- It caches already read unit databases, all processed 'UnitInfo's and
-- the 'WireMap'.
--
-- This module is meant to be imported as @Index@.
--
-- A short overview of how the different types here related to 'UnitState', 'UnitEnv'
-- and the 'HomeUnitEnv'.
--
-- ┌─────────┐
-- │ UnitEnv │
-- └────┬────┘
--      ├───────────────────────┐
--      │                       │
-- ┌────▼──────┐          ┌─────▼─────┐
-- │HomeUnitEnv│          │ UnitIndex ├────────────────┐
-- └────┬──────┘          └───────────┘                │
--      │                                              │
--      │      Reads cached unit DBs                   │
-- ┌────▼──────┐          ┌─────────────────────┐      │
-- │ UnitState ├──────────>ExternalUnitDatabases◄──────┤
-- └────┬──┬───┘          └─────────────────────┘      │
--      │  └───────────────────────┐                   │
--      │   Writes new UnitInfos   │                   │
--      │   during initialisation  │                   │
-- ┌────▼────────┐        ┌────────v──────────┐        │
-- │ UnitInfoMap │        │ GlobalUnitInfoMap ◄────────┘
-- └────┬────────┘        └────────^──────────┘
--      │                          │
--      └──────────────────────────┘
--          UnitInfoMap references
--          GlobalUnitInfoMap values
--          (All UnitInfos are shared)
--
-- Open arrow @A ───> B@: A uses B.
-- Closed arrow @A ◄─── B@: A is a field of B.
--
-- Also, see Note [Sharing 'UnitInfo's across the 'UnitEnv'] for more technical discussion
-- about sharing 'UnitInfo's.
module GHC.Unit.External.Index (
  -- * The 'UnitIndexCache'.
  -- A mutable wrapper around 'UnitIndex'
  UnitIndexCache(..),
  initUnitIndexCache,
  readUnitIndex,
  modifyUnitIndexCache,
  clearUnitIndexCache,
  cacheExternalUnitDatabase,
  readExternalUnitDatabases,
  readExternalUnitDatabase,
  -- * 'UnitIndex'
  UnitIndex,
  emptyUnitIndex,
  wiringMap,
  unwiringMap,
  globalUnits,
  externalUnitDatabases,
  setWireMap,
  wireMapExists,
  addUnitInfoMap,
  -- * 'GlobalUnitInfoMap'
  GlobalUnitInfoMap,
  lookupGlobalUnitInfoMap,
  emptyGlobalUnitInfoMap,
  mkGlobalUnitInfoMap,
  -- * 'GlobalUnitKey'
  GlobalUnitKey,
  UnitAbiHash,
  mkGlobalUnitKey,
  globalUnitKeyFromUnitInfo,
  -- * Wired-in units
  setupWiredInUnits,
  updateWiredInUnitIndex,
  unwireUnit,
  updateWiredInUnits,
  updateWiredInUnitsInUnitInfo,
  updateWiredInUnitIdInModule,
  -- * Reading external unit databases into the 'UnitIndexCache'
  readOrGetUnitDatabase,
  readUnitDatabases,
) where

import GHC.Prelude

import GHC.Data.OsPath
import GHC.Data.ShortText qualified as ST
import GHC.Types.Unique.Map
import GHC.Unit.Database
import GHC.Unit.External.Database
import GHC.Unit.External.Visibility
import GHC.Unit.External.Wired
import GHC.Unit.Info
import GHC.Unit.Types
import GHC.Utils.Logger

import Control.Monad (liftM)
import Data.Either
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)

-- ----------------------------------------------------------------------------
-- UnitIndex
-- ----------------------------------------------------------------------------

-- | Mutable version of 'UnitIndex'.
--
-- The 'UnitIndexCache' ensures that all calls to 'initUnits' will
-- share the 'UnitInfo' if it is possible.
--
-- To share the 'UnitInfo', the 'UnitInfo' needs to be fully-resolved, i.e., its wired-in
-- dependencies and modules need to be replaced with the 'UnitId' of the wired-in unit.
-- Thus, the 'UnitIndexCache' caches both the global 'WireMap' and the 'UnitInfoMap'.
--
-- The 'WireMap' is globally valid, as other parts of the compiler rely on the fact
-- that only one instance of wired-in units is used.
--
-- Memory Invariant: The 'UnitIndexCache' is the root object for retaining fully-resolved
-- 'UnitInfo'. 'UnitState' is expected to reference only 'UnitInfo's from the 'UnitIndexCache'.
-- There is exactly one fully-resolved 'UnitInfo' alive for each external unit per unit database.
--
-- A second instance may or may not be stored in the 'externalUnitDatabases', which represent the
-- in-memory cache of the on-disk unit databases.
newtype UnitIndexCache = UnitIndexCache
  { uic_index :: IORef UnitIndex
  }

initUnitIndexCache :: IO UnitIndexCache
initUnitIndexCache =
  UnitIndexCache <$> IORef.newIORef emptyUnitIndex

readUnitIndex :: UnitIndexCache -> IO UnitIndex
readUnitIndex uic =
  IORef.readIORef (uic_index uic)

modifyUnitIndexCache :: UnitIndexCache -> (UnitIndex -> UnitIndex) -> IO ()
modifyUnitIndexCache uic f =
  IORef.modifyIORef' (uic_index uic) f

clearUnitIndexCache :: UnitIndexCache -> IO ()
clearUnitIndexCache uic =
  modifyUnitIndexCache uic (const emptyUnitIndex)

cacheExternalUnitDatabase :: UnitIndexCache -> UnitDatabase UnitId -> IO ()
cacheExternalUnitDatabase uic db =
  modifyUnitIndexCache uic
    (\ ui ->
      ui
        { ui_externalUnitDatabases = insertExternalUnitDatabases db (ui_externalUnitDatabases ui)
        }
    )

readExternalUnitDatabases :: UnitIndexCache -> IO (ExternalUnitDatabases UnitId)
readExternalUnitDatabases uic =
  externalUnitDatabases <$> readUnitIndex uic

readExternalUnitDatabase :: UnitIndexCache -> OsPath -> IO (Maybe (UnitDatabase UnitId))
readExternalUnitDatabase uic path = do
  dbs <- readExternalUnitDatabases uic
  pure $ lookupExternalUnitDatabases path dbs

-- | Global index for external units that can be shared across multiple 'HomeUnitEnv's.
--
-- Allows sharing of the 'WireMap' and 'UnitInfo's that are stored in the 'UnitState'
-- of each 'HomeUnitEnv'.
--
-- See Note [Sharing 'UnitInfo's across the 'UnitEnv'] for details about memory usage.
data UnitIndex = UnitIndex
  { ui_wireMap :: !WireMap
    -- ^ A mapping from database unit keys to wired in unit ids.
    --
    -- At the moment, the 'WireMap' is global, there can only be one version of
    -- a wired-in package.
  , ui_unwireMap :: !UnwireMap
    -- ^ A mapping from wired in unit ids to unit keys from the database.
    --
    -- At the moment, the 'UnwireMap' is global, there can only be one version of
    -- a wired-in package.
  , ui_unitInfoMap :: !GlobalUnitInfoMap
    -- ^ A global map for all fully-resolved 'UnitInfo's.
    --
    -- See Note [Sharing 'UnitInfo's across the 'UnitEnv'] for more details
    -- what we use this for and what a fully-resolved 'UnitInfo' is.
  , ui_externalUnitDatabases :: !(ExternalUnitDatabases UnitId)
    -- ^ Cache the already processed unit databases in-memory.
    --
    -- These 'GenericUnitInfo's have their paths resolved, e.g., no @${pkgroot}@ is
    -- present any more.
  }

-- | Get the 'WireMap'.
--
-- At the moment, the 'WireMap' is global, there can only be one version of
-- a wired-in package.
wiringMap :: UnitIndex -> WireMap
wiringMap = ui_wireMap

-- | Get the 'UnwireMap'.
--
-- At the moment, the 'UnwireMap' is global, there can only be one version of
-- a wired-in package.
unwiringMap :: UnitIndex -> UnwireMap
unwiringMap = ui_unwireMap

-- | Access the already processed unit databases.
externalUnitDatabases :: UnitIndex -> ExternalUnitDatabases UnitId
externalUnitDatabases = ui_externalUnitDatabases

-- | Access the global map of fully-resolved 'UnitInfo's.
--
-- A 'UnitInfo' is fully-resolved, if its dependencies were updated to reference the
-- wired-in packages (e.g., 'wiringMap') and the wired-in packages are updated.
-- Further, the 'UnitInfo' is based on the 'ExternalUnitDatabases' results, resolving
-- variables such as @${pkgroot}@ in paths.
--
-- See Note [Sharing 'UnitInfo's across the 'UnitEnv'] for why this is helpful.
globalUnits :: UnitIndex -> GlobalUnitInfoMap
globalUnits = ui_unitInfoMap

emptyUnitIndex :: UnitIndex
emptyUnitIndex = UnitIndex
  { ui_wireMap = emptyWireMap
  , ui_unwireMap = emptyUnwireMap
  , ui_unitInfoMap = emptyGlobalUnitInfoMap
  , ui_externalUnitDatabases = emptyExternalUnitDatabases
  }

-- | Set the 'WireMap' of 'UnitIndex'.
-- Automatically computes the 'UnwireMap' based on the 'WireMap'.
setWireMap :: WireMap -> UnitIndex -> UnitIndex
setWireMap wired_map unit_index =
  unit_index
    { ui_wireMap = wired_map
    , ui_unwireMap = unwiringMapFromWireMap wired_map
    }

-- | Is there already a 'WireMap' in this 'UnitIndex'?
--
wireMapExists :: UnitIndex -> Bool
wireMapExists unit_index =
  not $ isWireMapEmpty (ui_wireMap unit_index)

addUnitInfoMap :: UnitInfoMap -> UnitIndex -> UnitIndex
addUnitInfoMap unit_info_map unit_index =
  unit_index
    { ui_unitInfoMap =
        -- Order should not matter, either it is exactly the same 'UnitInfo',
        -- or a new one.
        GlobalUnitInfoMap $ plusUniqMap_C Map.union newEntriesMap oldMap
    }
  where
    GlobalUnitInfoMap newEntriesMap = mkGlobalUnitInfoMap $ nonDetUniqMapToList unit_info_map
    GlobalUnitInfoMap oldMap = ui_unitInfoMap unit_index

-- ----------------------------------------------------------------------------
-- GlobalUnitInfoMap
-- ----------------------------------------------------------------------------

type UnitAbiHash = ST.ShortText

-- | Like a 'UnitInfoMap' but stores all 'UnitInfo's.
--
-- It is keyed by the 'UnitId' and the 'UnitInfo's 'UnitAbiHash'.
-- In modern cabal, there should never be a conflict of 'UnitId's, as cabal
-- hashes the Abi, dependency hashes, source hashes and more.
--
-- However, a user can choose a conflicting 'UnitId', causing a conflict after all.
-- We use the 'UnitAbiHash' for disambiguation. If both 'UnitId' and 'UnitAbiHash' are
-- identical in separate unit databases, we can assume they are the same unit, according
-- to the documentation of GHC.
newtype GlobalUnitInfoMap = GlobalUnitInfoMap (UniqMap UnitId (Map UnitAbiHash UnitInfo))

-- | Lookup the 'UnitInfo' in the 'GlobalUnitInfoMap'.
--
-- This does not check whether the 'GlobalUnitKey' refers to a unit that needs to be resolved
-- in the 'WireMap'.
-- For example, if the 'UnitId' is @ghc-internal-<version>@ (and @ghc-internal@ is a wired-in package),
-- then 'lookupGlobalUnitInfoMap' won't find it, as in the 'GlobalUnitInfoMap', the key is @ghc-internal@.
lookupGlobalUnitInfoMap :: GlobalUnitKey -> GlobalUnitInfoMap -> Maybe UnitInfo
lookupGlobalUnitInfoMap (GlobalUnitKey uid abiHash) (GlobalUnitInfoMap globalMap) =
  case lookupUniqMap globalMap uid of
    Nothing -> Nothing
    Just sameUnitId -> Map.lookup abiHash sameUnitId

mkGlobalUnitInfoMap :: [(UnitId, UnitInfo)] -> GlobalUnitInfoMap
mkGlobalUnitInfoMap unitInfos =
  GlobalUnitInfoMap $ listToUniqMap_C Map.union (map mkEntry unitInfos)
 where
  mkEntry (uid, v) = (uid, Map.singleton (unitAbiHash v) v)

emptyGlobalUnitInfoMap :: GlobalUnitInfoMap
emptyGlobalUnitInfoMap = GlobalUnitInfoMap emptyUniqMap

-- ----------------------------------------------------------------------------
-- GlobalUnitKey
-- ----------------------------------------------------------------------------

-- | A 'GlobalUnitKey' is a key that can globally identify a 'UnitInfo', not just
-- in the 'UnitInfoMap'.
--
-- The 'UnitId' and 'UnitAbiHash' uniquely identify a 'UnitInfo'.
data GlobalUnitKey =
  GlobalUnitKey
    !UnitId -- ^ Unit Id of the 'UnitInfo'
    !UnitAbiHash -- ^ ABI hash of the 'UnitInfo'

globalUnitKeyFromUnitInfo :: UnitInfo -> GlobalUnitKey
globalUnitKeyFromUnitInfo ui = mkGlobalUnitKey (unitId ui) (unitAbiHash ui)

mkGlobalUnitKey :: UnitId -> UnitAbiHash -> GlobalUnitKey
mkGlobalUnitKey = GlobalUnitKey

-- -----------------------------------------------------------------------------
-- Wired-in units
--
-- See Note [Wired-in units] in GHC.Unit.Types

-- | Find the wired-in units in the given '[UnitInfo]' if there isn't already
-- one in the 'UnitIndexCache'. If there is, simply return the existing 'WireMap'.
-- Otherwise, update the 'wiringMap' in the 'UnitIndexCache'
setupWiredInUnits :: Logger -> UnitPrecedenceMap -> [UnitInfo] -> VisibilityMap -> UnitIndexCache -> IO WireMap
setupWiredInUnits logger prec_map pkgs vis_map unit_index = do
  ui <- readUnitIndex unit_index
  if not $ wireMapExists ui
    then do
      wmap <- findWiredInUnits logger prec_map pkgs vis_map
      modifyUnitIndexCache unit_index (setWireMap wmap)
      pure wmap
    else do
      pure $ ui_wireMap ui

-- | Resolve the wired-in units in the '[UnitInfo]'.
-- If the fully-resolved can be found in 'UnitIndexCache', then we use it.
-- If the resolved 'UnitInfo' is new, we immediately cache it in the 'UnitIndexCache'.
-- We return the fully-resolved 'UnitInfo' list in the same order as provided.
updateWiredInUnitIndex :: WireMap -> [UnitInfo] -> UnitIndexCache -> IO [UnitInfo]
updateWiredInUnitIndex wired_map pkgs unit_index = do
  ui <- readUnitIndex unit_index
  let
    all_pkgs = updateWiredInUnits wired_map (ui_unitInfoMap ui) pkgs
    (new_pkgs', _pkgs_set) = partitionEithers all_pkgs
  -- Make sure we force the 'UnitInfo' here.
  -- Otherwise, we will retain a reference to the old 'UnitInfo'
  new_pkgs <- traverse evaluateUnitInfoLists new_pkgs'
  modifyUnitIndexCache unit_index (addUnitInfoMap $ mkUnitInfoMap new_pkgs)
  pure (map (either id id) all_pkgs)

-- | Given a wired-in 'Unit', "unwire" it into the 'Unit'
-- that it was recorded as in the package database.
unwireUnit :: UnitIndex -> Unit -> Unit
unwireUnit state uid@(RealUnit (Definite def_uid)) =
    maybe uid (RealUnit . Definite) (lookupUnwireMap def_uid (unwiringMap state))
unwireUnit _ uid = uid

updateWiredInUnits :: WireMap -> GlobalUnitInfoMap -> [UnitInfo] -> [Either UnitInfo UnitInfo]
updateWiredInUnits wiredInMap knownInfos pkgs =
  map (updateWiredInUnitsInUnitInfo wiredInMap knownInfos) pkgs

updateWiredInUnitsInUnitInfo :: WireMap -> GlobalUnitInfoMap -> UnitInfo -> Either UnitInfo UnitInfo
updateWiredInUnitsInUnitInfo wiredInMap knownInfos pkg =
  let
    upd_wired_in_pkg wiredInUnitId pkg =
      pkg { unitId         = wiredInUnitId
          , unitInstanceOf = wiredInUnitId
              -- every non instantiated unit is an instance of
              -- itself (required by Backpack...)
              --
              -- See Note [About units] in GHC.Unit
          }

    upd_deps pkg = pkg {
          unitDepends = map (upd_wired_in wiredInMap) (unitDepends pkg),
          unitExposedModules
            = map (\(k,v) -> (k, fmap (updateWiredInUnitIdInModule wiredInMap) v))
                  (unitExposedModules pkg)
        }

    -- First check whether this is a wired-in unit.
    -- If it is, we need to use the wired-in 'UnitId' for looking up the UnitInfo in the
    -- 'GlobalUnitInfoMap'. We also update the 'UnitInfo' to use the wired-in unitId, as that's
    -- how we are going to use it, if it isn't already present in the 'GlobalUnitInfoMap'.
    -- Otherwise, we simply look up the 'UnitInfo' in the 'GlobalUnitInfoMap'.
    (candidatePkg, key) =
      case lookupWireMap (unitId pkg) wiredInMap of
        Just wiredIn -> (upd_wired_in_pkg wiredIn pkg, mkGlobalUnitKey wiredIn (unitAbiHash pkg))
        Nothing      -> (pkg, globalUnitKeyFromUnitInfo pkg)
  in
    -- If the UnitInfo is not already present in the 'GlobalUnitInfoMap', we need to update
    -- all references to wired-in units.
    case lookupGlobalUnitInfoMap key knownInfos of
      Just ui -> Right ui
      Nothing -> Left (upd_deps candidatePkg)

-- Helper functions for rewiring Module and Unit.  These
-- rewrite Units of modules in wired-in packages to the form known to the
-- compiler, as described in Note [Wired-in units] in GHC.Unit.Types.
--
-- For instance, base-4.9.0.0 will be rewritten to just base, to match
-- what appears in GHC.Builtin.Names.

updateWiredInUnitIdInModule :: WireMap -> Module -> Module
updateWiredInUnitIdInModule wiredInMap (Module uid m) = Module (upd_wired_in_uid wiredInMap uid) m

upd_wired_in_uid :: WireMap -> Unit -> Unit
upd_wired_in_uid wiredInMap u = case u of
   HoleUnit -> HoleUnit
   RealUnit (Definite uid) -> RealUnit (Definite (upd_wired_in wiredInMap uid))
   VirtUnit indef_uid ->
      VirtUnit $ mkInstantiatedUnit
        (instUnitInstanceOf indef_uid)
        (map (\(x,y) -> (x,updateWiredInUnitIdInModule wiredInMap y)) (instUnitInsts indef_uid))

upd_wired_in :: WireMap -> UnitId -> UnitId
upd_wired_in wiredInMap key
    | Just key' <- lookupWireMap key wiredInMap = key'
    | otherwise = key

-- -----------------------------------------------------------------------------
-- Reading the unit database(s) into the 'UnitIndexCache'

readUnitDatabases :: Logger -> UnitIndexCache -> UnitDbConfig -> IO [UnitDatabase UnitId]
readUnitDatabases logger db_cache cfg = do
  conf_refs <- getUnitDbRefs cfg
  confs     <- liftM catMaybes $ mapM (resolveUnitDatabase cfg) conf_refs
  mapM (readOrGetUnitDatabase logger db_cache cfg) confs

-- | Get the cached 'UnitDatabase' or read the 'UnitDatabase' at the given location.
readOrGetUnitDatabase :: Logger -> UnitIndexCache -> UnitDbConfig -> OsPath -> IO (UnitDatabase UnitId)
readOrGetUnitDatabase logger db_cache cfg conf_file =
  readExternalUnitDatabase db_cache conf_file >>= \ case
    Nothing -> do
      new_db <- readUnitDatabase logger cfg conf_file
      cacheExternalUnitDatabase db_cache new_db
      pure new_db
    Just db ->
      pure db
