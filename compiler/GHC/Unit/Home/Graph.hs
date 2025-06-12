-- | A 'HomeUnitGraph' (HUG) collects information about all the home units.
-- Crucially, each node in a 'HomeUnitGraph' includes a 'HomePackageTable'.
--
-- Often, we don't want to query just a single 'HomePackageTable', but rather all
-- 'HomePackageTable's of all home units.
--
-- This module is responsible for maintaining this bridge between querying all
-- home units vs querying the home package table directly. Think 'lookupHug' vs
-- 'lookupHpt', 'hugAllInstances' vs 'hptAllInstances', where the @hug@ version
-- replies with information from all home units, and the @hpt@ version with
-- information pertaining to a single home unit.
--
-- Meant to be imported qualified as @HUG@.
-- Example usage:
--
-- @
-- import GHC.Unit.Home.Graph (HomeUnitGraph, HomeUnitEnv)
-- import qualified GHC.Unit.Home.Graph as HUG
-- usage = ... HUG.insertHug hug uid modname modinfo ...
-- @
module GHC.Unit.Home.Graph
  ( HomeUnitGraph
  , HomeUnitEnv(..)
  , mkHomeUnitEnv

  -- * Operations
  , addHomeModInfoToHug
  , restrictHug
  , renameUnitId
  , allUnits
  , updateUnitFlags

  -- ** Lookups
  , lookupHug
  , lookupHugByModule
  , lookupHugUnit
  , lookupHugUnitId
  , lookupAllHug
  , memberHugUnit
  , memberHugUnitId
  -- ** Reachability
  , transitiveHomeDeps

  -- * Very important queries
  , allInstances
  , allAnns
  , allCompleteSigs

  -- * Utilities
  , hugSCCs
  , hugFromList

  -- ** Printing
  , pprHomeUnitGraph
  , pprHomeUnitEnv

  -- * Auxiliary internal structure
  , UnitEnvGraph(..)
  , unitEnv_lookup_maybe
  , unitEnv_foldWithKey
  , unitEnv_singleton
  , unitEnv_adjust
  , unitEnv_keys
  , unitEnv_insert
  , unitEnv_new
  , unitEnv_lookup
  , unitEnv_traverseWithKey
  , unitEnv_assocs
  ) where

import GHC.Prelude

import GHC.Driver.DynFlags
import GHC.Unit.Home
import GHC.Unit.Home.ModInfo
import GHC.Unit.Home.PackageTable
import GHC.Unit.Module
import GHC.Unit.Module.ModIface
import GHC.Unit.State
import GHC.Utils.Monad (mapMaybeM)
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Core.FamInstEnv

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Data.Maybe
import GHC.Data.Graph.Directed

import GHC.Types.Annotations
import GHC.Types.CompleteMatch
import GHC.Core.InstEnv


-- | Get all 'CompleteMatches' (arising from COMPLETE pragmas) present across
-- all home units.
allCompleteSigs :: HomeUnitGraph -> IO CompleteMatches
allCompleteSigs hug = foldr go (pure []) hug where
  go hue = liftA2 (++) (hptCompleteSigs (homeUnitEnv_hpt hue))

-- | Find all the instance declarations (of classes and families) from
-- the Home Package Table filtered by the provided predicate function.
-- Used in @tcRnImports@, to select the instances that are in the
-- transitive closure of imports from the currently compiled module.
allInstances :: HomeUnitGraph -> IO (InstEnv, [FamInst])
allInstances hug = foldr go (pure (emptyInstEnv, [])) hug where
  go hue = liftA2 (\(a,b) (a',b') -> (a `unionInstEnv` a', b ++ b'))
                  (hptAllInstances (homeUnitEnv_hpt hue))

allAnns :: HomeUnitGraph -> IO AnnEnv
allAnns hug = foldr go (pure emptyAnnEnv) hug where
  go hue = liftA2 plusAnnEnv (hptAllAnnotations (homeUnitEnv_hpt hue))

--------------------------------------------------------------------------------
-- HomeUnitGraph (HUG)
--------------------------------------------------------------------------------

type HomeUnitGraph = UnitEnvGraph HomeUnitEnv

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

mkHomeUnitEnv :: UnitState -> Maybe [UnitDatabase UnitId] -> DynFlags -> HomePackageTable -> Maybe HomeUnit -> HomeUnitEnv
mkHomeUnitEnv us dbs dflags hpt home_unit = HomeUnitEnv
  { homeUnitEnv_units = us
  , homeUnitEnv_unit_dbs = dbs
  , homeUnitEnv_dflags = dflags
  , homeUnitEnv_hpt = hpt
  , homeUnitEnv_home_unit = home_unit
  }

--------------------------------------------------------------------------------
-- * Operations on HUG
--------------------------------------------------------------------------------

-- | Add an entry to the 'HomePackageTable' under the unit of that entry.
addHomeModInfoToHug :: HomeModInfo -> HomeUnitGraph -> IO ()
addHomeModInfoToHug hmi hug =
  case unitEnv_lookup_maybe hmi_unit hug of
    Nothing -> pprPanic "addHomeInfoToHug" (ppr hmi_mod)
    Just hue -> do
      addHomeModInfoToHpt hmi (homeUnitEnv_hpt hue)
  where
    hmi_mod :: Module
    hmi_mod  = mi_module (hm_iface hmi)
    hmi_unit = toUnitId (moduleUnit hmi_mod)

-- | Thin each HPT variable to only contain keys from the given dependencies.
-- This is used at the end of upsweep to make sure that only completely successfully loaded
-- modules are visible for subsequent operations.
restrictHug :: [(UnitId, [HomeModInfo])] -> HomeUnitGraph -> IO ()
restrictHug deps hug = unitEnv_foldWithKey (\k uid hue -> restrict_one uid hue >> k) (return ()) hug
  where
    deps_map = Map.fromList deps
    restrict_one uid hue  =
      restrictHpt (homeUnitEnv_hpt hue) (Map.findWithDefault [] uid deps_map)

-- | Rename a unit id in the 'HomeUnitGraph'
--
-- @'renameUnitId' oldUnit newUnit hug@, if @oldUnit@ is not found in @hug@, returns 'Nothing'.
-- If it exists, the result maps @newUnit@ to the 'HomeUnitEnv' of the
-- @oldUnit@ (and @oldUnit@ is removed from @hug@)
renameUnitId :: UnitId -> UnitId -> HomeUnitGraph -> Maybe HomeUnitGraph
renameUnitId oldUnit newUnit hug = case unitEnv_lookup_maybe oldUnit hug of
  Nothing -> Nothing
  Just oldHue -> pure $
    unitEnv_insert newUnit oldHue $
    unitEnv_delete oldUnit hug

-- | Retrieve all 'UnitId's of units in the 'HomeUnitGraph'.
allUnits :: HomeUnitGraph -> Set.Set UnitId
allUnits = unitEnv_keys

-- | Set the 'DynFlags' of the 'HomeUnitEnv' for unit in the 'HomeModuleGraph'
updateUnitFlags :: UnitId -> (DynFlags -> DynFlags) -> HomeUnitGraph -> HomeUnitGraph
updateUnitFlags uid f = unitEnv_adjust update uid
  where
    update hue = hue { homeUnitEnv_dflags = f (homeUnitEnv_dflags hue) }

--------------------------------------------------------------------------------
-- ** Reachability
--------------------------------------------------------------------------------

-- | Compute the transitive closure of a unit in the 'HomeUnitGraph'.
-- If the argument unit is not present in the graph returns Nothing.
transitiveHomeDeps :: UnitId -> HomeUnitGraph -> Maybe [UnitId]
transitiveHomeDeps uid hug = case lookupHugUnitId uid hug of
  Nothing -> Nothing
  Just hue -> Just $
    Set.toList (loop (Set.singleton uid) (homeUnitDepends (homeUnitEnv_units hue)))
    where
      loop acc [] = acc
      loop acc (uid:uids)
        | uid `Set.member` acc = loop acc uids
        | otherwise =
          let hue = homeUnitDepends
                    . homeUnitEnv_units
                    . expectJust
                    $ lookupHugUnitId uid hug
          in loop (Set.insert uid acc) (hue ++ uids)

--------------------------------------------------------------------------------
-- ** Lookups
--------------------------------------------------------------------------------

-- | Lookup the 'HomeModInfo' of a 'Module' in the 'HomeUnitGraph' given its
-- 'UnitId' and 'ModuleName' (via the 'HomePackageTable' of the corresponding unit)
lookupHug :: HomeUnitGraph -> UnitId -> ModuleName -> IO (Maybe HomeModInfo)
lookupHug hug uid mod = do
  case unitEnv_lookup_maybe uid hug of
    Nothing -> pure Nothing
    Just hue -> lookupHpt (homeUnitEnv_hpt hue) mod

-- | Lookup the 'HomeModInfo' of a 'Module' in the 'HomeUnitGraph' (via the 'HomePackageTable' of the corresponding unit)
lookupHugByModule :: Module -> HomeUnitGraph -> IO (Maybe HomeModInfo)
lookupHugByModule mod hug =
  case lookupHugUnit (moduleUnit mod) hug of
    Nothing -> pure Nothing
    Just env -> lookupHptByModule (homeUnitEnv_hpt env) mod

-- | Lookup all 'HomeModInfo' that have the same 'ModuleName' as the given 'ModuleName'.
-- 'ModuleName's are not unique in the case of multiple home units, so there can be
-- more than one possible 'HomeModInfo'.
--
-- You should always prefer 'lookupHug' and 'lookupHugByModule' when possible.
lookupAllHug :: HomeUnitGraph -> ModuleName -> IO [HomeModInfo]
lookupAllHug hug mod = mapMaybeM (\uid -> lookupHug hug uid mod) (Set.toList $ unitEnv_keys hug)

-- | Lookup a 'HomeUnitEnv' by 'UnitId' in a 'HomeUnitGraph'
lookupHugUnitId :: UnitId -> HomeUnitGraph -> Maybe HomeUnitEnv
lookupHugUnitId = unitEnv_lookup_maybe

-- | Check whether the 'UnitId' is present in the 'HomeUnitGraph'
memberHugUnitId :: UnitId -> HomeUnitGraph -> Bool
memberHugUnitId u = isJust . lookupHugUnitId u

-- | Lookup up the 'HomeUnitEnv' by the 'Unit' in the 'HomeUnitGraph'.
-- If the 'Unit' can be turned into a 'UnitId', we behave identical to 'lookupHugUnitId'.
--
-- A 'HoleUnit' is never part of the 'HomeUnitGraph', only instantiated 'Unit's
lookupHugUnit :: Unit -> HomeUnitGraph -> Maybe HomeUnitEnv
lookupHugUnit unit hug =
  if isHoleUnit unit
    then Nothing
    else lookupHugUnitId (toUnitId unit) hug

-- | Check whether the 'Unit' is present in the 'HomeUnitGraph'
--
-- A 'HoleUnit' is never part of the 'HomeUnitGraph', only instantiated 'Unit's
memberHugUnit :: Unit -> HomeUnitGraph -> Bool
memberHugUnit u = isJust . lookupHugUnit u

--------------------------------------------------------------------------------
-- * Internal representation map
--------------------------------------------------------------------------------
-- Note: we purposefully do not export functions like "elems" to maintain a
-- good clean interface with the HUG.

type UnitEnvGraphKey = UnitId

newtype UnitEnvGraph v = UnitEnvGraph
  { unitEnv_graph :: Map UnitEnvGraphKey v
  } deriving (Functor, Foldable, Traversable)

unitEnv_new :: Map UnitEnvGraphKey v -> UnitEnvGraph v
unitEnv_new m =
  UnitEnvGraph
    { unitEnv_graph = m
    }

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

unitEnv_singleton :: UnitEnvGraphKey -> v -> UnitEnvGraph v
unitEnv_singleton active m = UnitEnvGraph
  { unitEnv_graph = Map.singleton active m
  }

unitEnv_lookup_maybe :: UnitEnvGraphKey -> UnitEnvGraph v -> Maybe v
unitEnv_lookup_maybe u env = Map.lookup u (unitEnv_graph env)

unitEnv_keys :: UnitEnvGraph v -> Set.Set UnitEnvGraphKey
unitEnv_keys env = Map.keysSet (unitEnv_graph env)

unitEnv_foldWithKey :: (b -> UnitEnvGraphKey -> a -> b) -> b -> UnitEnvGraph a -> b
unitEnv_foldWithKey f z (UnitEnvGraph g)= Map.foldlWithKey' f z g

unitEnv_lookup :: UnitEnvGraphKey -> UnitEnvGraph v -> v
unitEnv_lookup u env = expectJust $ unitEnv_lookup_maybe u env

unitEnv_traverseWithKey :: Applicative f => (UnitEnvGraphKey -> a -> f b) -> UnitEnvGraph a -> f (UnitEnvGraph b)
unitEnv_traverseWithKey f unitEnv =
  UnitEnvGraph <$> Map.traverseWithKey f (unitEnv_graph unitEnv)

unitEnv_assocs :: UnitEnvGraph a -> [(UnitEnvGraphKey, a)]
unitEnv_assocs (UnitEnvGraph x) = Map.assocs x

--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

hugSCCs :: HomeUnitGraph -> [SCC UnitId]
hugSCCs hug = sccs where
  mkNode :: (UnitId, HomeUnitEnv) -> Node UnitId UnitId
  mkNode (uid, hue) = DigraphNode uid uid (homeUnitDepends (homeUnitEnv_units hue))
  nodes = map mkNode (Map.toList $ unitEnv_graph hug)

  sccs = stronglyConnCompFromEdgedVerticesOrd nodes

hugFromList :: [(UnitId, HomeUnitEnv)] -> HomeUnitGraph
hugFromList = UnitEnvGraph . Map.fromList

pprHomeUnitGraph :: HomeUnitGraph -> IO SDoc
pprHomeUnitGraph unitEnv = do
  docs <- mapM (\(k, v) -> pprHomeUnitEnv k v) $ Map.assocs $ unitEnv_graph unitEnv
  return $ vcat docs

pprHomeUnitEnv :: UnitId -> HomeUnitEnv -> IO SDoc
pprHomeUnitEnv uid env = do
  hptDoc <- pprHPT $ homeUnitEnv_hpt env
  return $
    ppr uid <+> text "(flags:" <+> ppr (homeUnitId_ $ homeUnitEnv_dflags env) <> text "," <+> ppr (fmap homeUnitId $ homeUnitEnv_home_unit env) <> text ")" <+> text "->"
    $$ nest 4 hptDoc

