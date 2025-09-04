{-# LANGUAGE LambdaCase #-}
-- | The 'HomePackageTable' (HPT) contains information about all modules that are part
-- of a home package. At its core, the information for each module is a
-- 'HomeModInfo'.
--
-- During upsweep, the HPT is a monotonically increasing data structure: it
-- only ever gets extended by inserting modules which are loaded and for which
-- we discover the information required to construct a 'ModInfo'.
--
-- There should only ever exist one single HPT for any given home unit. It's
-- crucial we don't accidentally leak HPTs (e.g. by filtering it, which used to
-- happen -- #25511), so the HPT is mutable and only its reference should be shared.
-- This is alright because the modules don't change throughout compilation.
--
-- :::WARNING:::
-- If you intend to change this interface, consider carefully whether you are
-- exposing memory-leak footguns which may end up being misused in the compiler
-- eventually. For instance, if you really, really, end up needing a way to take
-- a snapshot of the IORef (think: do you really need to?), at least make
-- obvious in the name like `snapshotCopyHpt`.
--
-- Or, do you really need a function to traverse all modules in the HPT? It is
-- often better to keep the computation internal to this module, such as in
-- 'hptCollectObjects'...
module GHC.Unit.Home.PackageTable
  (
    HomePackageTable(..)
  , emptyHomePackageTable

    -- * Lookups in the HPT
  , lookupHpt
  , lookupHptByModule

    -- * Extending the HPT
  , addHomeModInfoToHpt
  , addHomeModInfosToHpt

    -- * Restrict the HPT
  , restrictHpt

    -- * Queries about home modules
  , hptCompleteSigs
  , hptAllInstances
  , hptAllFamInstances
  , hptAllAnnotations

    -- ** More Traversal-based queries
  , hptCollectDependencies
  , hptCollectObjects
  , hptCollectModules

    -- ** Memory dangerous queries
  , concatHpt

    -- * Utilities
  , pprHPT

    -- * Internals
    --
    -- | These provide access to the internals of the HomePackageTable to
    -- facilitate existing workflows that used the previous API. For instance,
    -- if you were listing out all elements or merging, you can keep doing so by reading
    -- the internal IO ref and then using the moduleenv contents directly.
    --
    -- In GHC itself these should be avoided, and other uses should justify why
    -- it is not sufficient to go through the intended insert-only API.
  , hptInternalTableRef
  , hptInternalTableFromRef

    -- * Legacy API
    --
    -- | This API is deprecated and meant to be removed.
  , addToHpt
  , addListToHpt
  ) where

import GHC.Prelude
import GHC.Data.Maybe

import Data.IORef
import Control.Monad ((<$!>))
import qualified Data.Set as Set

import GHC.Core.FamInstEnv
import GHC.Core.InstEnv
import GHC.Linker.Types
import GHC.Types.Annotations
import GHC.Types.CompleteMatch
import GHC.Types.Unique.DFM
import GHC.Unit.Home.ModInfo
import GHC.Unit.Module
import GHC.Unit.Module.Deps
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModIface
import GHC.Utils.Outputable
import GHC.Types.Unique (getUnique, getKey)
import qualified GHC.Data.Word64Set as W64

-- | Helps us find information about modules in the home package
newtype HomePackageTable = HPT {

    table :: IORef (DModuleNameEnv HomeModInfo)
    -- ^ Domain = modules in this home unit
    --
    -- This is an IORef because we want to avoid leaking HPTs (see the particularly bad #25511).
    -- Moreover, the HPT invariant allows mutability in this table without compromising thread safety or soundness.
    -- To recall:
    --   A query to the HPT should depend only on data relevant to that query, such that
    --   there being more or less unrelated entries in the HPT does not influence the result in any way.
    --
    -- Note that the HPT increases monotonically, except at certain barrier
    -- points like when 'restrictHpt' is called. At these barriers, it is safe
    -- to temporarily violate the HPT monotonicity.
    --
    -- The elements of this table may be updated (e.g. on rehydration).
  }

-- | Create a new 'HomePackageTable'.
--
-- Be careful not to share it across e.g. different units, since it uses a
-- mutable variable under the hood to keep the monotonically increasing list of
-- loaded modules.
emptyHomePackageTable :: IO HomePackageTable
-- romes:todo: use a MutableArray directly?
emptyHomePackageTable = do
  table <- newIORef emptyUDFM
  return HPT{table}

--------------------------------------------------------------------------------
-- * Lookups in the HPT
--------------------------------------------------------------------------------

-- | Lookup the 'HomeModInfo' of a module in the HPT, given its name.
lookupHpt :: HomePackageTable -> ModuleName -> IO (Maybe HomeModInfo)
lookupHpt HPT{table=hpt} mn = (`lookupUDFM` mn) <$!> readIORef hpt

-- | Lookup the 'HomeModInfo' of a 'Module' in the HPT.
lookupHptByModule :: HomePackageTable -> Module -> IO (Maybe HomeModInfo)
lookupHptByModule hpt mod
  = -- The HPT is indexed by ModuleName, not Module,
    -- we must check for a hit on the right Module
    lookupHpt hpt (moduleName mod) >>= pure . \case
      Just hm | mi_module (hm_iface hm) == mod -> Just hm
      _otherwise                               -> Nothing

--------------------------------------------------------------------------------
-- * Extending the HPT
--------------------------------------------------------------------------------

-- | Add a new module to the HPT.
--
-- An HPT is a monotonically increasing data structure, holding information about loaded modules in a package.
-- This is the main function by which the HPT is extended or updated.
--
-- When the module of the inserted 'HomeModInfo' does not exist, a new entry in
-- the HPT is created for that module name.
-- When the module already has an entry, inserting a new one entry in the HPT
-- will always overwrite the existing entry for that module.
addHomeModInfoToHpt :: HomeModInfo -> HomePackageTable -> IO ()
addHomeModInfoToHpt hmi hpt = addToHpt hpt (moduleName (mi_module (hm_iface hmi))) hmi

{-# DEPRECATED addToHpt "Deprecated in favour of 'addHomeModInfoToHpt', as the module at which a 'HomeModInfo' is inserted should always be derived from the 'HomeModInfo' itself." #-}
-- After deprecation cycle, move `addToHpt` to a `where` clause inside `addHomeModInfoToHpt`.
addToHpt :: HomePackageTable -> ModuleName -> HomeModInfo -> IO ()
addToHpt HPT{table=hptr} mn hmi = do
  atomicModifyIORef' hptr (\hpt -> (addToUDFM hpt mn hmi, ()))
  -- If the key already existed in the map, this insertion is overwriting
  -- the HMI of a previously loaded module (likely in rehydration).

-- | 'addHomeModInfoToHpt' for multiple module infos.
addHomeModInfosToHpt :: HomePackageTable -> [HomeModInfo] -> IO ()
addHomeModInfosToHpt hpt = mapM_ (flip addHomeModInfoToHpt hpt)

-- | Thin each HPT variable to only contain keys from the given dependencies.
-- This is used at the end of upsweep to make sure that only completely successfully loaded
-- modules are visible for subsequent operations.
--
-- This is an exception to the invariant of the HPT -- that it grows
-- monotonically, never removing entries -- which is safe as long as it is only
-- called at barrier points, such as the end of upsweep, when all threads are
-- done and we want to clean up failed entries.
restrictHpt :: HomePackageTable -> [HomeModInfo] -> IO ()
restrictHpt HPT{table=hptr} hmis =
  let key_set = map (getKey . getUnique . hmi_mod) hmis
      hmi_mod hmi = moduleName (mi_module (hm_iface hmi))
  in atomicModifyIORef' hptr (\hpt -> (udfmRestrictKeysSet hpt (W64.fromList key_set), ()))

{-# DEPRECATED addListToHpt "Deprecated in favour of 'addHomeModInfosToHpt', as the module at which a 'HomeModInfo' is inserted should always be derived from the 'HomeModInfo' itself." #-}
-- After deprecation cycle, remove.
addListToHpt :: HomePackageTable -> [(ModuleName, HomeModInfo)] -> IO ()
addListToHpt hpt = mapM_ (uncurry (addToHpt hpt))

----------------------------------------------------------------------------------
---- * Queries
----------------------------------------------------------------------------------

-- | Get all 'CompleteMatches' (arising from COMPLETE pragmas) present in all
-- modules from this unit's HPT.
hptCompleteSigs :: HomePackageTable -> IO CompleteMatches
hptCompleteSigs = concatHpt (md_complete_matches . hm_details)

-- | Find all the instance declarations (of classes and families) from this Home Package Table
hptAllInstances :: HomePackageTable -> IO (InstEnv, [FamInst])
hptAllInstances hpt = do
  hits <- flip concatHpt hpt $ \mod_info -> do
     let details = hm_details mod_info
     return (md_insts details, md_fam_insts details)
  let (insts, famInsts) = unzip hits
  return (foldl' unionInstEnv emptyInstEnv insts, concat famInsts)

-- | Find all the family instance declarations from the HPT
hptAllFamInstances :: HomePackageTable -> IO (ModuleEnv FamInstEnv)
hptAllFamInstances = fmap mkModuleEnv . concatHpt (\hmi -> [(hmiModule hmi, hmiFamInstEnv hmi)])
  where
    hmiModule     = mi_module . hm_iface
    hmiFamInstEnv = extendFamInstEnvList emptyFamInstEnv
                      . md_fam_insts . hm_details

-- | All annotations from the HPT
hptAllAnnotations :: HomePackageTable -> IO AnnEnv
hptAllAnnotations = fmap mkAnnEnv . concatHpt (md_anns . hm_details)


--------------------------------------------------------------------------------
-- * Traversal-based queries
--------------------------------------------------------------------------------

-- | Collect the immediate dependencies of all modules in the HPT into a Set.
-- The immediate dependencies are given by the iface as @'dep_direct_pkgs' . 'mi_deps'@.
--
-- Note: This should be a query on the 'ModuleGraph', since we don't really
-- ever want to collect *all* dependencies. The current caller of this function
-- currently takes all dependencies only to then filter them with an ad-hoc transitive closure check.
-- See #25639
hptCollectDependencies :: HomePackageTable -> IO (Set.Set (IfaceImportLevel, UnitId))
hptCollectDependencies HPT{table} = do
  hpt <- readIORef table
  return $
    foldr (Set.union . dep_direct_pkgs . mi_deps . hm_iface) Set.empty hpt

-- | Collect the linkable object of all modules in the HPT.
-- The linkable objects are given by @'homeModInfoObject'@.
--
-- $O(n)$ in the number of modules in the HPT.
hptCollectObjects :: HomePackageTable -> IO [Linkable]
hptCollectObjects HPT{table} = do
  hpt <- readIORef table
  return $
    foldr ((:) . expectJust . homeModInfoObject) [] hpt

-- | Collect all module ifaces in the HPT
--
-- $O(n)$ in the number of modules in the HPT.
hptCollectModules :: HomePackageTable -> IO [Module]
hptCollectModules HPT{table} = do
  hpt <- readIORef table
  return $
    foldr ((:) . mi_module . hm_iface) [] hpt

--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

-- | Pretty print a 'HomePackageTable'.
--
-- Make sure you really do need to print the whole HPT before infusing too much
-- code with IO.
--
-- For instance, in the HUG, it suffices to print the unit-keys present in the
-- unit map in failed lookups.
pprHPT :: HomePackageTable -> IO SDoc
-- A bit arbitrary for now
pprHPT HPT{table=hptr} = do
  hpt <- readIORef hptr
  return $!
    pprUDFM hpt $ \hms ->
      vcat [ ppr (mi_module (hm_iface hm))
           | hm <- hms ]

----------------------------------------------------------------------------------
-- THE TYPE OF FOOTGUNS WE DON'T WANT TO EXPOSE
----------------------------------------------------------------------------------

-- eltsHpt :: HomePackageTable -> [HomeModInfo]
-- filterHpt :: (HomeModInfo -> Bool) -> HomePackageTable -> HomePackageTable
-- mapHpt :: (HomeModInfo -> HomeModInfo) -> HomePackageTable -> HomePackageTable
-- delFromHpt :: HomePackageTable -> ModuleName -> HomePackageTable
-- listToHpt :: [(ModuleName, HomeModInfo)] -> HomePackageTable
-- listHMIToHpt :: [HomeModInfo] -> HomePackageTable

----------------------------------------------------------------------------------
-- Would be fine, but may lead to linearly traversing the HPT unnecessarily
-- (e.g. `lastLoadedKey` superseded bad usages)
----------------------------------------------------------------------------------

-- allHpt :: (HomeModInfo -> Bool) -> HomePackageTable -> Bool
-- allHpt = allUDFM

-- anyHpt :: (HomeModInfo -> Bool) -> HomePackageTable -> Bool
-- anyHpt = anyUDFM

----------------------------------------------------------------------------------
-- Would be ok to expose this function very /careful/ with the argument function
----------------------------------------------------------------------------------

-- | Like @concatMap f . 'eltsHpt'@, but filters out all 'HomeModInfo' for which
-- @f@ returns the empty list before doing the sort inherent to 'eltsUDFM'.
--
-- If this function is ever exposed from the HPT module, make sure the
-- argument function doesn't introduce leaks.
concatHpt :: (HomeModInfo -> [a]) -> HomePackageTable -> IO [a]
concatHpt f HPT{table} = do
  hpt <- readIORef table
  return $ concat . eltsUDFM . mapMaybeUDFM g $ hpt
  where
    g hmi = case f hmi of { [] -> Nothing; as -> Just as }

--------------------------------------------------------------------------------
-- * Internals (see haddocks!)
--------------------------------------------------------------------------------

-- | Gets the internal 'IORef' which holds the 'HomeModInfo's of this HPT.
-- Use with care.
hptInternalTableRef :: HomePackageTable -> IORef (DModuleNameEnv HomeModInfo)
hptInternalTableRef = table

-- | Construct a HomePackageTable from the IORef.
-- Use with care, only if you can really justify going around the intended insert-only API.
hptInternalTableFromRef :: IORef (DModuleNameEnv HomeModInfo) -> IO HomePackageTable
hptInternalTableFromRef ref = do
  return HPT {
    table = ref
  }

